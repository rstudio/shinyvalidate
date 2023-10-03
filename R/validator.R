# The overall implementation of InputValidator is extremely simple right now, at
# the expense of performance. My assumption is that validation rule functions
# will run extremely quickly and not have meaningful side effects. We have an
# opportunity to optimize so that validation rules only execute when 1) they are
# added, 2) the input value changes, 3) a reactive read that's performed during
# the validation invalidates, or 4) a prior rule around the input that was
# formerly failing now passes.
#
# The way to accomplish this would be to create a reactive expression for each
# rule, plus a reactiveValues object to track the rules for each input. Each
# input would also get a reactive expression for the overall validity of that
# input. It would look something like:
#
#   reactive({
#     for (rule in rv_rules[[id]]) {
#       result <- rule()  # rule is a reactive that has a read to input[[id]]
#       if (!is.null(result)) {
#         return(result)
#       }
#     }
#     return(NULL)
#   })
#
# Then sv$validate() would just be collecting all of these.

#' Shiny validation object
#'
#' @description An R6 class for adding realtime input validation to Shiny apps.
#'
#'   `InputValidator` objects are designed to be created as local variables in
#'   Shiny server functions and Shiny module server functions. The Shiny app
#'   author can register zero, one, or multiple validation rules for each input
#'   field in their UI, using the `InputValidator$add_rule()` method.
#'
#'   Once an `InputValidator` object is created and populated with rules, it can
#'   be used in a few ways:
#'
#'   1. The `InputValidator$enable()` method can be called to display real-time
#'      feedback to users about what inputs are failing validation, and why.
#'   2. The `InputValidator$is_valid()` method returns `TRUE` if and only if all
#'      of the validation rules are passing; this can be checked before
#'      executing actions that depend on the inputs being valid.
#'   3. The `InputValidator$validate()` method is a lower-level feature that
#'      directly returns information about what fields failed validation, and
#'      why.
#'
#'   It's possible to have multiple `InputValidator` objects for each Shiny app.
#'   One scenario where this makes sense is if an app contains multiple forms
#'   that are completely unrelated to each other; each form would have its own
#'   `InputValidator` instance with a distinct set of rules.
#'
#' @export
InputValidator <- R6::R6Class("InputValidator", cloneable = FALSE,
  private = list(
    session = NULL,
    enabled = FALSE,
    observer_handle = NULL,
    priority = numeric(0),
    condition_ = NULL,
    rules = NULL,
    validator_infos = NULL,
    is_child = FALSE
  ),
  public = list(
    #' @description
    #' Create a new validator object.
    #'
    #' @param priority When a validator object is enabled, it creates an
    #'   internal [shiny::observe()] to keep validation feedback in the UI
    #'   up-to-date. This parameter controls the priority of that observer. It's
    #'   highly recommended to keep this value higher than the priorities of any
    #'   observers that do actual work, so users see validation updates quickly.
    #' @param session The Shiny `session` object. (You should probably just use
    #'   the default.)
    initialize = function(priority = 1000, session = shiny::getDefaultReactiveDomain()) {
      if (is.null(session)) {
        stop("InputValidator objects should be created in the context of Shiny server functions or Shiny module server functions")
      }
      private$session <- session
      private$priority <- priority
      private$condition_ <- shiny::reactiveVal(NULL, label = "validator_condition")
      private$rules <- shiny::reactiveVal(list(), label = "validation_rules")
      private$validator_infos <- shiny::reactiveVal(list(), label = "child_validators")
      
      # Inject shinyvalidate dependencies (just once)
      if (!isTRUE(session$userData[["shinyvalidate-initialized"]])) {
        shiny::insertUI("body", "beforeEnd",
          list(htmldep(), htmltools::HTML("")),
          immediate = TRUE, session = session
        )
        session$userData[["shinyvalidate-initialized"]] <- TRUE
      }
    },
    #' @description For internal use only.
    #' @param validator An `InputValidator` object.
    parent = function(validator) {
      self$disable()
      private$is_child <- TRUE
    },
    #' @description Gets or sets a condition that overrides all of the rules in
    #'   this validator. Before performing validation, this validator will
    #'   execute the `cond` function. If `cond` returns `TRUE`, then
    #'   validation continues as normal; if `FALSE`, then the validation rules
    #'   will be skipped and treated as if they are all passing.
    #' @param cond If this argument is missing, then the method returns the
    #'   currently set condition function. If not missing, then `cond` must
    #'   be either a zero-argument function that returns `TRUE` or `FALSE`; a
    #'   single-sided formula that results in `TRUE` or `FALSE`; or `NULL`
    #'   (which is equivalent to `~ TRUE`).
    #' @return If `cond` is missing, then either `NULL` or a zero-argument
    #'   function; if `cond` is provided, then nothing of consequence is
    #'   returned.
    condition = function(cond) {
      if (missing(cond)) {
        private$condition_()
      } else {
        if (inherits(cond, "formula")) {
          cond <- rlang::as_function(cond)
        }
        if (!is.function(cond) && !is.null(cond)) {
          stop("`cond` argument must be NULL, function, or formula")
        }
        private$condition_(cond)
      }
    },
    #' @description Add another `InputValidator` object to this one, as a
    #'   "child". Any time this validator object is asked for its validity, it
    #'   will only return `TRUE` if all of its child validators are also valid;
    #'   and when this validator object is enabled (or disabled), then all of
    #'   its child validators are enabled (or disabled) as well.
    #'
    #'   This is intended to help with validating Shiny modules. Each module can
    #'   create its own `InputValidator` object and populate it with rules, then
    #'   return that object to the caller.
    #'
    #' @param validator An `InputValidator` object.
    #' @param label An optional label for the `InputValidator` object. By
    #'   default, a label will be automatically generated.
    add_validator = function(validator, label = deparse(substitute(validator))) {
      if (!inherits(validator, "InputValidator")) {
        stop("add_validator was called with an invalid `validator` argument; InputValidator object expected")
      }

      label <- paste0(label, collapse = "\n")
      
      validator$parent(self)
      private$validator_infos(c(shiny::isolate(private$validator_infos()),
        list(
          list(validator = validator, label = label)
        )
      ))
      invisible(self)
    },
    #' @description Add an input validation rule. Each input validation rule
    #'   applies to a single input. You can add multiple validation rules for a
    #'   single input by calling `add_rule()` multiple times; the first
    #'   validation rule for an input that fails will be used, and will prevent
    #'   subsequent rules for that input from executing.
    #'
    #' @param inputId A single-element character vector indicating the ID of the
    #'   input that this rule applies to. (Note that this name should _not_ be
    #'   qualified by a module namespace; e.g. pass `"x"` and not
    #'   `session$ns("x")`.)
    #' @param rule A function that takes (at least) one argument: the input's
    #'   value. The function should return `NULL` if it passes validation, and
    #'   if not, a single-element character vector or HTML tag containing an
    #'   error message to display to the user near the input. You can
    #'   alternatively provide a single-sided formula instead of a function,
    #'   using `.` as the variable name for the input value being validated.
    #' @param ... Optional: Additional arguments to pass to the `rule` function
    #'   whenever it is invoked.
    #' @param session. The session object to which the input belongs. (There's
    #'   almost never a reason to change this from the default.)
    add_rule = function(inputId, rule, ..., session. = shiny::getDefaultReactiveDomain()) {
      args <- rlang::list2(...)
      
      label <- deparse(substitute(rule))
      label <- paste0(label, collapse = "\n")

      if (inherits(rule, "formula")) {
        rule <- rlang::as_function(rule)
      }
      if (!is.function(rule)) {
        stop("Invalid `rule` argument; a function or formula is expected")
      }
      applied_rule <- function(value) {
        # Do this instead of purrr::partial because purrr::partial doesn't
        # support leaving a "hole" for the first argument
        do.call(rule, c(list(value), args))
      }
      rule_info <- list(rule = applied_rule, label = label, session = session.)
      private$rules(c(shiny::isolate(private$rules()), stats::setNames(list(rule_info), inputId)))
      invisible(self)
    },
    #' @description Begin displaying input validation feedback in the user
    #'   interface. Once enabled, this validator object will automatically keep
    #'   the feedback up-to-date. (It's safe to call the `enable()` method
    #'   on an already-enabled validator.) If this validator object has been
    #'   added to another validator object using `InputValidator$add_validator`,
    #'   calls to `enable()` on this validator will be ignored.
    enable = function() {
      if (private$is_child) {
        return()
      }
      if (!private$enabled) {
        shiny::withReactiveDomain(private$session, {
          private$observer_handle <- shiny::observe({
            results <- self$validate()
            private$session$sendCustomMessage("validation-jcheng5", results)
          }, priority = private$priority)
        })
        
        private$enabled <- TRUE
      }
      invisible(self)
    },
    #' @description Clear existing input validation feedback in the user
    #'   interface for all inputs represented in this validator's ruleset, and
    #'   stop providing feedback going forward. Once disabled, `enable()` can be
    #'   called to resume input validation.
    disable = function() {
      if (private$enabled) {
        private$observer_handle$destroy()
        private$observer_handle <- NULL
        private$enabled <- FALSE
        if (!private$is_child) {
          results <- self$validate()
          results <- lapply(results, function(x) NULL)
          private$session$sendCustomMessage("validation-jcheng5", results)
        }
      }
    },
    #' @description Returns `TRUE` if all input validation rules currently pass,
    #'   `FALSE` if not.
    fields = function() {
      fieldslist <- unlist(lapply(private$validator_infos(), function(validator_info) {
        validator_info$validator$fields()
      }))
      
      fullnames <- mapply(names(private$rules()), private$rules(), FUN = function(name, rule) {
        rule$session$ns(name)
      })
      
      unique(c(fieldslist, fullnames))
    },
    #' @description Returns `TRUE` if all input validation rules currently pass,
    #'   `FALSE` if not.
    is_valid = function() {
      results <- self$validate()
      all(vapply(results, is.null, logical(1), USE.NAMES = FALSE))
    },
    #' @description Run validation rules and gather results. For advanced usage
    #'   only; most apps should use the `is_valid()` and `enable()` methods
    #'   instead. The return value of this method is a named list, where the
    #'   names are (fully namespace qualified) input IDs, and the values are
    #'   either `NULL` (if the input value is passing) or a single-element
    #'   character vector describing a validation problem.
    validate = function() {
      verbose <- getOption("shinyvalidate.verbose", FALSE)
      if (isTRUE(verbose)) {
        message(verbose_prefix, "\U25BC InputValidator$validate() starting (",
          timestamp_str(), ")")
      }
      result <- self$`_validate_impl`(if (isTRUE(verbose)) verbose_indent else FALSE)
      if (isTRUE(verbose)) {
        message(verbose_prefix, "\U25B2 InputValidator$validate() complete (",
          timestamp_str(), ")")
      }
      result
    },
    # indent is character() if logging, FALSE if not.
    # Sadly this method cannot be private, because we need parent InputValidator
    # instances to call their childrens' _validate_impl methods.
    #' @description For internal use only.
    #' @param indent For internal use only.
    `_validate_impl` = function(indent) {
      console_log <- function(...) {
        if (is.character(indent)) {
          prefix <- paste0(verbose_prefix, indent)
          msg <- paste0(...)
          msg <- gsub("(^|\\n)", paste0("\\1", prefix), msg)
          message(msg)
        }
      }
      child_indent <- if (is.character(indent)) paste0(indent, verbose_indent) else FALSE
      
      condition <- private$condition_()
      skip_all <- is.function(condition) && !isTRUE(condition())
      if (skip_all) {
        console_log("condition() is FALSE, skipping validation")
        fields <- self$fields()
        return(setNames(rep_len(list(), length(fields)), fields))
      }
      
      dependency_results <- list()
      for (validator_info in private$validator_infos()) {
        console_log("Running child validator '", validator_info$label, "'")
        child_results <- validator_info$validator$`_validate_impl`(child_indent)
        dependency_results <- merge_results(dependency_results, child_results)
      }

      results <- list()
      mapply(names(private$rules()), private$rules(), FUN = function(name, rule) {
        fullname <- rule$session$ns(name)
        # Short-circuit if already errored or if skip_validation() was returned
        # by an earlier rule for this input
        if (!is.null(results[[fullname]])) {
          console_log("Skipping `", name, "`: ", rule$label)
          return()
        }
        
        console_log("Trying `", name, "`: ", rule$label)
        result <- tryCatch(
          shiny::withLogErrors(rule$rule(rule$session$input[[name]])),
          shiny.silent.error = function(e) {
            "An unexpected error occurred during input validation"
          },
          error = function(e) {
            if (inherits(e, "shiny.custom.error") || !isTRUE(getOption("shiny.sanitize.errors", FALSE))) {
              paste0("An unexpected error occurred during input validation: ",
                     conditionMessage(e))
            } else {
              "An unexpected error occurred during input validation"
            }
          }
        )
        result_is_html <- FALSE
        if (any(class(result) %in% c("shiny.tag", "shiny.tag.list", "shiny.tag.function", "html"))) {
          result <- as.character(result)
          result_is_html <- TRUE
        }
        # Validation rules are required to return one of the following:
        # * NULL: the value has passed the validation rule
        # * character(1): the rule didn't pass validation
        # * skip_validation(): the value has passed and subsequent rules
        #   should be skipped
        is_valid_result <- is.null(result) ||
          (is.character(result) && length(result) == 1) ||
          identical(skip_validation(), result)
        if (!is_valid_result) {
          stop("Result of '", name, "' validation was not a single-character vector (actual class: ", class(result)[1], ")")
        }
        # Note that if there's an error in rule(), we won't get to the next
        # line
        if (is.null(result)) {
          console_log("  ...Passed")
          if (!fullname %in% names(results)) {
            # Can't do results[[fullname]] <<- NULL, that just removes the element
            results <<- c(results, stats::setNames(list(NULL), fullname))
          }
        } else if (identical(skip_validation(), result)) {
          console_log("  ...Skipping remaining rules")
          # Put a non-NULL, non-error value here to prevent remaining rules
          # from executing (i.e., skipping validation steps)
          results[[fullname]] <<- TRUE
        } else {
          console_log("  ...Failed")
          results[[fullname]] <<- list(type = "error", message = result, is_html = result_is_html)
        }
      })
      # Change all TRUE entries to NULL. We have to use list(NULL) instead of
      # NULL because the latter would simply remove these entries from the list.
      # The effect of list(NULL) is correct though, you end up with NULL entries
      # and not list(NULL) entries.
      results[vapply(results, isTRUE, logical(1), USE.NAMES = FALSE)] <- list(NULL)
      
      merge_results(dependency_results, results)
    }
  )
)

#' Skip any normal validation performed by a rule
#'
#' While the predominant role of the `skip_validation()` function is tied to the
#' [sv_optional()] function (where it's used internally), you can also return
#' `skip_validation()` from custom validation rules. When returned, all
#' subsequent validation rules defined for the input will be skipped.
#'
#' @return A function that returns a sentinel value, signaling to shinyvalidate
#'   that any further validation rules for an input are to be skipped.
#'
#' @export
skip_validation <- local({
  .skip_validation <- structure(list(), class = "shinyvalidate.skip_validation")
  function() {
    .skip_validation
  }
})

# Combines two results lists (names are input IDs, values are NULL or a string).
# We combine the two results lists by giving resultsA priority over resultsB,
# except in the case where resultsA has a NULL element and resultsB's
# corresponding element is non-NULL.
merge_results <- function(resultsA, resultsB) {
  results <- c(resultsA, resultsB)
  # Reorder to put non-NULLs first; then dedupe
  has_error <- !vapply(results, is.null, logical(1))
  results <- results[c(which(has_error), which(!has_error))]
  results <- results[!duplicated(names(results))]
  results
}

#' Check whether an input value has been provided
#' 
#' @description
#' This function takes an input value and uses heuristics to guess whether it
#' represents an "empty" input vs. one that the user has provided. This will
#' vary by input type; for example, a [shiny::textInput()] is `""` when empty,
#' while a [shiny::numericInput()] is `NA`.
#' 
#' `input_provided` returns `TRUE` for all values except:
#' 
#' * `NULL`
#' * `""`
#' * An empty atomic vector or list
#' * An atomic vector that contains only missing (`NA`) values
#' * A character vector that contains only missing and/or `""` values
#' * An object of class `"try-error"`
#' * A value that represents an unclicked [shiny::actionButton()]
#' 
#' 
#' @param val Values to test for availability in a Shiny context.
#' 
#' @return A logical vector of length 1.
#' 
#' @details 
#' This function is based on [shiny::isTruthy()] but tweaked here in
#' shinyvalidate to change the treatment of `FALSE` values: `isTruthy(FALSE)`
#' returns `FALSE`, but `input_provided(FALSE)` returns `TRUE`. This difference
#' is motivated by `shiny::checkboxInput()`, where `isTruthy()` answers the
#' question of "is the input present _and checked_" while `input_provided` is
#' just "is the input present".
#' 
#' @export
input_provided <- function(val) {
  
  # The reason this differs from shiny::isTruthy is because isTruthy is more
  # about "is the value present and true?", so e.g. a reactive that requires a
  # checkbox to be checked can just use req(input$checkbox1). For validation
  # purposes, we're only interested in whether an input was provided at all.
  #
  # Specific differences:
  # * FALSE (or a logical vector containing only FALSEs) is not truthy, but it
  #   is provided.
  
  if (is.null(val))
    return(FALSE)
  if (inherits(val, 'try-error'))
    return(FALSE)
  if (!is.atomic(val))
    return(TRUE)
  if (length(val) == 0)
    return(FALSE)
  if (all(is.na(val)))
    return(FALSE)
  if (is.character(val) && !any(nzchar(stats::na.omit(val))))
    return(FALSE)
  if (inherits(val, 'shinyActionButtonValue') && val == 0)
    return(FALSE)
  
  TRUE
}

timestamp_str <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%OS3", usetz = TRUE)
}

verbose_prefix <- "[shinyvalidate] "
verbose_indent <- "  "
