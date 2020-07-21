# The overall implementation of ShinyValidator is extremely simple right now, at
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
#' @export
ShinyValidator <- R6::R6Class("ShinyValidator", cloneable = FALSE,
  private = list(
    session = NULL,
    enabled = FALSE,
    observer_handle = NULL,
    priority = numeric(0),
    rules = NULL,
    validators = list()
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
    initialize = function(priority = 1000, session = getDefaultReactiveDomain()) {
      if (is.null(session)) {
        stop("ShinyValidator objects must be created in the context of Shiny server functions or Shiny module server functions")
      }
      private$session <- session
      private$priority <- priority
      private$rules <- reactiveVal(list(), label = "validation_rules")
      
      # Inject shinyvalidate dependencies (just once)
      if (!isTRUE(session$userData[["shinyvalidate-initialized"]])) {
        shiny::insertUI("body", "beforeEnd", list(htmldep(), HTML("")), immediate = TRUE)
        session$userData[["shinyvalidate-initialized"]] <- TRUE
      }
    },
    #' @description Add another `ShinyValidator` object to this one, as a
    #' "child". Any time this validator object is asked for its validity, it
    #' will only return `TRUE` if all of its child validators are also valid;
    #' and when this validator object is enabled (or disabled), then all of its
    #' child validators are enabled (or disabled) as well.
    #'
    #' This is intended to help with validating Shiny modules. Each module can
    #' create its own `ShinyValidator` object and populate it with rules, then
    #' return that object to the caller.
    #'
    #' @param validator A `ShinyValidator` object.
    add_validator = function(validator) {
      if (!inherits(validator, "ShinyValidator")) {
        stop("add_validator was called with an invalid `validator` argument; ShinyValidator object expected")
      }
      private$validators <- c(private$validators, list(validator))
      invisible(self)
    },
    #' @description Add an input validation rule. Each input validation rule
    #'   applies to a single input. You can add multiple validation rules for a
    #'   single input, by calling `add_rules()` multiple times; the first
    #'   validation rule for an input that fails will be used, and will prevent
    #'   subsequent rules for that input from executing.
    #'
    #' @param inputId A single-element character vector indicating the ID of the
    #'   input that this rule applies to. (Note that this name should _not_ be
    #'   qualified by a module namespace; e.g. pass `"x"` and not
    #'   `session$ns("x")`.)
    #' @param rule A function that takes (at least) one argument: the input's
    #'   value. The function should return `NULL` if it passes validation, and
    #'   if not, a single-element character vector containing an error message
    #'   to display to the user near the input. You can alternatively provide a
    #'   single-sided formula instead of a function, using `.` as the variable
    #'   name for the input value being validated.
    #' @param ... Optional: Additional arguments to pass to the `rule` function
    #'   whenever it is invoked.
    #' @param session. The session object to which the input belongs. (There's
    #'   almost never a reason to change this from the default.)
    add_rule = function(inputId, rule, ..., session. = getDefaultReactiveDomain()) {
      args <- rlang::list2(...)
      if (is.null(rule)) {
        rule <- function(value, ...) NULL
      }
      if (inherits(rule, "formula")) {
        rule <- rlang::as_function(rule)
      }
      applied_rule <- function(value) {
        # Do this instead of purrr::partial because purrr::partial doesn't
        # support leaving a "hole" for the first argument
        do.call(rule, c(list(value), args))
      }
      rule_info <- list(rule = applied_rule, session = session.)
      private$rules(c(isolate(private$rules()), setNames(list(rule_info), inputId)))
      invisible(self)
    },
    #' @description Begin displaying input validation feedback in the user
    #'   interface. Once enabled, this validator object will automatically keep
    #'   the feedback up-to-date. (It's safe to call the `enable()` method
    #'   on an already-enabled validator.)
    enable = function() {
      for (validator in private$validators) {
        validator$enable()
      }
      if (!private$enabled) {
        withReactiveDomain(private$session, {
          private$observer_handle <- observe({
            results <- self$validate(include_child_validators = FALSE)
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
      for (validator in private$validators) {
        validator$disable()
      }
      if (private$enabled) {
        private$observer_handle$destroy()
        private$observer_handle <- NULL
        private$enabled <- FALSE
        results <- self$validate(include_child_validators = FALSE)
        results <- lapply(results, function(x) NULL)
        private$session$sendCustomMessage("validation-jcheng5", results)
      }
    },
    #' @description Returns `TRUE` if all input validation rules currently pass,
    #'   `FALSE` if not.
    #' @param include_child_validators If `TRUE`, this method will only return
    #'   `TRUE` if all child validators also return `TRUE`. If `FALSE`, then
    #'   only the rules registered directly against this validator object will
    #'   be considered.
    is_valid = function(include_child_validators = TRUE) {
      results <- self$validate(include_child_validators = include_child_validators)
      all(vapply(results, is.null, logical(1), USE.NAMES = FALSE))
    },
    #' @description Run validation rules and gather results. For advanced usage
    #'   only; most apps should use the `is_valid()` and `enable()` methods
    #'   instead. The return value of this method is a named list, where the
    #'   names are (fully namespace qualified) input IDs, and the values are
    #'   either `NULL` (if the input value is passing) or a single-element
    #'   character vector describing a validation problem.
    #' @param include_child_validators If `TRUE`, this method will call the
    #'   `validate()` method on all child validator objects and merge the
    #'   results with this validator's results. If `FALSE`, then only the rules
    #'   directly added to this validator object will be considered.
    validate = function(include_child_validators = TRUE) {
      dependency_results <- list()
      if (include_child_validators) {
        for (validator in private$validators) {
          dependency_results <- merge_results(dependency_results, validator$validate())
        }
      }

      results <- list()
      mapply(names(private$rules()), private$rules(), FUN = function(name, rule) {
        fullname <- rule$session$ns(name)
        # Short-circuit if already errored
        if (!is.null(results[[fullname]])) return()
        
        try({
          result <- rule$rule(rule$session$input[[name]])
          if (!is.null(result) && (!is.character(result) || length(result) != 1)) {
            stop("Result of '", name, "' validation was not a single-character vector")
          }
          # Note that if there's an error in rule(), we won't get to the next
          # line
          if (is.null(result)) {
            if (!fullname %in% names(results)) {
              # Can't do results[[fullname]] <<- NULL, that just removes the element
              results <<- c(results, setNames(list(NULL), fullname))
            }
          } else {
            results[[fullname]] <<- result
          }
        })
      })
      
      merge_results(dependency_results, results)
    }
  )
)

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
