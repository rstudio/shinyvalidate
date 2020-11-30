# TODO:
# sv_lookup - make sure value is in a list of choices

# TODO: When {shiny} has a localization language set, we can finally use preset,
# localized error messages (as gluestrings). The gluestring used in the
# `glue::glue_data_safe()` call will be obtained via a function that obtains a
# localized string vector and focuses it on the `lang` setting.
# Also we will normalize the `lang` value to ensure that NULL -> "en" and
# other locale inputs match supported languages in {shinyvalidate}
# lang <- normalize_lang(lang)
# message <- glue::glue_data_safe(get_lsv("between")[[lang]])

#' Validate that the field is present
#'
#' Call `sv_required()` to generate a validation function that ensures an input
#' value is present. By default, the definition of "is present" is based on
#' [shiny::isTruthy()], which is the logic used by the [shiny::req()] function
#' as well.
#'
#' @param message The validation error message to be displayed if the test does
#'   not pass.
#' @param test A single-argument function, or single-sided formula (using `.` to
#'   access the value to test), that returns `TRUE` for success and `FALSE` for
#'   failure.
#' @return A function suitable for using as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' # Ignore withReactiveDomain(), it's just required to get this example to run
#' # outside of Shiny
#' shiny::withReactiveDomain(shiny::MockShinySession$new(), {
#' 
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: ensure that `input$title` is present, and return a terse
#'   # validation message if not
#'   iv$add_rule("title", sv_required())
#' 
#'   # You can easily provide a custom message to display
#'   iv$add_rule("email", sv_required("An email is required"))
#' 
#'   # Provide a `test` argument to change the definition of "is present";
#'   # in this example, any non-NULL value will be accepted.
#'   iv$add_rule("choices", sv_required(test = is.null))
#'
#' })
#' @export
sv_required <- function(message = "Required", test = shiny::isTruthy) {
  force(message)
  force(test)
  
  if (inherits(test, "formula")) {
    test <- rlang::as_function(test)
  }
  
  function(value) {
    if (!test(value)) {
      message
    }
  }
}

#' Validate that a field matches a regular expression
#'
#' A validation function, suitable for use with `InputValidator$add_rule()`,
#' that checks whether input values match the specified regular expression.
#'
#' @param pattern Character string containing a regular expression (or character
#'   string if `fixed = TRUE`) to be tested against. If a character vector of
#'   length 2 or more is supplied, the first element is used with a warning.
#' @param message The validation error message to use if a value fails to match
#'   the pattern.
#' @param ignore.case,perl,fixed,useBytes,invert Passed through to [base::grepl()].
#' @return A function suitable for using as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' # Ignore withReactiveDomain(), it's just required to get this example to run
#' # outside of Shiny
#' shiny::withReactiveDomain(shiny::MockShinySession$new(), {
#' 
#'   iv <- InputValidator$new()
#'
#'   iv$add_rule("lookup_id",
#'     sv_regex("^[a-zA-Z0-9]$", "Only alphanumeric characters are allowed")
#'   )
#'
#'   # If you're more comfortable with wildcards than regex, use glob2rx
#'   iv$add_rule("image_filename",
#'     sv_regex(glob2rx("*.png"),
#'       message = "A filename ending in png was expected",
#'       ignore.case = TRUE
#'     )
#'   )
#'
#' })
#'
#' @export
sv_regex <- function(pattern, message, ignore.case = FALSE, perl = FALSE,
  fixed = FALSE, useBytes = FALSE, invert = FALSE) {

  force(pattern)
  force(message)
  force(ignore.case)
  force(perl)
  force(fixed)
  force(useBytes)
  force(invert)
  
  function(value) {
    result <- grepl(pattern, value, ignore.case = ignore.case, perl = perl,
      fixed = fixed, useBytes = useBytes)
    
    if (invert) {
      result <- !result
    }
    
    if (!result) {
      return(message)
    }
  }
}

#' Validate that a field is a number
#'
#' `sv_numeric` and `sv_integer` validate that a field `is.numeric` and
#' `is.integer`, respectively. By default, only a single, finite, not-missing,
#' valid number/integer is allowed, but each of those criteria can be controlled
#' via arguments.
#'
#' @param message The validation error message to use if a value fails to match
#'   the rule.
#' @param allowMultiple If `FALSE` (the default), then the length of the input
#'   vector must be exactly one; if `TRUE`, then any length is allowed
#'   (including a length of zero; use [sv_required()] if one or more values
#'   should be required).
#' @param allowNA If `FALSE` (the default), then any `NA` element will cause
#'   validation to fail.
#' @param allowNaN If `FALSE` (the default), then any `NaN` element will cause
#'   validation to fail.
#' @param allowInfinite If `FALSE` (the default), then any `Inf` or `-Inf`
#'   element will cause validation to fail.
#' @return A function suitable for using as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' # Ignore withReactiveDomain(), it's just required to get this example to run
#' # outside of Shiny
#' shiny::withReactiveDomain(shiny::MockShinySession$new(), {
#' 
#'   iv <- InputValidator$new()
#' 
#'   iv$add_rule("count", sv_integer())
#'   iv$add_rule("count", ~if (. <= 0) "A positive value is required")
#'
#' })
#' @export
sv_numeric <- function(message = "A number is required", allowMultiple = FALSE,
  allowNA = FALSE, allowNaN = FALSE, allowInfinite = FALSE) {

  force(message)
  force(allowMultiple)
  force(allowNA)
  force(allowNaN)
  force(allowInfinite)

  function(value) {
    if (!is.numeric(value)) {
      return(message)
    }
    if (length(value) == 0) {
      return(message)
    }
    if (!allowMultiple && length(value) != 1) {
      return(message)
    }
    if (!allowNA && any(is.na(value))) {
      return(message)
    }
    if (!allowNaN && any(is.nan(value))) {
      return(message)
    }
    if (!allowInfinite && any(!is.finite(value))) {
      return(message)
    }
  }
}

#' @rdname sv_numeric
#' @export
sv_integer <- function(message = "An integer is required",
  allowMultiple = FALSE, allowNA = FALSE, allowNaN = FALSE,
  allowInfinite = FALSE) {
  
  force(message)
  force(allowMultiple)
  force(allowNA)
  force(allowNaN)
  force(allowInfinite)
  
  function(value) {
    if (!is.integer(value)) {
      return(message)
    }
    if (length(value) == 0) {
      return(message)
    }
    if (!allowMultiple && length(value) != 1) {
      return(message)
    }
    if (!allowNA && any(is.na(value))) {
      return(message)
    }
    if (!allowNaN && any(is.nan(value))) {
      return(message)
    }
    if (!allowInfinite && any(!is.finite(value))) {
      return(message)
    }
  }
}

#' Validate that a field is a number bounded by minimum and maximum values
#'
#' `sv_numeric` and `sv_integer` validate that a field `is.numeric` and
#' `is.integer`, respectively. By default, only a single, finite, not-missing,
#' valid number/integer is allowed, but each of those criteria can be controlled
#' via arguments.
#' 
#' @param left,right The left and right boundary values. Inclusively for each of
#'   the boundaries is set with the `inclusive` argument; the defaults are set
#'   for inclusive bounds.
#' @param inclusive A two-element logical vector that indicates whether the
#'   `left` and `right` bounds, respectively, should be inclusive. Both bounds
#'   are by default are inclusive, using `c(TRUE, TRUE)`.
#' @param allow_na,allow_nan If `FALSE` (the default for both options), then any
#'   `NA` or `NaN` element will cause validation to fail.
#' @param message The validation error message to use if a value fails to match
#'   the rule. By default, this is generic message provided by **shinyvalidate**
#'   but a custom message can be provided here.
#'
#' @return A function suitable for using as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' # Ignore withReactiveDomain(), it's just required to get this example to run
#' # outside of Shiny
#' shiny::withReactiveDomain(shiny::MockShinySession$new(), {
#' 
#'   iv <- InputValidator$new()
#' 
#'   iv$add_rule("count", sv_between(10, 10000))
#'   iv$add_rule("count", ~if (. <= 0) "A positive value is required")
#'
#' })
#' @export
sv_between <- function(left,
                       right,
                       inclusive = c(TRUE, TRUE),
                       allow_na = FALSE,
                       allow_nan = FALSE,
                       message = NULL) {
  force(allow_na)
  force(allow_nan)
  force(message)
  
  if (is.null(message)) {
    message <- 
      glue::glue_data_safe(
        list(left = left, right = right),
        "Must be between {left} and {right}."
      )
  }
  
  function(value) {
    # TODO: use the "noNA" lsv for the message
    if (!allow_na && any(is.na(value))) {
      return(message)
    }
    # TODO: use the "noNaN" lsv for the message
    if (!allow_nan && any(is.nan(value))) {
      return(message)
    }
    
    # TODO: perhaps check that `value` has a class where
    # comparison operators successfully eval to a logical value,
    # or better yet, use a `try()/tryCatch()` scheme here
    
    l_of_left <- if (inclusive[1]) value < left else value <= left
    r_of_right <- if (inclusive[2]) value > right else value >= right
    
    if (any(l_of_left, r_of_right)) {
      return(message)
    }
  }
}

#' Validate that a field is part of a defined set
#'
#' The `sv_in_set()` function checks whether the field value is part of a
#' specified set of values.
#' 
#' @param set A vector or list of elements for which the field value must be a
#'   part of to pass validation. To allow an empty field, `NA` should be
#'   included in the `set` vector. Optionally, `NaN` can be included as well.
#' @param message The validation error message to use if a value fails to match
#'   the rule. Use `"{values_text}"` within the message to include a short list
#'   of values based on `set`.
#' @param msg_limit The limit of `set` values to include in the
#'   automatically-generated error message (i.e., when `message = NULL`, the
#'   default). If the number of elements provided in `set` is greater than
#'   `msg_limit` then only the first `<message_limit>` set elements will be
#'   echoed along with text that states how many extra elements are part of the
#'   `set`.
#'
#' @return A function suitable for using as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' # Ignore withReactiveDomain(), it's just required to get this example to run
#' # outside of Shiny
#' shiny::withReactiveDomain(shiny::MockShinySession$new(), {
#' 
#'   iv <- InputValidator$new()
#' 
#'   iv$add_rule("count", sv_in_set(1:5))
#'   iv$add_rule("count", ~if (. <= 0) "A positive value is required")
#'
#' })
#' @export
sv_in_set <- function(set,
                      message = "Must be in the set of {values_text}.",
                      msg_limit = 3) {
  force(set)
  force(message)
  force(msg_limit)
  
  if (length(set) < 1) {
    stop("The `set` must contain values.", call. = FALSE)
  }
  
  values_text <- prepare_values_text(set, limit = msg_limit)
  
  message <- 
    glue::glue_data_safe(
      list(values_text = values_text),
      message
    )
  
  function(value) {
    
    if (!all(value %in% set)) {
      return(message)
    }
  }
}

prepare_values_text <- function(set,
                                limit) {
  
  # TODO: consider special handling of `NA`, producing additional
  # text that states that empty fields are allowed (right now, `NA`
  # is just echoed in the string if it is in the first `1:limit`
  # elements of the `set`)
  
  if (!is.null(limit) && length(set) > limit) {
    
    num_omitted <- length(set) - limit
    
    values_str <- paste0(set[seq_len(limit)], collapse = ", ")
    
    additional_text <- glue::glue("(and {num_omitted} more)")
      
    values_str <- paste0(values_str, " ", additional_text)
    
  } else {
    values_str <- paste0(set, collapse = ", ")
  }
  
  values_str
}
