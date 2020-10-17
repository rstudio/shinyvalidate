# TODO:
# sv_lookup - make sure value is in a list of choices

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
#' @param minimum,maximum The minimum and maximum values of the numerical
#'   bounds, which are both inclusive.
#' @param allowNA If `FALSE` (the default), then any `NA` element will cause
#'   validation to fail.
#' @param allowNaN If `FALSE` (the default), then any `NaN` element will cause
#'   validation to fail.
#' @param message The validation error message to use if a value fails to match
#'   the rule. If left as `NULL`, a validation error message will be generated
#'   according to the chosen language (specified in `lang`).
#' @param lang The language to use for automatic creation of validation error
#'   messages. By default, `NULL` will create English (`"en"`) text. Other
#'   options include French (`"fr"`), German (`"de"`) and Spanish (`"es"`).
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
sv_between <- function(minimum,
                       maximum,
                       allowNA = FALSE,
                       allowNaN = FALSE,
                       message = NULL,
                       lang = NULL) {
  force(allowNA)
  force(allowNaN)
  force(message)
  
  # TODO: consider adding arg for specifying inclusive bounds
  
  # TODO: normalize lang (ensures that NULL -> "en" and other inputs
  # match supported langs in shinyvalidate)
  # lang <- normalize_lang(lang)
  
  # TODO: The gluestring will be obtained via a function that obtains
  # a localized string vector and focuses it on the spoken language
  # TODO: Use a safe version of `glue::glue()`
  # message <- glue::glue(get_lsv("between")[[lang]])
  if (is.null(message)) {
    message <- glue::glue("The field must be between {minimum} and {maximum}")
  }
  
  function(value) {
    # TODO: use the "noNA" lsv for the message
    if (!allowNA && any(is.na(value))) {
      return(message)
    }
    # TODO: use the "noNaN" lsv for the message
    if (!allowNaN && any(is.nan(value))) {
      return(message)
    }
    
    # TODO: ensure that `value` is coercible to numeric
    
    if ((as.numeric(value) < minimum) | (as.numeric(value) > maximum)) {
      return(message)
    }
  }
}
