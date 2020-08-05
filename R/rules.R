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
#' \dontrun{
#' iv <- InputValidator$new()
#' 
#' # Basic usage: ensure that `input$title` is present, and return a terse
#' # validation message if not
#' iv$add_rule("title", sv_required())
#' 
#' # You can easily provide a custom message to display
#' iv$add_rule("email", sv_required("An email is required"))
#' 
#' # Provide a `test` argument to change the definition of "is present";
#' # in this example, any non-NULL value will be accepted.
#' iv$add_rule("choices", sv_required(test = is.null))
#' }
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

#' Validate based on regular expression
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

#' @export
sv_integer <- function(message, allowMultiple = FALSE, allowNA = FALSE,
  allowNaN = FALSE, allowInfinite = FALSE) {
  
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

#' @export
sv_numeric <- function(message, allowMultiple = FALSE, allowNA = FALSE,
  allowNaN = FALSE, allowInfinite = FALSE) {

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
