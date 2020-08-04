# TODO:
# sv_lookup - make sure value is in a list of choices

#' @export
sv_required <- function(message = "Required", test = isTruthy) {
  sv_predicate(test, message)
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
