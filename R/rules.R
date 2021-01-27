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
#' [input_provided()].
#'
#' @param message The validation error message to be displayed if the test does
#'   not pass.
#' @param test A single-argument function, or single-sided formula (using `.` to
#'   access the value to test), that returns `TRUE` for success and `FALSE` for
#'   failure.
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
#' })
#'
#' @export
sv_required <- function(message = "Required", 
                        test = input_provided) {
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

#' Indicate that a field is optional
#'
#' @description
#' Call `sv_optional()` to generate a validation function that indicates an
#' input is allowed to _not_ be present. If an `sv_optional()` rule sees that an
#' input is not present, subsequent rules for that input are skipped and the
#' input is considered valid. Otherwise, the rule simply passes.
#' (`sv_optional()` will never return a validation error/message.)
#'
#' By default, the definition of "is present" is based on [input_provided()].
#'
#' Child validators (see [`InputValidator$add_validator()`][InputValidator]) are
#' not affected by `sv_optional()` rules in parent validators; only rules in the
#' same validator instance as the `sv_optional()` will be skipped.
#'
#' @param test A single-argument function, or single-sided formula (using `.` to
#'   access the value to test), that returns `TRUE` for success and `FALSE` for
#'   failure.
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
#'   # An email is not required, but if present, it must be valid
#'   iv$add_rule("email", sv_optional())
#'   iv$add_rule("email", ~ if (!is_valid_email(.)) "Please provide a valid email")
#' })
#' @export
sv_optional <- function(test = input_provided) {
  force(test)

  if (inherits(test, "formula")) {
    test <- rlang::as_function(test)
  }

  function(value) {
    if (!test(value)) {
      skip_validation()
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
#' @param ignore.case,perl,fixed,useBytes,invert Options passed through to
#'   [base::grepl()].
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
#' })
#'
#' @export
sv_regex <- function(pattern,
                     message,
                     ignore.case = FALSE,
                     perl = FALSE,
                     fixed = FALSE,
                     useBytes = FALSE,
                     invert = FALSE) {
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
#' The `sv_numeric()` function validates that a field is numeric with the
#' [base::is.numeric()] function. By default, only a single, finite,
#' not-missing, valid number is allowed, but each of those criteria can be
#' controlled via arguments.
#'
#' @param message The validation error message to use if a value is not numeric.
#' @param allow_multiple If `FALSE` (the default), then the length of the input
#'   vector must be exactly one; if `TRUE`, then any length is allowed
#'   (including a length of zero; use [sv_required()] if one or more values
#'   should be required).
#' @param allow_na,allow_nan If `FALSE` (the default for both options), then any
#'   `NA` or `NaN` element will cause validation to fail.
#' @param allow_inf If `FALSE` (the default), then any `Inf` or `-Inf` element
#'   will cause validation to fail.
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
#'   iv$add_rule("count", sv_numeric())
#'   iv$add_rule("count", ~if (. <= 0) "A positive value is required")
#' })
#'
#' @export
sv_numeric <- function(message = "A number is required",
                       allow_multiple = FALSE,
                       allow_na = FALSE,
                       allow_nan = FALSE,
                       allow_inf = FALSE) {
  force(message)
  force(allow_multiple)
  force(allow_na)
  force(allow_nan)
  force(allow_inf)

  compose_rules(
    sv_basic(
      allow_multiple = allow_multiple,
      allow_na = allow_na,
      allow_nan = allow_nan,
      allow_inf = allow_inf
    ),
    function(value) {
      
      if (length(value) == 0) {
        return(err_condition_messages[["err_zero_length_value"]])
      }
      
      # Validation test
      res <- is.numeric(value) || is.infinite(value) || is.na(value)
      
      if (!all(res)) {
        return(message)
      }
    }
  )
}

#' Validate that a field is an integer
#'
#' The `sv_integer()` function validates that a field is an integer with the
#' [base::is.integer()] function. By default, only a single, finite,
#' not-missing, valid integer is allowed, but each of those criteria can be
#' controlled via arguments.
#'
#' @param message The validation error message to use if a value is not an
#'   integer.
#' @inheritParams sv_numeric
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
#'   iv$add_rule("count", sv_integer())
#'   iv$add_rule("count", ~if (. <= 0) "A positive value is required")
#' })
#'
#' @export
sv_integer <- function(message = "An integer is required",
                       allow_multiple = FALSE,
                       allow_na = FALSE,
                       allow_nan = FALSE,
                       allow_inf = FALSE) {
  force(message)
  force(allow_multiple)
  force(allow_na)
  force(allow_nan)
  force(allow_inf)

  compose_rules(
    sv_basic(
      allow_multiple = allow_multiple,
      allow_na = allow_na,
      allow_nan = allow_nan,
      allow_inf = allow_inf
    ),
    function(value) {
      
      if (length(value) == 0) {
        return(err_condition_messages[["err_zero_length_value"]])
      }
      
      # Validation test
      res <- is.integer(value) || is.infinite(value) || is.na(value)
      
      if (!all(res)) {
        return(message)
      }
    }
  )
}

#' Validate that a field is a number bounded by minimum and maximum values
#'
#' The `sv_between()` function validates that a field has values between left
#' and right boundary values. Both bounds are inclusive by default, but both can
#' be set as either inclusive or exclusive with the `inclusive` argument. In its
#' default mode, the validation check will effectively be of the form `<left> <=
#' <field> <= <right>`.
#'
#' @param left,right The left and right boundary values. Inclusively for each of
#'   the boundaries is set with the `inclusive` argument; the defaults are set
#'   for inclusive bounds.
#' @param inclusive A two-element logical vector that indicates whether the
#'   `left` and `right` bounds, respectively, should be inclusive. Both bounds
#'   are by default are inclusive, using `c(TRUE, TRUE)`.
#' @param message_fmt The validation error message to use if a value fails to
#'   match the rule. The message can be customized by using the `"{left}"` and
#'   `"{right}"` string parameters, which allows for the insertion of the `left`
#'   and `right` values. While the default message uses both of these string
#'   parameters, they are not required in a user-defined `message_fmt` string.
#' @inheritParams sv_numeric
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
#' })
#'
#' @export
sv_between <- function(left,
                       right,
                       inclusive = c(TRUE, TRUE),
                       message_fmt = "Must be between {left} and {right}.",
                       allow_na = FALSE,
                       allow_nan = FALSE) {
  force(left)
  force(right)
  force(inclusive)
  force(message_fmt)
  force(allow_na)
  force(allow_nan)

  # Prechecking of inputs; this stops the function immediately (not entering
  # the validation phase) since failures here are recognized as usage errors
  check_input_length(input = left, input_name = "left")
  check_input_length(input = right, input_name = "right")
  
  # TODO: allow for check of multiple values with `allow_multiple`
  
  message <-
    glue::glue_data_safe(
      list(left = left, right = right),
      message_fmt
    )
  
  compose_rules(
    sv_basic(
      allow_multiple = TRUE,
      allow_na = allow_na,
      allow_nan = allow_nan,
      allow_inf = TRUE
    ),
    function(value) {

      l_of_left <- if (inclusive[1]) value < left else value <= left
      r_of_right <- if (inclusive[2]) value > right else value >= right
      
      # Remove NA and NaN values
      l_of_left <- l_of_left[!is.na(l_of_left)]
      r_of_right <- r_of_right[!is.na(r_of_right)]
      
      if (any(l_of_left, r_of_right)) {
        return(message)
      }
    }
  )
}

#' Validate that a field is part of a defined set
#'
#' The `sv_in_set()` function checks whether the field value is part of a
#' specified set of values.
#'
#' @param set A vector or list of elements for which the field value must be a
#'   part of to pass validation. To allow an empty field, `NA` should be
#'   included in the `set` vector. Optionally, `NaN` can be included as well.
#' @param message_fmt The validation error message to use if a value fails to
#'   match the rule. The message can be customized by using the
#'   `"{values_text}"` string parameter, which allows for the insertion of `set`
#'   values (formatted internally as a text list and controlled via the
#'   `set_limit` parameter). While the default message uses this string
#'   parameter, it is not required in a user-defined `message_fmt` string.
#' @param set_limit The limit of `set` values to include in the
#'   automatically-generated error message (i.e., when `message = NULL`, the
#'   default). If the number of elements provided in `set` is greater than
#'   `set_limit` then only the first `<message_limit>` set elements will be
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
#'   iv$add_rule("rating", sv_in_set(1:5, set_limit = 5))
#' })
#'
#' @export
sv_in_set <- function(set,
                      message_fmt = "Must be in the set of {values_text}.",
                      set_limit = 3) {
  force(set)
  force(message_fmt)
  force(set_limit)

  # Prechecking of inputs; this stops the function immediately (not entering
  # the validation phase) since failures here are recognized as usage errors
  check_input_length(input = set, input_name = "set")
  check_input_length(input = set_limit, input_name = "set_limit")

  values_text <- prepare_values_text(set, limit = set_limit)

  message <-
    glue::glue_data_safe(
      list(values_text = values_text),
      message_fmt
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

#' Validate that a field is greater than a specified value
#'
#' The `sv_gt()` function compares the field value to a specified value with the
#' `>` operator.
#'
#' @param rhs The right hand side (RHS) value is to be used for the comparison
#'   with the field value. The validation check will effectively be of the form
#'   `<field> > <rhs>`.
#' @param message_fmt The validation error message to use if the field fails the
#'   validation test. Use the `"{rhs}"` string parameter to customize the
#'   message, including what was set in `rhs`. While the default message uses
#'   this string parameter, it is not required in a user-defined `message_fmt`
#'   string.
#' @param allow_multiple If `FALSE` (the default), then the length of the input
#'   vector must be exactly one; if `TRUE`, then any length is allowed
#'   (including a length of zero; use [sv_required()] if one or more values
#'   should be required).
#' @inheritParams sv_numeric
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
#'   iv$add_rule("number", sv_gt(3))
#' })
#'
#' @export
sv_gt <- function(rhs,
                  message_fmt = "Must be greater than {rhs}.",
                  allow_multiple = FALSE,
                  allow_na = FALSE,
                  allow_nan = FALSE,
                  allow_inf = FALSE) {

  sv_comparison(
    rhs = rhs,
    message_fmt = message_fmt,
    allow_multiple = allow_multiple,
    allow_na = allow_na,
    allow_nan = allow_nan,
    allow_inf = allow_inf,
    operator = `>`
  )
}


#' Validate that a field is greater than or equal to a specified value
#'
#' The `sv_gte()` function compares the field value to a specified value with
#' the `>=` operator.
#'
#' @param rhs The right hand side (RHS) value is to be used for the comparison
#'   with the field value. The validation check will effectively be of the form
#'   `<field> >= <rhs>`.
#' @inheritParams sv_gt
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
#'   iv$add_rule("number", sv_gte(4))
#' })
#'
#' @export
sv_gte <- function(rhs,
                   message_fmt = "Must be greater than or equal to {rhs}.",
                   allow_multiple = FALSE,
                   allow_na = FALSE,
                   allow_nan = FALSE,
                   allow_inf = FALSE) {

  sv_comparison(
    rhs = rhs,
    message_fmt = message_fmt,
    allow_multiple = allow_multiple,
    allow_na = allow_na,
    allow_nan = allow_nan,
    allow_inf = allow_inf,
    operator = `>=`
  )
}

#' Validate that a field is less than a specified value
#'
#' The `sv_lt()` function compares the field value to a specified value with the
#' `<` operator.
#'
#' @param rhs The right hand side (RHS) value is to be used for the comparison
#'   with the field value. The validation check will effectively be of the form
#'   `<field> < <rhs>`.
#' @inheritParams sv_gt
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
#'   iv$add_rule("number", sv_lt(8))
#' })
#'
#' @export
sv_lt <- function(rhs,
                  message_fmt = "Must be less than {rhs}.",
                  allow_multiple = FALSE,
                  allow_na = FALSE,
                  allow_nan = FALSE,
                  allow_inf = FALSE) {

  sv_comparison(
    rhs = rhs,
    message_fmt = message_fmt,
    allow_multiple = allow_multiple,
    allow_na = allow_na,
    allow_nan = allow_nan,
    allow_inf = allow_inf,
    operator = `<`
  )
}

#' Validate that a field is less than or equal to a specified value
#'
#' The `sv_lte()` function compares the field value to a specified value with
#' the `<=` operator.
#'
#' @param rhs The right hand side (RHS) value is to be used for the comparison
#'   with the field value. The validation check will effectively be of the form
#'   `<field> <= <rhs>`.
#' @inheritParams sv_gt
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
#'   iv$add_rule("number", sv_lte(7))
#' })
#'
#' @export
sv_lte <- function(rhs,
                   message_fmt = "Must be less than or equal to {rhs}.",
                   allow_multiple = FALSE,
                   allow_na = FALSE,
                   allow_nan = FALSE,
                   allow_inf = FALSE) {

  sv_comparison(
    rhs = rhs,
    message_fmt = message_fmt,
    allow_multiple = allow_multiple,
    allow_na = allow_na,
    allow_nan = allow_nan,
    allow_inf = allow_inf,
    operator = `<=`
  )
}

#' Validate that a field is equal to a specified value
#'
#' The `sv_equal()` function compares the field value to a specified value with
#' the `==` operator.
#'
#' @param rhs The right hand side (RHS) value is to be used for the comparison
#'   with the field value. The validation check will effectively be of the form
#'   `<field> == <rhs>`.
#' @inheritParams sv_gt
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
#'   iv$add_rule("sum", sv_equal(10))
#' })
#'
#' @export
sv_equal <- function(rhs,
                     message_fmt = "Must be equal to {rhs}.",
                     allow_multiple = FALSE,
                     allow_na = FALSE,
                     allow_nan = FALSE,
                     allow_inf = FALSE) {

  sv_comparison(
    rhs = rhs,
    message_fmt = message_fmt,
    allow_multiple = allow_multiple,
    allow_na = allow_na,
    allow_nan = allow_nan,
    allow_inf = allow_inf,
    operator = `==`
  )
}

#' Validate that a field is not equal to a specified value
#'
#' The `sv_not_equal()` function compares the field value to a specified value
#' with the `!=` operator.
#'
#' @param rhs The right hand side (RHS) value is to be used for the comparison
#'   with the field value. The validation check will effectively be of the form
#'   `<field> != <rhs>`.
#' @inheritParams sv_gt
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
#'   iv$add_rule("score", sv_not_equal(0))
#' })
#'
#' @export
sv_not_equal <- function(rhs,
                         message_fmt = "Must not be equal to {rhs}.",
                         allow_multiple = FALSE,
                         allow_na = FALSE,
                         allow_nan = FALSE,
                         allow_inf = FALSE) {

  sv_comparison(
    rhs = rhs,
    message_fmt = message_fmt,
    allow_multiple = allow_multiple,
    allow_na = allow_na,
    allow_nan = allow_nan,
    allow_inf = allow_inf,
    operator = `!=`
  )
}

sv_comparison <- function(rhs,
                          message_fmt,
                          allow_multiple,
                          allow_na,
                          allow_nan,
                          allow_inf,
                          operator) {

  force(rhs)
  force(message_fmt)

  # Prechecking of inputs; this stops the function immediately (not entering
  # the validation phase) since failures here are recognized as usage errors
  check_input_length(input = rhs, input_name = "rhs")
  
  # Preparation of the message
  message <-
    glue::glue_data_safe(
      list(rhs = rhs),
      message_fmt
    )

  # Testing of `value` and validation
  compose_rules(
    sv_basic(
      allow_multiple = allow_multiple,
      allow_na = allow_na,
      allow_nan = allow_nan,
      allow_inf = allow_inf
    ),
    function(value) {
      # Validation test
      res <- operator(value, rhs)
      
      # Remove NA and NaN values
      res <- res[!is.na(res)]
      
      if (!all(res)) {
        return(message)
      }
    }
  )
}

#' Combine shinyvalidate rule functions
#' 
#' @description
#' Takes multiple shinyvalidate rule functions, and returns a shinyvalidate rule
#' function. When this resulting rule function is invoked, it will try each of
#' its constituent rule functions in order; the first validation error that is
#' detected will be returned immediately and the remaining rules will not be
#' tried.
#'
#' This function is not intended to be used by Shiny app authors (i.e. not for
#' `InputValidator$add_rule("x", compose_rules(...))`), but for developers of
#' reusable shinyvalidate rule functions. See examples.
#' 
#' @param ... Any number of shinyvalidate rule functions; earlier rules will be
#'   attempted before later rules. Argument names are ignored. Single-sided
#'   formulas are also accepted instead of a function, using `.` as the variable
#'   name for the input value.
#'   
#' @return A function suitable for using as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#' 
#' @examples
#' # Create a new shinyvalidate rule that is composed
#' # of two `sv_*()` rule functions (`sv_integer()` and
#' # `sv_gt()`, and a custom function for ensuring
#' # a number is even)
#' positive_even_integer <- function() {
#'   compose_rules(
#'     sv_integer(),
#'     sv_gt(0),
#'     ~ if (. %% 2 == 1) "Must be an even number"
#'   )
#' }
#' 
#' # Use the `positive_even_integer()` rule function
#' # to check that a supplied value is an integer, greater
#' # than zero, and even (in that order)
#' shiny::withReactiveDomain(shiny::MockShinySession$new(), {
#'   
#'   iv <- InputValidator$new()
#'   
#'   iv$add_rule("foo", compose_rules(sv_required(), sv_numeric()))
#'   iv$add_rule("foo", positive_even_integer())
#' })
#' 
#' @export
compose_rules <- function(...) {
  
  rule_fns <- lapply(rlang::list2(...), function(rule_fn) {
    if (is.function(rule_fn)) {
      rule_fn
    } else if (rlang::is_formula(rule_fn)) {
      rlang::as_function(rule_fn)
    } else {
      stop("All arguments to combine_rules must be functions or formulas")
    }
  })
  
  function(value) {
    for (rule_fn in rule_fns) {
      res <- rule_fn(value)
      if (!is.null(res)) return(res)
    }
    NULL
  }
}

sv_basic <- function(allow_multiple,
                     allow_na,
                     allow_nan,
                     allow_inf) {
  
  force(allow_multiple)
  force(allow_na)
  force(allow_nan)
  force(allow_inf)
  
  function(value) {
    # Validity testing of `value` within set constraints
    if (!allow_multiple && length(value) != 1) {
      return(err_condition_messages[["err_allow_multiple"]])
    }
    if (!allow_na && any(is.na(value) & !is.nan(value))) {
      return(err_condition_messages[["err_allow_na"]])
    }
    if (!allow_nan && any(is.nan(value))) {
      return(err_condition_messages[["err_allow_nan"]])
    }
    if (!allow_inf && any(is.infinite(value))) {
      return(err_condition_messages[["err_allow_infinite"]])
    }
    NULL
  }
}

check_input_length <- function(input,
                               input_name,
                               stop_message = "The input for `{input_name}` must contain values.") {
  
  if (length(input) < 1) {
    
    stop_message <-
      glue::glue_data_safe(
        list(input_name = input_name),
        stop_message
      )
    
    stop(stop_message, call. = FALSE)
  }
}

err_condition_messages <- 
  list(
    err_zero_length_value = "Must not contain zero values.",
    err_allow_multiple = "Must not contain multiple values.",
    err_allow_na = "Must not contain `NA` values.",
    err_allow_nan = "Must not contain `NaN` values.",
    err_allow_infinite = "Must not contain infinite values."
  )
