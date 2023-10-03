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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("name", "Name")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: ensure that `input$name` is present,
#'   # and return a terse validation message if not
#'   iv$add_rule("name", sv_required())
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' # There are some alternatives to the above example,
#' # and the following snippets can serve to replace
#' # the `iv$add_rule(...)` statement
#' 
#' # (1) Providing a custom message to display
#' # when validation fails:
#' 
#' # iv$add_rule("email", sv_required("An email is required"))
#'
#' # (2) Providing a `test` argument to change
#' # the definition of "is present"; in this
#' # snippet, any non-NULL value will be accepted:
#' 
#' # iv$add_rule("choices", sv_required(test = is.null))
#' 
#' @family rule functions
#' 
#' @seealso The [sv_optional()] function, which takes a different approach to
#'   field presence.
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("email", "Email")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_optional()` is often paired with
#'   # another `sv_*()` function; below, an email in
#'   # `input$email` is not required, but if present, it
#'   # must be valid
#'   iv$add_rule("email", sv_optional())
#'   iv$add_rule("email", sv_email())
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The [sv_required()] function, which takes a different approach to
#'   field presence.
#' 
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("lookup_id", "Lookup ID")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_regex()` requires both a regex
#'   # pattern and message to display if the validation
#'   # of `input$lookup_id` fails
#'   iv$add_rule(
#'     "lookup_id",
#'     sv_regex("^[a-zA-Z0-9]*$", "Only alphanumeric characters allowed")
#'   )
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' # As an alternative to the above example, the
#' # following snippet can serve to replace the
#' # `iv$add_rule(...)` statement
#' 
#' # If you're more comfortable with wildcards
#' # (i.e., globbing) than with regular expressions,
#' # use `glob2rx()` in `pattern`
#' 
#' # iv$add_rule(
#' #   "lookup_id",
#' #   sv_regex(
#' #     pattern = glob2rx("*.png"),
#' #     message = "A filename ending in 'png' was expected",
#' #     ignore.case = TRUE
#' #   )
#' # )
#' 
#' @family rule functions
#' 
#' @seealso The [sv_email()] and [sv_url()] functions, which are specialized
#'   regex-based functions for validating email addresses and URLs.
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

#' Validate that a field contains an email address
#'
#' A validation function, suitable for use with `InputValidator$add_rule()`,
#' that checks whether an input value looks like a valid email address.
#'
#' @param message The validation error message to use if a value doesn't match a
#'   regex pattern for email address detection.
#' @inheritParams sv_numeric
#' @param allow_na If `FALSE`, then any `NA` element will cause validation to 
#'   fail.
#'
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("email", "Email")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_email()` works well with its
#'   # defaults; a message will be displayed if the
#'   # validation of `input$email` fails
#'   iv$add_rule("email", sv_email())
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The [sv_url()] function, another specialized regex-based function
#'   for validating URLs. For general regex-based validation the [sv_regex()]
#'   function is useful.
#'
#' @export
sv_email <- function(message = "Not a valid email address",
                     allow_multiple = FALSE,
                     allow_na = FALSE) {
  force(message)
  force(allow_multiple)
  force(allow_na)
  
  compose_rules(
    sv_basic(
      allow_multiple = allow_multiple,
      allow_na = allow_na,
      allow_nan = FALSE,
      allow_inf = FALSE
    ),
    function(value) {
 
      # Regular expression taken from
      # https://www.nicebread.de/validating-email-adresses-in-r/
      res <-
        grepl(
          "^\\s*[A-Z0-9._%&'*+`/=?^{}~-]+@[A-Z0-9.-]+\\.[A-Z0-9]{2,}\\s*$",
          as.character(value),
          ignore.case = TRUE
        )
        
      res <- res | is.na(value)
      
      if (!all(res)) {
        return(message)
      }
    }
  )
}

#' Validate that a field contains a URL
#'
#' A validation function, suitable for use with `InputValidator$add_rule()`,
#' that checks whether an input value is a valid URL.
#'
#' @param message The validation error message to use if a value doesn't match a
#'   regex pattern for URL detection.
#' @inheritParams sv_email
#'
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("url", "URL")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_url()` works well with its
#'   # defaults; a message will be displayed if the
#'   # validation of `input$address` fails
#'   iv$add_rule("url", sv_url())
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The [sv_email()] function, another specialized regex-based function
#'   for validating email addresses. For general regex-based validation the
#'   [sv_regex()] function is useful.
#'
#' @export
sv_url <- function(message = "Not a valid URL",
                   allow_multiple = FALSE,
                   allow_na = FALSE) {
  force(message)
  force(allow_multiple)
  force(allow_na)
  
  compose_rules(
    sv_basic(
      allow_multiple = allow_multiple,
      allow_na = allow_na,
      allow_nan = FALSE,
      allow_inf = FALSE
    ),
    function(value) {
      
      # Regular expression taken from
      # https://gist.github.com/dperini/729294
      res <-
        grepl(
          "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$",
          as.character(value),
          ignore.case = TRUE,
          perl = TRUE
        )
      
      res <- res | is.na(value)
      
      if (!all(res)) {
        return(message)
      }
    }
  )
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#' 
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("rating", "Rating")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_numeric()` works well with its
#'   # defaults; a message will be displayed if the
#'   # validation of `input$rating` fails
#'   iv$add_rule("rating", sv_numeric())
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The [sv_integer()] function, which tests whether a field value is a
#'   number that is integer-like.
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
        return(err_msg_zero_length_value)
      }
      
      # Validation test
      res <- is.numeric(value) | is.infinite(value) | is.na(value)
      
      if (!all(res)) {
        return(message)
      }
    }
  )
}

#' Validate that a field is a number that is integer-like
#'
#' The `sv_integer()` function validates that a field is 'integer-like' with the
#' `{value} %% 1 == 0` test. Very large values (generally with absolute exponent
#' values greater than 15) won't be validated correctly due to floating point
#' imprecision. By default, only a single, finite, not-missing, valid numbers
#' are allowed, but each of those criteria can be controlled via arguments.
#'
#' @param message The validation error message to use if a value is not an
#'   integer.
#' @inheritParams sv_numeric
#' 
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("count", "Count")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_integer()` works well with its
#'   # defaults; a message will be displayed if the
#'   # validation of `input$count` fails
#'   iv$add_rule("count", sv_integer())
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The [sv_numeric()] function, which tests whether a field value is
#'   simply numeric.
#'
#' @export
sv_integer <- function(message = "An integer is required",
                       allow_multiple = FALSE,
                       allow_na = FALSE,
                       allow_nan = FALSE) {
  force(message)
  force(allow_multiple)
  force(allow_na)
  force(allow_nan)

  compose_rules(
    sv_basic(
      allow_multiple = allow_multiple,
      allow_na = allow_na,
      allow_nan = allow_nan,
      allow_inf = FALSE
    ),
    function(value) {
      
      if (length(value) == 0) {
        return(err_msg_zero_length_value)
      }
      
      # Validation test
      res <- 
        suppressWarnings(value %% 1 == 0) |
        is.infinite(value) |
        is.na(value)
      
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#' 
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("count", "Count")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_between()` requires `left` and
#'   # `right` boundary values; a message will be
#'   # displayed if the validation of `input$count` fails
#'   iv$add_rule("count", sv_between(10, 100))
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The [sv_in_set()] function, which tests whether a field values are
#'   part of a specified set.
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
#' The `sv_in_set()` function checks whether the field value is a member of a
#' specified set of values.
#'
#' @param set A vector or list of elements for which the field value must be a
#'   part of (`value %in% set` must be `TRUE`) to pass validation. To allow an
#'   empty field, `NA` should be included in the `set` vector. Optionally, `NaN`
#'   can be included as well.
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#' 
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("rating", "Rating")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_in_set()` requires a value
#'   # set given as a vector; a message will be
#'   # shown if the validation of `input$rating` fails
#'   iv$add_rule("rating", sv_in_set(1:5))
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The [sv_between()] function, which tests whether a field values
#'   between two boundary values.
#'
#' @export
sv_in_set <- function(set,
                      message_fmt = "Must be in the set of {values_text}.",
                      set_limit = 3) {
  force(set)
  force(message_fmt)
  force(set_limit)
  
  # Allows passing in of factors directly; if we don't call `unique` here, then
  # the validation message will contain duplicates ("Must be in the set of
  # setosa, setosa, setosa...").
  set <- unique(set)

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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#' 
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("number", "Number")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_gt()` requires a value
#'   # to compare against the field value; a message
#'   # will be shown if the validation of
#'   # `input$number` fails
#'   iv$add_rule("number", sv_gt(0))
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The other comparison-based rule functions: [sv_gte()], [sv_lt()],
#' [sv_lte()], [sv_equal()], and [sv_not_equal()]. The [sv_gte()] function may
#' be needed if the field value should also pass validation when equal to the
#' comparison value.
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'   
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("number", "Number")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_gte()` requires a value
#'   # to compare against the field value; a message
#'   # will be shown if the validation of
#'   # `input$number` fails
#'   iv$add_rule("number", sv_gte(1))
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The other comparison-based rule functions: [sv_gt()], [sv_lt()],
#' [sv_lte()], [sv_equal()], and [sv_not_equal()]. The [sv_gt()] function may
#' be needed if the field value should not pass validation when it is equal to
#' the comparison value.
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'   
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("number", "Number")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_lt()` requires a value
#'   # to compare against the field value; a message
#'   # will be shown if the validation of
#'   # `input$number` fails
#'   iv$add_rule("number", sv_lt(10))
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The other comparison-based rule functions: [sv_gt()], [sv_gte()],
#' [sv_lte()], [sv_equal()], and [sv_not_equal()]. The [sv_lte()] function may
#' be needed if the field value should also pass validation when equal to the
#' comparison value.
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'   
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("number", "Number")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_lte()` requires a value
#'   # to compare against the field value; a message
#'   # will be shown if the validation of
#'   # `input$number` fails
#'   iv$add_rule("number", sv_lte(0))
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The other comparison-based rule functions: [sv_gt()], [sv_gte()],
#' [sv_lt()], [sv_equal()], and [sv_not_equal()]. The [sv_lt()] function may
#' be needed if the field value should not pass validation when it is equal to
#' the comparison value.
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'   
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("number", "Number")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_equal()` requires a value
#'   # to compare against the field value; a message
#'   # will be shown if the validation of
#'   # `input$number` fails
#'   iv$add_rule("number", sv_equal(1))
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The other comparison-based rule functions: [sv_gt()], [sv_gte()],
#'   [sv_lt()], [sv_lte()], and [sv_not_equal()] (which serves as the opposite
#'   function to `sv_equal()`).
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
#' @return A function suitable for use as an
#'   [`InputValidator$add_rule()`][InputValidator] rule.
#'   
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("score", "Number")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Basic usage: `sv_not_equal()` requires a value
#'   # to compare against the field value; a message
#'   # will be shown if the validation of
#'   # `input$score` fails
#'   iv$add_rule("score", sv_not_equal(0))
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
#' 
#' @seealso The other comparison-based rule functions: [sv_gt()], [sv_gte()],
#'   [sv_lt()], [sv_lte()], and [sv_equal()] (which serves as the opposite
#'   function to `sv_not_equal()`).
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
#' @return A function suitable for use as an
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
#' 
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyvalidate)
#' 
#' ui <- fluidPage(
#'   textInput("value", "Value")
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   # Validation rules are set in the server, start by
#'   # making a new instance of an `InputValidator()`
#'   iv <- InputValidator$new()
#' 
#'   # Add two `add_rule()` statements: one that
#'   # combines `sv_required()` and `sv_numeric()` in
#'   # single rule, and another that is defined
#'   # through the use of `compose_rules()`
#'   iv$add_rule("value", compose_rules(sv_required(), sv_numeric()))
#'   iv$add_rule("value", positive_even_integer())
#' 
#'   # Finally, `enable()` the validation rules
#'   iv$enable()
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
#' 
#' @family rule functions
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
      return(err_msg_allow_multiple)
    }
    if (!allow_na && any(is.na(value) & !is.nan(value))) {
      return(err_msg_allow_na)
    }
    if (!allow_nan && any(is.nan(value))) {
      return(err_msg_allow_nan)
    }
    if (!allow_inf && any(is.infinite(value))) {
      return(err_msg_allow_infinite)
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

# Error messages
err_msg_zero_length_value <- "Must not contain zero values."
err_msg_allow_multiple <- "Must not contain multiple values."
err_msg_allow_na <- "Must not contain `NA` values."
err_msg_allow_nan <- "Must not contain `NaN` values."
err_msg_allow_infinite <- "Must not contain infinite values."
