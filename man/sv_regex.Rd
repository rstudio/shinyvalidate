% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rules.R
\name{sv_regex}
\alias{sv_regex}
\title{Validate that a field matches a regular expression}
\usage{
sv_regex(
  pattern,
  message,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE,
  invert = FALSE
)
}
\arguments{
\item{pattern}{Character string containing a regular expression (or character
string if \code{fixed = TRUE}) to be tested against. If a character vector of
length 2 or more is supplied, the first element is used with a warning.}

\item{message}{The validation error message to use if a value fails to match
the pattern.}

\item{ignore.case, perl, fixed, useBytes, invert}{Options passed through to
\code{\link[base:grep]{base::grepl()}}.}
}
\value{
A function suitable for use as an
\code{\link[=InputValidator]{InputValidator$add_rule()}} rule.
}
\description{
A validation function, suitable for use with \code{InputValidator$add_rule()},
that checks whether input values match the specified regular expression.
}
\examples{
## Only run examples in interactive R sessions
if (interactive()) {

library(shiny)
library(shinyvalidate)

ui <- fluidPage(
  textInput("lookup_id", "Lookup ID")
)

server <- function(input, output, session) {
  
  # Validation rules are set in the server, start by
  # making a new instance of an `InputValidator()`
  iv <- InputValidator$new()

  # Basic usage: `sv_regex()` requires both a regex
  # pattern and message to display if the validation
  # of `input$lookup_id` fails
  iv$add_rule(
    "lookup_id",
    sv_regex("^[a-zA-Z0-9]*$", "Only alphanumeric characters allowed")
  )

  # Finally, `enable()` the validation rules
  iv$enable()
}

shinyApp(ui, server)

}

# As an alternative to the above example, the
# following snippet can serve to replace the
# `iv$add_rule(...)` statement

# If you're more comfortable with wildcards
# (i.e., globbing) than with regular expressions,
# use `glob2rx()` in `pattern`

# iv$add_rule(
#   "lookup_id",
#   sv_regex(
#     pattern = glob2rx("*.png"),
#     message = "A filename ending in 'png' was expected",
#     ignore.case = TRUE
#   )
# )

}
\seealso{
The \code{\link[=sv_email]{sv_email()}} and \code{\link[=sv_url]{sv_url()}} functions, which are specialized
regex-based functions for validating email addresses and URLs.

Other rule functions: 
\code{\link{compose_rules}()},
\code{\link{sv_between}()},
\code{\link{sv_email}()},
\code{\link{sv_equal}()},
\code{\link{sv_gte}()},
\code{\link{sv_gt}()},
\code{\link{sv_in_set}()},
\code{\link{sv_integer}()},
\code{\link{sv_lte}()},
\code{\link{sv_lt}()},
\code{\link{sv_not_equal}()},
\code{\link{sv_numeric}()},
\code{\link{sv_optional}()},
\code{\link{sv_required}()},
\code{\link{sv_url}()}
}
\concept{rule functions}
