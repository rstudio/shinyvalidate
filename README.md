# shinyvalidate

<!-- badges: start -->
[![R build status](https://github.com/jcheng5/shinyvalidate/workflows/R-CMD-check/badge.svg)](https://github.com/jcheng5/shinyvalidate/actions)
<!-- badges: end -->


**WARNING: Highly experimental, API still in flux**

## Overview

shinyvalidate adds input validation capabilities to [Shiny](https://shiny.rstudio.com).

## Prior art

* [shinyFeedback](https://github.com/merlinoa/shinyFeedback) by [@merlinoa](https://github.com/merlinoa), who graciously provided feedback on the design of shinyvalidate.

## Installation

shinyvalidate is not yet available on CRAN. In the meantime, you can install it from GitHub using the remotes package.

```r
remotes::install_github("rstudio/shinyvalidate")
```

## Basic usage

To add validation to your Shiny app, you need to:

1. Create an InputValidator object: `iv <- InputValidator$new()`
2. Add one or more validation rules to the InputValidator: `iv$add_rule("title", need, message = "A title is required")`
3. Turn the validator on: `iv$enable()`

That's all you need to do to get validation messages to show up. Here is a simple example:

```r
library(shiny)
library(shinyvalidate)

ui <- fluidPage(
  textInput("name", "Name"),
  textInput("email", "Email")
)

server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("name", need, label = "Name")
  iv$add_rule("email", need, label = "Email")
  iv$add_rule("email", ~ if (!is_valid_email(.)) "Please provide a valid email")
  iv$enable()
}

# From https://www.nicebread.de/validating-email-adresses-in-r/
is_valid_email <- function(x) {
  grepl("^\\s*[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\s*$", as.character(x), ignore.case=TRUE)
}

shinyApp(ui, server)
```

To learn about other features of shinyvalidate, including deferred validation, programmatically accessing validation results, and validating Shiny modules, see the full usage guide (TODO).

## Compatibility

TODO
