library(shiny)
library(shinyvalidate)

# From https://www.nicebread.de/validating-email-adresses-in-r/
is_valid_email <- function(x) {
  grepl("^\\s*[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\s*$", as.character(x), ignore.case=TRUE)
}

ui <- fluidPage(
  includeMarkdown("README.md"),
  hr(),
  textInput("name", "Name"),
  textInput("email", "Email"),
  p(
    "Is user input valid: ",
    uiOutput("valid", container = tags$strong)
  )
)

server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("name", need, label = "Name")
  iv$add_rule("email", need, label = "Email")
  iv$add_rule("email", ~ if (!is_valid_email(.)) "Please provide a valid email")
  iv$enable()
  
  output$valid <- renderUI({
    if (iv$is_valid())
      tags$span(class = "text-success", "Yes!")
    else
      tags$span(class = "text-danger", "No")
  })
}

shinyApp(ui, server)
