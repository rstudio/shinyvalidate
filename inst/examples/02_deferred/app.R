library(shiny)
library(shinyvalidate)

# From https://www.nicebread.de/validating-email-adresses-in-r/
is_valid_email <- function(x) {
  grepl("^\\s*[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\s*$", as.character(x), ignore.case=TRUE)
}

ui <- fluidPage(
  includeMarkdown("README.md"),
  hr(),
  textInput("name", "Name: *"),
  textInput("email", "Email:"),
  checkboxGroupInput("topics", "Topics (choose two or more): *", choices = c(
    "Statistics",
    "Machine learning",
    "Visualization",
    "Programming"
  )),
  checkboxInput("accept_terms", strong("I accept the terms and conditions *")),
  helpText("* Required fields"),
  p(
    actionButton("submit", "Submit", class = "btn-primary"),
    actionButton("reset", "Reset")
  )
)

server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("name", need, label = "Name")
  iv$add_rule("email", ~ if (isTruthy(.) && !is_valid_email(.)) "Please provide a valid email")
  iv$add_rule("topics", ~ if (length(.) < 2) "Please choose two or more topics")
  iv$add_rule("accept_terms", need, message = "The terms and conditions must be accepted in order to proceed")
  
  observeEvent(input$submit, {
    if (iv$is_valid()) {
      reset_form()
      showNotification(
        "Thanks for your submission!", 
        id = "submit_message", type = "message")
    } else {
      iv$enable() # Start showing validation feedback
      showNotification(
        "Please correct the errors in the form and try again",
        id = "submit_message", type = "error")
    }
  })
  
  observeEvent(input$reset, {
    reset_form()
  })
  
  reset_form <- function() {
    iv$disable()
    removeNotification("submit_message")
    updateTextInput(session, "name", value = "")
    updateTextInput(session, "email", value = "")
    updateCheckboxGroupInput(session, "topics", selected = character(0))
    updateCheckboxInput(session, "accept_terms", value = FALSE)
  }
}

shinyApp(ui, server)
