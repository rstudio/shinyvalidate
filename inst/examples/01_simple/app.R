library(shiny)
library(shinyvalidate)
library(markdown)

ui <- fluidPage(
  textInput("name", "Name"),
  textInput("email", "Email"),
  textOutput("greeting")
)

server <- function(input, output, session) {

  # Create an InputValidator object
  iv <- InputValidator$new()
  
  # Add validation rules
  iv$add_rule("name", sv_required())
  iv$add_rule("email", sv_required())
  iv$add_rule("email", ~ if (!is_valid_email(.)) "Not a valid email")
  
  # Start displaying errors in the UI
  iv$enable()
  
  output$greeting <- renderText({
    # Don't proceed if any input is invalid
    req(iv$is_valid())
    
    paste0("Nice to meet you, ", input$name, " <", input$email, ">!")
  })
}

# From https://www.nicebread.de/validating-email-adresses-in-r/
is_valid_email <- function(x) {
  grepl("^\\s*[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\s*$", as.character(x), ignore.case=TRUE)
}

shinyApp(ui, server)
