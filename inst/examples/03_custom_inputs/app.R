library(shiny)
library(shinyvalidate)

ui <- fluidPage(
  includeMarkdown("README.md"),
  hr(),
  camera_input("selfie", "Take a selfie"),
  actionButton("submit", "Submit", class = "btn-primary")
)

server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("selfie", need, label = "Selfie")
  
  observeEvent(input$submit, {
    if (iv$is_valid()) {
      removeUI("#selfie, #submit", multiple = TRUE)
      insertUI(".container-fluid", "beforeEnd", "Thank you for your submission!")
      
      # input$selfie contains raw png data, suitable for png::readPNG()
    } else {
      iv$enable() # Start showing validation feedback
    }
  })
}

shinyApp(ui, server)