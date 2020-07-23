library(shiny)
library(shinyvalidate)

ui <- fluidPage(
  includeMarkdown("README.md"),
  hr(),
  div(id = "form",
    radioButtons("type", "Profile image type", choices = c(
      "Take selfie" = "selfie",
      "Upload image file" = "upload",
      "Gravatar" = "gravatar"
    )),
    conditionalPanel("input.type === 'selfie'",
      camera_input("selfie", "Take a selfie")
    ),
    conditionalPanel("input.type === 'upload'",
      fileInput("upload", NULL, accept = c("image/jpeg", "image/png")),
      uiOutput("upload_preview", container = tags$p)
    ),
    conditionalPanel("input.type === 'gravatar'",
      textInput("email", "Email address"),
      uiOutput("gravatar_preview", container = tags$p)
    ),
    actionButton("submit", "Submit", class = "btn-primary")
  )
)

server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("selfie", ~if (input$type == "selfie") need(., label = "Selfie"))
  iv$add_rule("upload", ~if (input$type == "upload") need(., label = "Upload"))
  iv$add_rule("upload", function(upload) {
    if (input$type == "upload" && !tools::file_ext(upload$name) %in% c("jpg", "jpeg", "png")) {
      "A JPEG or PNG file is required"
    }
  })
  iv$add_rule("email", ~if (input$type == "gravatar") need(., label = "Email"))
  
  output$upload_preview <- renderUI({
    req(input$upload)
    req(nrow(input$upload) == 1)
    req(tools::file_ext(input$upload$name) %in% c("jpg", "jpeg", "png"))
    
    uri <- base64enc::dataURI(file = input$upload$datapath,
      mime = input$upload$type)
    tags$img(src = uri, style = htmltools::css(max_width = "300px", max_height = "300px"))
  })
  
  output$gravatar_preview <- renderUI({
    req(input$email)
    req(is_valid_email(input$email))
    
    email <- gsub("^\\s*(.*?)\\s*$", "\\1", input$email)
    email <- tolower(email)
    hash <- digest::digest(email, algo = "md5", serialize = FALSE)
    url <- paste0("https://www.gravatar.com/avatar/", hash)

    tags$img(src = url, alt = "Gravatar image")
  })
  
  observeEvent(input$submit, {
    if (iv$is_valid()) {
      removeUI("#form")
      insertUI(".container-fluid", "beforeEnd", "Thank you for your submission!")
      
      # input$selfie contains raw png data, suitable for png::readPNG()
    } else {
      iv$enable() # Start showing validation feedback
    }
  })
}

# From https://www.nicebread.de/validating-email-adresses-in-r/
is_valid_email <- function(x) {
  grepl("^\\s*[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\s*$", as.character(x), ignore.case=TRUE)
}

shinyApp(ui, server)