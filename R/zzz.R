# Add a `::` to shiny to avoid "no usage" warning
dummy_import <- function() {
  shiny::runApp
}
