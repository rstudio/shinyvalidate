htmldep <- function() {
  htmltools::htmlDependency(
    "shinyvalidate",
    version = packageVersion("shinyvalidate"),
    src = "assets",
    script = "shinyvalidate.js",
    package = "shinyvalidate",
    all_files = FALSE
  )
}