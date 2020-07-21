htmldep <- function() {
  htmltools::htmlDependency(
    "shinyvalidate",
    version = utils::packageVersion("shinyvalidate"),
    src = "assets",
    script = "shinyvalidate.js",
    package = "shinyvalidate",
    all_files = FALSE
  )
}