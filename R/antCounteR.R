#' @export
antCounteR <- function(...) {
  shiny::runApp(paste0(find.package("antCounteR"), "/antCounteR"), ...)
}