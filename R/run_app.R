#' Lancer l'application Takuzu
#'
#' @return L'application shiny composée du server et de l'ui définis au préalable.
#' @export
run_app <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
