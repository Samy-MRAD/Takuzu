#' Interface utilisateur de l'app Takuzu
#'
#' @return Un objet UI Shiny
#' @import shiny
#' @import shinyjs
#' @export
app_ui <- function() {
  fluidPage(
    useShinyjs(),
    titlePanel("Takuzu 8x8"),
    sidebarPanel(
      radioButtons('diff', "Choisissez un niveau de difficulté :", choices = c("facile", "moyen", "difficile")),
      actionButton('launch', "Lancez le niveau"),
      actionButton("help","Révéler une case", disabled = TRUE),
      actionButton("verif","Vérifier la grille", disabled = TRUE)
    ),
    tags$style(HTML(".btn-grid { display: grid; grid-template-columns: repeat(8, 50px); gap: 2px; justify-content: center; }
                     .btn-grid button { width: 50px; height: 50px; font-size: 12px; }
                     .btn_custom { width: 50px; height: 50px; font-size: 18px; font-weight: bold; text-align: center; vertical-align: middle; border: 2px solid black; border-radius: 5px; }")),
    uiOutput("matrice_boutons")
  )
}
