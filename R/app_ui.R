#' Interface utilisateur de l'app Takuzu
#'
#' @return Un objet UI Shiny
#' @import shiny
#' @import shinyjs
#' @export
app_ui <- function() {
  fluidPage(
    useShinyjs(),
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: 'Segoe UI', sans-serif;
      }

      .sidebar-btn {
      width: 100%;
      max-width: 250px;
      margin: 10px auto;
      font-weight: bold;
      font-size: 16px;
      white-space: normal;
      word-wrap: break-word;
      border-radius: 8px;
      background-color: #6699cc;
      color: white;
      border: none;
      transition: background-color 0.3s;
      }

      .sidebar-btn:hover {
      background-color: #4a7aa8;
      }

      .sidebar-btn:disabled {
      background-color: #cccccc;
      color: #666666;
      cursor: not-allowed;
      }

      .btn-grid {
        display: grid;
        grid-template-columns: repeat(8, 50px);
        gap: 5px;
        justify-content: center;
        margin-top: 20px;
      }

      .btn-grid button {
        width: 50px;
        height: 50px;
        font-size: 18px;
        font-weight: bold;
        border-radius: 8px;
      }

      .btn_custom {
        background-color: #ffffff;
        color: #333333;
        border: 2px solid #dee2e6;
        transition: 0.2s;
      }

      .btn_custom:hover {
        background-color: #e2e6ea;
      }

      .btn_custom.disabled {
        background-color: #f1f1f1;
        color: #999999;
        border-color: #cccccc;
      }

      .btn_error {
        background-color: #ff6b6b !important;
        color: white !important;
        border-color: #ff6b6b !important;
      }

      .btn_fixed {
      background-color: #e9ecef !important;
      color: #000000 !important;
      font-weight: normal;
      border-color: #ced4da !important;
      cursor: not-allowed !important;
      }
    ")),

    titlePanel("Takuzu"),

    sidebarPanel(
      radioButtons('diff', "Choisissez un niveau de difficulté :", choices = c("facile", "moyen", "difficile")),
      actionButton('launch', "Lancer le niveau", class = "sidebar-btn"),
      actionButton("help", "Révéler une case", class = "sidebar-btn"),
      actionButton("verif", "Vérifier la grille", class = "sidebar-btn")
    ),

    uiOutput("matrice_boutons")
  )
}
