library(shiny)

ui <- fluidPage(
  titlePanel("Takuzu 8x8"),
  sidebarPanel(
    radioButtons('diff', "Choisissez un niveau de difficulté :", choices = c("facile", "moyen", "difficile")),
    actionButton('launch', "Lancez le niveau")
  ),
  
  tags$style(HTML("
    .btn-grid {
      display: grid;
      grid-template-columns: repeat(8, 50px);
      gap: 2px;
      justify-content: center;
    }
    .btn-grid button {
      width: 50px;
      height: 50px;
      font-size: 12px;
    }
    .btn_custom {
      width: 50px;
      height: 50px;
      font-size: 18px;
      font-weight: bold;
      text-align: center;
      vertical-align: middle;
      border: 2px solid black;
      border-radius: 5px;
    }
  ")),
  
  uiOutput("matrice_boutons")
)

server <- function(input, output, session) {
  nRows <- 8
  nCols <- 8
  total_buttons <- nRows * nCols
  
  # États possibles : "", "0", "1"
  states <- c("", "0", "1")
  
  # Génération d'une grille aléatoire au chargement
  state <- reactiveValues(values = sample(states, total_buttons, replace = TRUE, prob = c(0.3, 0.35, 0.35)))
  
  output$matrice_boutons <- renderUI({
    boutons <- lapply(1:total_buttons, function(i) {
      actionButton(inputId = paste0("bouton_", i), label = state$values[i], class = "btn_custom")
    })
    div(class = "btn-grid", boutons)
  })
  
  # Fonction pour alterner entre "", "0" et "1" lorsqu'on clique sur un bouton
  lapply(1:total_buttons, function(i) {
    observeEvent(input[[paste0("bouton_", i)]], {
      current_index <- which(states == state$values[i])
      next_index <- (current_index %% length(states)) + 1
      state$values[i] <- states[next_index]
    })
  })
}

shinyApp(ui = ui, server = server)
