#' Serveur de l'app Takuzu
#'
#' @return La fonction serveur de Shiny
#' @import shiny
#' @import shinyjs
#' @export
app_server <- function(input, output, session) {
  nRows <- 8
  nCols <- 8
  total_buttons <- nRows * nCols

  nb_cases_vides <- list(
    facile = total_buttons - 38,
    moyen = total_buttons - 30,
    difficile = total_buttons - 22
  )

  states <- c("", "0", "1")

  state <- reactiveValues(values = matrix(rep("", total_buttons), nrow = nRows, ncol = nCols))
  solution <- reactiveValues(values = NULL)

  observeEvent(input$launch, {
    niveau <- input$diff
    grille_completee <- generer_grille_valide(nRows, nCols)
    solution$values <- grille_completee
    grille <- masquer_cases(grille_completee, nb_cases_vides[[niveau]])
    state$values <- grille

    # Activer le bouton "Révéler une case" après le lancement du niveau
    enable("help")
    enable("verif")
  })

  output$matrice_boutons <- renderUI({
    boutons <- lapply(1:total_buttons, function(i) {
      row <- ((i - 1) %/% nCols) + 1
      col <- ((i - 1) %% nCols) + 1
      actionButton(inputId = paste0("bouton_", i), label = state$values[row, col], class = "btn_custom")
    })
    div(class = "btn-grid", boutons)
  })

  lapply(1:total_buttons, function(i) {
    observeEvent(input[[paste0("bouton_", i)]], {
      row <- ((i - 1) %/% nCols) + 1
      col <- ((i - 1) %% nCols) + 1
      current_index <- which(states == state$values[row, col])
      next_index <- (current_index %% length(states)) + 1
      state$values[row, col] <- states[next_index]
    })
  })

  observeEvent(input$help, {
    empty_indices <- which(state$values == "", arr.ind = TRUE)
    if (nrow(empty_indices) > 0) {
      idx <- empty_indices[sample(1:nrow(empty_indices), 1), ]
      row <- idx[1]
      col <- idx[2]
      state$values[row, col] <- solution$values[row, col]
    }
  })

  observeEvent(input$verif, {
    if (all(state$values == solution$values)) {
      showModal(modalDialog("Bravo ! Grille correcte ", easyClose = TRUE))
    } else {
      showModal(modalDialog("Il ya des erreurs", easyClose = TRUE))
    }
  })
}
