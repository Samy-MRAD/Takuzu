#' Serveur de l'app Takuzu
#'
#' @return La fonction serveur de Shiny
#' @import shiny
#' @import shinyjs
#' @export
app_server <- function(input, output, session) {
  # Format de la grille : 8x8
  nRows <- 8
  nCols <- 8
  total_buttons <- nRows * nCols

  # Cases vides en fonction de la difficulté
  nb_cases_vides <- list(
    facile = total_buttons - 40,
    moyen = total_buttons - 30,
    difficile = total_buttons - 20
  )

  # Etats possibles des cases : vides, 0 ou 1
  states <- c("", "0", "1")

  # ReactiveValues utilisées dans la suite
  compteur_help <- reactiveValues(remaining = 3)
  compteur_verif <- reactiveValues(remaining = 1)
  state <- reactiveValues(values = matrix(rep("", total_buttons), nrow = nRows, ncol = nCols))
  solution <- reactiveValues(values = NULL)
  erreurs <- reactiveValues(indices = NULL)

  # Lancement de la grille en utilisant la fonction generer_grille_valide
  observeEvent(input$launch, {
    niveau <- input$diff
    grille_completee <- generer_grille_valide(nRows, nCols)
    solution$values <- grille_completee
    grille <- masquer_cases(grille_completee, nb_cases_vides[[niveau]])
    state$values <- grille

    erreurs$indices <- NULL #Supp les cases rouges

    # compteurs d'utilisations restantes pour les boutons
    compteur_help$remaining <- 3
    compteur_verif$remaining <- 1
    updateActionButton(session, "verif", label = "Vérifier la grille (1 restante)")
    updateActionButton(session, "help", label = "Révéler une case (3 restantes)")

    # Activer le bouton "Révéler une case" après le lancement du niveau
    enable("help")
    enable("verif")
  })

  # Création de la grille
  output$matrice_boutons <- renderUI({
    boutons <- lapply(1:total_buttons, function(i) {
      row <- ((i - 1) %/% nCols) + 1
      col <- ((i - 1) %% nCols) + 1

      # Détection des cases eronnées
      is_error <- !is.null(erreurs$indices) &&
        any(apply(erreurs$indices, 1, function(ind) all(ind == c(row, col))))

      # Change la couleur en fonction
      bouton_class <- if (is_error) "btn_custom btn_error" else "btn_custom"

      actionButton(inputId = paste0("bouton_", i),
                   label = state$values[row, col],
                   class = bouton_class)
    })
    div(
      class = "btn-grid",
      boutons
    )
  })

  # Au clic, passe à la valeur suivante
  lapply(1:total_buttons, function(i) {
    observeEvent(input[[paste0("bouton_", i)]], {
      row <- ((i - 1) %/% nCols) + 1
      col <- ((i - 1) %% nCols) + 1
      current_index <- which(states == state$values[row, col])
      next_index <- (current_index %% length(states)) + 1
      state$values[row, col] <- states[next_index]

      # Retirer la case des erreurs si elle a été modifiée
      if (!is.null(erreurs$indices)) {
        erreurs$indices <- erreurs$indices[!apply(erreurs$indices, 1, function(ind) all(ind == c(row, col))), , drop = FALSE]
      }
    })
  })

  # Bouton help qui affiche une case aléatoire
  observeEvent(input$help, {
    if (compteur_help$remaining > 0) {
      empty_indices <- which(state$values == "", arr.ind = TRUE) # détection des cases encore vides

      if (nrow(empty_indices) > 0) {
        idx <- empty_indices[sample(1:nrow(empty_indices), 1), ]
        row <- idx[1]
        col <- idx[2]
        state$values[row, col] <- solution$values[row, col]

        compteur_help$remaining <- compteur_help$remaining - 1

        # Met à jour le label avec le nombre d'utilisations restantes
        updateActionButton(session, "help",
                           label = paste0("Révéler une case (", compteur_help$remaining, " restantes)"))

        # Si on atteint 0, on désactive le bouton
        if (compteur_help$remaining == 0) {
          disable("help")
        }
      }
    }
  })

  # Affichage grille complétée quand la grille est justement remplie
  observe({
    if (!is.null(solution$values)) {
      current <- state$values
      # Vérifie que la grille est pleine (pas de cases vides)
      if (all(current != "") && all(current == solution$values)) {
        showModal(modalDialog("Bravo, grille complétée !", easyClose = TRUE))
      }
    }
  })

  # Code pour le bouton de vérification de la grille
  observeEvent(input$verif, {
    if (compteur_verif$remaining > 0) {
      erreurs$indices <- NULL  # on vide les erreurs précédentes

      if (all(state$values == solution$values)) {
        showModal(modalDialog("Bravo, grille correcte !", easyClose = TRUE))
      } else {
        diff_matrix <- state$values != solution$values
        erreurs$indices <- which(diff_matrix, arr.ind = TRUE)
        showModal(modalDialog("Il y a des erreurs", easyClose = TRUE))
      }

      compteur_verif$remaining <- compteur_verif$remaining - 1
      updateActionButton(session, "verif", label = paste0("Vérifier la grille (", compteur_verif$remaining, " restante)"))

      # Utilisation unique
      if (compteur_verif$remaining == 0) {
        disable("verif")
      }
    }
  })
}
