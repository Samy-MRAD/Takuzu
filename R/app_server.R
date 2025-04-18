#' Serveur de l'application Shiny pour le jeu Takuzu
#'
#' Cette fonction définit la logique serveur de l'application Takuzu.
#' Elle gère la génération des grilles, le masquage de cases selon la difficulté,
#' les interactions utilisateur avec les boutons, les vérifications de grille,
#' l'affichage d'aides, et la détection de réussite.
#'
#' @return Une fonction utilisée comme serveur dans une application Shiny.
#'
#' @details
#' Le jeu Takuzu est basé sur une grille binaire (0 et 1), où l'utilisateur doit
#' compléter une grille selon certaines règles de placement. Cette fonction serveur :
#' \itemize{
#'   \item Crée la grille en fonction du niveau choisi (facile, moyen, difficile).
#'   \item Utilise des objets \code{reactiveValues} pour suivre l'état de la grille.
#'   \item Gère les interactions de l'utilisateur avec chaque bouton de la grille,
#'         permettant de faire défiler les valeurs possibles.
#'   \item Permet à l'utilisateur de révéler jusqu'à 3 cases correctes avec un bouton d'aide.
#'   \item Autorise une vérification unique de la grille via un bouton dédié.
#'   \item Affiche un message de félicitations en cas de réussite.
#' }
#'
#' La grille est générée avec la fonction \code{generer_grille_valide}, puis partiellement masquée
#' avec \code{masquer_cases}.
#'
#'
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
    facile = total_buttons - 35,
    moyen = total_buttons - 28,
    difficile = total_buttons - 21
  )

  # Etats possibles des cases : vides, 0 ou 1
  states <- c("", "0", "1")

  # ReactiveValues utilisées dans la suite
  compteur_help <- reactiveValues(remaining = 3)
  compteur_verif <- reactiveValues(remaining = 1)
  state <- reactiveValues(values = matrix(rep("", total_buttons), nrow = nRows, ncol = nCols))
  solution <- reactiveValues(values = NULL)
  erreurs <- reactiveValues(indices = NULL)
  validees <- reactiveValues(indices = NULL)
  cases_fixes <- reactiveValues(indices = NULL)
  lancement_fait <- reactiveVal(FALSE)

  observeEvent(input$launch, {
    niveau <- input$diff
    grille_completee <- generer_grille_valide(nRows, nCols)
    solution$values <- grille_completee
    grille <- masquer_cases(grille_completee, nb_cases_vides[[niveau]])
    state$values <- grille

    erreurs$indices <- NULL
    validees$indices <- NULL
    compteur_help$remaining <- 3
    compteur_verif$remaining <- 1
    updateActionButton(session, "verif", label = "Vérifier la grille (1 restante)")
    updateActionButton(session, "help", label = "Révéler une case (3 restantes)")
    enable("help")
    enable("verif")

    # Cases fixes
    cases_fixes$indices <- which(grille != "", arr.ind = TRUE)

    # Changement du nom de la case au premier clic
    if (!lancement_fait()) {
      lancement_fait(TRUE)
      updateActionButton(session, "launch", label = "Lancer un autre niveau")
    }
  })

  # Création de la grille
  output$matrice_boutons <- renderUI({
    boutons <- lapply(1:total_buttons, function(i) {
      row <- ((i - 1) %/% nCols) + 1
      col <- ((i - 1) %% nCols) + 1

      # Erreur ?
      is_error <- !is.null(erreurs$indices) &&
        any(apply(erreurs$indices, 1, function(ind) all(ind == c(row, col))))

      # Est-ce une case FIXE (affichée dès le début) ?
      is_fixed <- !is.null(cases_fixes$indices) &&
        any(apply(cases_fixes$indices, 1, function(ind) all(ind == c(row, col))))

      # Est-ce une case VALIDÉE ?
      is_valid <- !is.null(validees$indices) &&
        any(apply(validees$indices, 1, function(ind) all(ind == c(row, col))))

      # Classes CSS : ajout d'une classe spéciale si c'est une case fixe
      bouton_class <- "btn_custom"
      if (is_error) {
        bouton_class <- paste(bouton_class, "btn_error")
      } else if (is_valid) {
        bouton_class <- paste(bouton_class, "btn_valid")
      }
      if (is_fixed) {
        bouton_class <- paste(bouton_class, "btn_fixed")
      }

      actionButton(
        inputId = paste0("bouton_", i),
        label = state$values[row, col],
        class = bouton_class,
        disabled = is_fixed
      )
    })

    div(
      class = "btn-grid",
      boutons
    )
  })
  # Permet de cliquer sur les boutons de la grille pour changer les valeurs
  lapply(1:total_buttons, function(i) {
    observeEvent(input[[paste0("bouton_", i)]], {
      row <- ((i - 1) %/% nCols) + 1
      col <- ((i - 1) %% nCols) + 1

      # Ne rien faire si la case est fixe
      if (!is.null(state$fixed) && state$fixed[row, col]) return()

      # au clic passe à la valeur suivante
      current_index <- which(states == state$values[row, col])
      next_index <- (current_index %% length(states)) + 1
      state$values[row, col] <- states[next_index]

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
        validees$indices <- rbind(validees$indices, c(row, col)) # Ajouter cette case aux cases grises


        # Si la case faisait partie des erreurs, on la retire
        if (!is.null(erreurs$indices)) {
          erreurs$indices <- erreurs$indices[!apply(erreurs$indices, 1, function(ind) all(ind == c(row, col))), , drop = FALSE]
        }

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

      # affichage d'un message quand la grille est bien ou mal complétée
      if (all(state$values == solution$values)) {
        showModal(modalDialog("Bravo, grille correcte !", easyClose = TRUE))
      } else {
        diff_matrix <- state$values != solution$values
        erreurs$indices <- which(diff_matrix, arr.ind = TRUE)
        validees$indices <- which(!diff_matrix, arr.ind = TRUE)
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
