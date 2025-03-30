library(shiny)

### LISTE DE FONCTIONS ###

# Fonction pour générer une grille de Takuzu valide pour que le code efface ensuite un
# nombre de cases en fonction de la difficulté
generer_grille_valide <- function(nRows, nCols) {
  grille <- matrix("", nrow = nRows, ncol = nCols)
  
  for (i in 1:nRows) {
    zeros <- 0
    ones <- 0
    for (j in 1:nCols) {
      possible_values <- c("0", "1")
      if (j > 2 && grille[i, j-1] == grille[i, j-2]) {
        possible_values <- setdiff(possible_values, grille[i, j-1])
      }
      if (i > 2 && grille[i-1, j] == grille[i-2, j]) {
        possible_values <- setdiff(possible_values, grille[i-1, j])
      }
      
      # Assurer une répartition égale des 0 et 1
      if (zeros >= nCols / 2) {
        possible_values <- "1"
      } else if (ones >= nCols / 2) {
        possible_values <- "0"
      }
      
      if (length(possible_values) > 0) {
        grille[i, j] <- sample(possible_values, 1)
        if (grille[i, j] == "0") {
          zeros <- zeros + 1
        } else {
          ones <- ones + 1
        }
      }
    }
  }
  return(grille)
}

# Fonction pour masquer un nombre donné de cases
masquer_cases <- function(grille, nb_vide) {
  total_buttons <- length(grille)
  indices <- sample(1:total_buttons, nb_vide)
  grille[indices] <- ""
  return(grille)
}

ui <- fluidPage(
  titlePanel("Takuzu 8x8"),
  sidebarPanel(
    radioButtons('diff', "Choisissez un niveau de difficulté :", choices = c("facile", "moyen", "difficile")),
    actionButton('launch', "Lancez le niveau")
  ),
  actionButton("help","Révéler une case"),
  
  tags$style(HTML(".btn-grid { display: grid; grid-template-columns: repeat(8, 50px); gap: 2px; justify-content: center; }
                  .btn-grid button { width: 50px; height: 50px; font-size: 12px; }
                  .btn_custom { width: 50px; height: 50px; font-size: 18px; font-weight: bold; text-align: center; vertical-align: middle; border: 2px solid black; border-radius: 5px; }")),
  uiOutput("matrice_boutons")
)

server <- function(input, output, session) {
  nRows <- 8
  nCols <- 8
  total_buttons <- nRows * nCols
  
  # Nombre de cases à masquer selon la difficulté
  nb_cases_vides <- list(
    facile = total_buttons - 38,
    moyen = total_buttons - 30,
    difficile = total_buttons - 22
  )
  
  # États possibles : "", "0" ou "1"
  states <- c("", "0", "1")
  
  state <- reactiveValues(values = matrix(rep("", total_buttons), nrow = nRows, ncol = nCols))
  solution <- reactiveValues(values = NULL)
  observeEvent(input$launch, {
    niveau <- input$diff
    grille_complet <- generer_grille_valide(nRows, nCols)
    solution$values <- grille_complet
    grille <- masquer_cases(grille_complet, nb_cases_vides[[niveau]])
    state$values <- grille
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
  observeEvent(input$help,{
    #Chercher les cases vide 
    empty_indices <- which(state$values == "", arr.ind = TRUE)
    #Si il reste des cases vides 
    if (nrow(empty_indices) > 0) {
      # Choisir une case vide au hasard
      idx <- empty_indices[sample(1:nrow(empty_indices), 1), ]
      row <- idx[1]
      col <- idx[2]
          # Remplir cette case avec la valeur correcte qu'on a stockée dans solution
    state$values[row, col] <- solution$values[row, col]
    }
  })
}

shinyApp(ui = ui, server = server)

### PACKAGE ###
# Fonctions :
# Générer la grille 
# Corriger la grille
# Donner un indice au joueur
# Vérifier la grille pour le joueur 
# Alerter le joueur quand il fait un "move" impossible

# Library :
# Shiny et c'est tout normalement


# Version finale : le code génère au préalable une grille remplie qui respecte les règles
# puis cache un nombre de cases en fonction de la difficulté, l'interface offre des choix
# à l'utilisateur, comme celui de pouvoir voir si son coup est optimal ou d'avoir des 
# indices sur la meilleure manière de remplir une case. Il faudrait donc s'assurer que la
# grille proposée n'admette qu'une unique solution (peut être est-ce trop demandé ?). En
# dernier recours, il faudrait créer une fonction qui donne une alerte à l'utilisateur si 
# son coup est faux OU une fonction qui permette à l'utilisateur à tout instant de vérifier 
# si sa grille respecte les règles.
