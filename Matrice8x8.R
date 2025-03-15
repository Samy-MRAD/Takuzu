library(shiny)

### LISTE DE FONCTIONS ###

# Fonction pour corriger la grille selon la règle "pas plus de 2 signes consécutifs identiques"
comptage_elt <- function(grille, nRows, nCols){
  S_row <- numeric(nRows)
  S_col <- numeric(nCols)
  for (i in 1:nRows) {
    S_row[i] <- colSums(i)
  }
  for (j in 1:nCols) {
    S_col[j] <- rowSums(j)
  }
  return(S_row, S_col)
}


corriger_grille <- function(grille, nRows, nCols) {
  for (i in 1:nRows) {
    for (j in 3:nCols) {
      if (grille[i, j] != "" && grille[i, j] == grille[i, j-1] && grille[i, j] == grille[i, j-2]) {
        grille[i, j] <- sample(setdiff(c("0", "1"), grille[i, j]), 1)
      }
    }
  }
  
  for (j in 1:nCols) {
    for (i in 3:nRows) {
      if (grille[i, j] != "" && grille[i, j] == grille[i-1, j] && grille[i, j] == grille[i-2, j]) {
        grille[i, j] <- sample(setdiff(c("0", "1"), grille[i, j]), 1)
      }
    }
  
  }
  return(grille)
}

ui <- fluidPage(
  titlePanel("Takuzu 8x8"),
  sidebarPanel(
    radioButtons('diff', "Choisissez un niveau de difficulté :", choices = c("facile", "moyen", "difficile")),
    actionButton('launch', "Lancez le niveau")
  ),
  
  tags$style(HTML(".btn-grid { display: grid; grid-template-columns: repeat(8, 50px); gap: 2px; justify-content: center; }
                  .btn-grid button { width: 50px; height: 50px; font-size: 12px; }
                  .btn_custom { width: 50px; height: 50px; font-size: 18px; font-weight: bold; text-align: center; vertical-align: middle; border: 2px solid black; border-radius: 5px; }")),
  
  uiOutput("matrice_boutons")
)

server <- function(input, output, session) {
  nRows <- 8
  nCols <- 8
  total_buttons <- nRows * nCols
  
  # Nombre de cases à remplir selon la difficulté
  nb_cases_remplies <- list(
    facile = 28,
    moyen = 22,
    difficile = 16
  )
  
  # États possibles : "", "0" ou "1"
  states <- c("", "0", "1")
  
  state <- reactiveValues(values = matrix(rep("", total_buttons), nrow = nRows, ncol = nCols))
  
  observeEvent(input$launch, {
    niveau <- input$diff
    grille <- matrix("", nrow = nRows, ncol = nCols)
    indices <- sample(1:total_buttons, nb_cases_remplies[[niveau]])
    
    for (idx in indices) {
      row <- ((idx - 1) %/% nCols) + 1
      col <- ((idx - 1) %% nCols) + 1
      grille[row, col] <- sample(c("0", "1"), 1)  # Évite de remplir avec "" au départ
    }
    
    grille <- corriger_grille(grille, nRows, nCols)
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

