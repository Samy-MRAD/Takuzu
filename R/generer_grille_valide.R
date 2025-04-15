#' Génère une grille de Takuzu valide
#'
#' Cette fonction permet de générer une grille valide de Takuzu déjà remplie,
#' en respectant les règles du jeu (pas plus de deux chiffres identiques consécutifs,
#' répartition équilibrée des 0 et 1).
#'
#' @param nRows Nombre de lignes de la grille.
#' @param nCols Nombre de colonnes de la grille.
#'
#' @return Une matrice `nRows` x `nCols` remplie de "0" et "1" (caractères).
#'
#' @export
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
