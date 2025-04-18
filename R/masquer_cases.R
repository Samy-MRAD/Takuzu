#' Fonction qui permet de cacher des cases de la grille issue de la fonction `generer_grille_valide`
#'
#' Cette fonction prend une grille valide de Takuzu et remplace un certain
#' nombre de cases par des cases vides, afin de créer une grille jouable.
#'
#' @param grille Grille valide de Takuzu (matrice de "0"/"1").
#' @param nb_vide Nombre de cases à cacher (entier).
#'
#' @return Une matrice avec des "0", "1" et des cases vides (chaînes vides).
#'
#' @export
masquer_cases <- function(grille, nb_vide) {
  total_buttons <- length(grille)
  indices <- sample(1:total_buttons, nb_vide) # masque des cases en fonction de nb_vide
  grille[indices] <- ""
  return(grille)
}
