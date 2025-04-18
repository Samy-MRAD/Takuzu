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
generer_grille_valide <- function(nRows, nCols, max_essais = 1000) {

  # Boucle : on retente 1000 fois jusqu'à obtenir une grille valide
  for (essai in 1:max_essais) {
    grille <- matrix("", nrow = nRows, ncol = nCols)
    ligne_ok <- TRUE # pour suivre la validité de la grille

    # Construction ligne par ligne
    for (i in 1:nRows) {
      zeros <- 0
      ones <- 0
      ligne <- rep("", nCols)

      for (j in 1:nCols) {
        possible_values <- c("0", "1")

        # Éviter les triples horizontalement
        if (j > 2 && ligne[j-1] == ligne[j-2]) {
          possible_values <- setdiff(possible_values, ligne[j-1])
        }

        # Éviter les triples verticalement
        if (i > 2 && grille[i-1, j] == grille[i-2, j]) {
          possible_values <- setdiff(possible_values, grille[i-1, j])
        }

        # Répartition équilibrée
        if (zeros >= nCols / 2) {
          possible_values <- setdiff(possible_values, "0")
        }
        if (ones >= nCols / 2) {
          possible_values <- setdiff(possible_values, "1")
        }

        # Si aucune valeur possible, on abandonne cette tentative
        if (length(possible_values) == 0) {
          ligne_ok <- FALSE
          break
        }

        # Choix aléatoire parmi les possibilités restantes
        ligne[j] <- sample(possible_values, 1)
        if (ligne[j] == "0") zeros <- zeros + 1
        if (ligne[j] == "1") ones <- ones + 1
      }

      if (!ligne_ok) break # Si la ligne est invalide, on stop la tentative

      # Vérification unicité de la ligne
      if (i > 1) {
        lignes_existantes <- apply(grille[1:(i-1), , drop = FALSE], 1, paste, collapse = "")
        if (paste(ligne, collapse = "") %in% lignes_existantes) {
          ligne_ok <- FALSE
          break
        }
      }

      grille[i, ] <- ligne
    }

    # Vérification unicité des colonnes
    if (ligne_ok) {
      colonnes <- apply(grille, 2, paste, collapse = "")
      if (!any(duplicated(colonnes))) {
        return(grille)
      }
    }
  }

  stop("Impossible de générer une grille valide après plusieurs essais.")
}
