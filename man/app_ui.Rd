% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_ui.R
\name{app_ui}
\alias{app_ui}
\title{Interface utilisateur de l'application Shiny pour le jeu Takuzu}
\usage{
app_ui()
}
\value{
Un objet UI compatible avec l'interface d'une application Shiny.
}
\description{
Cette fonction crée l'interface utilisateur (UI) de l'application Takuzu.
Elle définit la structure visuelle de la page et applique un style personnalisé
pour une meilleure expérience utilisateur.
}
\details{
L'application utilise un design simple et réactif.
Elle contient :
\itemize{
\item Une barre latérale avec les boutons de contrôle : choix du niveau de difficulté,
bouton pour lancer une partie, révéler une case, et vérifier la grille.
\item Un panneau principal contenant la grille du jeu générée dynamiquement.
\item Des styles CSS intégrés via \code{tags$style()} pour personnaliser l'apparence des boutons,
de la grille et des différents états des cases.
}

Les boutons de la grille sont générés dans le serveur (voir \code{output$matrice_boutons}),
et sont stylisés pour refléter l'état actuel du jeu (valide, erroné, fixe).
}
