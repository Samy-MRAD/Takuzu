# Takuzu

Ce package R propose une application Shiny pour jouer au jeu de logique **Takuzu** (aussi appelé Binairo), avec des grilles 8x8 générées dynamiquement.

## 📦 Installation et compilation du package

1. **Cloner ou télécharger le dépôt** contenant les fichiers du package.

2. Ouvrir le dossier du package dans RStudio (ouvrir le fichier `.Rproj` si présent, ou bien ouvrir le dossier comme projet RStudio).

3. Installer les dépendances nécessaires :

```r
install.packages(c("devtools", "shiny", "shinyjs"))

4. Utiliser l'application :
renv::restore() # pour recharger l'ensemble des packages déjà installés
devtools::load_all() # pour charger l'intégralité du package 
run_app() # pour lancer l'application
