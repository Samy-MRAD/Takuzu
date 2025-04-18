# Takuzu

## Description

Ce package R propose une application Shiny pour jouer au jeu de logique **Takuzu** (aussi appelé Binairo), avec des grilles 8x8 générées dynamiquement. L'application inclut :
- Une interface intuitive pour compléter la grille
- Des boutons de contrôle pour valider, recommencer ou dévoiler la solution
- Une vérification automatique de la grille une fois complétée

## Règles du jeu

Pour remplir la grille, le joueur doit suivre les règles suivantes :

1. Pas plus de deux chiffres identiques consécutifs (horizontalement ou verticalement).
2. Le même nombre de 0 et de 1 dans chaque ligne et chaque colonne.
3. Aucune ligne ou colonne identique à une autre.


Ce package R propose une application Shiny pour jouer au jeu de logique **Takuzu** (aussi appelé Binairo), avec des grilles 8x8 générées dynamiquement.

## Installation et compilation du package

1. Cloner ou télécharger le dépôt contenant les fichiers du package.

Lancez la commande suivante dans un terminal :

```bash
git clone https://github.com/nom-utilisateur/takuzu.git
```

2. Ouvrir le dossier du package dans RStudio (ouvrir le fichier `Takuzu.Rproj` si présent, ou bien ouvrir le dossier comme projet RStudio).

3. Il faut ensuite installer les dépendances nécessaires, lancez la commande suivante dans RStudio :

```r
install.packages(c("devtools", "shiny", "shinyjs"))
```

4. Et enfin lancer l'application :

```r
devtools::load_all()
run_app()
```

## Contributeurs 
- Samy M'RAD
- Fabian CONDAMY

