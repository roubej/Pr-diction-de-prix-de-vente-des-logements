# Prédiction de prix de vente des logements : Ridge, Lasso & Elastic Net

Projet R (Master 2 Data Science & Systèmes d'Information, Analyse Prédictive).
Données : **Ames Housing** (ventes réelles à Ames, Iowa, 2006-2010, prix en dollars US).

## Contenu du projet (dossier `housing/`)

| Fichier | Rôle |
|---|---|
| `housing.Rproj` | Ouvre le projet dans RStudio (fixe le dossier de travail) |
| `main.R` | Lance toute l'analyse, dans l'ordre |
| `R/01_packages.R` | Installe et charge les packages, fixe la graine |
| `R/functions.R` | **Fonctions partagées** : `prepare_ames()`, `make_new_x()`, `metrics_reg()` |
| `R/02_exploration.R` | Graphiques d'exploration |
| `R/03_preprocessing.R` | Prétraitement (appelle `prepare_ames()`) |
| `R/04_ridge.R`, `R/05_lasso.R` | Modèles Ridge et Lasso (validation croisée) |
| `R/06_comparison.R` | Comparaison Ridge / Lasso |
| `R/07_prediction.R` | Exemple de prédiction |
| `app/app.R` | **Application Shiny** (8 onglets, français, mode clair/sombre) |
| `data/AmesHousing.csv` | Jeu de données |
| `results/` | Graphiques et tableau générés par `main.R` |

L'analyse (`main.R`) et l'application utilisent **le même prétraitement** (`R/functions.R`),
donc leurs résultats sont cohérents.

## Installation (une seule fois, dans la console R)

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "glmnet", "caret",
                   "tidyverse", "corrplot", "Metrics", "scales"))
```

## Lancer l'analyse

1. Double-cliquer sur `housing/housing.Rproj` (RStudio).
2. Ouvrir `main.R` puis cliquer sur **Source** (ou `source("main.R")`).
3. Les graphiques et `comparison_models.csv` sont écrits dans `results/`.

## Lancer l'application

1. Ouvrir `housing/app/app.R` dans RStudio.
2. Cliquer sur **Run App** (ou, depuis `housing/` : `shiny::runApp("app")`).

Onglets : Données · Exploration · Modèle · Variables · Comparaison · Robustesse · Prédiction · Théorie.

Options utiles de l'onglet Modèle :
- **Exclure les 5 maisons de plus de 4 000 pi²** (ventes atypiques) : améliore et stabilise les résultats ;
- **Modéliser log(prix)** : à utiliser avec l'exclusion ci-dessus (sinon le résultat peut être très instable) ;
- l'onglet **Robustesse** répète l'évaluation sur plusieurs découpages aléatoires (moyenne ± écart-type).
Le bouton lune/soleil en haut à droite bascule entre mode clair et mode sombre.

## Reproductibilité

`set.seed(123)` dans `01_packages.R`; la graine est aussi réglable dans l'application.
