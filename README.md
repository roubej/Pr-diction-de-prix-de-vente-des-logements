# Prédiction de prix de vente des logements

Ce projet a été réalisé dans le cadre du cours d'Analyse Prédictive. Son objectif est d'étudier et de comparer les méthodes de régression Ridge et Lasso pour prédire le prix de vente des logements à partir d'un ensemble de caractéristiques immobilières.

Le projet utilise le jeu de données Ames Housing et met en œuvre une analyse complète : exploration des données, prétraitement, modélisation, évaluation et comparaison des performances des modèles.

## Objectif

L'objectif principal est de comparer deux méthodes de régression pénalisée :

- Ridge : réduit les coefficients sans les annuler complètement
- Lasso : sélectionne les variables les plus importantes en réduisant certains coefficients à zéro

Les modèles sont évalués selon des métriques de performance telles que :

- RMSE
- MAE
- R²

## Structure du projet

```text
Pr-diction-de-prix-de-vente-des-logements/
├── README.md
├── housing/
│   ├── app/
│   │   └── app.R
│   ├── data/
│   │   └── AmesHousing.csv
│   ├── results/
│   ├── R/
│   │   ├── 01_packages.R
│   │   ├── 02_exploration.R
│   │   ├── 03_preprocessing.R
│   │   ├── 04_ridge.R
│   │   ├── 05_lasso.R
│   │   ├── 06_comparison.R
│   │   └── 07_prediction.R
│   ├── main.R
│   └── housing.Rproj
└── Guide_Presentations_Analyse_Predictive.pdf
```

## Données

Le projet utilise le dataset Ames Housing, qui contient plusieurs variables décrivant les logements, notamment :

- surface habitable
- qualité générale
- année de construction
- surface du sous-sol
- garage
- nombre de chambres
- nombre de salles de bain
- etc.

La variable cible est le prix de vente (`SalePrice`).

## Pipeline d'analyse

Le script principal `housing/main.R` exécute les étapes suivantes :

1. Chargement des packages nécessaires
2. Lecture des données
3. Exploration et visualisation des variables
4. Prétraitement des données
5. Modélisation Ridge
6. Modélisation Lasso
7. Comparaison des performances
8. Exemple de prédiction
9. Sauvegarde des résultats dans le dossier `housing/results/`

## Application Shiny

Un prototype d'application interactive est également disponible dans :

- `housing/app/app.R`

Cette application permet de :

- choisir entre le modèle Ridge ou Lasso
- saisir des caractéristiques d'une maison
- obtenir une estimation du prix de vente
- consulter les performances du modèle
- visualiser les prédictions par rapport aux valeurs réelles

## Prérequis

Avant de lancer le projet, assurez-vous d'avoir installé les packages R suivants :

```r
install.packages(c("shiny", "glmnet", "caret", "ggplot2", "dplyr", "readr"))
```

## Exécution de l'analyse

Ouvrez RStudio et exécutez le script principal :

```r
source("housing/main.R")
```

Le script va charger les données, exécuter les analyses et générer les graphiques dans le dossier :

```text
housing/results/
```

## Exécution de l'application

Pour lancer l'application Shiny :

```r
setwd("housing")
runApp("app")
```

ou directement depuis le dossier `housing/app` :

```r
source("app/app.R")
```

## Résultats attendus

Le projet permet de comparer les deux méthodes de régression pénalisée et de déterminer laquelle donne les meilleurs résultats selon la métrique retenue.

Les résultats incluent généralement :

- une comparaison visuelle des performances
- un exemple de prédiction sur une nouvelle maison
- des graphiques de diagnostics et d'évaluation

## Remarques

Ce projet montre une approche d'analyse prédictive appliquée à un problème de régression. Il illustre bien l'intérêt des méthodes de régularisation dans le cadre de données avec un grand nombre de variables corrélées.

## Licence

Le projet est fourni à des fins pédagogiques dans le cadre du cours d'Analyse Prédictive.

