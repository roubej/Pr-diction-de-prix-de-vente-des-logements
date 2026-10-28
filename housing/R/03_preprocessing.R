# ============================================================
# 03 - Prétraitement des données
# ============================================================
# Le travail est fait par prepare_ames() (fichier R/functions.R),
# la MÊME fonction que celle utilisée par l'application Shiny.
# Étapes : suppression des identifiants, facteurs, découpage 80/20,
# imputation (médiane / "Missing" / "Other"), encodage en variables
# indicatrices (dummyVars).
# ============================================================

cat("\n===== PRETRAITEMENT =====\n")

source("R/functions.R")

prep <- prepare_ames(data, p = 0.80, seed = 123)

# On garde les mêmes noms d'objets que dans la version précédente,
# car 04_ridge.R à 07_prediction.R les utilisent.
x_train     <- prep$x_train
x_test      <- prep$x_test
y_train     <- prep$y_train
y_test      <- prep$y_test
train_data  <- prep$train_data
test_data   <- prep$test_data
dummy_model <- prep$dummy

cat("Train :", nrow(train_data), "observations\n")
cat("Test  :", nrow(test_data), "observations\n")

cat("\nDimensions après encodage :\n")
cat("X Train :", dim(x_train), "\n")
cat("X Test  :", dim(x_test), "\n")
cat("\nNombre de NA dans X Train :", sum(is.na(x_train)), "\n")
cat("Nombre de NA dans X Test :", sum(is.na(x_test)), "\n")

# glmnet effectue par défaut la standardisation des variables
