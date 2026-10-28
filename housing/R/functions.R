# ============================================================
# functions.R - Fonctions partagées (analyse ET application Shiny)
# ============================================================
# Ce fichier est appelé par :
#   - R/03_preprocessing.R  (analyse complète lancée par main.R)
#   - app/app.R             (application Shiny)
# Ainsi, les deux utilisent EXACTEMENT le même prétraitement.
# Nécessite le package caret (createDataPartition, dummyVars).
# ============================================================

# ------------------------------------------------------------
# prepare_ames() : prétraitement complet
#   data : data.frame brut (AmesHousing.csv)
#   p    : proportion du jeu d'apprentissage (ex. 0.80)
#   seed : graine aléatoire (reproductibilité)
# Retourne une liste avec les matrices x_train / x_test, les cibles
# y_train / y_test et les objets utiles pour prédire de nouvelles maisons.
# ------------------------------------------------------------
prepare_ames <- function(data, p = 0.80, seed = 123) {

  # 1. Suppression des identifiants
  d <- data[, !(names(data) %in% c("order", "pid")), drop = FALSE]

  # 2. Chaînes de caractères -> facteurs
  for (col in names(d)) {
    if (is.character(d[[col]])) d[[col]] <- as.factor(d[[col]])
  }

  # 3. Séparation apprentissage / test
  set.seed(seed)
  idx   <- caret::createDataPartition(d$saleprice, p = p, list = FALSE)
  train <- d[idx, , drop = FALSE]
  test  <- d[-idx, , drop = FALSE]

  # 4. Cible et variables explicatives
  y_train <- train$saleprice
  y_test  <- test$saleprice
  x_train_raw <- train[, names(train) != "saleprice", drop = FALSE]
  x_test_raw  <- test[,  names(test)  != "saleprice", drop = FALSE]

  # 5a. Variables numériques : NA -> médiane calculée sur le TRAIN
  num_cols <- names(x_train_raw)[sapply(x_train_raw, is.numeric)]
  medians  <- sapply(num_cols, function(col) {
    m <- median(x_train_raw[[col]], na.rm = TRUE)
    if (is.na(m)) 0 else m
  })

  # 5b. Variables catégorielles : NA -> "Missing", modalité inconnue -> "Other"
  fac_cols    <- names(x_train_raw)[sapply(x_train_raw, is.factor)]
  levels_list <- list()
  for (col in fac_cols) {
    v <- as.character(x_train_raw[[col]])
    v[is.na(v)] <- "Missing"
    levels_list[[col]] <- unique(c(unique(v), "Other"))
  }

  clean <- function(df) {
    for (col in num_cols) df[[col]][is.na(df[[col]])] <- medians[[col]]
    for (col in fac_cols) {
      v <- as.character(df[[col]])
      v[is.na(v)] <- "Missing"
      v[!(v %in% levels_list[[col]])] <- "Other"
      df[[col]] <- factor(v, levels = levels_list[[col]])
    }
    df
  }
  x_train_raw <- clean(x_train_raw)
  x_test_raw  <- clean(x_test_raw)

  # 6. Encodage des variables catégorielles (variables indicatrices)
  dummy   <- caret::dummyVars(~ ., data = x_train_raw, fullRank = TRUE)
  x_train <- as.matrix(predict(dummy, newdata = x_train_raw))
  x_test  <- as.matrix(predict(dummy, newdata = x_test_raw))

  # Valeur la plus fréquente de chaque variable catégorielle (pour prédire)
  modes <- sapply(fac_cols, function(col) {
    names(which.max(table(x_train_raw[[col]])))
  })

  list(x_train = x_train, x_test = x_test,
       y_train = y_train, y_test = y_test,
       x_train_raw = x_train_raw, x_test_raw = x_test_raw,
       dummy = dummy, medians = medians, modes = modes,
       train_data = train, test_data = test)
}

# ------------------------------------------------------------
# make_new_x() : construit la matrice d'UNE nouvelle maison
#   overrides : liste nommée des caractéristiques saisies
#               (ex. list(overall_qual = 7, gr_liv_area = 1500))
# Toutes les autres variables prennent une valeur "typique"
# (médiane pour les numériques, modalité la plus fréquente sinon).
# Quelques règles de cohérence évitent des maisons impossibles.
# ------------------------------------------------------------
make_new_x <- function(prep, overrides = list()) {
  ref <- prep$x_train_raw[1, , drop = FALSE]
  for (col in names(prep$medians)) ref[[col]] <- prep$medians[[col]]
  for (col in names(prep$modes)) {
    ref[[col]] <- factor(prep$modes[[col]], levels = levels(prep$x_train_raw[[col]]))
  }

  set_val <- function(col, value) {
    if (!(col %in% names(ref))) return(invisible(NULL))
    if (col %in% names(prep$modes)) {
      v <- as.character(value)
      if (!(v %in% levels(ref[[col]]))) v <- "Other"
      ref[[col]] <<- factor(v, levels = levels(ref[[col]]))
    } else {
      ref[[col]] <<- as.numeric(value)
    }
  }
  for (nm in names(overrides)) set_val(nm, overrides[[nm]])

  # --- Règles de cohérence (variables dérivées des saisies) ---
  ov <- overrides
  if (!is.null(ov$year_built)) {
    set_val("year_remod_add", ov$year_built)   # pas de rénovation avant la construction
    set_val("garage_yr_blt",  ov$year_built)   # garage construit avec la maison
  }
  if (!is.null(ov$garage_cars)) set_val("garage_area", 250 * ov$garage_cars)
  if (!is.null(ov$gr_liv_area)) {
    sous_sol <- if (is.null(ov$total_bsmt_sf)) 0 else ov$total_bsmt_sf
    etage1 <- if (sous_sol > 0) min(sous_sol, ov$gr_liv_area) else ov$gr_liv_area
    set_val("X1st_flr_sf", etage1)
    set_val("X2nd_flr_sf", ov$gr_liv_area - etage1)
  }
  if (!is.null(ov$bedroom_abvgr)) set_val("totrms_abvgrd", ov$bedroom_abvgr + 3)

  as.matrix(predict(prep$dummy, newdata = ref))
}

# ------------------------------------------------------------
# Métriques (mêmes formules que dans 04_ridge.R / 05_lasso.R)
# ------------------------------------------------------------
metrics_reg <- function(pred, obs) {
  c(RMSE = caret::RMSE(pred, obs),
    MAE  = caret::MAE(pred, obs),
    R2   = caret::R2(pred, obs))
}
