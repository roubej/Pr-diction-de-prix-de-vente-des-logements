# ============================================================
# 03 - Prétraitement des données
# ============================================================

cat("\n===== PRETRAITEMENT =====\n")

data_clean <- data

# ------------------------------------------------------------
# 1. Suppression des identifiants
# ------------------------------------------------------------

data_clean <- data_clean %>%
  select(-order, -pid)

# ------------------------------------------------------------
# 2. Conversion des chaînes de caractères en facteurs
# ------------------------------------------------------------

data_clean <- data_clean %>%
  mutate(
    across(where(is.character), as.factor)
  )

# ------------------------------------------------------------
# 3. Division Train / Test
# ------------------------------------------------------------

set.seed(123)

train_index <- createDataPartition(
  data_clean$saleprice,
  p = 0.80,
  list = FALSE
)

train_data <- data_clean[train_index, ]
test_data  <- data_clean[-train_index, ]

cat("Train :", nrow(train_data), "observations\n")
cat("Test  :", nrow(test_data), "observations\n")

# ------------------------------------------------------------
# 4. Variable cible
# ------------------------------------------------------------

y_train <- train_data$saleprice
y_test  <- test_data$saleprice

# On retire SalePrice des variables explicatives
x_train_raw <- train_data %>%
  select(-saleprice)

x_test_raw <- test_data %>%
  select(-saleprice)

# ------------------------------------------------------------
# 5. Traitement des valeurs manquantes
# ------------------------------------------------------------

# Variables numériques :
# remplacement par la médiane calculée sur le TRAIN

numeric_cols <- names(
  x_train_raw %>%
    select(where(is.numeric))
)

for(col in numeric_cols) {
  
  med <- median(
    x_train_raw[[col]],
    na.rm = TRUE
  )
  
  # Si une colonne est complètement vide
  if(is.na(med)) {
    med <- 0
  }
  
  x_train_raw[[col]][is.na(x_train_raw[[col]])] <- med
  x_test_raw[[col]][is.na(x_test_raw[[col]])] <- med
}

# ------------------------------------------------------------
# Variables catégorielles
# ------------------------------------------------------------

factor_cols <- names(
  x_train_raw %>%
    select(where(is.factor))
)

for(col in factor_cols) {
  
  # Transformer temporairement en caractères
  train_values <- as.character(x_train_raw[[col]])
  test_values  <- as.character(x_test_raw[[col]])
  
  # NA devient "Missing"
  train_values[is.na(train_values)] <- "Missing"
  test_values[is.na(test_values)]   <- "Missing"
  
  # Modalités présentes dans le train
  train_levels <- unique(train_values)
  
  # Si une nouvelle modalité apparaît dans le test,
  # on la considère comme "Other"
  test_values[
    !(test_values %in% train_levels)
  ] <- "Other"
  
  levels_final <- unique(
    c(train_levels, "Other")
  )
  
  x_train_raw[[col]] <- factor(
    train_values,
    levels = levels_final
  )
  
  x_test_raw[[col]] <- factor(
    test_values,
    levels = levels_final
  )
}

# ------------------------------------------------------------
# 6. Encodage des variables catégorielles
# ------------------------------------------------------------

# dummyVars crée les variables indicatrices
dummy_model <- dummyVars(
  ~ .,
  data = x_train_raw,
  fullRank = TRUE
)

x_train <- predict(
  dummy_model,
  newdata = x_train_raw
)

x_test <- predict(
  dummy_model,
  newdata = x_test_raw
)

# Conversion en matrices
x_train <- as.matrix(x_train)
x_test  <- as.matrix(x_test)

# Vérification
cat("\nDimensions après encodage :\n")
cat("X Train :", dim(x_train), "\n")
cat("X Test  :", dim(x_test), "\n")

cat("\nNombre de NA dans X Train :", sum(is.na(x_train)), "\n")
cat("Nombre de NA dans X Test :", sum(is.na(x_test)), "\n")

# glmnet effectue par défaut la standardisation des variables