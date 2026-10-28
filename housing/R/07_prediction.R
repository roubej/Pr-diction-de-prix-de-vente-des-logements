# ============================================================
# 07 - Exemple de prédiction
# ============================================================

cat("\n===== EXEMPLE DE PREDICTION =====\n")

# Pour commencer, on prend une observation du jeu de test
new_house <- x_test[1, , drop = FALSE]

prediction_ridge <- predict(
  ridge_model,
  newx = new_house,
  s = lambda_ridge_min
)

prediction_lasso <- predict(
  lasso_model,
  newx = new_house,
  s = lambda_lasso_min
)

cat(
  "Prix réel :",
  round(y_test[1], 2),
  "$\n"
)

cat(
  "Prédiction Ridge :",
  round(as.numeric(prediction_ridge), 2),
  "$\n"
)

cat(
  "Prédiction Lasso :",
  round(as.numeric(prediction_lasso), 2),
  "$\n"
)