# ============================================================
# 06 - Comparaison Ridge / Lasso
# ============================================================

cat("\n===== COMPARAISON DES MODELES =====\n")

# ------------------------------------------------------------
# Tableau des performances
# ------------------------------------------------------------

comparison <- data.frame(
  Modele = c(
    "Ridge",
    "Lasso"
  ),
  
  Lambda = c(
    lambda_ridge_min,
    lambda_lasso_min
  ),
  
  RMSE = c(
    rmse_ridge,
    rmse_lasso
  ),
  
  MAE = c(
    mae_ridge,
    mae_lasso
  ),
  
  R2 = c(
    r2_ridge,
    r2_lasso
  )
)

comparison <- comparison %>%
  mutate(
    Lambda = round(Lambda, 4),
    RMSE = round(RMSE, 2),
    MAE = round(MAE, 2),
    R2 = round(R2, 4)
  )

print(comparison)

write.csv(
  comparison,
  "results/comparison_models.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# Comparaison RMSE
# ------------------------------------------------------------

p_rmse <- ggplot(
  comparison,
  aes(x = Modele, y = RMSE, fill = Modele)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = RMSE),
    vjust = -0.5
  ) +
  labs(
    title = "Comparaison du RMSE",
    x = "",
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

print(p_rmse)

ggsave(
  "results/comparison_rmse.png",
  p_rmse,
  width = 7,
  height = 5
)

# ------------------------------------------------------------
# Prix réel vs prédit - Ridge
# ------------------------------------------------------------

results_test <- data.frame(
  Prix_reel = y_test,
  Ridge = pred_ridge,
  Lasso = pred_lasso
)

p_ridge <- ggplot(
  results_test,
  aes(x = Prix_reel, y = Ridge)
) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Ridge : prix réels vs prix prédits",
    x = "Prix réel",
    y = "Prix prédit"
  ) +
  theme_minimal()

ggsave(
  "results/ridge_real_vs_predicted.png",
  p_ridge,
  width = 7,
  height = 5
)

# ------------------------------------------------------------
# Prix réel vs prédit - Lasso
# ------------------------------------------------------------

p_lasso <- ggplot(
  results_test,
  aes(x = Prix_reel, y = Lasso)
) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Lasso : prix réels vs prix prédits",
    x = "Prix réel",
    y = "Prix prédit"
  ) +
  theme_minimal()

ggsave(
  "results/lasso_real_vs_predicted.png",
  p_lasso,
  width = 7,
  height = 5
)

# ------------------------------------------------------------
# Quel modèle est meilleur ?
# ------------------------------------------------------------

if(rmse_ridge < rmse_lasso) {
  
  cat(
    "\nSelon le RMSE, Ridge donne la meilleure performance prédictive.\n"
  )
  
} else {
  
  cat(
    "\nSelon le RMSE, Lasso donne la meilleure performance prédictive.\n"
  )
}

cat(
  "\nLasso a sélectionné",
  nrow(selected_lasso),
  "variables sur",
  ncol(x_train),
  "variables après encodage.\n"
)