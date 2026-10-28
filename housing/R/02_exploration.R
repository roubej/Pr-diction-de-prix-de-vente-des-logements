# ============================================================
# 02 - Exploration des données
# ============================================================

cat("\n===== EXPLORATION DES DONNEES =====\n")

# Dimensions
cat("\nNombre de lignes :", nrow(data), "\n")
cat("Nombre de colonnes :", ncol(data), "\n")

# Structure
str(data)

# Résumé
summary(data)

# ------------------------------------------------------------
# Valeurs manquantes
# ------------------------------------------------------------

missing_values <- data.frame(
  variable = names(data),
  nb_na = colSums(is.na(data))
)

missing_values <- missing_values %>%
  mutate(
    pourcentage = round(nb_na / nrow(data) * 100, 2)
  ) %>%
  arrange(desc(nb_na))

print(head(missing_values, 20))

# ------------------------------------------------------------
# Distribution du prix de vente
# ------------------------------------------------------------

p1 <- ggplot(data, aes(x = saleprice)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution du prix de vente",
    x = "Prix de vente ($)",
    y = "Nombre de logements"
  ) +
  theme_minimal()

print(p1)

ggsave(
  "results/distribution_saleprice.png",
  p1,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Boxplot du prix
# ------------------------------------------------------------

p2 <- ggplot(data, aes(y = saleprice)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Boxplot du prix de vente",
    y = "Prix de vente ($)",
    x = ""
  ) +
  theme_minimal()

print(p2)

# ------------------------------------------------------------
# Surface habitable vs prix
# ------------------------------------------------------------

p3 <- ggplot(data, aes(x = gr_liv_area, y = saleprice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Surface habitable et prix de vente",
    x = "Surface habitable",
    y = "Prix de vente"
  ) +
  theme_minimal()

print(p3)

ggsave(
  "results/surface_vs_prix.png",
  p3,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Qualité générale vs prix
# ------------------------------------------------------------

p4 <- ggplot(
  data,
  aes(x = factor(overall_qual), y = saleprice)
) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Prix selon la qualité générale",
    x = "Qualité générale",
    y = "Prix de vente"
  ) +
  theme_minimal()

print(p4)

ggsave(
  "results/qualite_vs_prix.png",
  p4,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Corrélations entre variables numériques
# ------------------------------------------------------------

numeric_data <- data %>%
  select(where(is.numeric))

correlations <- cor(
  numeric_data,
  use = "pairwise.complete.obs"
)

# Corrélation avec SalePrice
cor_saleprice <- sort(
  correlations[, "saleprice"],
  decreasing = TRUE
)

cat("\nVariables les plus corrélées avec SalePrice :\n")
print(head(cor_saleprice, 15))

# Sélection des variables les plus corrélées
top_variables <- names(
  sort(abs(correlations[, "saleprice"]),
       decreasing = TRUE)[1:15]
)

png(
  "results/correlation_matrix.png",
  width = 1000,
  height = 800
)

corrplot(
  correlations[top_variables, top_variables],
  method = "color",
  type = "upper",
  tl.cex = 0.7
)

dev.off()