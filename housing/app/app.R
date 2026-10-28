# ============================================================
# APPLICATION SHINY - Ridge / Lasso / Elastic Net
# Dataset : Ames Housing (prix de vente en dollars US)
# ============================================================
# Lancement : ouvrir ce fichier dans RStudio puis cliquer sur "Run App",
#             ou dans la console : shiny::runApp("app")  (depuis housing/)
#
# Packages nécessaires (à installer UNE fois, dans la console) :
#   install.packages(c("shiny", "bslib", "plotly", "DT", "glmnet", "caret"))
#
# Organisation du fichier :
#   1. Packages et données        (exécuté une seule fois au démarrage)
#   2. Fonctions d'affichage      (thème des graphiques, formats français)
#   3. UI                         (l'interface : 7 onglets)
#   4. SERVER                     (le calcul : modèle, graphiques, prédiction)
# ============================================================

# ------------------------------------------------------------
# 1. PACKAGES ET DONNEES
# ------------------------------------------------------------
needed  <- c("shiny", "bslib", "plotly", "DT", "glmnet", "caret")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Packages manquants : ", paste(missing, collapse = ", "),
       ".\nInstallez-les avec : install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
}
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(glmnet)
library(caret)

# Chemin du projet : fonctionne avec "Run App" (dossier app/) ou depuis housing/
root <- if (file.exists("../data/AmesHousing.csv")) ".." else
        if (file.exists("data/AmesHousing.csv"))    "."  else
        stop("Fichier data/AmesHousing.csv introuvable. Ouvrez le projet housing.Rproj.", call. = FALSE)

source(file.path(root, "R", "functions.R"))   # prepare_ames(), make_new_x(), metrics_reg()

data <- read.csv(file.path(root, "data", "AmesHousing.csv"), stringsAsFactors = FALSE)

num_vars <- setdiff(names(data)[sapply(data, is.numeric)], c("order", "pid", "saleprice"))
neighborhoods <- sort(unique(data$neighborhood))

# Noms des variables en français (ceux qui manquent gardent leur nom d'origine)
labels_fr <- c(
  saleprice = "Prix de vente ($)", overall_qual = "Qualité générale (1-10)",
  overall_cond = "État général (1-10)", gr_liv_area = "Surface habitable (pi²)",
  year_built = "Année de construction", year_remod_add = "Année de rénovation",
  total_bsmt_sf = "Surface du sous-sol (pi²)", garage_cars = "Places de garage",
  garage_area = "Surface du garage (pi²)", full_bath = "Salles de bain complètes",
  half_bath = "Demi-salles de bain", bedroom_abvgr = "Chambres",
  lot_area = "Surface du terrain (pi²)", lot_frontage = "Façade du terrain (pi)",
  X1st_flr_sf = "Surface du 1er niveau (pi²)", X2nd_flr_sf = "Surface du 2e niveau (pi²)",
  totrms_abvgrd = "Pièces (hors sous-sol)", fireplaces = "Cheminées",
  mas_vnr_area = "Surface de placage (pi²)", wood_deck_sf = "Terrasse en bois (pi²)",
  open_porch_sf = "Porche ouvert (pi²)", neighborhood = "Quartier",
  house_style = "Style de maison", bldg_type = "Type de bâtiment",
  central_air = "Climatisation centrale", kitchen_qual = "Qualité de la cuisine",
  ms_zoning = "Zonage", sale_condition = "Condition de vente",
  garage_yr_blt = "Année du garage", bsmtfin_sf_1 = "Sous-sol aménagé (pi²)", bsmt_unf_sf = "Sous-sol non fini (pi²)",
  enclosed_porch = "Porche fermé (pi²)", bsmt_qual = "Qualité du sous-sol", exter_qual = "Qualité extérieure",
  roof_matl = "Matériau du toit", misc_feature = "Équipement divers", condition_2 = "Condition (2)",
  bsmt_exposure = "Accès extérieur du sous-sol", pool_qc = "Qualité de la piscine", ms_subclass = "Type de logement"
)
# Nom lisible : libellé français, sinon le nom d'origine sans les « _ »
lab <- function(v) ifelse(v %in% names(labels_fr), unname(labels_fr[v]), gsub("_", " ", v))

# Codes de qualité -> français
qual_fr <- c(Ex = "Excellent", Gd = "Bon", TA = "Moyen", Fa = "Passable", Po = "Mauvais", Missing = "Absent", None = "Aucun")

# Colonnes issues du codage 0/1 (ex. "bsmt_qual.Ex") -> "Qualité du sous-sol : Excellent"
pretty_var <- function(v) {
  vapply(v, function(x) {
    parts <- strsplit(x, ".", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      lv <- paste(parts[-1], collapse = ".")
      if (lv %in% names(qual_fr)) lv <- qual_fr[[lv]]
      paste0(lab(parts[1]), " : ", lv)
    } else lab(x)
  }, character(1), USE.NAMES = FALSE)
}

# Ventes atypiques : 3 grandes maisons vendues à bas prix (ventes « Partial »)
atyp_rows <- which(data$gr_liv_area > 4000 & data$saleprice < 200000)

# Textes du tableau DT en français
dt_lang <- list(search = "Rechercher :", lengthMenu = "Afficher _MENU_ lignes",
                info = "Lignes _START_ à _END_ sur _TOTAL_", zeroRecords = "Aucun résultat",
                thousands = "\u00a0", decimal = ",",
                paginate = list(previous = "Précédent", `next` = "Suivant"))

# Variables catégorielles proposées dans l'onglet Exploration
cat_choices <- c("Quartier" = "neighborhood", "Qualité générale" = "overall_qual",
                 "Style de maison" = "house_style", "Type de bâtiment" = "bldg_type",
                 "Climatisation centrale" = "central_air", "Qualité de la cuisine" = "kitchen_qual",
                 "Zonage" = "ms_zoning", "Condition de vente" = "sale_condition")

# ------------------------------------------------------------
# 2. FONCTIONS D'AFFICHAGE
# ------------------------------------------------------------
# Style des tableaux DT : bootstrap5 si la version de DT le permet, sinon bootstrap4
dt_style <- tryCatch({
  ok <- eval(formals(DT::datatable)$style)
  if ("bootstrap5" %in% ok) "bootstrap5" else "bootstrap4"
}, error = function(e) "default")

fmt_k <- function(x) paste0(formatC(round(x / 1000), format = "d", big.mark = "\u00a0"), "\u00a0k$")
fmt_usd <- function(x) paste0(formatC(round(x), format = "d", big.mark = "\u00a0"), "\u00a0$")
fmt_num <- function(x, d = 3) format(round(x, d), nsmall = d, decimal.mark = ",")

# Couleurs des graphiques selon le mode clair / sombre
pal <- function(mode) {
  if (identical(mode, "dark")) {
    list(fg = "#E6EAF0", grid = "rgba(255,255,255,0.12)", main = "#2BB3C4", accent = "#F2B450", muted = "#9AA3B2",
         band = "rgba(43,179,196,0.25)", bad = "#FF7F6B", mid = "#2b3038")
  } else {
    list(fg = "#1d2a3a", grid = "rgba(0,0,0,0.10)", main = "#0F7B8A", accent = "#D9822B", muted = "#7A8499",
         band = "rgba(15,123,138,0.20)", bad = "#C0392B", mid = "#f5f5f5")
  }
}

# Applique le même style (fond transparent, couleurs, polices) à tous les graphiques
style_plot <- function(p, mode, title = NULL, xtitle = NULL, ytitle = NULL, ...) {
  cl <- pal(mode)
  p %>%
    layout(
      title = list(text = title, x = 0.02, font = list(size = 15)),
      xaxis = list(title = xtitle, gridcolor = cl$grid, zerolinecolor = cl$grid),
      yaxis = list(title = ytitle, gridcolor = cl$grid, zerolinecolor = cl$grid),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = cl$fg), margin = list(t = 55),
      legend = list(orientation = "h", y = -0.25), ...
    ) %>%
    config(displaylogo = FALSE, locale = "fr")
}

# Ligne verticale + étiquette (pour λ, moyenne, prix estimé...)
vline <- function(x, color, dash = "dash") {
  list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
       line = list(color = color, width = 1.6, dash = dash))
}
vlabel <- function(x, text, color, y = 1.04) {
  list(x = x, y = y, yref = "paper", text = text, showarrow = FALSE,
       font = list(color = color, size = 11))
}

# ------------------------------------------------------------
# 3. UI (interface)
# ------------------------------------------------------------
ui <- page_navbar(
  title = "Prix immobilier · Ridge, Lasso & Elastic Net",
  id = "nav", lang = "fr", fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#0F7B8A", secondary = "#E8A33D",
                   base_font = c("Segoe UI", "system-ui", "-apple-system", "sans-serif")),
  header = tagList(
    tags$style(HTML("
      .card-header { font-weight: 600; }
      .bslib-value-box .value-box-value { font-size: 1.6rem; }
      .small-note { font-size: 0.85rem; opacity: 0.75; }
      .bslib-grid > .card, .bslib-grid > div { min-width: 0; }
      .js-plotly-plot, .plotly.html-widget { max-width: 100%; }
    ")),
    # Les graphiques créés dans un onglet caché sont redimensionnés quand l'onglet s'affiche
    tags$script(HTML("$(document).on('shown.bs.tab', function() { setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 150); });"))
  ),

  # ---------------- Onglet 1 : Données ----------------
  nav_panel("Données", icon = icon("table"),
    layout_column_wrap(width = 1/4, fill = FALSE,
      value_box("Logements", format(nrow(data), big.mark = "\u00a0"), theme = "primary"),
      value_box("Variables", ncol(data) - 2, theme = "primary"),
      value_box("Prix médian", fmt_usd(median(data$saleprice)), theme = "primary"),
      value_box("Cellules manquantes",
                paste0(fmt_num(100 * mean(is.na(data)), 1), " %"), theme = "primary")
    ),
    layout_column_wrap(width = 1/2,
      card(card_header("À propos du jeu de données"),
        p("Ames Housing : ventes ", strong("réelles"), " de maisons à Ames (Iowa, États-Unis)
          entre 2006 et 2010, d'après le registre du service d'évaluation de la ville.
          Compilé par Dean De Cock (2011, ", em("Journal of Statistics Education"), ")."),
        tags$ul(
          tags$li(strong("Cible : "), "saleprice, prix de vente en dollars US."),
          tags$li(strong("Explicatives : "), "environ 80 variables (surfaces, qualité, année,
                  quartier, garage, sous-sol...), numériques et catégorielles."),
          tags$li(strong("Valeurs atypiques : "), "5 maisons de plus de 4 000 pi², dont 3 ventes
                  « Partial » (maisons non terminées) vendues très en dessous de leur valeur."),
          tags$li(strong("Pourquoi ce jeu : "), "beaucoup de variables corrélées pour un nombre
                  limité de logements, donc situation idéale pour Ridge, Lasso et Elastic Net.")
        ),
        p(class = "small-note", "1 pi² (pied carré) = 0,093 m².")),
      card(card_header("Variables avec des valeurs manquantes"),
           plotlyOutput("plot_na", height = "320px"), full_screen = TRUE)
    ),
    card(card_header("Aperçu des données"), DTOutput("tbl_data"), full_screen = TRUE)
  ),

  # ---------------- Onglet 2 : Exploration ----------------
  nav_panel("Exploration", icon = icon("chart-column"),
    navset_card_tab(id = "explo",
      nav_panel("Distribution du prix",
        radioButtons("price_scale", NULL, inline = TRUE,
                     choices = c("Prix ($)" = "lin", "log(prix)" = "log")),
        plotlyOutput("plot_price", height = "400px"),
        uiOutput("price_stats")),
      nav_panel("Relation avec le prix",
        layout_column_wrap(width = 1/2, fill = FALSE,
          selectInput("xvar", "Variable explicative",
                      choices = setNames(num_vars, lab(num_vars)), selected = "gr_liv_area"),
          checkboxInput("color_qual", "Colorer selon la qualité générale", TRUE)),
        plotlyOutput("plot_scatter", height = "430px")),
      nav_panel("Prix par catégorie",
        selectInput("catvar", "Variable catégorielle", choices = cat_choices),
        plotlyOutput("plot_box", height = "520px")),
      nav_panel("Corrélations",
        sliderInput("topn_corr", "Nombre de variables les plus corrélées au prix",
                    min = 5, max = 25, value = 12, step = 1),
        plotlyOutput("plot_corr", height = "780px"))
    )
  ),

  # ---------------- Onglet 3 : Modèle ----------------
  nav_panel("Modèle", icon = icon("sliders"),
    layout_sidebar(
      sidebar = sidebar(width = 330, title = "Paramètres du modèle",
        selectInput("algo", "Algorithme", c("Ridge", "Lasso", "Elastic Net"), selected = "Lasso"),
        conditionalPanel("input.algo == 'Elastic Net'",
          sliderInput("alpha", "Mélange α (0 = Ridge, 1 = Lasso)",
                      min = 0.05, max = 0.95, value = 0.5, step = 0.05)),
        radioButtons("lam_rule", "Choix de λ",
          choices = c("λ min (meilleure erreur CV)" = "min",
                      "λ 1se (modèle plus simple)" = "1se",
                      "Manuel" = "manuel")),
        conditionalPanel("input.lam_rule == 'manuel'",
          sliderInput("loglam", "log10(λ)", min = -4, max = 6, value = 3, step = 0.1)),
        sliderInput("folds", "Plis de validation croisée", min = 3, max = 10, value = 10, step = 1),
        sliderInput("split", "Part d'apprentissage (%)", min = 60, max = 90, value = 80, step = 5),
        numericInput("seed", "Graine aléatoire", value = 123, min = 1, step = 1),
        checkboxInput("excl", "Exclure les 5 maisons de plus de 4 000 pi² (ventes atypiques)", FALSE),
        p(class = "small-note", "Retire 3 ventes « Partial » très atypiques et 2 très grandes maisons,
          comme le suggère l'auteur du jeu de données. Améliore et stabilise les résultats."),
        checkboxInput("use_log", "Modéliser log(prix) au lieu du prix", FALSE),
        uiOutput("log_warning"),
        actionButton("train", "Entraîner le modèle", icon = icon("play"),
                     class = "btn-primary w-100"),
        hr(),
        downloadButton("dl_pred", "Prédictions du test (CSV)", class = "btn-outline-secondary btn-sm w-100")
      ),
      layout_column_wrap(width = 1/4, fill = FALSE,
        value_box("RMSE (test)", textOutput("vb_rmse"), theme = "primary"),
        value_box("MAE (test)", textOutput("vb_mae"), theme = "primary"),
        value_box("R² (test)", textOutput("vb_r2"), theme = "primary"),
        value_box("Variables retenues", textOutput("vb_nvar"), theme = "primary")
      ),
      textOutput("lam_info"),
      navset_card_tab(id = "res",
        nav_panel("Validation croisée", plotlyOutput("plot_cv", height = "420px")),
        nav_panel("Chemins des coefficients", plotlyOutput("plot_path", height = "460px")),
        nav_panel("Réel vs prédit", plotlyOutput("plot_pred", height = "460px")),
        nav_panel("Résidus",
          layout_column_wrap(width = 1/2,
            plotlyOutput("plot_resid", height = "400px"),
            plotlyOutput("plot_resid_hist", height = "400px")))
      )
    )
  ),

  # ---------------- Onglet 4 : Variables ----------------
  nav_panel("Variables", icon = icon("list-ol"),
    card(card_header("Variables les plus influentes (coefficients standardisés)"),
      sliderInput("topn", "Nombre de variables affichées", min = 5, max = 40, value = 15, step = 1),
      plotlyOutput("plot_coef", height = "520px"),
      p(class = "small-note", "Coefficient standardisé = coefficient × écart-type de la variable :
        permet de comparer des variables d'unités différentes. Les barres turquoise augmentent le
        prix, les rouges le diminuent. Les barres orange (⚠) sont des catégories présentes dans 3 maisons
        ou moins : leur coefficient absorbe surtout une vente atypique, il ne faut pas l'interpréter."),
      full_screen = TRUE),
    card(card_header("Tous les coefficients non nuls"),
      downloadButton("dl_coef", "Télécharger (CSV)", class = "btn-outline-secondary btn-sm"),
      DTOutput("tbl_coef"))
  ),

  # ---------------- Onglet 5 : Comparaison ----------------
  nav_panel("Comparaison", icon = icon("scale-balanced"),
    card(
      p("Compare la régression classique (référence), Ridge, Lasso et Elastic Net (α = 0,5) avec les mêmes
        réglages que l'onglet « Modèle » (découpage, graine, plis, log-prix, exclusion des ventes atypiques).
        Attention : un seul découpage peut être trompeur, voir l'onglet « Robustesse »."),
      actionButton("compare", "Comparer les trois modèles", icon = icon("play"),
                   class = "btn-primary", style = "max-width:320px")),
    uiOutput("compare_out")
  ),

  # ---------------- Onglet 5b : Robustesse ----------------
  nav_panel("Robustesse", icon = icon("shuffle"),
    card(
      p("Un seul découpage apprentissage/test peut être trop favorable ou trop sévère. Ici, on répète
        l'évaluation sur plusieurs découpages aléatoires (graines 1, 2, 3...) avec les réglages de l'onglet
        « Modèle » (λ, plis, log-prix, exclusion des ventes atypiques), pour voir si les résultats sont stables."),
      layout_column_wrap(width = 1/3, fill = FALSE,
        sliderInput("n_splits", "Nombre de découpages", min = 5, max = 20, value = 10, step = 1),
        div(style = "padding-top:28px", actionButton("robust", "Lancer l'évaluation", icon = icon("play"), class = "btn-primary")),
        p(class = "small-note", style = "padding-top:28px", "Compter environ 5 secondes par découpage.")
      )),
    uiOutput("robust_out")
  ),

  # ---------------- Onglet 6 : Prédiction ----------------
  nav_panel("Prédiction", icon = icon("house"),
    layout_sidebar(
      sidebar = sidebar(width = 330, title = "Caractéristiques du logement",
        selectInput("p_neigh", "Quartier", choices = neighborhoods,
                    selected = names(which.max(table(data$neighborhood)))),
        sliderInput("p_qual", "Qualité générale (1-10)", min = 1, max = 10, value = 6, step = 1),
        numericInput("p_area", "Surface habitable (pi²)", value = 1500, min = 300, max = 6000, step = 50),
        textOutput("p_area_m2"),
        numericInput("p_year", "Année de construction", value = 1995, min = 1870, max = 2010, step = 1),
        numericInput("p_bsmt", "Surface du sous-sol (pi²)", value = 1000, min = 0, max = 4000, step = 50),
        sliderInput("p_garage", "Places de garage", min = 0, max = 5, value = 2, step = 1),
        sliderInput("p_bath", "Salles de bain complètes", min = 0, max = 4, value = 2, step = 1),
        sliderInput("p_bed", "Chambres", min = 0, max = 8, value = 3, step = 1),
        actionButton("estimate", "Estimer le prix", icon = icon("calculator"),
                     class = "btn-primary w-100"),
        p(class = "small-note", style = "margin-top:10px",
          "Les autres caractéristiques prennent la valeur typique du jeu d'apprentissage
           (médiane ou modalité la plus fréquente).")
      ),
      uiOutput("p_warning"),
      layout_column_wrap(width = 1/3, fill = FALSE,
        value_box("Prix estimé", textOutput("p_price"), theme = "primary"),
        value_box("Fourchette indicative (± 1 RMSE)", textOutput("p_range"), theme = "secondary"),
        value_box("Prix médian du quartier", textOutput("p_neigh_med"), theme = "secondary")
      ),
      card(card_header("Où se situe ce logement ?"),
           plotlyOutput("plot_where", height = "380px")),
      p(class = "small-note", "Le modèle utilisé est celui entraîné dans l'onglet « Modèle »
        (algorithme, λ et paramètres choisis). L'estimation est indicative.")
    )
  ),

  # ---------------- Onglet 7 : Théorie ----------------
  nav_panel("Théorie", icon = icon("book"),
    layout_column_wrap(width = 1/2,
      card(card_header("Pourquoi régulariser ?"),
        p("Avec beaucoup de variables corrélées, les moindres carrés (MCO) donnent des coefficients
          instables : ils changent fortement d'un échantillon à l'autre et prédisent mal sur de nouvelles
          données. La régularisation pénalise la taille des coefficients : on accepte un petit biais
          pour une forte baisse de variance (compromis biais-variance).")),
      card(card_header("Les trois méthodes"),
        tags$ul(
          tags$li(strong("Ridge (L2) : "), "minimise (1/2n)‖y − Xβ‖² + (λ/2)‖β‖². Réduit tous les
                  coefficients sans jamais les annuler. Très stable avec la colinéarité."),
          tags$li(strong("Lasso (L1) : "), "minimise (1/2n)‖y − Xβ‖² + λ‖β‖₁. Annule certains
                  coefficients : il sélectionne des variables."),
          tags$li(strong("Elastic Net : "), "minimise (1/2n)‖y − Xβ‖² + λ[(1−α)/2 ‖β‖² + α‖β‖₁]
                  (α = 0 : Ridge, α = 1 : Lasso). Utile quand des variables corrélées vont ensemble.")),
        p(class = "small-note", "Formules au format du package glmnet utilisé ici.")),
      card(card_header("Choisir λ"),
        p("λ règle la force de la pénalité. On le choisit par validation croisée : « λ min » donne
          la plus petite erreur, « λ 1se » donne le modèle le plus simple dont l'erreur reste à un
          écart-type du minimum. Les variables sont standardisées automatiquement par glmnet.")),
      card(card_header("Lire l'application"),
        tags$ol(
          tags$li("Données et Exploration : comprendre le jeu de données."),
          tags$li("Modèle : choisir l'algorithme et les paramètres, puis « Entraîner »."),
          tags$li("Variables : voir quelles variables comptent."),
          tags$li("Comparaison : Ridge vs Lasso vs Elastic Net."),
          tags$li("Prédiction : estimer le prix d'un nouveau logement."))),
      card(card_header("Ventes atypiques et robustesse"),
        p("Trois grandes maisons « Partial » (non terminées) ont été vendues très en dessous de leur valeur. Si
          elles tombent dans le jeu de test, l'erreur augmente beaucoup ; si elles sont dans l'apprentissage, le
          modèle crée des variables « rares » pour les absorber. L'onglet Robustesse mesure cet effet, et
          l'option « Exclure les 5 maisons de plus de 4 000 pi² » le corrige.")),
      card(card_header("Limites"),
        p("Modèle linéaire : il ne capture pas les relations non linéaires ni certaines interactions.
          Les coefficients sont biaisés, donc il n'y a pas d'intervalles de confiance classiques. Les
          données concernent une seule ville américaine sur 2006-2010 : l'application sert à illustrer
          la méthode, pas à estimer le marché actuel."))
    )
  ),

  # Bouton mode clair / sombre (à droite de la barre de navigation)
  nav_spacer(),
  nav_item(input_dark_mode(id = "mode", mode = "light"))
)

# ------------------------------------------------------------
# 4. SERVER (calculs et graphiques)
# ------------------------------------------------------------
server <- function(input, output, session) {

  # Mode clair / sombre choisi avec le bouton de la barre de navigation
  mode <- reactive(if (is.null(input$mode)) "light" else input$mode)

  # ==========================================================
  # A. Onglet Données
  # ==========================================================
  output$tbl_data <- renderDT({
    datatable(data, style = dt_style, class = "table-sm", rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE, language = dt_lang))
  })

  output$plot_na <- renderPlotly({
    na <- sort(colSums(is.na(data)), decreasing = TRUE)
    na <- head(na[na > 0], 15)
    cl <- pal(mode())
    plot_ly(x = unname(na), y = factor(names(na), levels = rev(names(na))), type = "bar",
            orientation = "h", marker = list(color = cl$main),
            hovertemplate = "%{y} : %{x} valeurs manquantes<extra></extra>") %>%
      style_plot(mode(), xtitle = "Nombre de valeurs manquantes")
  })

  # ==========================================================
  # B. Onglet Exploration
  # ==========================================================
  output$plot_price <- renderPlotly({
    cl <- pal(mode())
    y <- if (input$price_scale == "log") log(data$saleprice) else data$saleprice
    plot_ly(x = y, type = "histogram", nbinsx = 40,
            marker = list(color = cl$main, line = list(color = "white", width = 0.4)),
            hovertemplate = "Intervalle : %{x}<br>Logements : %{y}<extra></extra>") %>%
      style_plot(mode(), title = "Distribution du prix de vente",
                 xtitle = if (input$price_scale == "log") "log(prix)" else "Prix de vente ($)",
                 ytitle = "Nombre de logements",
                 shapes = list(vline(mean(y), cl$accent), vline(median(y), cl$bad, "dot")),
                 annotations = list(vlabel(mean(y), "moyenne", cl$accent),
                                    vlabel(median(y), "médiane", cl$bad, y = 1.12)))
  })

  output$price_stats <- renderUI({
    x <- data$saleprice
    sk <- mean((x - mean(x))^3) / sd(x)^3
    p(class = "small-note",
      paste0("Moyenne : ", fmt_usd(mean(x)), " · Médiane : ", fmt_usd(median(x)),
             " · Écart-type : ", fmt_usd(sd(x)), " · Asymétrie : ", fmt_num(sk, 2),
             ". La distribution est étalée vers la droite (quelques prix très élevés) : ",
             "modéliser log(prix) peut améliorer l'ajustement."))
  })

  output$plot_scatter <- renderPlotly({
    req(input$xvar)
    cl <- pal(mode())
    d <- data.frame(x = data[[input$xvar]], y = data$saleprice, q = data$overall_qual)
    d <- d[complete.cases(d), ]
    r <- cor(d$x, d$y)
    ln <- lm(y ~ x, d)
    xs <- seq(min(d$x), max(d$x), length.out = 50)
    mk <- if (isTRUE(input$color_qual)) {
      list(color = d$q, colorscale = "Viridis", showscale = TRUE, opacity = 0.65, size = 6,
           colorbar = list(title = "Qualité"))
    } else list(color = cl$main, opacity = 0.5, size = 6)
    p <- plot_ly() %>%
      add_markers(x = d$x, y = d$y, marker = mk, name = "Logements",
                  hovertemplate = paste0(lab(input$xvar), " : %{x}<br>Prix : %{y:,.0f} $<extra></extra>")) %>%
      add_lines(x = xs, y = predict(ln, data.frame(x = xs)), name = "Tendance linéaire",
                line = list(color = cl$accent, width = 3))
    # Les 3 ventes atypiques (grandes maisons « Partial » vendues à bas prix), cerclées en rouge
    if (input$xvar == "gr_liv_area") {
      p <- p %>% add_markers(x = data$gr_liv_area[atyp_rows], y = data$saleprice[atyp_rows], name = "Ventes atypiques",
        marker = list(symbol = "circle-open", size = 16, color = cl$bad, line = list(color = cl$bad, width = 2.5)),
        hovertemplate = "Vente atypique<br>Surface : %{x} pi²<br>Prix : %{y:,.0f} $<extra></extra>")
    }
    p %>% style_plot(mode(), title = paste0("Prix selon « ", lab(input$xvar), " »  (r = ", fmt_num(r, 2), ")"),
                 xtitle = lab(input$xvar), ytitle = "Prix de vente ($)")
  })

  output$plot_box <- renderPlotly({
    req(input$catvar)
    cl <- pal(mode())
    g <- factor(data[[input$catvar]])
    ord <- names(sort(tapply(data$saleprice, g, median)))
    g <- factor(g, levels = ord)
    plot_ly(x = data$saleprice, y = g, type = "box", orientation = "h",
            marker = list(color = cl$main), line = list(color = cl$main),
            fillcolor = cl$band, boxpoints = "outliers") %>%
      style_plot(mode(), title = paste0("Prix de vente par « ", names(cat_choices)[cat_choices == input$catvar], " »"),
                 xtitle = "Prix de vente ($)", ytitle = NULL)
  })

  output$plot_corr <- renderPlotly({
    req(input$topn_corr)
    cl <- pal(mode())
    nums <- data[, c("saleprice", num_vars)]
    r <- suppressWarnings(cor(nums, use = "pairwise.complete.obs")[, "saleprice"])
    r <- r[names(r) != "saleprice"]
    top <- names(sort(abs(r), decreasing = TRUE))[seq_len(input$topn_corr)]
    cm <- cor(nums[, c("saleprice", top)], use = "pairwise.complete.obs")
    nm <- lab(colnames(cm))
    plot_ly(z = cm, x = nm, y = nm, type = "heatmap", zmin = -1, zmax = 1,
            colorscale = list(c(0, cl$bad), c(0.5, cl$mid), c(1, cl$main)),
            text = round(cm, 2), texttemplate = "%{text}", textfont = list(size = 10),
            hovertemplate = "%{x}<br>%{y}<br>r = %{z:.2f}<extra></extra>") %>%
      style_plot(mode(), title = "Corrélations entre le prix et les variables les plus liées") %>%
      layout(xaxis = list(tickmode = "array", tickvals = nm, ticktext = nm, tickangle = -45, automargin = TRUE),
             yaxis = list(autorange = "reversed", tickmode = "array", tickvals = nm, ticktext = nm, automargin = TRUE),
             margin = list(l = 190, b = 190, t = 55))
  })

  # ==========================================================
  # C. Entraînement du modèle (déclenché par le bouton, et au démarrage)
  # ==========================================================
  fit <- eventReactive(input$train, {
    withProgress(message = "Entraînement du modèle...", value = 0, {
      incProgress(0.15, detail = "Préparation des données")
      dat   <- if (isTRUE(input$excl)) data[data$gr_liv_area <= 4000, ] else data
      prep  <- prepare_ames(dat, p = input$split / 100, seed = input$seed)
      alpha <- switch(input$algo, "Ridge" = 0, "Lasso" = 1, input$alpha)
      uselog <- isTRUE(input$use_log)
      yt <- if (uselog) log(prep$y_train) else prep$y_train

      incProgress(0.35, detail = "Validation croisée")
      set.seed(input$seed)
      cv  <- cv.glmnet(prep$x_train, yt, alpha = alpha, nfolds = input$folds)
      lam <- switch(input$lam_rule, "min" = cv$lambda.min, "1se" = cv$lambda.1se, 10^input$loglam)

      incProgress(0.3, detail = "Ajustement final")
      model <- glmnet(prep$x_train, yt, alpha = alpha, lambda = lam)
      path  <- glmnet(prep$x_train, yt, alpha = alpha)

      pred <- as.numeric(predict(model, newx = prep$x_test))
      if (uselog) pred <- exp(pred)
      met  <- metrics_reg(pred, prep$y_test)

      beta <- as.numeric(coef(model))[-1]
      sds  <- apply(prep$x_train, 2, sd)
      coefs <- data.frame(variable = colnames(prep$x_train), coefficient = beta,
                          coef_std = beta * sds, n_train = as.numeric(colSums(prep$x_train != 0)),
                          stringsAsFactors = FALSE)
      # Colonne 0/1 (catégorie) présente dans 3 maisons ou moins : variable « rare »
      coefs$rare <- grepl(".", coefs$variable, fixed = TRUE) & coefs$n_train <= 3

      list(prep = prep, alpha = alpha, algo = input$algo, uselog = uselog, cv = cv, lambda = lam,
           model = model, path = path, pred = pred, metrics = met, coefs = coefs, sds = sds,
           folds = input$folds, lam_rule = input$lam_rule, seed = input$seed,
           data_used = dat, excl = isTRUE(input$excl), split = input$split)
    })
  }, ignoreNULL = FALSE)

  output$log_warning <- renderUI({
    if (isTRUE(input$use_log) && !isTRUE(input$excl)) {
      div(class = "alert alert-warning", style = "font-size:0.85rem",
          "Attention : avec les ventes atypiques, log(prix) peut être très instable (l'erreur varie beaucoup
           selon le découpage). Cochez aussi « Exclure... » ou vérifiez l'onglet Robustesse.")
    }
  })

  output$vb_rmse <- renderText(fmt_usd(fit()$metrics["RMSE"]))
  output$vb_mae  <- renderText(fmt_usd(fit()$metrics["MAE"]))
  output$vb_r2   <- renderText(fmt_num(fit()$metrics["R2"], 3))
  output$vb_nvar <- renderText({
    f <- fit(); paste0(sum(f$coefs$coefficient != 0), " / ", sum(f$sds > 0))
  })
  output$lam_info <- renderText({
    f <- fit()
    fl <- function(x) format(signif(x, 4), decimal.mark = ",", big.mark = "\u00a0")
    paste0("Algorithme : ", f$algo, if (f$algo == "Elastic Net") paste0(" (α = ", f$alpha, ")") else "",
           " · λ choisi = ", fl(f$lambda), " (log10 = ", fmt_num(log10(f$lambda), 2), ")",
           " · λ min = ", fl(f$cv$lambda.min), " · λ 1se = ", fl(f$cv$lambda.1se),
           if (f$uselog) " · cible : log(prix)" else "",
           if (f$excl) " · sans les 5 maisons > 4 000 pi²" else "")
  })

  output$plot_cv <- renderPlotly({
    f <- fit(); cl <- pal(mode()); cv <- f$cv
    # On affiche la racine de l'erreur quadratique (RMSE) : lisible, en dollars
    d <- data.frame(l = log10(cv$lambda), m = sqrt(cv$cvm), lo = sqrt(cv$cvlo), hi = sqrt(cv$cvup), nz = cv$nzero)
    mn <- min(d$m)
    lc <- log10(f$lambda); l1 <- log10(cv$lambda.min); l2 <- log10(cv$lambda.1se)
    plot_ly(d, x = ~l) %>%
      add_ribbons(ymin = ~lo, ymax = ~hi, fillcolor = cl$band, line = list(width = 0),
                  name = "± 1 écart-type", hoverinfo = "skip") %>%
      add_lines(y = ~m, name = "Erreur de validation croisée", line = list(color = cl$main, width = 3),
                text = ~nz, hovertemplate = "log10(λ) = %{x:.2f}<br>Erreur = %{y:,.0f}<br>Variables non nulles : %{text}<extra></extra>") %>%
      style_plot(mode(), title = paste0("Choix de λ par validation croisée (", f$folds, " plis)"),
                 xtitle = "log10(λ)  (plus à droite = modèle plus simple)",
                 ytitle = if (f$uselog) "Erreur (échelle log)" else "Erreur de prédiction ($)",
                 shapes = list(vline(l1, cl$accent), vline(l2, cl$bad, "dot"), vline(lc, cl$fg, "solid")),
                 annotations = list(vlabel(l1, "λ min", cl$accent), vlabel(l2, "λ 1se", cl$bad, y = 1.12),
                                    vlabel(lc, "λ choisi", cl$fg, y = 1.20))) %>%
      layout(yaxis = list(range = c(mn * 0.9, mn * 2.2)))
  })

  output$plot_path <- renderPlotly({
    f <- fit(); cl <- pal(mode())
    B <- as.matrix(f$path$beta) * f$sds
    keep <- order(apply(abs(B), 1, max), decreasing = TRUE)[seq_len(min(20, nrow(B)))]
    x <- log10(f$path$lambda)
    p <- plot_ly()
    for (i in keep) {
      p <- add_lines(p, x = x, y = B[i, ], name = rownames(B)[i], line = list(width = 1.8),
                     hovertemplate = paste0(rownames(B)[i], "<br>log10(λ) = %{x:.2f}<br>coef. std = %{y:,.0f}<extra></extra>"))
    }
    p %>% style_plot(mode(), title = "Chemins des coefficients (20 variables les plus fortes)",
                     xtitle = "log10(λ)", ytitle = "Coefficient standardisé",
                     shapes = list(vline(log10(f$lambda), cl$fg, "solid")),
                     annotations = list(vlabel(log10(f$lambda), "λ choisi", cl$fg)),
                     legend = list(orientation = "v", font = list(size = 10)))
  })

  output$plot_pred <- renderPlotly({
    f <- fit(); cl <- pal(mode())
    y <- f$prep$y_test; r <- range(c(y, f$pred))
    plot_ly() %>%
      add_markers(x = y, y = f$pred, name = "Logements du test",
                  marker = list(color = cl$main, opacity = 0.55, size = 7),
                  hovertemplate = "Réel : %{x:,.0f} $<br>Prédit : %{y:,.0f} $<extra></extra>") %>%
      add_lines(x = r, y = r, name = "Prédiction parfaite", line = list(color = cl$accent, dash = "dash", width = 2)) %>%
      style_plot(mode(), title = paste0("Prix réels vs prix prédits (R² = ", fmt_num(f$metrics["R2"], 3), ")"),
                 xtitle = "Prix réel ($)", ytitle = "Prix prédit ($)")
  })

  output$plot_resid <- renderPlotly({
    f <- fit(); cl <- pal(mode())
    res <- f$prep$y_test - f$pred
    plot_ly(x = f$pred, y = res, type = "scatter", mode = "markers",
            marker = list(color = cl$main, opacity = 0.55, size = 7),
            hovertemplate = "Prédit : %{x:,.0f} $<br>Résidu : %{y:,.0f} $<extra></extra>") %>%
      style_plot(mode(), title = "Résidus selon le prix prédit", xtitle = "Prix prédit ($)",
                 ytitle = "Résidu = réel − prédit ($)",
                 shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
                                    line = list(color = cl$accent, dash = "dash"))))
  })

  output$plot_resid_hist <- renderPlotly({
    f <- fit(); cl <- pal(mode())
    res <- f$prep$y_test - f$pred
    plot_ly(x = res, type = "histogram", nbinsx = 35,
            marker = list(color = cl$main, line = list(color = "white", width = 0.4))) %>%
      style_plot(mode(), title = "Distribution des résidus", xtitle = "Résidu ($)", ytitle = "Logements")
  })

  output$dl_pred <- downloadHandler(
    filename = function() "predictions_test.csv",
    content = function(file) {
      f <- fit()
      write.csv(data.frame(prix_reel = f$prep$y_test, prix_predit = round(f$pred, 0),
                           residu = round(f$prep$y_test - f$pred, 0)), file, row.names = FALSE)
    }
  )

  # ==========================================================
  # D. Onglet Variables
  # ==========================================================
  coefs_nz <- reactive({
    d <- fit()$coefs
    d <- d[d$coefficient != 0, ]
    d[order(abs(d$coef_std), decreasing = TRUE), ]
  })

  output$plot_coef <- renderPlotly({
    cl <- pal(mode())
    top <- head(coefs_nz(), input$topn)
    validate(need(nrow(top) > 0, "Aucune variable retenue avec ce λ : diminuez λ."))
    nm <- pretty_var(top$variable)
    nm <- ifelse(top$rare, paste0(nm, "  ⚠ ", top$n_train, " maison(s)"), nm)
    col <- ifelse(top$rare, cl$accent, ifelse(top$coef_std > 0, cl$main, cl$bad))
    plot_ly(x = top$coef_std, y = factor(nm, levels = rev(unique(nm))),
            type = "bar", orientation = "h", marker = list(color = col),
            hovertemplate = "%{y}<br>Coefficient standardisé : %{x:,.0f}<extra></extra>") %>%
      style_plot(mode(), title = paste0(fit()$algo, " : ", nrow(top), " variables les plus influentes"),
                 xtitle = "Effet sur le prix de +1 écart-type de la variable ($)") %>%
      layout(margin = list(l = 260, t = 55))
  })

  output$tbl_coef <- renderDT({
    d <- coefs_nz()
    out <- data.frame(Variable = pretty_var(d$variable), `Nom technique` = d$variable, Coefficient = d$coefficient,
                      `Coefficient standardisé` = d$coef_std,
                      `Maisons concernées` = ifelse(grepl(".", d$variable, fixed = TRUE), d$n_train, NA),
                      check.names = FALSE)
    datatable(out, style = dt_style, class = "table-sm", rownames = FALSE,
      options = list(pageLength = 10, language = dt_lang)) %>%
      formatRound(c("Coefficient", "Coefficient standardisé"), 2)
  })

  output$dl_coef <- downloadHandler(
    filename = function() "coefficients.csv",
    content = function(file) write.csv(coefs_nz(), file, row.names = FALSE)
  )

  # ==========================================================
  # E. Onglet Comparaison
  # ==========================================================
  compare_res <- eventReactive(input$compare, {
    f <- fit(); prep <- f$prep
    yt <- if (f$uselog) log(prep$y_train) else prep$y_train
    algos <- list(Ridge = 0, Lasso = 1, "Elastic Net" = 0.5)
    withProgress(message = "Comparaison des modèles...", value = 0, {
      # Référence : régression classique (MCO)
      incProgress(0.1, detail = "Régression classique")
      ols <- suppressWarnings(lm(yt ~ ., data = as.data.frame(prep$x_train)))
      po  <- suppressWarnings(as.numeric(predict(ols, as.data.frame(prep$x_test))))
      if (f$uselog) po <- exp(po)
      ok  <- is.finite(po); mo <- metrics_reg(po[ok], prep$y_test[ok])
      first <- data.frame(Modele = "Régression classique", Alpha = NA, Lambda = NA, RMSE = mo["RMSE"], MAE = mo["MAE"],
                          R2 = mo["R2"], Variables = sum(!is.na(coef(ols))[-1]), row.names = NULL)
      out <- lapply(names(algos), function(nm) {
        incProgress(0.3, detail = nm)
        set.seed(f$seed)
        cv  <- cv.glmnet(prep$x_train, yt, alpha = algos[[nm]], nfolds = f$folds)
        lam <- if (f$lam_rule == "1se") cv$lambda.1se else cv$lambda.min
        m   <- glmnet(prep$x_train, yt, alpha = algos[[nm]], lambda = lam)
        pr  <- as.numeric(predict(m, newx = prep$x_test))
        if (f$uselog) pr <- exp(pr)
        mt  <- metrics_reg(pr, prep$y_test)
        data.frame(Modele = nm, Alpha = algos[[nm]], Lambda = lam, RMSE = mt["RMSE"],
                   MAE = mt["MAE"], R2 = mt["R2"],
                   Variables = sum(as.numeric(coef(m))[-1] != 0), row.names = NULL)
      })
      do.call(rbind, c(list(first), out))
    })
  })

  output$compare_out <- renderUI({
    req(compare_res())
    tagList(
      layout_column_wrap(width = 1/2,
        card(card_header("Résultats sur le jeu de test"), DTOutput("tbl_compare")),
        card(card_header("Erreur de prédiction (RMSE)"), plotlyOutput("plot_cmp_rmse", height = "320px"))),
      layout_column_wrap(width = 1/2,
        card(card_header("R² sur le test"), plotlyOutput("plot_cmp_r2", height = "320px")),
        card(card_header("Variables retenues"), plotlyOutput("plot_cmp_nvar", height = "320px"))),
      uiOutput("cmp_text")
    )
  })

  output$tbl_compare <- renderDT({
    d <- compare_res()
    d$Lambda <- signif(d$Lambda, 4)
    names(d) <- c("Modèle", "α", "λ", "RMSE ($)", "MAE ($)", "R²", "Variables retenues")
    datatable(d, style = dt_style, class = "table-sm", rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE, language = dt_lang)) %>%
      formatRound(c("RMSE ($)", "MAE ($)"), 0) %>% formatRound("R²", 3)
  })

  cmp_bar <- function(col, ytitle, digits, ymax = NULL) {
    d <- compare_res(); cl <- pal(mode())
    x <- factor(d$Modele, levels = d$Modele)
    mx <- if (is.null(ymax)) max(d[[col]]) * 1.18 else ymax
    plot_ly(x = x, y = d[[col]], type = "bar",
            marker = list(color = c(cl$muted, cl$main, cl$accent, cl$bad)),
            text = formatC(d[[col]], format = "f", digits = digits, big.mark = "\u00a0"), textposition = "outside",
            hovertemplate = paste0("%{x}<br>", ytitle, " : %{text}<extra></extra>")) %>%
      style_plot(mode(), ytitle = ytitle) %>% layout(yaxis = list(range = c(0, mx)))
  }
  output$plot_cmp_rmse <- renderPlotly(cmp_bar("RMSE", "RMSE ($)", 0))
  output$plot_cmp_r2   <- renderPlotly(cmp_bar("R2", "R²", 3, ymax = 1.1))
  output$plot_cmp_nvar <- renderPlotly(cmp_bar("Variables", "Nombre de variables", 0))

  output$cmp_text <- renderUI({
    d <- compare_res(); best <- d$Modele[which.min(d$RMSE)]
    card(p(strong("Lecture : "), paste0("sur ce découpage, le plus petit RMSE est celui de ", best,
      " (", fmt_usd(min(d$RMSE)), "). Lasso et Elastic Net retiennent ",
      d$Variables[d$Modele == "Lasso"], " et ", d$Variables[d$Modele == "Elastic Net"],
      " variables, contre ", d$Variables[d$Modele == "Ridge"], " pour Ridge (qui les garde toutes).",
      " Si les écarts sont petits, préférez le modèle le plus simple, et vérifiez la stabilité dans l'onglet Robustesse.")))
  })

  # ==========================================================
  # E2. Onglet Robustesse : plusieurs découpages aléatoires
  # ==========================================================
  robust_res <- eventReactive(input$robust, {
    f <- fit(); n <- input$n_splits; dat <- f$data_used
    withProgress(message = "Évaluation sur plusieurs découpages...", value = 0, {
      rows <- vector("list", n)
      for (sd_ in seq_len(n)) {
        incProgress(1 / n, detail = paste0("Découpage ", sd_, " sur ", n))
        p  <- prepare_ames(dat, p = f$split / 100, seed = sd_)
        yt <- if (f$uselog) log(p$y_train) else p$y_train
        one <- function(nm, a) {
          set.seed(sd_)
          cv  <- cv.glmnet(p$x_train, yt, alpha = a, nfolds = f$folds)
          lam <- if (f$lam_rule == "1se") cv$lambda.1se else cv$lambda.min
          m   <- glmnet(p$x_train, yt, alpha = a, lambda = lam)
          pr  <- as.numeric(predict(m, newx = p$x_test)); if (f$uselog) pr <- exp(pr)
          data.frame(Decoupage = sd_, Modele = nm, t(metrics_reg(pr, p$y_test)), row.names = NULL)
        }
        ols <- suppressWarnings(lm(yt ~ ., data = as.data.frame(p$x_train)))
        po  <- suppressWarnings(as.numeric(predict(ols, as.data.frame(p$x_test)))); if (f$uselog) po <- exp(po)
        ok  <- is.finite(po)
        r <- rbind(data.frame(Decoupage = sd_, Modele = "Régression classique", t(metrics_reg(po[ok], p$y_test[ok])), row.names = NULL),
                   one("Ridge", 0), one("Lasso", 1), one("Elastic Net", 0.5))
        r$Atypiques <- sum(as.character(atyp_rows) %in% rownames(p$test_data))
        rows[[sd_]] <- r
      }
      r <- do.call(rbind, rows)
      r$Modele <- factor(r$Modele, levels = c("Régression classique", "Ridge", "Lasso", "Elastic Net"))
      r
    })
  })

  output$robust_out <- renderUI({
    req(robust_res())
    tagList(
      layout_column_wrap(width = 1/2,
        card(card_header("Erreur moyenne sur l'ensemble des découpages"), DTOutput("tbl_robust")),
        card(card_header("Dispersion du RMSE selon le découpage"), plotlyOutput("plot_robust_box", height = "340px"))),
      card(card_header("RMSE du Lasso pour chaque découpage"), plotlyOutput("plot_robust_seed", height = "340px")),
      uiOutput("robust_text")
    )
  })

  output$tbl_robust <- renderDT({
    r <- robust_res()
    d <- do.call(rbind, lapply(split(r, r$Modele), function(x) data.frame(
      Modele = as.character(x$Modele[1]), RMSE_moy = mean(x$RMSE), RMSE_sd = sd(x$RMSE),
      RMSE_min = min(x$RMSE), RMSE_max = max(x$RMSE), R2_moy = mean(x$R2), row.names = NULL)))
    names(d) <- c("Modèle", "RMSE moyen ($)", "Écart-type ($)", "RMSE min ($)", "RMSE max ($)", "R² moyen")
    datatable(d, style = dt_style, class = "table-sm", rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE, language = dt_lang)) %>%
      formatRound(c("RMSE moyen ($)", "Écart-type ($)", "RMSE min ($)", "RMSE max ($)"), 0) %>% formatRound("R² moyen", 3)
  })

  output$plot_robust_box <- renderPlotly({
    r <- robust_res(); cl <- pal(mode())
    plot_ly(r, x = ~Modele, y = ~RMSE, type = "box", boxpoints = "all", jitter = 0.4, pointpos = 0,
            marker = list(color = cl$main, opacity = 0.7), line = list(color = cl$main), fillcolor = cl$band,
            hovertemplate = "%{x}<br>RMSE : %{y:,.0f} $<extra></extra>") %>%
      style_plot(mode(), ytitle = "RMSE ($)")
  })

  output$plot_robust_seed <- renderPlotly({
    r <- robust_res(); cl <- pal(mode()); l <- r[r$Modele == "Lasso", ]
    a <- l[l$Atypiques == 0, ]; b <- l[l$Atypiques >= 1, ]
    p <- plot_ly()
    if (nrow(a) > 0) p <- p %>% add_markers(x = a$Decoupage, y = a$RMSE, name = "Aucune vente atypique dans le test",
        marker = list(color = cl$main, size = 13), hovertemplate = "Découpage %{x}<br>RMSE : %{y:,.0f} $<extra></extra>")
    if (nrow(b) > 0) p <- p %>% add_markers(x = b$Decoupage, y = b$RMSE, name = "Au moins une vente atypique dans le test",
        marker = list(color = cl$bad, size = 13), text = b$Atypiques,
        hovertemplate = "Découpage %{x}<br>RMSE : %{y:,.0f} $<br>Ventes atypiques dans le test : %{text}<extra></extra>")
    p %>% style_plot(mode(), xtitle = "Numéro du découpage", ytitle = "RMSE du Lasso ($)")
  })

  output$robust_text <- renderUI({
    r <- robust_res(); l <- r[r$Modele == "Lasso", ]
    parts <- paste0("Sur ", length(unique(r$Decoupage)), " découpages aléatoires, le Lasso a un RMSE moyen de ",
                    fmt_usd(mean(l$RMSE)), " (écart-type ", fmt_usd(sd(l$RMSE)), ").")
    if (any(l$Atypiques >= 1) && any(l$Atypiques == 0)) {
      parts <- paste0(parts, " Quand le test contient au moins une des 3 ventes atypiques, le RMSE moyen est de ",
                      fmt_usd(mean(l$RMSE[l$Atypiques >= 1])), " ; sinon il est de ", fmt_usd(mean(l$RMSE[l$Atypiques == 0])),
                      ". Les ventes atypiques expliquent donc l'essentiel de la variabilité : cochez « Exclure... » dans l'onglet Modèle pour comparer.")
    }
    card(p(strong("Lecture : "), parts))
  })

  # ==========================================================
  # F. Onglet Prédiction
  # ==========================================================
  output$p_area_m2 <- renderText({
    req(!is.na(input$p_area)); paste0("≈ ", round(input$p_area * 0.092903), " m²")
  })

  output$p_warning <- renderUI({
    msgs <- c()
    chk <- function(v, val, name) {
      if (!is.na(val) && (val < min(data[[v]], na.rm = TRUE) || val > max(data[[v]], na.rm = TRUE)))
        msgs <<- c(msgs, paste0(name, " (", val, ") est hors de la plage observée (",
                                min(data[[v]], na.rm = TRUE), " à ", max(data[[v]], na.rm = TRUE), ")."))
    }
    chk("gr_liv_area", input$p_area, "La surface habitable")
    chk("year_built", input$p_year, "L'année de construction")
    chk("total_bsmt_sf", input$p_bsmt, "La surface du sous-sol")
    if (length(msgs) == 0) return(NULL)
    div(class = "alert alert-warning", HTML(paste0("Attention : ", paste(msgs, collapse = "<br>"),
                                                    "<br>L'estimation sera peu fiable.")))
  })

  # Se recalcule au clic sur « Estimer » ET quand le modèle est ré-entraîné
  pred_res <- reactive({
    input$estimate
    f <- fit()
    isolate({
      req(!is.na(input$p_area), !is.na(input$p_year), !is.na(input$p_bsmt))
      ov <- list(neighborhood = input$p_neigh, overall_qual = input$p_qual,
                 gr_liv_area = input$p_area, year_built = input$p_year,
                 total_bsmt_sf = input$p_bsmt, garage_cars = input$p_garage,
                 full_bath = input$p_bath, bedroom_abvgr = input$p_bed)
      xnew <- make_new_x(f$prep, ov)
      p <- as.numeric(predict(f$model, newx = xnew))
      if (f$uselog) p <- exp(p)
      tr <- f$prep$train_data
      list(price = p, rmse = unname(f$metrics["RMSE"]),
           neigh = median(tr$saleprice[tr$neighborhood == input$p_neigh]))
    })
  })

  output$p_price <- renderText(fmt_usd(pred_res()$price))
  output$p_range <- renderText({
    r <- pred_res(); paste0(fmt_k(max(r$price - r$rmse, 0)), " – ", fmt_k(r$price + r$rmse))
  })
  output$p_neigh_med <- renderText({
    m <- pred_res()$neigh; if (is.na(m)) "n/d" else fmt_usd(m)
  })

  output$plot_where <- renderPlotly({
    r <- pred_res(); cl <- pal(mode())
    plot_ly(x = data$saleprice, type = "histogram", nbinsx = 45,
            marker = list(color = cl$main, opacity = 0.75, line = list(color = "white", width = 0.4)),
            hovertemplate = "Prix : %{x}<br>Logements : %{y}<extra></extra>") %>%
      style_plot(mode(), title = "Prix estimé par rapport à tous les logements du jeu de données",
                 xtitle = "Prix de vente ($)", ytitle = "Nombre de logements",
                 shapes = list(
                   list(type = "rect", x0 = max(r$price - r$rmse, 0), x1 = r$price + r$rmse,
                        y0 = 0, y1 = 1, yref = "paper", fillcolor = cl$band, line = list(width = 0)),
                   vline(r$price, cl$accent, "solid")),
                 annotations = list(vlabel(r$price, "prix estimé", cl$accent)))
  })
}

shinyApp(ui = ui, server = server)
