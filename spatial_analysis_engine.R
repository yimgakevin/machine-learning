# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  spatial_analysis_engine.R — Analyse spatiale EXPERT                         ║
# ║  LISA · Moran's I · Hotspots · Clustering spatial                           ║
# ║  TIER 2 : Analyse spatiale avancée niveau scientifique                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(sf)
library(tidyverse)
library(spdep)  # Pour LISA, Moran
library(Matrix)

# ══════════════════════════════════════════════════════════════════════════════
# 1. LISA — Local Indicators of Spatial Association
# Identifie hotspots, coldspots, outliers
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer LISA pour chaque région
#'
#' @param sf_data objet sf avec données + géométries
#' @param col_var colonne variable d'intérêt
#' @param col_id colonne d'identification
#' @param alpha seuil p-value (défaut: 0.05)
#'
#' @return data.frame avec classification LISA
calc_lisa <- function(sf_data, col_var, col_id, alpha = 0.05) {

  # Extraire données — exclure les zones sans données (NA)
  vals_all <- sf_data[[col_var]]
  sf_full <- sf_data
  if (any(is.na(vals_all))) {
    sf_data <- sf_data[!is.na(vals_all), , drop = FALSE]
    message(sprintf("[LISA] %d zones sans données exclues de l'analyse",
                    sum(is.na(vals_all))))
  }
  if (nrow(sf_data) < 4)
    stop("Analyse LISA impossible : moins de 4 zones avec données")
  vals <- sf_data[[col_var]]
  ids <- sf_data[[col_id]]
  
  # Créer matrice de poids (rook adjacency)
  # Basée sur les frontières partagées
  nb <- spdep::poly2nb(sf_data)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Standardiser les valeurs
  vals_stand <- (vals - mean(vals, na.rm = TRUE)) / sd(vals, na.rm = TRUE)
  
  # LISA (Local Moran's I)
  lisa <- spdep::localmoran(vals_stand, lw, zero.policy = TRUE)
  
  # Extraire résultats
  results <- tibble::tibble(
    id = ids,
    value = vals,
    Ii = lisa[, 1],          # Local Moran's I
    p_value = lisa[, 5],     # p-value
    significant = lisa[, 5] < alpha
  )
  
  # Classifie r LISA
  # High-High = hotspot
  # Low-Low = coldspot
  # High-Low / Low-High = outliers
  
  results <- results %>%
    mutate(
      val_category = if_else(value > mean(value, na.rm = TRUE), "High", "Low"),
      lag_category = if_else(
        spdep::lag.listw(lw, value, zero.policy = TRUE) > mean(value, na.rm = TRUE),
        "High", "Low"
      ),
      lisa_class = case_when(
        !significant ~ "Non-significatif",
        val_category == "High" & lag_category == "High" ~ "Hotspot (H-H)",
        val_category == "Low" & lag_category == "Low" ~ "Coldspot (L-L)",
        val_category == "High" & lag_category == "Low" ~ "Outlier (H-L)",
        val_category == "Low" & lag_category == "High" ~ "Outlier (L-H)",
        TRUE ~ "Autre"
      ),
      lisa_color = case_when(
        lisa_class == "Hotspot (H-H)" ~ "#d32f2f",         # Rouge
        lisa_class == "Coldspot (L-L)" ~ "#4caf50",        # Vert
        lisa_class == "Outlier (H-L)" ~ "#ff9800",         # Orange
        lisa_class == "Outlier (L-H)" ~ "#2196f3",         # Bleu
        TRUE ~ "#cccccc"                                   # Gris
      )
    )

  # Le rendu cartographique attend un objet sf enrichi, pas seulement le tibble.
  sf_out <- sf_data
  idx <- match(as.character(sf_out[[col_id]]), as.character(results$id))
  sf_out$Ii          <- results$Ii[idx]
  sf_out$p_value     <- results$p_value[idx]
  sf_out$significant <- results$significant[idx]
  sf_out$lisa_class  <- results$lisa_class[idx]
  sf_out$lisa_color  <- results$lisa_color[idx]

  list(sf_data = sf_out, sf_full = sf_full, results = results, classification = "lisa")
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. MORAN'S I GLOBAL — Test d'auto-corrélation spatiale globale
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer Moran's I pour la région entière
#'
#' @param sf_data objet sf
#' @param col_var colonne variable
#'
#' @return list avec statistiques Moran
calc_morans_i <- function(sf_data, col_var) {

  vals_all <- sf_data[[col_var]]
  if (any(is.na(vals_all))) {
    sf_data <- sf_data[!is.na(vals_all), , drop = FALSE]
    message(sprintf("[MORAN] %d zones sans données exclues de l'analyse",
                    sum(is.na(vals_all))))
  }
  if (nrow(sf_data) < 4)
    stop("Test Moran's I impossible : moins de 4 zones avec données")
  vals <- sf_data[[col_var]]
  nb <- spdep::poly2nb(sf_data)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Test Moran's I
  mi <- spdep::moran.test(vals, lw, zero.policy = TRUE)
  
  # Interpréter
  interpretation <- case_when(
    mi$statistic > 0.3 ~ "Auto-corrélation spatiale FORTE",
    mi$statistic > 0.1 ~ "Auto-corrélation spatiale MODÉRÉE",
    mi$statistic > -0.1 ~ "Auto-corrélation spatiale FAIBLE",
    TRUE ~ "Auto-corrélation spatiale NÉGATIVE"
  )
  
  list(
    statistic = mi$statistic,
    I_value = as.numeric(mi$statistic),
    p_value = mi$p.value,
    significant = mi$p.value < 0.05,
    interpretation = interpretation,
    expected = mi$estimate[2],
    variance = mi$estimate[3]
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. HOTSPOT IDENTIFICATION — Où la malnutrition est critique
# ══════════════════════════════════════════════════════════════════════════════

#' Identifier et classer les hotspots par urgence
#'
#' @param lisa_results résultat de calc_lisa
#' @param sf_data objet sf d'origine
#'
#' @return data.frame ranking
rank_hotspots <- function(lisa_results, sf_data = NULL) {
  
  results <- lisa_results %>%
    filter(significant & lisa_class == "Hotspot (H-H)") %>%
    arrange(desc(value)) %>%
    mutate(
      priority = case_when(
        value >= 30 ~ "CRITIQUE - Action immédiate",
        value >= 20 ~ "SÉRIEUX - Action rapide",
        value >= 15 ~ "MONTRER - Surveillance renforcée",
        TRUE ~ "NORMAL"
      ),
      priority_color = case_when(
        value >= 30 ~ "#8b0000",   # Dark red
        value >= 20 ~ "#d32f2f",   # Red
        value >= 15 ~ "#ff6f00",   # Orange
        TRUE ~ "#4caf50"           # Green
      ),
      rank = row_number()
    ) %>%
    select(id, value, Ii, p_value, priority, priority_color, rank)
  
  results
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. RADAR CHART DATA — Comparaison profils nutritionnels régionaux
# ══════════════════════════════════════════════════════════════════════════════

#' Préparer données pour radar charts (région vs moyenne)
#'
#' @param sf_data objet sf
#' @param indicators vecteur de colonnes à inclure
#' @param region_id région à comparer
#'
#' @return data.frame format radar
prepare_radar_data <- function(sf_data, indicators = c("GAM", "HAZ", "WAZ", "MUAC_GAM"), 
                               region_id = NULL) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    select(id, all_of(indicators))
  
  # Calculer moyennes nationales
  national_means <- colMeans(d[, indicators], na.rm = TRUE)
  
  # Si région spécifiée, la comparer
  if (!is.null(region_id)) {
    region_data <- d %>%
      filter(id == region_id) %>%
      select(all_of(indicators)) %>%
      as.numeric()
    
    comparison <- tibble::tibble(
      indicator = names(national_means),
      region = region_data,
      national = national_means,
      difference = region_data - national_means
    )
  } else {
    comparison <- tibble::tibble(
      indicator = names(national_means),
      national = national_means
    )
  }
  
  comparison
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. HEATMAP CORRÉLATIONS INTER-RÉGIONALES
# ══════════════════════════════════════════════════════════════════════════════

#' Matrice de corrélation géographique
#'
#' @param sf_data objet sf
#' @param indicators colonnes
#'
#' @return matrice corrélation
calc_correlation_matrix <- function(sf_data, indicators = c("GAM", "HAZ", "WAZ", "MUAC_GAM")) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    select(all_of(indicators))
  
  cor(d, use = "complete.obs")
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. DISTANCE MATRICE — Pour clustering spatial
# ══════════════════════════════════════════════════════════════════════════════

#' Créer matrice distances inter-régionales
#'
#' @param sf_data objet sf
#'
#' @return matrice distances
calc_distance_matrix <- function(sf_data) {
  
  # Centroides
  centroids <- st_centroid(sf_data)
  coords <- st_coordinates(centroids)
  
  # Distance euclidienne
  dist_matrix <- as.matrix(dist(coords))
  
  dist_matrix
}

# ══════════════════════════════════════════════════════════════════════════════
# 7. CLUSTERING SPATIAL — K-means sur variables nutritionnelles
# ══════════════════════════════════════════════════════════════════════════════

#' Clustering spatial des régions
#'
#' @param sf_data objet sf
#' @param indicators colonnes
#' @param k nombre de clusters
#'
#' @return sf_data avec colonnes cluster
spatial_clustering <- function(sf_data, indicators = c("GAM", "HAZ", "WAZ"), k = 3) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    select(all_of(indicators))
  
  # Standardiser
  d_stand <- scale(d)
  
  # K-means
  km <- kmeans(d_stand, centers = k, nstart = 20)
  
  # Joindre au sf_data
  sf_data$cluster <- factor(km$cluster)
  sf_data$cluster_name <- case_when(
    km$cluster == 1 ~ "Cluster 1: Haute prévalence",
    km$cluster == 2 ~ "Cluster 2: Prévalence modérée",
    km$cluster == 3 ~ "Cluster 3: Basse prévalence",
    TRUE ~ paste0("Cluster ", km$cluster)
  )
  
  sf_data$cluster_color <- case_when(
    km$cluster == 1 ~ "#d32f2f",
    km$cluster == 2 ~ "#ff9800",
    km$cluster == 3 ~ "#4caf50",
    TRUE ~ "#999999"
  )
  
  sf_data
}

# ══════════════════════════════════════════════════════════════════════════════
# 8. VARIANCE ANALYSIS — ANOVA par cluster spatial
# ══════════════════════════════════════════════════════════════════════════════

#' ANOVA pour tester différences inter-clusters
#'
#' @param sf_data objet sf avec colonne cluster
#' @param col_var variable à analyser
#'
#' @return résultats ANOVA
spatial_anova <- function(sf_data, col_var) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    filter(!is.na(cluster), !is.na(!!rlang::sym(col_var)))
  
  # ANOVA
  anova_result <- aov(as.formula(paste0(col_var, " ~ cluster")), data = d)
  summary_anova <- summary(anova_result)
  
  list(
    f_statistic = summary_anova[[1]]$`F value`[1],
    p_value = summary_anova[[1]]$`Pr(>F)`[1],
    significant = summary_anova[[1]]$`Pr(>F)`[1] < 0.05,
    interpretation = if_else(
      summary_anova[[1]]$`Pr(>F)`[1] < 0.05,
      "Différences SIGNIFICATIVES entre clusters",
      "Pas de différences significatives"
    )
  )
}
