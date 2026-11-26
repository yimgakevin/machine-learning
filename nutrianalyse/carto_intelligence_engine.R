# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  carto_intelligence_engine.R — Géographie intelligente EXPERT               ║
# ║  IC 95% · Sparklines · Algorithme priorisation · Rapports                   ║
# ║  TIER 3 : Intelligence géographique + recommandations                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(sf)
library(tidyverse)
library(binom)  # Pour IC Clopper-Pearson

# ══════════════════════════════════════════════════════════════════════════════
# 1. CONFIDENCE INTERVALS — IC 95% par région (Clopper-Pearson)
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer IC 95% pour une prévalence
#'
#' @param successes nombre de cas
#' @param total effectif total
#' @param method "clopper-pearson" | "wilson" | "normal"
#'
#' @return list(lower, upper)
calc_prevalence_ic <- function(successes, total, method = "clopper-pearson") {
  
  if (total == 0 || is.na(successes) || is.na(total)) {
    return(list(lower = NA, upper = NA))
  }
  
  successes <- as.integer(successes)
  total <- as.integer(total)
  
  if (method == "clopper-pearson") {
    # Exact binomial CI
    ic <- binom::binom.confint(successes, total, method = "exact")
    list(lower = ic$lower[1] * 100, upper = ic$upper[1] * 100)
  } else if (method == "wilson") {
    # Wilson score CI
    ic <- binom::binom.confint(successes, total, method = "wilson")
    list(lower = ic$lower[1] * 100, upper = ic$upper[1] * 100)
  } else {
    # Normal approximation
    p <- successes / total
    se <- sqrt(p * (1 - p) / total)
    list(
      lower = max(0, (p - 1.96 * se) * 100),
      upper = min(100, (p + 1.96 * se) * 100)
    )
  }
}

#' Ajouter IC à un sf_data
#'
#' @param sf_data objet sf
#' @param col_pct colonne prévalence (%)
#' @param col_n colonne effectif
#'
#' @return sf_data avec colonnes ic_low, ic_high
add_ic_to_sf <- function(sf_data, col_pct, col_n) {
  
  sf_data <- sf_data %>%
    mutate(
      # Convertir % en cas
      n_cases = round((!!rlang::sym(col_pct) / 100) * !!rlang::sym(col_n)),
      n_total = !!rlang::sym(col_n),
      # Calculer IC
      ic = map2(n_cases, n_total, 
                ~calc_prevalence_ic(.x, .y, method = "wilson")),
      ic_low = map_dbl(ic, ~.x$lower),
      ic_high = map_dbl(ic, ~.x$upper)
    ) %>%
    select(-ic, -n_cases, -n_total)
  
  sf_data
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. SPARKLINES TEMPORELLES — Si données historiques (3-5 ans)
# ══════════════════════════════════════════════════════════════════════════════

#' Générer sparkline mini pour région (tendance)
#'
#' @param values vecteur de valeurs (séries temporelles)
#' @param width, height dimensions
#'
#' @return HTML string avec graphique inline
gen_sparkline_html <- function(values, width = 60, height = 20) {
  
  values <- values[!is.na(values)]
  
  if (length(values) < 2) {
    return("<span style='font-size:9px;color:#999;'>N/A</span>")
  }
  
  # Normaliser
  min_val <- min(values)
  max_val <- max(values)
  range_val <- max_val - min_val
  
  if (range_val == 0) {
    norm_vals <- rep(0.5, length(values))
  } else {
    norm_vals <- (values - min_val) / range_val
  }
  
  # SVG points
  points <- paste(
    seq(0, width, length.out = length(norm_vals)),
    height - (norm_vals * height),
    collapse = " "
  )
  
  svg <- paste0(
    "<svg width='", width, "' height='", height, "' ",
    "style='vertical-align:middle;margin:0 4px;'>",
    "<polyline points='", points, "' ",
    "fill='none' stroke='#008DC9' stroke-width='1.5'/>",
    "</svg>"
  )
  
  svg
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. ALGORITHME PRIORISATION — Score urgence intervention
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer score d'urgence intervention
#'
#' @param sf_data objet sf avec colonnes valeur, n, cluster
#' @param col_var colonne prévalence
#' @param col_n colonne effectif
#' @param weights vecteur poids (prévalence, urgence, population, accès)
#'
#' @return data.frame avec scores et ranking
calc_intervention_priority <- function(sf_data, col_var, col_n,
                                       weights = c(0.35, 0.30, 0.20, 0.15)) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    filter(!is.na(!!rlang::sym(col_var)), !is.na(!!rlang::sym(col_n)))
  
  # Composantes du score
  d <- d %>%
    mutate(
      # Score 1 : Prévalence (0-100)
      score_prevalence = !!rlang::sym(col_var),
      
      # Score 2 : Urgence (basée sur prévalence)
      score_urgence = case_when(
        !!rlang::sym(col_var) >= 30 ~ 100,
        !!rlang::sym(col_var) >= 20 ~ 75,
        !!rlang::sym(col_var) >= 15 ~ 50,
        !!rlang::sym(col_var) >= 10 ~ 25,
        TRUE ~ 0
      ),
      
      # Score 3 : Population affectée (normalisé 0-100)
      n_affected = round((!!rlang::sym(col_var) / 100) * !!rlang::sym(col_n)),
      score_population = (n_affected / max(n_affected, na.rm = TRUE)) * 100,
      
      # Score 4 : Accès (approximation basée sur population)
      # Région plus dense = plus difficile d'accès
      score_access = case_when(
        !!rlang::sym(col_n) > 10000 ~ 30,   # Difficile
        !!rlang::sym(col_n) > 5000 ~ 50,    # Modéré
        TRUE ~ 100                           # Facile
      ),
      
      # Score composite (0-100)
      priority_score = (
        weights[1] * score_prevalence +
        weights[2] * score_urgence +
        weights[3] * score_population +
        weights[4] * score_access
      ),
      
      # Classification
      priority_class = case_when(
        priority_score >= 75 ~ "CRITIQUE - Action immédiate",
        priority_score >= 60 ~ "SÉRIEUX - Action rapide",
        priority_score >= 40 ~ "MONTRER - Surveillance renforcée",
        TRUE ~ "NORMAL - Suivre"
      ),
      
      # Couleur
      priority_color = case_when(
        priority_score >= 75 ~ "#8b0000",
        priority_score >= 60 ~ "#d32f2f",
        priority_score >= 40 ~ "#ff9800",
        TRUE ~ "#4caf50"
      ),
      
      # Recommandation d'action
      recommendation = case_when(
        priority_score >= 75 ~ "Déployer intervention nutritionnelle d'urgence. Évaluation rapide. Traitement immédiat.",
        priority_score >= 60 ~ "Renforcer services nutritionnels. Dépistage accéléré. Formation agents.",
        priority_score >= 40 ~ "Sensibilisation communautaire. Suivi régulier. Renforcement capacités.",
        TRUE ~ "Surveillance continue. Partage données. Coordination locale."
      )
    ) %>%
    select(id, !!rlang::sym(col_var), !!rlang::sym(col_n),
           n_affected, priority_score, priority_class, 
           priority_color, recommendation) %>%
    arrange(desc(priority_score))
  
  d
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. RAPPORT GÉOGRAPHIQUE — Synthèse complète par région
# ══════════════════════════════════════════════════════════════════════════════

#' Générer rapport texte pour région
#'
#' @param sf_data objet sf
#' @param region_id région cible
#' @param col_var colonne variable
#' @param col_n colonne effectif
#'
#' @return texte Markdown
gen_region_report <- function(sf_data, region_id, col_var, col_n) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    filter(id == region_id)
  
  if (nrow(d) == 0) return("Région non trouvée")
  
  val <- d[[col_var]]
  n <- d[[col_n]]
  
  # Contexte national
  national_mean <- mean(sf_data[[col_var]], na.rm = TRUE)
  
  rapport <- paste0(
    "## ", region_id, "\n\n",
    "**Prévalence** : ", round(val, 1), "%\n\n",
    "**Effectif** : ", format(n, big.mark = " "), " enfants\n\n",
    "**Comparaison nationale** : ",
    if_else(val > national_mean, 
            paste0(round(val - national_mean, 1), "% AU-DESSUS de la moyenne"),
            paste0(round(national_mean - val, 1), "% EN-DESSOUS de la moyenne")),
    "\n\n"
  )
  
  rapport
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. EXPORT GÉOGRAPHIQUE — GeoJSON, CSV, Shapefile
# ══════════════════════════════════════════════════════════════════════════════

#' Exporter données géographiques
#'
#' @param sf_data objet sf
#' @param format "geojson" | "csv" | "shapefile"
#' @param filename nom fichier
#'
#' @return fichier ou objet
export_spatial_data <- function(sf_data, format = "geojson", filename = NULL) {
  
  if (format == "geojson") {
    if (is.null(filename)) filename <- "regions.geojson"
    st_write(sf_data, filename, delete_dsn = TRUE)
    return(paste("Exporté :", filename))
  }
  
  else if (format == "csv") {
    if (is.null(filename)) filename <- "regions.csv"
    d <- sf_data %>%
      as.data.frame() %>%
      select(-geometry)
    write_csv(d, filename)
    return(paste("Exporté :", filename))
  }
  
  else if (format == "shapefile") {
    if (is.null(filename)) filename <- "regions"
    st_write(sf_data, paste0(filename, ".shp"), delete_dsn = TRUE)
    return(paste("Exporté : ", filename, ".shp + fichiers associés"))
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. SYNTHÈSE GÉOGRAPHIQUE COMPACTE
# ══════════════════════════════════════════════════════════════════════════════

#' Résumé une ligne par région
#'
#' @param sf_data objet sf
#' @param col_var colonne variable
#' @param col_n colonne effectif
#'
#' @return data.frame compact
summary_geographic <- function(sf_data, col_var, col_n) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    arrange(desc(!!rlang::sym(col_var))) %>%
    mutate(
      rank = row_number(),
      category = case_when(
        !!rlang::sym(col_var) >= 15 ~ "Critique",
        !!rlang::sym(col_var) >= 10 ~ "Sérieux",
        !!rlang::sym(col_var) >= 5 ~ "Pauvre",
        TRUE ~ "Acceptable"
      )
    ) %>%
    select(rank, id, !!rlang::sym(col_var), !!rlang::sym(col_n), category)
  
  d
}
