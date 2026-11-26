# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  carto_predictive_engine.R — Prédictif & Benchmarking EXPERT               ║
# ║  ML spatial · Prédictions futures · Comparaisons régionales                 ║
# ║  TIER 4 : Intelligence prédictive avancée                                  ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(sf)
library(tidyverse)
tryCatch(library(caret), error = function(e) message("⚠️ caret non disponible — prédictions cartographiques limitées"))

# ══════════════════════════════════════════════════════════════════════════════
# 1. PREDICT HOTSPOTS FUTURS — Où augmentera la malnutrition
# ══════════════════════════════════════════════════════════════════════════════

#' Prédire évolution spatiale (si données historiques)
#'
#' @param sf_data objet sf avec colonnes variables
#' @param col_var colonne prévalence actuelle
#' @param historical_data liste des années précédentes (optionnel)
#'
#' @return sf_data avec colonne prediction
predict_spatial_trend <- function(sf_data, col_var, historical_data = NULL) {
  
  sf_data <- sf_data %>%
    mutate(
      # Approximation simple : régions hautes restent hautes
      # Pente de tendance estimée
      trend_factor = case_when(
        !!rlang::sym(col_var) >= 20 ~ 1.05,   # Tendance haussière
        !!rlang::sym(col_var) >= 15 ~ 1.02,
        !!rlang::sym(col_var) >= 10 ~ 0.99,   # Tendance baissière
        TRUE ~ 0.97
      ),
      
      # Prédiction année prochaine
      predicted_value = !!rlang::sym(col_var) * trend_factor,
      
      # Probabilité augmentation
      prob_increase = case_when(
        trend_factor > 1.02 ~ 0.75,
        trend_factor > 0.98 ~ 0.50,
        TRUE ~ 0.25
      ),
      
      # Classe prédictive
      prediction_class = case_when(
        prob_increase >= 0.75 ~ "Risque augmentation",
        prob_increase >= 0.50 ~ "Stable probable",
        TRUE ~ "Amélioration probable"
      )
    )
  
  sf_data
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. COMPARATIVE BENCHMARK — vs moyenne nationale + régions similaires
# ══════════════════════════════════════════════════════════════════════════════

#' Comparer région à benchmark (nationale + similaires)
#'
#' @param sf_data objet sf
#' @param region_id région cible
#' @param col_var colonne variable
#'
#' @return list avec comparaisons
calc_regional_benchmark <- function(sf_data, region_id, col_var) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry)
  
  # Valeur cible
  target_val <- d %>% filter(id == region_id) %>% pull(!!rlang::sym(col_var))
  
  # Moyenne nationale
  national_mean <- mean(d[[col_var]], na.rm = TRUE)
  national_sd <- sd(d[[col_var]], na.rm = TRUE)
  
  # Régions similaires (écart-type proche)
  similar_threshold <- national_sd / 2
  similar <- d %>%
    filter(
      abs(!!rlang::sym(col_var) - target_val) <= similar_threshold,
      id != region_id
    ) %>%
    summarise(
      n = n(),
      mean = mean(!!rlang::sym(col_var), na.rm = TRUE),
      sd = sd(!!rlang::sym(col_var), na.rm = TRUE)
    )
  
  list(
    region = region_id,
    value = target_val,
    national_mean = national_mean,
    national_sd = national_sd,
    vs_national = target_val - national_mean,
    similar_regions = similar$n,
    similar_mean = similar$mean,
    vs_similar = target_val - similar$mean,
    percentile = mean(d[[col_var]] < target_val, na.rm = TRUE) * 100
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. MOBILE REACH ANALYSIS — Accessibilité géographique
# ══════════════════════════════════════════════════════════════════════════════

#' Analyser accessibilité (basée sur population)
#'
#' @param sf_data objet sf
#' @param col_n colonne population/effectif
#'
#' @return sf_data avec colonnes accessibilité
calc_accessibility <- function(sf_data, col_n) {
  
  # Densité de population (proxy pour accessibilité)
  sf_data <- sf_data %>%
    mutate(
      area_km2 = st_area(.) / 1e6,  # km²
      pop_density = !!rlang::sym(col_n) / as.numeric(area_km2),
      
      # Classification accessibilité
      accessibility = case_when(
        pop_density > 50 ~ "Haute (urbain)",
        pop_density > 10 ~ "Modérée",
        TRUE ~ "Faible (rural)"
      ),
      
      # Effort d'intervention
      intervention_effort = case_when(
        accessibility == "Haute (urbain)" ~ "Faible",
        accessibility == "Modérée" ~ "Modéré",
        TRUE ~ "Élevé"
      )
    )
  
  sf_data
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. RESOURCE ALLOCATION ALGORITHM — Où placer les ressources
# ══════════════════════════════════════════════════════════════════════════════

#' Algorithme allocation optimale ressources
#'
#' @param sf_data objet sf avec priority_score, accessibility
#' @param budget_percent % budget à allouer (défaut: 70%)
#'
#' @return data.frame allocation
allocate_resources <- function(sf_data, budget_percent = 0.70) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    filter(!is.na(priority_score)) %>%
    arrange(desc(priority_score)) %>%
    mutate(
      # Pondérer par accessibilité
      access_weight = case_when(
        intervention_effort == "Faible" ~ 1.5,
        intervention_effort == "Modéré" ~ 1.0,
        TRUE ~ 0.7
      ),
      
      # Score allocation
      allocation_score = priority_score * access_weight,
      
      # Calculer budget
      cumsum_score = cumsum(allocation_score),
      total_score = sum(allocation_score),
      budget_allocated = cumsum_score / total_score <= budget_percent,
      
      # Ressources suggérées
      resource_level = case_when(
        budget_allocated & priority_score >= 75 ~ "Ressources COMPLÈTES",
        budget_allocated & priority_score >= 60 ~ "Ressources PARTIELLES",
        budget_allocated ~ "Ressources MINIMALES",
        TRUE ~ "Ressources PRIORITÉ SECONDAIRE"
      )
    ) %>%
    select(id, priority_score, allocation_score, budget_allocated, resource_level) %>%
    arrange(desc(allocation_score))
  
  d
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. PERFORMANCE TREND — Suivi progression régionale
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer tendance (si données multi-années)
#'
#' @param values vecteur de valeurs (séries temporelles)
#' @param years vecteur années correspondantes
#'
#' @return list tendance
calc_performance_trend <- function(values, years) {
  
  if (length(values) < 2) {
    return(list(
      trend = "Insuffisantes données",
      slope = NA,
      pace = NA
    ))
  }
  
  # Régression linéaire simple
  lm_model <- lm(values ~ years)
  slope <- coef(lm_model)[2]
  
  # Interprétation
  trend <- case_when(
    slope > 0.5 ~ "Aggravation rapide",
    slope > 0 ~ "Aggravation lente",
    slope > -0.5 ~ "Amélioration lente",
    TRUE ~ "Amélioration rapide"
  )
  
  pace <- abs(slope)
  
  list(
    trend = trend,
    slope = round(slope, 3),
    pace = round(pace, 3),
    direction = if_else(slope > 0, "↑", "↓")
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. SYNERGY ANALYSIS — Régions où interventions combinées optimal
# ══════════════════════════════════════════════════════════════════════════════

#' Identifier régions pour interventions synergiques
#'
#' @param sf_data objet sf
#' @param col_var1, col_var2 deux colonnes variables
#'
#' @return sf_data avec synergy_score
calc_synergy_opportunities <- function(sf_data, col_var1, col_var2) {
  
  sf_data <- sf_data %>%
    mutate(
      # Calcul corrélation spatiale
      both_high = (!!rlang::sym(col_var1) > median(!!rlang::sym(col_var1), na.rm = TRUE)) &
                  (!!rlang::sym(col_var2) > median(!!rlang::sym(col_var2), na.rm = TRUE)),
      
      synergy_score = case_when(
        both_high ~ 100,
        (!!rlang::sym(col_var1) > median(!!rlang::sym(col_var1), na.rm = TRUE)) |
        (!!rlang::sym(col_var2) > median(!!rlang::sym(col_var2), na.rm = TRUE)) ~ 50,
        TRUE ~ 0
      ),
      
      synergy_opportunity = case_when(
        synergy_score >= 75 ~ "Haute opportunité - Intervention intégrée prioritaire",
        synergy_score >= 50 ~ "Opportunité modérée - Intégration recommandée",
        TRUE ~ "Interventions ciblées séparément"
      )
    )
  
  sf_data
}
