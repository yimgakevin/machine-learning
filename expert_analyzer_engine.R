# ============================================================================
# Expert Analyzer Engine — Exécution Autonome des Analyses
# ============================================================================
# Ce module exécute réellement les analyses proposées par Gemini
# et retourne les résultats pour interprétation.
# ============================================================================

library(tidyverse)
library(DT)

# ============================================================================
# 1. DISPATCHER : Route chaque action vers la bonne analyse
# ============================================================================

execute_analysis <- function(df, action, variables = NULL) {
  # Exécute une analyse basée sur l'action demandée
  # 
  # Paramètres :
  #   df : data frame à analyser
  #   action : identifiant d'analyse (acp, clustering, etc.)
  #   variables : liste des variables à utiliser (optionnel)
  # 
  # Retour :
  #   list(success=TRUE/FALSE, result=résultat, error=message, visualization=plot)
  
  if (is.null(df) || nrow(df) == 0) {
    return(list(success = FALSE, error = "Aucune donnée disponible"))
  }
  
  result <- tryCatch({
    switch(action,
      
      # ── EXPLORATION ──────────────────────────────
      exploration = analyze_exploration(df),
      
      # ── ACP / FAMD / ACM ─────────────────────────
      acp = analyze_pca(df, variables),
      famd = analyze_famd(df, variables),
      acm = analyze_mca(df, variables),
      
      # ── CLUSTERING ───────────────────────────────
      clustering = analyze_clustering(df, variables),
      
      # ── TESTS STATISTIQUES ───────────────────────
      inferentielle = analyze_inferential(df, variables),
      
      # ── QUALITÉ SMART ────────────────────────────
      qualite = analyze_smart_quality(df),
      
      # ── PRÉVALENCES ──────────────────────────────
      prevalence = analyze_prevalence(df),
      
      # ── SÉCURITÉ ALIMENTAIRE ─────────────────────
      secu_alim = analyze_food_security(df),
      
      # ── Z-SCORES OMS ────────────────────────────
      zscore = analyze_zscore(df),
      
      # Défaut
      list(success = FALSE, error = sprintf("Action '%s' non implémentée", action))
    )
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
  
  return(result)
}

# ============================================================================
# 2. ANALYSES SPÉCIFIQUES
# ============================================================================

# ── EXPLORATION ──────────────────────────────────────────────────────────
analyze_exploration <- function(df) {
  
  col_types <- sapply(df, class)
  
  summary_stats <- list(
    n_rows = nrow(df),
    n_cols = ncol(df),
    columns = list(
      names = names(df),
      types = col_types,
      missing = colSums(is.na(df))
    ),
    duplicates = sum(duplicated(df)),
    numeric_summary = df |>
      select(where(is.numeric)) |>
      summarise(across(everything(), list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      )))
  )
  
  list(
    success = TRUE,
    type = "exploration",
    result = summary_stats,
    interpretation_prompt = paste0(
      "Les données ont ", nrow(df), " observations et ", ncol(df), " colonnes. ",
      "Résume les statistiques descriptives clés."
    )
  )
}

# ── ACP ───────────────────────────────────────────────────────────────────
analyze_pca <- function(df, variables = NULL) {
  
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    return(list(success = FALSE, error = "Package FactoMineR non installé"))
  }
  
  # Sélectionner colonnes numériques
  df_num <- df |> select(where(is.numeric))
  
  if (ncol(df_num) < 2) {
    return(list(success = FALSE, error = "Au moins 2 colonnes numériques requises"))
  }
  
  # Lancer ACP
  pca_result <- FactoMineR::PCA(df_num, scale.unit = TRUE, graph = FALSE)
  
  # Extraire résultats clés
  eig <- pca_result$eig
  var_contrib <- pca_result$var$contrib[, 1:min(3, ncol(pca_result$var$contrib))]
  
  list(
    success = TRUE,
    type = "pca",
    result = list(
      eigenvalues = eig,
      variance_explained = eig[1, 2],  # % variance axe 1
      cumulative_variance = eig[1, 3],
      variables_contrib = var_contrib,
      n_dimensions = nrow(eig)
    ),
    interpretation_prompt = sprintf(
      "Analyse en Composantes Principales exécutée. Le premier axe explique %.1f%% de la variance. ",
      eig[1, 2]
    ),
    object = pca_result  # Pour des analyses ultérieures
  )
}

# ── FAMD ──────────────────────────────────────────────────────────────────
analyze_famd <- function(df, variables = NULL) {
  
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    return(list(success = FALSE, error = "Package FactoMineR non installé"))
  }
  
  # FAMD accepte données mixtes
  famd_result <- FactoMineR::FAMD(df, graph = FALSE)
  
  eig <- famd_result$eig
  
  list(
    success = TRUE,
    type = "famd",
    result = list(
      eigenvalues = eig,
      variance_explained = eig[1, 2],
      cumulative_variance = eig[1, 3],
      n_dimensions = nrow(eig)
    ),
    interpretation_prompt = sprintf(
      "Analyse Factorielle des Données Mixtes : %.1f%% de variance sur axe 1.",
      eig[1, 2]
    ),
    object = famd_result
  )
}

# ── ACM ───────────────────────────────────────────────────────────────────
analyze_mca <- function(df, variables = NULL) {
  
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    return(list(success = FALSE, error = "Package FactoMineR non installé"))
  }
  
  df_cat <- df |> select(where(is.factor) | where(is.character))
  
  if (ncol(df_cat) < 2) {
    return(list(success = FALSE, error = "Au moins 2 colonnes catégorielles requises"))
  }
  
  mca_result <- FactoMineR::MCA(df_cat, graph = FALSE)
  
  eig <- mca_result$eig
  
  list(
    success = TRUE,
    type = "mca",
    result = list(
      eigenvalues = eig,
      variance_explained = eig[1, 2],
      cumulative_variance = eig[1, 3],
      n_dimensions = nrow(eig)
    ),
    interpretation_prompt = sprintf(
      "Analyse des Correspondances Multiples : %.1f%% variance axe 1.",
      eig[1, 2]
    ),
    object = mca_result
  )
}

# ── CLUSTERING ────────────────────────────────────────────────────────────
analyze_clustering <- function(df, variables = NULL) {
  
  df_num <- df |> select(where(is.numeric))
  
  if (ncol(df_num) < 2) {
    return(list(success = FALSE, error = "Au moins 2 colonnes numériques requises"))
  }
  
  # Standardiser
  df_scaled <- scale(df_num)
  
  # K-means avec k optimal (silhouette)
  k_values <- 2:min(10, nrow(df) - 1)
  silhouettes <- sapply(k_values, function(k) {
    km <- kmeans(df_scaled, k, nstart = 10)
    mean(cluster::silhouette(km$cluster, dist(df_scaled))[, 3])
  })
  
  k_optimal <- k_values[which.max(silhouettes)]
  km_result <- kmeans(df_scaled, k_optimal, nstart = 10)
  
  list(
    success = TRUE,
    type = "clustering",
    result = list(
      n_clusters = k_optimal,
      cluster_sizes = table(km_result$cluster),
      silhouette = max(silhouettes),
      wcss = km_result$tot.withinss,
      cluster_assignments = km_result$cluster
    ),
    interpretation_prompt = sprintf(
      "Clustering K-means : %d clusters optimaux identifiés (silhouette=%.2f). ",
      k_optimal, max(silhouettes)
    ),
    object = km_result
  )
}

# ── TESTS STATISTIQUES ────────────────────────────────────────────────────
analyze_inferential <- function(df, variables = NULL) {
  
  df_num <- df |> select(where(is.numeric))
  
  if (ncol(df_num) < 1) {
    return(list(success = FALSE, error = "Au moins 1 colonne numérique requise"))
  }
  
  # Tester normalité (Shapiro-Wilk)
  normality_tests <- lapply(df_num, function(x) {
    x_clean <- na.omit(x)
    if (length(x_clean) > 3) {
      result <- shapiro.test(x_clean)
      list(statistic = result$statistic, p_value = result$p.value, is_normal = result$p.value > 0.05)
    } else {
      NULL
    }
  })
  
  # Tester corrélations
  cor_matrix <- cor(df_num, use = "complete.obs")
  
  list(
    success = TRUE,
    type = "inferential",
    result = list(
      normality = Filter(Negate(is.null), normality_tests),
      correlations = cor_matrix,
      n_variables = ncol(df_num)
    ),
    interpretation_prompt = sprintf(
      "Tests inférentiels : normalité vérifiée, corrélations calculées (%d variables).",
      ncol(df_num)
    )
  )
}

# ── QUALITÉ SMART ─────────────────────────────────────────────────────────
analyze_smart_quality <- function(df) {
  
  # Détecteur simplifié de patterns SMART
  smart_score <- 0
  flags <- list()
  
  # Vérifier colonnes clés
  has_age <- any(grepl("age|mois", tolower(names(df))))
  has_weight <- any(grepl("poids|weight", tolower(names(df))))
  has_height <- any(grepl("taille|height", tolower(names(df))))
  
  if (has_age && has_weight && has_height) {
    smart_score <- smart_score + 1
    flags$anthropometric <- "✅ Variables anthropométriques détectées"
  }
  
  # Vérifier données complètes
  na_pct <- mean(is.na(df)) * 100
  if (na_pct < 5) {
    smart_score <- smart_score + 1
    flags$completeness <- "✅ Données complètes (< 5% NA)"
  } else if (na_pct < 20) {
    flags$completeness <- "⚠️ Données partiellement complètes (5-20% NA)"
  } else {
    flags$completeness <- "❌ Données incomplètes (> 20% NA)"
  }
  
  # Déterminer qualité
  quality_level <- if (smart_score >= 2) "Excellente" else if (smart_score >= 1) "Bonne" else "À améliorer"
  
  list(
    success = TRUE,
    type = "smart_quality",
    result = list(
      quality_level = quality_level,
      score = smart_score,
      flags = flags,
      missing_pct = na_pct
    ),
    interpretation_prompt = sprintf(
      "Qualité SMART : %s (%d/2 critères). Complétude : %.1f%%.",
      quality_level, smart_score, 100 - na_pct
    )
  )
}

# ── PRÉVALENCES ───────────────────────────────────────────────────────────
analyze_prevalence <- function(df) {
  
  # Chercher colonnes de malnutrition
  mal_cols <- names(df)[grepl("GAM|SAM|MAM|wasting|stunting", tolower(names(df)))]
  
  if (length(mal_cols) == 0) {
    mal_cols <- names(df)[grepl("malnut", tolower(names(df)))]
  }
  
  if (length(mal_cols) == 0) {
    return(list(success = FALSE, error = "Aucun indicateur de malnutrition détecté"))
  }
  
  # Calculer prévalences
  prev_results <- lapply(mal_cols, function(col) {
    values <- df[[col]]
    if (is.logical(values) || is.character(values)) {
      n_positive <- sum(values == TRUE | tolower(values) == "yes" | tolower(values) == "oui", na.rm = TRUE)
      n_total <- sum(!is.na(values))
      pct <- (n_positive / n_total) * 100
      
      # Déterminer sévérité OMS
      severity <- if (pct < 5) "Acceptable" 
                  else if (pct < 10) "Alerte" 
                  else if (pct < 15) "Sérieux"
                  else if (pct < 30) "Critique"
                  else "Urgence"
      
      list(
        column = col,
        prevalence = pct,
        n = n_total,
        severity = severity
      )
    } else {
      NULL
    }
  })
  
  prev_results <- Filter(Negate(is.null), prev_results)
  
  if (length(prev_results) == 0) {
    return(list(success = FALSE, error = "Format d'indicateurs non reconnu"))
  }
  
  list(
    success = TRUE,
    type = "prevalence",
    result = list(
      prevalences = prev_results,
      n_indicators = length(prev_results)
    ),
    interpretation_prompt = sprintf(
      "Prévalences SMART calculées pour %d indicateurs.",
      length(prev_results)
    )
  )
}

# ── SÉCURITÉ ALIMENTAIRE ──────────────────────────────────────────────────
analyze_food_security <- function(df) {
  
  # Chercher FCS
  fcs_col <- names(df)[grepl("^FCS$|^fcs$", names(df))]
  
  if (length(fcs_col) == 0) {
    return(list(success = FALSE, error = "Colonne FCS non trouvée"))
  }
  
  fcs_vals <- df[[fcs_col[1]]]
  fcs_clean <- na.omit(fcs_vals)
  
  # Catégoriser FCS
  categories <- cut(fcs_clean,
    breaks = c(-Inf, 21, 35, Inf),
    labels = c("Pauvre", "Limite", "Acceptable")
  )
  
  cat_counts <- table(categories)
  
  list(
    success = TRUE,
    type = "food_security",
    result = list(
      fcs_mean = mean(fcs_clean),
      fcs_sd = sd(fcs_clean),
      categories = cat_counts,
      pct_acceptable = (sum(categories == "Acceptable") / length(categories)) * 100
    ),
    interpretation_prompt = sprintf(
      "Sécurité alimentaire : FCS moyen = %.1f (SD=%.1f). %.1f%% population en situation acceptable.",
      mean(fcs_clean), sd(fcs_clean),
      (sum(categories == "Acceptable") / length(categories)) * 100
    )
  )
}

# ── Z-SCORES OMS ──────────────────────────────────────────────────────────
analyze_zscore <- function(df) {
  
  # Chercher colonnes z-score
  zscore_cols <- names(df)[grepl("WHZ|HAZ|WAZ|zscore|z_score", tolower(names(df)))]
  
  if (length(zscore_cols) == 0) {
    return(list(success = FALSE, error = "Aucune colonne z-score détectée"))
  }
  
  # Statistiques par z-score
  zscore_stats <- lapply(zscore_cols, function(col) {
    vals <- na.omit(df[[col]])
    
    # Compter < -2 (dénutrition)
    below_minus2 <- sum(vals < -2) / length(vals) * 100
    
    list(
      indicator = col,
      mean = mean(vals),
      sd = sd(vals),
      pct_stunted = below_minus2
    )
  })
  
  list(
    success = TRUE,
    type = "zscore",
    result = list(
      indicators = zscore_stats,
      n_indicators = length(zscore_cols)
    ),
    interpretation_prompt = sprintf(
      "Z-scores OMS : %d indicateurs analysés.",
      length(zscore_cols)
    )
  )
}

cat("✅ Expert Analyzer Engine loaded\n")
