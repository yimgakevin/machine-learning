# ============================================================================
# Expert Interpreter — Interprétation intelligente des résultats d'analyse
# ============================================================================
# Prend les résultats bruts d'une analyse et génère une interprétation
# intelligente, contextualisée, au niveau publication.
# ============================================================================

# ============================================================================
# 1. FONCTION PRINCIPALE : Interpréter un résultat d'analyse
# ============================================================================

interpret_analysis_result <- function(analysis_result, data_context = NULL) {
  # Interprète un résultat d'analyse et génère une explication intelligente
  #
  # Paramètres :
  #   analysis_result : résultat brut de execute_analysis()
  #   data_context : contexte des données (optionnel, pour plus de richesse)
  #
  # Retour :
  #   list(interpretation, insights, recommendations, next_steps)
  
  if (!analysis_result$success) {
    return(list(
      success = FALSE,
      error = analysis_result$error
    ))
  }
  
  type <- analysis_result$type
  result <- analysis_result$result
  
  # Dispatcher vers l'interpréteur spécialisé
  interpretation <- switch(type,
    exploration = interpret_exploration(result, data_context),
    pca = interpret_pca(result, data_context),
    famd = interpret_famd(result, data_context),
    mca = interpret_mca(result, data_context),
    clustering = interpret_clustering(result, data_context),
    inferential = interpret_inferential(result, data_context),
    smart_quality = interpret_smart_quality(result, data_context),
    prevalence = interpret_prevalence(result, data_context),
    food_security = interpret_food_security(result, data_context),
    zscore = interpret_zscore(result, data_context),
    
    # Défaut
    list(
      interpretation = "Analyse complétée.",
      insights = list(),
      recommendations = list()
    )
  )
  
  return(list(
    success = TRUE,
    type = type,
    interpretation = interpretation
  ))
}

# ============================================================================
# 2. INTERPRÉTEURS SPÉCIALISÉS
# ============================================================================

# ── EXPLORATION ───────────────────────────────────────────────────────────
interpret_exploration <- function(result, data_context) {
  
  insights <- list()
  recommendations <- list()
  
  n_rows <- result$n_rows
  n_cols <- result$n_cols
  missing <- mean(result$columns$missing)
  duplicates <- result$duplicates
  
  interpretation <- paste0(
    sprintf("📊 **Jeu de données** : %d observations, %d variables\n", n_rows, n_cols),
    sprintf("📈 **Qualité** : %.1f%% complétude, %d doublons\n\n", 100 - missing, duplicates),
    "**Analyse :**\n"
  )
  
  # Insights
  if (missing < 5) {
    insights[[length(insights) + 1]] <- "✅ Données très complètes — peu de valeurs manquantes"
  } else if (missing < 20) {
    insights[[length(insights) + 1]] <- "⚠️ Complétude modérée — vérifier les patterns de données manquantes"
  } else {
    insights[[length(insights) + 1]] <- "❌ Données incomplètes — imputation ou analyse des raisons nécessaire"
  }
  
  if (duplicates > 0) {
    insights[[length(insights) + 1]] <- sprintf("⚠️ %d doublons détectés — à vérifier", duplicates)
  }
  
  # Recommandations
  if (n_cols < 5) {
    recommendations[[length(recommendations) + 1]] <- "Ajouter plus de variables pour des analyses plus riches"
  }
  
  recommendations[[length(recommendations) + 1]] <- "Commencer par une exploration univariée pour chaque variable"
  recommendations[[length(recommendations) + 1]] <- "Examiner les correlations bivariées si colonnes numériques"
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("uni", "biv")  # Onglets suggérés
  )
}

# ── ACP ───────────────────────────────────────────────────────────────────
interpret_pca <- function(result, data_context) {
  
  var_exp <- result$variance_explained
  cum_var <- result$cumulative_variance
  n_dims <- result$n_dimensions
  
  interpretation <- paste0(
    "🔬 **Analyse en Composantes Principales**\n\n",
    sprintf("Le **premier axe explique %.1f%%** de la variance totale. ", var_exp),
    sprintf("Cumulativement, les 3 premiers axes expliquent ~%.1f%% de la variance.\n\n",
            min(90, cum_var))
  )
  
  insights <- list()
  recommendations <- list()
  
  # Insights
  if (var_exp > 50) {
    insights[[length(insights) + 1]] <- paste0(
      "✅ **Forte structuration** : le premier axe capture > 50% de la variance. ",
      "Les données ont une dimension dominante."
    )
  } else if (var_exp > 30) {
    insights[[length(insights) + 1]] <- paste0(
      "⚠️ **Variance distribuée** : plusieurs axes importants. ",
      "Besoin d'au moins 2-3 dimensions pour résumer les données."
    )
  } else {
    insights[[length(insights) + 1]] <- paste0(
      "💡 **Données complexes** : variance bien distribuée. ",
      "L'ACP révèle la multi-dimensionnalité intrinsèque des données."
    )
  }
  
  insights[[length(insights) + 1]] <- sprintf(
    "🎯 **Suffisance** : %d axes expliquent ~95%% de la variance",
    min(which(result$eigenvalues[, 3] >= 95))
  )
  
  # Recommandations
  recommendations[[length(recommendations) + 1]] <- "Examiner les contributions des variables au premier axe (cercle des corrélations)"
  recommendations[[length(recommendations) + 1]] <- "Représenter les individus sur les 2 premiers axes pour détecter des groupes"
  recommendations[[length(recommendations) + 1]] <- "Croiser avec clustering pour identifier des profils"
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("clustering", "biv")
  )
}

# ── FAMD ──────────────────────────────────────────────────────────────────
interpret_famd <- function(result, data_context) {
  
  var_exp <- result$variance_explained
  
  interpretation <- paste0(
    "🔮 **Analyse Factorielle des Données Mixtes**\n\n",
    sprintf("Premier axe : %.1f%% variance | Données mixtes (numériques + catégorielles) analysées ensemble.\n",
            var_exp)
  )
  
  insights <- list(
    "✅ FAMD combine ACP (numériques) + ACM (catégorielles) — idéal pour données hétérogènes",
    sprintf("📊 Variance axe 1 : %.1f%% → structure claire ou données complexes ?", var_exp)
  )
  
  recommendations <- list(
    "Explorer le biplot : comment numériques et catégorielles se projettent",
    "Analyser les modalités catégorielles les plus influentes",
    "Croiser avec clustering pour typologies"
  )
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("clustering", "descriptif")
  )
}

# ── MCA ───────────────────────────────────────────────────────────────────
interpret_mca <- function(result, data_context) {
  
  var_exp <- result$variance_explained
  
  interpretation <- paste0(
    "🎨 **Analyse des Correspondances Multiples**\n\n",
    sprintf("Variables catégorielles : %.1f%% variance axe 1 | Détecte les associations entre catégories.\n",
            var_exp)
  )
  
  insights <- list(
    "✅ ACM révèle les profils/typologies de population",
    sprintf("📍 Variance axe 1 : %.1f%% — opposition principale entre catégories", var_exp)
  )
  
  recommendations <- list(
    "Examiner quelles catégories s'opposent sur axe 1",
    "Utiliser pour segmentation de population",
    "Valider clusters avec chi² tests"
  )
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("clustering", "inferentielle")
  )
}

# ── CLUSTERING ────────────────────────────────────────────────────────────
interpret_clustering <- function(result, data_context) {
  
  k <- result$n_clusters
  silhouette <- result$silhouette
  
  interpretation <- paste0(
    sprintf("🔗 **Clustering K-means : %d clusters**\n\n", k),
    sprintf("Silhouette : %.2f (qualité de séparation) | Tailles : ",
            silhouette),
    paste(result$cluster_sizes, collapse = ", "),
    "\n"
  )
  
  insights <- list()
  
  if (silhouette > 0.7) {
    insights[[length(insights) + 1]] <- "✅ **Clustering excellent** : clusters bien séparés et cohésifs"
  } else if (silhouette > 0.5) {
    insights[[length(insights) + 1]] <- "⚠️ **Clustering acceptable** : clusters distincts mais avec chevauchement"
  } else {
    insights[[length(insights) + 1]] <- "💡 **Clusters peu séparé** : considérer K différent ou données peu clusterisables"
  }
  
  insights[[length(insights) + 1]] <- sprintf("📊 %d clusters naturels détectés pour segmentation", k)
  
  recommendations <- list(
    "Examiner les profils de chaque cluster (variables principales)",
    "Vérifier la stabilité en relançant avec seeds différentes",
    "Valider biologiquement/socialement l'existence de ces clusters"
  )
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("descriptif", "biv")
  )
}

# ── TESTS INFÉRENTIELS ────────────────────────────────────────────────────
interpret_inferential <- function(result, data_context) {
  
  interpretation <- "🧪 **Tests inférentiels** : normalité vérifiée, corrélations calculées\n\n"
  
  insights <- list()
  
  # Compter variables normales
  n_normal <- sum(sapply(result$normality, function(x) if (!is.null(x)) x$is_normal else FALSE))
  total <- length(result$normality)
  
  insights[[length(insights) + 1]] <- sprintf(
    "%d/%d variables sont normalement distribuées (Shapiro-Wilk, p>0.05)",
    n_normal, total
  )
  
  # Corrélations fortes
  cor_mat <- result$correlations
  diag(cor_mat) <- NA
  strong_cors <- which(abs(cor_mat) > 0.7, arr.ind = TRUE)
  
  if (nrow(strong_cors) > 0) {
    insights[[length(insights) + 1]] <- sprintf(
      "🔗 **%d corrélations fortes** (r > 0.7) détectées — possible multicolinéarité",
      nrow(strong_cors) / 2
    )
  }
  
  recommendations <- list(
    "Utiliser tests paramétriques si normalité confirmée, non-paramétriques sinon",
    "Attention à la multicolinéarité pour régressions",
    "Tester indépendance avec Chi² pour variables catégorielles"
  )
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("inferentielle", "descriptif")
  )
}

# ── QUALITÉ SMART ─────────────────────────────────────────────────────────
interpret_smart_quality <- function(result, data_context) {
  
  level <- result$quality_level
  score <- result$score
  
  interpretation <- sprintf(
    "✅ **Qualité SMART : %s** (%d/2 critères)\n\n",
    level, score
  )
  
  insights <- result$flags
  
  recommendations <- list()
  if (level == "À améliorer") {
    recommendations[[length(recommendations) + 1]] <- "Ajouter variables anthropométriques (âge, poids, taille)"
    recommendations[[length(recommendations) + 1]] <- "Améliorer la complétude des données"
  } else if (level == "Bonne") {
    recommendations[[length(recommendations) + 1]] <- "Données prêtes pour analyses SMART"
    recommendations[[length(recommendations) + 1]] <- "Exécuter plausibility check détaillé"
  } else {
    recommendations[[length(recommendations) + 1]] <- "✅ Données de haute qualité SMART"
    recommendations[[length(recommendations) + 1]] <- "Procéder aux analyses de prévalence"
  }
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("qualite", "prevalence")
  )
}

# ── PRÉVALENCES ───────────────────────────────────────────────────────────
interpret_prevalence <- function(result, data_context) {
  
  n_indic <- result$n_indicators
  
  interpretation <- sprintf(
    "📈 **Prévalences SMART** : %d indicateurs calculés\n\n",
    n_indic
  )
  
  insights <- list()
  
  for (prev in result$prevalences) {
    pct <- prev$prevalence
    severity <- prev$severity
    severity_emoji <- switch(severity,
      "Acceptable" = "✅",
      "Alerte" = "🟡",
      "Sérieux" = "🟠",
      "Critique" = "🔴",
      "Urgence" = "🟥"
    )
    
    insights[[length(insights) + 1]] <- sprintf(
      "%s **%s** : %.1f%% (%s)",
      severity_emoji, prev$column, pct, severity
    )
  }
  
  recommendations <- list(
    "Comparer avec seuils OMS/SMART",
    "Stratifier par âge/sexe/région si données disponibles",
    "Contextualiser avec situation humanitaire"
  )
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("descriptif", "cartographie")
  )
}

# ── SÉCURITÉ ALIMENTAIRE ──────────────────────────────────────────────────
interpret_food_security <- function(result, data_context) {
  
  fcs_mean <- result$fcs_mean
  pct_accept <- result$pct_acceptable
  
  interpretation <- sprintf(
    "🍽️ **Sécurité alimentaire** : FCS moyen = %.1f\n\n",
    fcs_mean
  )
  
  insights <- list(
    sprintf("📊 %.1f%% population en situation alimentaire acceptable (FCS > 35)", pct_accept),
    if (pct_accept < 60) "❌ Majorité de la population en insécurité alimentaire" else "✅ Situation alimentaire généralement acceptable"
  )
  
  recommendations <- list(
    "Analyser composants FCS (groupes alimentaires consommés)",
    "Croiser avec coping strategies (rCSI)",
    "Stratifier par géographie/groupe vulnérable"
  )
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("secu_alim", "descriptif")
  )
}

# ── Z-SCORES OMS ──────────────────────────────────────────────────────────
interpret_zscore <- function(result, data_context) {
  
  n_indic <- result$n_indicators
  
  interpretation <- sprintf(
    "📐 **Z-scores OMS 2006** : %d indicateurs d'anthropométrie\n\n",
    n_indic
  )
  
  insights <- list()
  
  for (ind in result$indicators) {
    pct <- ind$pct_stunted
    insights[[length(insights) + 1]] <- sprintf(
      "%s : %.1f%% dénutrition chronique (z < -2)",
      ind$indicator, pct
    )
  }
  
  recommendations <- list(
    "Comparer avec références OMS",
    "Analyser trends temporels",
    "Stratifier par groupe d'âge"
  )
  
  list(
    interpretation = interpretation,
    insights = insights,
    recommendations = recommendations,
    next_steps = list("descriptif", "univariee")
  )
}

cat("✅ Expert Interpreter loaded\n")
