# ============================================================================
# Analysis Agent v3 - Autonomous Data Analysis Execution
# ============================================================================
# Agent exécute analyses autonomement: ACP, ACM, FAMD, tests stats, ML
# Sélection automatique meilleures méthodes basées sur données
# Validation hypothèses avant exécution
# Interprétation résultats en langage naturel
# ============================================================================

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(e1071)

# Note: dépendances (config_api_secure_v2, gemini_api_v3, context_manager_v3,
# config_oms, rdim_validator) sont sourcées par app_v2_final.R. Ne pas re-sourcer ici.

# ============================================================================
# 1. INITIALIZE ANALYSIS AGENT
# ============================================================================

initialize_analysis_agent <- function(user_context = NULL) {
  
  agent <- list(
    type = "analysis",
    user_context = user_context,
    conversation = initialize_conversation(user_context),
    analysis_history = list(),
    created = Sys.time(),
    analyses_executed = 0,
    status = "ready"
  )
  
  class(agent) <- c("analysis_agent", "list")
  
  cat("✅ Analysis Agent Initialized\n")
  
  return(agent)
}

# ============================================================================
# 2. ANALYZE REQUEST - Interpréter demande d'analyse
# ============================================================================

analyze_request <- function(agent, user_request, df) {
  
  analysis_request <- list(
    original_request = user_request,
    user_df = df,
    timestamp = Sys.time()
  )
  
  # Déterminer type d'analyse demandé
  request_type <- detect_analysis_type(user_request)
  
  # Extraire variables mentionnées
  mentioned_vars <- extract_variables_from_request(user_request, df)
  
  # Construire analyse plan
  analysis_plan <- list(
    type = request_type,
    variables = mentioned_vars,
    method = NULL,  # À déterminer
    options = list()
  )
  
  analysis_request$plan <- analysis_plan
  
  return(analysis_request)
}

# ============================================================================
# 3. AUTO-SELECT METHOD - Sélection Automatique Méthode
# ============================================================================

auto_select_method <- function(df, variables = NULL, analysis_type = "exploratory") {
  
  if (is.null(variables)) {
    variables <- names(df)
  }
  
  var_data <- df |> select(all_of(variables))
  
  # Compter types variables
  numeric_vars <- names(select_if(var_data, is.numeric))
  categorical_vars <- names(select_if(var_data, ~!is.numeric(.)))
  
  n_numeric <- length(numeric_vars)
  n_categorical <- length(categorical_vars)
  
  # Déterminer meilleure méthode
  recommended_method <- if (analysis_type == "exploratory") {
    
    if (n_numeric >= 3 && n_categorical == 0) {
      "PCA"  # ACP
    } else if (n_numeric >= 3 && n_categorical >= 1) {
      "FAMD"  # ACP for mixed data
    } else if (n_categorical >= 2 && n_numeric == 0) {
      "MCA"  # ACM
    } else if (n_numeric >= 2 && n_categorical == 1) {
      "correlation_boxplot"
    } else {
      "descriptive"
    }
    
  } else if (analysis_type == "clustering") {
    
    if (n_numeric >= 3) {
      "kmeans"
    } else if (n_numeric >= 2) {
      "hierarchical"
    } else {
      "descriptive"
    }
    
  } else if (analysis_type == "hypothesis_test") {
    
    if (n_numeric >= 1 && n_categorical == 1) {
      "t_test_or_anova"
    } else if (n_categorical >= 2) {
      "chi_squared"
    } else {
      "correlation"
    }
    
  } else {
    "descriptive"
  }
  
  return(recommended_method)
}

# ============================================================================
# 4. VALIDATE ASSUMPTIONS - Vérifier Hypothèses Statistiques
# ============================================================================

validate_analysis_assumptions <- function(
  df,
  variables,
  method
) {
  
  assumptions <- list(
    method = method,
    passed = TRUE,
    warnings = c(),
    errors = c()
  )
  
  if (method == "PCA") {
    # Validation ACP
    validation <- validate_pca(df, variables)
    assumptions$passed <- validation$valid
    assumptions$warnings <- c(assumptions$warnings, validation$warnings)
    assumptions$errors <- c(assumptions$errors, validation$errors)
    
  } else if (method == "MCA") {
    # Validation ACM
    validation <- validate_mca(df, variables)
    assumptions$passed <- validation$valid
    assumptions$warnings <- c(assumptions$warnings, validation$warnings)
    
  } else if (method == "FAMD") {
    # Validation FAMD
    validation <- validate_famd(df, variables)
    assumptions$passed <- validation$valid
    assumptions$warnings <- c(assumptions$warnings, validation$warnings)
    
  } else if (method == "t_test_or_anova") {
    # Validation tests paramétrique
    assumptions <- check_t_test_assumptions(df, variables, assumptions)
    
  } else if (method == "chi_squared") {
    # Validation chi-squared
    assumptions <- check_chi_squared_assumptions(df, variables, assumptions)
  }
  
  return(assumptions)
}

check_t_test_assumptions <- function(df, variables, assumptions) {
  
  numeric_vars <- names(select_if(df[, variables, drop = FALSE], is.numeric))
  
  for (var in numeric_vars) {
    vals <- na.omit(df[[var]])
    if (length(vals) < 3) {
      assumptions$warnings <- c(assumptions$warnings,
        sprintf("⚠️  '%s' : trop peu de valeurs valides (%d) pour tester la normalité", var, length(vals)))
      next
    }
    # Normalité (Shapiro-Wilk si n < 5000)
    if (length(vals) < 5000) {
      p_val <- tryCatch(shapiro.test(vals)$p.value, error = function(e) NA_real_)
      if (!is.na(p_val) && p_val < 0.05) {
        assumptions$warnings <- c(
          assumptions$warnings,
          sprintf("⚠️  '%s' non-normalement distribuée (p=%.3f) - considérer test non-paramétrique", var, p_val)
        )
      }
    }
    
    # Variance homogène (Levene si groupes)
    # À implémenter si besoin
  }
  
  return(assumptions)
}

check_chi_squared_assumptions <- function(df, variables, assumptions) {
  
  # Effectifs min = 5
  for (var in variables) {
    counts <- table(df[[var]])
    if (any(counts < 5)) {
      assumptions$warnings <- c(
        assumptions$warnings,
        sprintf("⚠️  '%s' a cellules avec effectif < 5 - résultats chi² peuvent être biaisés", var)
      )
    }
  }
  
  return(assumptions)
}

# ============================================================================
# 5. EXECUTE ANALYSIS - Exécuter Analyse
# ============================================================================

execute_analysis <- function(
  agent,
  df,
  analysis_request
) {
  
  tryCatch({
    
    # Valider hypothèses
    assumptions <- validate_analysis_assumptions(
      df,
      analysis_request$plan$variables,
      analysis_request$plan$method
    )
    
    if (!assumptions$passed && length(assumptions$errors) > 0) {
      return(list(
        success = FALSE,
        error = paste(assumptions$errors, collapse = "\n"),
        assumptions = assumptions
      ))
    }
    
    # Exécuter l'analyse
    result <- switch(
      analysis_request$plan$method,
      
      PCA = execute_pca_analysis(df, analysis_request$plan$variables),
      
      MCA = execute_mca_analysis(df, analysis_request$plan$variables),
      
      FAMD = execute_famd_analysis(df, analysis_request$plan$variables),
      
      kmeans = execute_kmeans_analysis(df, analysis_request$plan$variables),
      
      hierarchical = execute_hierarchical_analysis(df, analysis_request$plan$variables),
      
      t_test_or_anova = execute_t_test_analysis(df, analysis_request$plan$variables),
      
      chi_squared = execute_chi_squared_analysis(df, analysis_request$plan$variables),
      
      correlation = execute_correlation_analysis(df, analysis_request$plan$variables),
      
      descriptive = execute_descriptive_analysis(df, analysis_request$plan$variables),
      
      list(error = "Method not implemented")
    )
    
    # Tracker analyse
    agent$analysis_history <- c(
      agent$analysis_history,
      list(list(
        method = analysis_request$plan$method,
        variables = analysis_request$plan$variables,
        timestamp = Sys.time(),
        success = is.null(result$error)
      ))
    )
    
    agent$analyses_executed <- agent$analyses_executed + 1
    
    return(list(
      success = TRUE,
      result = result,
      assumptions = assumptions,
      agent = agent
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = conditionMessage(e),
      agent = agent
    ))
  })
}

# ============================================================================
# 6. SPECIFIC ANALYSIS IMPLEMENTATIONS
# ============================================================================

execute_pca_analysis <- function(df, variables) {
  
  df_numeric <- df |> select(all_of(variables))
  
  pca_result <- PCA(df_numeric, scale.unit = TRUE, ncp = min(5, ncol(df_numeric)))
  
  return(list(
    method = "PCA",
    pca_object = pca_result,
    explained_var = pca_result$eig,
    loadings = pca_result$var$coord,
    individuals = pca_result$ind$coord
  ))
}

execute_mca_analysis <- function(df, variables) {
  
  df_categorical <- df |> select(all_of(variables))
  
  mca_result <- MCA(df_categorical, ncp = min(5, ncol(df_categorical)))
  
  return(list(
    method = "MCA",
    mca_object = mca_result,
    explained_var = mca_result$eig,
    variables = mca_result$var$coord
  ))
}

execute_famd_analysis <- function(df, variables) {
  
  df_mixed <- df |> select(all_of(variables))
  
  famd_result <- FAMD(df_mixed, ncp = min(5, ncol(df_mixed)))
  
  return(list(
    method = "FAMD",
    famd_object = famd_result,
    explained_var = famd_result$eig,
    individuals = famd_result$ind$coord
  ))
}

execute_kmeans_analysis <- function(df, variables) {
  
  df_numeric <- df |> select(all_of(variables)) |> scale()
  
  # Déterminer optimal k (elbow method)
  wcss <- sapply(1:min(10, nrow(df_numeric)), function(k) {
    kmeans(df_numeric, centers = k, nstart = 10)$tot.withinss
  })
  
  optimal_k <- which.min(diff(wcss)) + 1
  optimal_k <- max(2, min(optimal_k, 5))
  
  km_result <- kmeans(df_numeric, centers = optimal_k, nstart = 10)
  
  return(list(
    method = "K-means",
    kmeans_object = km_result,
    optimal_k = optimal_k,
    cluster_sizes = table(km_result$cluster),
    wcss_by_k = wcss
  ))
}

execute_hierarchical_analysis <- function(df, variables) {
  
  df_numeric <- df |> select(all_of(variables)) |> scale()
  
  dist_matrix <- dist(df_numeric, method = "euclidean")
  hc_result <- hclust(dist_matrix, method = "ward.D2")
  
  return(list(
    method = "Hierarchical Clustering",
    hclust_object = hc_result,
    dendrogram = as.dendrogram(hc_result)
  ))
}

execute_t_test_analysis <- function(df, variables) {
  
  numeric_vars <- names(select_if(df |> select(all_of(variables)), is.numeric))
  categorical_vars <- setdiff(variables, numeric_vars)
  
  if (length(numeric_vars) == 0 || length(categorical_vars) == 0) {
    return(list(error = "Need numeric and categorical variables"))
  }
  
  outcome <- numeric_vars[1]
  group_var <- categorical_vars[1]
  
  groups <- unique(df[[group_var]])
  
  if (length(groups) == 2) {
    # T-test
    test_result <- t.test(df[[outcome]] ~ df[[group_var]])
  } else {
    # ANOVA
    test_result <- aov(as.formula(paste0(outcome, "~", group_var)), data = df)
    test_result <- summary(test_result)
  }
  
  return(list(
    method = "T-test / ANOVA",
    test_result = test_result,
    outcome = outcome,
    grouping_var = group_var
  ))
}

execute_chi_squared_analysis <- function(df, variables) {
  
  if (length(variables) < 2) {
    return(list(error = "Need at least 2 variables"))
  }
  
  var1 <- variables[1]
  var2 <- variables[2]
  
  contingency_table <- table(df[[var1]], df[[var2]])
  chi_result <- chisq.test(contingency_table)
  
  return(list(
    method = "Chi-squared",
    chi_result = chi_result,
    contingency_table = contingency_table,
    var1 = var1,
    var2 = var2
  ))
}

execute_correlation_analysis <- function(df, variables) {
  
  numeric_vars <- names(select_if(df |> select(all_of(variables)), is.numeric))
  
  if (length(numeric_vars) < 2) {
    return(list(error = "Need at least 2 numeric variables"))
  }
  
  df_numeric <- df |> select(all_of(numeric_vars))
  
  cor_matrix <- cor(df_numeric, use = "complete.obs")
  
  return(list(
    method = "Correlation",
    correlation_matrix = cor_matrix,
    variables = numeric_vars
  ))
}

execute_descriptive_analysis <- function(df, variables) {
  
  summary_stats <- df |>
    select(all_of(variables)) |>
    summarise(across(
      everything(),
      list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        n = ~sum(!is.na(.))
      ),
      .names = "{.col}_{.fn}"
    ))
  
  return(list(
    method = "Descriptive",
    summary = summary_stats,
    n_rows = nrow(df),
    n_cols = length(variables)
  ))
}

# ============================================================================
# 7. GENERATE INTERPRETATION - Interpréter Résultats
# ============================================================================

generate_interpretation <- function(analysis_result) {
  
  # Construire prompt pour Gemini
  result_summary <- summarize_analysis_result(analysis_result)
  
  interpretation_prompt <- sprintf(
    "Interpréte les résultats d'analyse statistique suivante:\n\nMéthode: %s\n\nRésumé:\n%s\n\nDonne une interprétation claire et pédagogue adaptée à des professionnels de santé.",
    analysis_result$method,
    result_summary
  )
  
  interpretation <- tryCatch({
    call_gemini_api(
      prompt = interpretation_prompt,
      max_tokens = 1500,
      temperature = 0.7
    )
  }, error = function(e) {
    paste("⚠️  Erreur interprétation:", conditionMessage(e))
  })
  
  return(interpretation)
}

summarize_analysis_result <- function(result) {
  
  summary <- switch(
    result$method,
    PCA = sprintf(
      "ACP avec %d variables. Variance expliquée par PC1-PC3: %.1f%%, %.1f%%, %.1f%%",
      nrow(result$loadings),
      result$explained_var[1, 2],
      result$explained_var[2, 2],
      result$explained_var[3, 2]
    ),
    "K-means" = sprintf(
      "Clustering K-means avec k=%d. Tailles clusters: %s",
      result$optimal_k,
      paste(result$cluster_sizes, collapse = ", ")
    ),
    "Chi-squared" = sprintf(
      "Chi-squared test entre %s et %s. χ² = %.2f, p-value = %.4f",
      result$var1,
      result$var2,
      result$chi_result$statistic,
      result$chi_result$p.value
    ),
    "Correlation" = sprintf(
      "Matrice corrélation pour %d variables numériques",
      length(result$variables)
    ),
    "Analyse complétée avec succès"
  )
  
  return(summary)
}

# ============================================================================
# 8. SUGGEST VISUALIZATIONS
# ============================================================================

suggest_visualizations <- function(analysis_result) {
  
  suggestions <- switch(
    analysis_result$method,
    
    PCA = c(
      "Biplot (variables + individuals)",
      "Scree plot (variance expliquée)",
      "Heatmap correlations composantes principales"
    ),
    
    MCA = c(
      "Biplot catégories + individus",
      "Heatmap contributions variables"
    ),
    
    "K-means" = c(
      "Clusters dans PC1-PC2",
      "Elbow plot",
      "Silhouette plot"
    ),
    
    "Correlation" = c(
      "Heatmap corrélations",
      "Network plot associations"
    ),
    
    "Scatter plots by group",
    
    c()
  )
  
  return(suggestions)
}

# ============================================================================
# 9. HELPER FUNCTIONS
# ============================================================================

detect_analysis_type <- function(user_request) {
  
  request_lower <- tolower(user_request)
  
  if (grepl("ACP|PCA|principal|component", request_lower)) {
    return("dimension_reduction")
  } else if (grepl("ACM|MCA|correspondance", request_lower)) {
    return("dimension_reduction")
  } else if (grepl("FAMD|mixed", request_lower)) {
    return("dimension_reduction")
  } else if (grepl("cluster|group|partition", request_lower)) {
    return("clustering")
  } else if (grepl("test|hypothèse|significat|p-value", request_lower)) {
    return("hypothesis_test")
  } else if (grepl("comparaison|difference|anova|t-test", request_lower)) {
    return("hypothesis_test")
  } else if (grepl("correlation|association", request_lower)) {
    return("correlation")
  } else {
    return("exploratory")
  }
}

extract_variables_from_request <- function(user_request, df) {
  
  # Chercher mentions de variables
  df_names <- names(df)
  mentioned <- c()
  
  for (var in df_names) {
    if (grepl(var, user_request, ignore.case = TRUE)) {
      mentioned <- c(mentioned, var)
    }
  }
  
  # Si aucune variable mentionnée, prendre tous numériques
  if (length(mentioned) == 0) {
    mentioned <- names(select_if(df, is.numeric))
  }
  
  return(mentioned)
}

# ============================================================================
# 10. EXPORT
# ============================================================================

cat("✅ Analysis Agent v3 loaded\n")
