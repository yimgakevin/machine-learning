# ============================================================================
# Diagnose Agent v3 - Intelligent Error Detection & Auto-Correction
# ============================================================================
# Détecte erreurs utilisateur intelligemment
# Explique cause racine pédagogiquement
# Propose 2-3 solutions (simple → expert)
# Apprend des erreurs pour améliorer futurs prompts
# ============================================================================

library(tidyverse)

# Note: dépendances sourcées par app_v2_final.R. Ne pas re-sourcer ici.

# ============================================================================
# 1. ERROR CATALOG - Catalogue Erreurs Connues
# ============================================================================

ERROR_CATALOG <- list(
  
  incompatible_variable_types = list(
    description = "Variables type incompatible pour méthode",
    examples = "ACP avec variables catégories, t-test avec 1 seul groupe",
    detection = function(error_msg) {
      grepl("numeric|categorical|compatible|type", error_msg, ignore.case = TRUE)
    },
    severity = "HIGH",
    solutions = list(
      simple = "Sélectionner variables appropriées au type d'analyse",
      intermediate = "Transformer variables (encoding catégories, discrétisation numériques)",
      expert = "Utiliser méthode mixte (FAMD) pour variables hétérogènes"
    )
  ),
  
  missing_required_variables = list(
    description = "Variables requises manquantes",
    examples = "Comparaison groupes sans variable groupement",
    detection = function(error_msg) {
      grepl("missing|required|need|empty|select", error_msg, ignore.case = TRUE)
    },
    severity = "HIGH",
    solutions = list(
      simple = "Vérifier variables disponibles dans dataset",
      intermediate = "Importer données complètes ou imputer manquants",
      expert = "Analyse sensibilité avec différentes imputations"
    )
  ),
  
  insufficient_sample_size = list(
    description = "Échantillon trop petit pour analyse",
    examples = "N < 30 pour test paramétrique, k-means avec < 10 points",
    detection = function(error_msg) {
      grepl("n|sample|size|small|insufficient", error_msg, ignore.case = TRUE)
    },
    severity = "MEDIUM",
    solutions = list(
      simple = "Regrouper catégories ou utiliser test non-paramétrique",
      intermediate = "Augmenter puissance avec plus données ou réduction dimension",
      expert = "Bayesian approach avec prior informatifs"
    )
  ),
  
  violated_statistical_assumptions = list(
    description = "Hypothèses statistiques violées",
    examples = "Normalité échouée pour t-test, effectifs < 5 pour chi²",
    detection = function(error_msg) {
      grepl("normal|assumption|violated|invalid|condition", error_msg, ignore.case = TRUE)
    },
    severity = "MEDIUM",
    solutions = list(
      simple = "Utiliser équivalent non-paramétrique (Mann-Whitney, Kruskal-Wallis)",
      intermediate = "Transformer données (log, Box-Cox) puis réessayer",
      expert = "Bootstrap ou permutation tests"
    )
  ),
  
  missing_too_much_data = list(
    description = "Trop de données manquantes",
    examples = "> 30% manquants, colonne vide à 100%",
    detection = function(error_msg) {
      grepl("missing|na|empty|null|pct", error_msg, ignore.case = TRUE)
    },
    severity = "MEDIUM",
    solutions = list(
      simple = "Exclure colonne ou observations avec trop manquants",
      intermediate = "Imputation KNN ou régression",
      expert = "Analyse sensibilité avec bornage manquants"
    )
  ),
  
  illogical_parameter_values = list(
    description = "Paramètres analytiques illogiques",
    examples = "k > n observations, max_iter = 0, température hors [0,1]",
    detection = function(error_msg) {
      grepl("parameter|invalid|out of range|bound", error_msg, ignore.case = TRUE)
    },
    severity = "LOW",
    solutions = list(
      simple = "Utiliser valeurs par défaut recommandées",
      intermediate = "Valider paramètres vs données (k < min(n, 10) pour kmeans)",
      expert = "Grid search ou optimisation bayésienne"
    )
  ),
  
  inconsistent_results = list(
    description = "Résultats logiquement incohérents",
    examples = "Corrélation > 1, p-value > 1, IC ne contient pas estimateur",
    detection = function(error_msg) {
      grepl("inconsistent|impossible|incoherent|contradiction", error_msg, ignore.case = TRUE)
    },
    severity = "HIGH",
    solutions = list(
      simple = "Vérifier données source pour erreurs saisie",
      intermediate = "Nettoyer valeurs aberrantes, re-calculer",
      expert = "Debugger code analytique pour erreurs calcul"
    )
  ),
  
  unspecified_error = list(
    description = "Erreur générale non catégorisée",
    examples = "Erreurs runtime variées",
    detection = function(error_msg) TRUE,
    severity = "MEDIUM",
    solutions = list(
      simple = "Recommencer avec paramètres plus simples",
      intermediate = "Consulter messages erreur détaillés",
      expert = "Debugger code source pour trace erreur"
    )
  )
)

# ============================================================================
# 2. INITIALIZE DIAGNOSE AGENT
# ============================================================================

initialize_diagnose_agent <- function(user_context = NULL) {
  
  agent <- list(
    type = "diagnose",
    user_context = user_context,
    conversation = initialize_conversation(user_context),
    error_log = list(),
    errors_diagnosed = 0,
    auto_fixes_attempted = 0,
    auto_fixes_succeeded = 0,
    created = Sys.time(),
    status = "ready"
  )
  
  class(agent) <- c("diagnose_agent", "list")
  
  cat("✅ Diagnose Agent Initialized\n")
  
  return(agent)
}

# ============================================================================
# 3. DETECT USER ERROR
# ============================================================================

detect_user_error <- function(
  agent,
  error_message,
  user_action = NULL,
  context_data = NULL
) {
  
  error_info <- list(
    original_error = error_message,
    user_action = user_action,
    context = context_data,
    timestamp = Sys.time()
  )
  
  # Catégoriser erreur
  error_type <- categorize_error(error_message)
  error_info$type <- error_type
  error_info$catalog_entry <- ERROR_CATALOG[[error_type]]
  
  # Déterminer sévérité
  error_info$severity <- ERROR_CATALOG[[error_type]]$severity
  
  # Logger erreur
  agent$error_log <- c(agent$error_log, list(error_info))
  agent$errors_diagnosed <- agent$errors_diagnosed + 1
  
  return(list(
    error_info = error_info,
    agent = agent
  ))
}

categorize_error <- function(error_message) {
  
  error_lower <- tolower(error_message)
  
  # Chercher dans catalog erreurs
  for (error_type in names(ERROR_CATALOG)) {
    detector <- ERROR_CATALOG[[error_type]]$detection
    if (detector(error_message)) {
      return(error_type)
    }
  }
  
  return("unspecified_error")
}

# ============================================================================
# 4. EXPLAIN ROOT CAUSE - Expliquer Cause Racine Pédagogiquement
# ============================================================================

explain_root_cause <- function(
  agent,
  error_info,
  user_context = NULL
) {
  
  catalog_entry <- error_info$catalog_entry
  
  explanation_prompt <- sprintf(
    "Explique de manière pédagogue (pour professionnel de santé) pourquoi cette erreur est survenue:\n\nType erreur: %s\n\nMessage erreur: %s\n\nAction utilisateur: %s\n\nSois simple, éducatif. Explique:\n1. Qu'est-ce qui s'est passé\n2. Pourquoi c'est une erreur\n3. Comment l'éviter à l'avenir",
    error_info$type,
    error_info$original_error,
    error_info$user_action %||% "action non spécifiée"
  )
  
  explanation <- tryCatch({
    call_gemini_api(
      prompt = explanation_prompt,
      max_tokens = 1000,
      temperature = 0.7
    )
  }, error = function(e) {
    paste(
      "## Cause Racine",
      sprintf("Erreur: %s", error_info$type),
      sprintf("Message: %s", error_info$original_error),
      sep = "\n\n"
    )
  })
  
  return(explanation)
}

# ============================================================================
# 5. SUGGEST SOLUTIONS - Proposer 2-3 Solutions
# ============================================================================

suggest_solutions <- function(
  agent,
  error_info,
  context_data = NULL
) {
  
  catalog_entry <- error_info$catalog_entry
  solutions <- catalog_entry$solutions
  
  solutions_text <- sprintf(
    "## Solutions Proposées pour: %s\n\n",
    error_info$type
  )
  
  solutions_text <- paste0(
    solutions_text,
    "### 1. Solution Simple (Recommandée pour débuter)\n",
    solutions$simple,
    "\n\n",
    "### 2. Solution Intermédiaire (Niveau Professionnel)\n",
    solutions$intermediate,
    "\n\n",
    "### 3. Solution Avancée (Niveau Expert/Recherche)\n",
    solutions$expert
  )
  
  # Enrichir avec suggestions Gemini
  enriched <- enrich_solutions_with_gemini(
    solutions_text,
    error_info,
    context_data
  )
  
  return(enriched)
}

enrich_solutions_with_gemini <- function(
  base_solutions,
  error_info,
  context_data
) {
  
  enrichment_prompt <- sprintf(
    "Enrichis ces solutions avec exemples concrets pour données numérique/épidémiologiques:\n\n%s\n\nDonne exemples R si applicable.",
    base_solutions
  )
  
  enriched <- tryCatch({
    call_gemini_api(
      prompt = enrichment_prompt,
      max_tokens = 1200,
      temperature = 0.7
    )
  }, error = function(e) {
    base_solutions
  })
  
  return(enriched)
}

# ============================================================================
# 6. AUTO-CORRECT - Correction Automatique Avec Approbation
# ============================================================================

attempt_auto_correction <- function(
  agent,
  error_info,
  data = NULL,
  parameters = NULL
) {
  
  auto_fix_result <- list(
    attempted = FALSE,
    success = FALSE,
    method = NULL,
    result = NULL,
    user_approval = "pending"  # pending|approved|rejected
  )
  
  error_type <- error_info$type
  
  # Déterminer si correction auto possible
  correction_possible <- error_type %in% c(
    "illogical_parameter_values",
    "missing_required_variables",
    "insufficient_sample_size"
  )
  
  if (!correction_possible) {
    return(auto_fix_result)
  }
  
  auto_fix_result$attempted <- TRUE
  
  # Essayer corrections
  if (error_type == "illogical_parameter_values") {
    auto_fix_result$method <- "apply_defaults"
    auto_fix_result$result <- list(
      message = "Paramètres illogiques remplacés par valeurs par défaut",
      auto_corrected = TRUE
    )
    
  } else if (error_type == "missing_required_variables") {
    auto_fix_result$method <- "auto_select"
    auto_fix_result$result <- list(
      message = "Variables auto-sélectionnées basées sur disponibilité",
      variables_selected = names(data)[1:3]
    )
    
  } else if (error_type == "insufficient_sample_size") {
    auto_fix_result$method <- "aggregate_categories"
    auto_fix_result$result <- list(
      message = "Catégories agrégées pour augmenter taille échantillon",
      aggregated = TRUE
    )
  }
  
  auto_fix_result$success <- !is.null(auto_fix_result$result)
  
  agent$auto_fixes_attempted <- agent$auto_fixes_attempted + 1
  if (auto_fix_result$success) {
    agent$auto_fixes_succeeded <- agent$auto_fixes_succeeded + 1
  }
  
  return(list(
    auto_fix = auto_fix_result,
    agent = agent
  ))
}

# ============================================================================
# 7. FULL DIAGNOSIS & SOLUTIONS WORKFLOW
# ============================================================================

run_full_diagnosis <- function(
  agent,
  error_message,
  user_action = NULL,
  context_data = NULL
) {
  
  # Étape 1: Détecter erreur
  detection <- detect_user_error(
    agent,
    error_message,
    user_action,
    context_data
  )
  agent <- detection$agent
  error_info <- detection$error_info
  
  # Étape 2: Expliquer cause racine
  cause_explanation <- explain_root_cause(agent, error_info)
  
  # Étape 3: Proposer solutions
  solutions <- suggest_solutions(agent, error_info, context_data)
  
  # Étape 4: Tenter auto-correction
  auto_correction <- attempt_auto_correction(
    agent,
    error_info,
    context_data
  )
  agent <- auto_correction$agent
  auto_fix <- auto_correction$auto_fix
  
  # Étape 5: Proposer ressources éducatives
  education <- provide_education(error_info)
  
  # Assembler diagnostic complet
  diagnosis <- list(
    error_type = error_info$type,
    severity = error_info$severity,
    root_cause = cause_explanation,
    solutions = solutions,
    auto_fix = auto_fix,
    education = education,
    timestamp = Sys.time()
  )
  
  return(list(
    diagnosis = diagnosis,
    agent = agent
  ))
}

# ============================================================================
# 8. PROVIDE EDUCATION - Ressources Apprentissage
# ============================================================================

provide_education <- function(error_info) {
  
  error_type <- error_info$type
  
  education <- list(
    concept = error_info$catalog_entry$description,
    learning_path = switch(
      error_type,
      
      incompatible_variable_types = c(
        "1. Comprendre types variables (numérique vs catégorique)",
        "2. Connaître exigences chaque méthode",
        "3. Apprendre transformations variables"
      ),
      
      violated_statistical_assumptions = c(
        "1. Revoir hypothèses statistiques de base",
        "2. Tester normalité et homogénéité variance",
        "3. Apprendre tests non-paramétriques"
      ),
      
      missing_too_much_data = c(
        "1. Analyse manquants (MCAR, MAR, MNAR)",
        "2. Imputation simple vs avancée",
        "3. Analyse sensibilité manquants"
      ),
      
      c("1. Revoir concepts statistiques foundamentaux")
    ),
    
    resources = c(
      "📖 OMS Guidelines: https://www.who.int/",
      "📚 SMART Handbook: https://smartmethodology.org/",
      "🎓 Statistique pour santé: Voir R packages 'gtsummary', 'tidymodels'"
    )
  )
  
  return(education)
}

# ============================================================================
# 9. LEARNING FROM ERRORS - Apprendre des Erreurs
# ============================================================================

extract_learning_patterns <- function(agent) {
  
  if (length(agent$error_log) == 0) {
    return(list(
      total_errors = 0,
      patterns = NULL
    ))
  }
  
  error_types <- map_chr(agent$error_log, ~.x$type)
  error_freq <- table(error_types)
  
  learning <- list(
    total_errors = length(agent$error_log),
    most_common = names(error_freq)[which.max(error_freq)],
    error_distribution = as.list(error_freq),
    success_rate = round(agent$auto_fixes_succeeded / 
                        max(agent$auto_fixes_attempted, 1) * 100, 1)
  )
  
  return(learning)
}

# ============================================================================
# 10. PRINT DIAGNOSIS REPORT
# ============================================================================

print_diagnosis_report <- function(diagnosis) {
  
  cat("\n")
  cat("╔═════════════════════════════════════════════════╗\n")
  cat("║  🔍 DIAGNOSTIC DÉTAILLÉ                        ║\n")
  cat("╠═════════════════════════════════════════════════╣\n")
  cat("│ Type Erreur:  ", diagnosis$error_type, "\n")
  cat("│ Sévérité:     ", diagnosis$severity, "\n")
  cat("├─────────────────────────────────────────────────┤\n")
  cat("│ CAUSE RACINE:\n│\n")
  cat(paste0("│ ", strsplit(diagnosis$root_cause, "\n")[[1]], collapse = "\n│ "), "\n")
  cat("├─────────────────────────────────────────────────┤\n")
  cat("│ SOLUTIONS:\n│\n")
  cat(paste0("│ ", strsplit(diagnosis$solutions, "\n")[[1]], collapse = "\n│ "), "\n")
  cat("╚═════════════════════════════════════════════════╝\n")
}

# ============================================================================
# 11. EXPORT
# ============================================================================

cat("✅ Diagnose Agent v3 loaded\n")
