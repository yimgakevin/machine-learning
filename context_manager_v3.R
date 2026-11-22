# ============================================================================
# Context Manager v3 - Capture Contexte Enrichi pour Agents IA
# ============================================================================
# Extrait et structure informations utilisateur/données/analyses pour
# fourni contexte aux agents IA (Chat, Analysis, Report, Diagnose)
# ============================================================================

library(tidyverse)
library(stringi)

# ============================================================================
# 1. CONTEXTE UTILISATEUR
# ============================================================================

create_user_context <- function() {
  
  user_context <- list(
    # Langue détectée
    language = detect_user_language(),
    
    # Localisation géographique
    country = NULL,
    continent = NULL,
    
    # Profil expertise
    expertise_level = "intermediate",  # novice|intermediate|expert
    
    # Domaine spécialisation
    specialization = "nutrition",  # nutrition|epidemiology|food_security|other
    
    # Historique session
    session_start = Sys.time(),
    messages_count = 0,
    
    # Préférences
    report_style = "technical",  # executive|technical|field|student
    export_format = "html",  # html|docx|pdf
    
    # Metadata
    timezone = Sys.timezone(),
    device_type = "desktop"
  )
  
  return(user_context)
}

# ============================================================================
# 2. CONTEXTE DONNÉES
# ============================================================================

capture_data_context <- function(df) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  
  # Statistiques basiques
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  # Types colonnes
  col_types <- map_chr(df, ~class(.x)[1])
  n_numeric <- sum(col_types %in% c("numeric", "integer"))
  n_categorical <- sum(col_types %in% c("character", "factor"))
  
  # Valeurs manquantes
  na_summary <- colSums(is.na(df))
  pct_missing <- round(mean(na_summary) / n_rows * 100, 2)
  
  # Aperçu colonnes
  column_info <- tibble(
    name = names(df),
    type = col_types,
    n_unique = map_int(df, ~n_distinct(.x, na.rm = TRUE)),
    n_missing = na_summary,
    sample_values = map_chr(df, ~paste(na.omit(.x)[1:3], collapse = ", "))
  )
  
  # Déterminer si données SMART/OMS
  smart_indicators <- detect_smart_indicators(df)
  oms_indicators <- detect_oms_indicators(df)
  
  data_context <- list(
    dimensions = list(rows = n_rows, cols = n_cols),
    columns_info = column_info,
    data_quality = list(
      missing_percent = pct_missing,
      duplicates = sum(duplicated(df)),
      outliers_percent = round(detect_outliers(df) / n_rows * 100, 2)
    ),
    variable_types = list(
      numeric = n_numeric,
      categorical = n_categorical
    ),
    detected_indicators = list(
      smart = smart_indicators,
      oms = oms_indicators
    ),
    suggestions = generate_data_suggestions(df, smart_indicators, oms_indicators)
  )
  
  return(data_context)
}

# ============================================================================
# 3. CONTEXTE ANALYSES
# ============================================================================

capture_analysis_context <- function(analysis_history = NULL) {
  
  if (is.null(analysis_history)) {
    analysis_history <- list()
  }
  
  # Dernières analyses exécutées
  recent_analyses <- tail(analysis_history, 5)
  
  # Patterns analyses
  analysis_context <- list(
    recent_analyses = recent_analyses,
    analysis_count = length(analysis_history),
    
    # Méthodes utilisées
    methods_used = extract_methods(analysis_history),
    
    # Variables souvent analysées
    frequently_used_vars = extract_frequent_vars(analysis_history),
    
    # Erreurs rencontrées
    errors_encountered = extract_errors(analysis_history),
    
    # Progression utilisateur
    progression = calculate_progression(analysis_history)
  )
  
  return(analysis_context)
}

# ============================================================================
# 4. BUILD AGENT PROMPT - Construire Prompt Contextuel
# ============================================================================

build_agent_prompt <- function(
  base_prompt,
  user_context = NULL,
  data_context = NULL,
  analysis_context = NULL,
  agent_type = "chat"  # chat|analysis|report|diagnose
) {
  
  prompt_parts <- list(base_prompt)
  
  # Ajouter contexte utilisateur
  if (!is.null(user_context)) {
    user_section <- sprintf(
      "\n\n📊 USER CONTEXT:\n- Language: %s\n- Expertise: %s\n- Specialization: %s",
      user_context$language,
      user_context$expertise_level,
      user_context$specialization
    )
    prompt_parts <- c(prompt_parts, user_section)
  }
  
  # Ajouter contexte données
  if (!is.null(data_context)) {
    data_section <- sprintf(
      "\n\n📈 DATA CONTEXT:\n- Size: %d rows × %d cols\n- Missing: %.1f%%\n- Detected: %s indicators",
      data_context$dimensions$rows,
      data_context$dimensions$cols,
      data_context$data_quality$missing_percent,
      paste(c(
        if (length(data_context$detected_indicators$smart) > 0) "SMART",
        if (length(data_context$detected_indicators$oms) > 0) "OMS"
      ), collapse = ", ")
    )
    prompt_parts <- c(prompt_parts, data_section)
  }
  
  # Ajouter contexte analyses
  if (!is.null(analysis_context)) {
    analysis_section <- sprintf(
      "\n\n🔬 ANALYSIS CONTEXT:\n- Previous analyses: %d\n- Methods: %s\n- Progression: %s",
      analysis_context$analysis_count,
      paste(analysis_context$methods_used, collapse = ", "),
      analysis_context$progression
    )
    prompt_parts <- c(prompt_parts, analysis_section)
  }
  
  # Ajouter instructions spécifiques agent
  agent_instructions <- get_agent_instructions(agent_type, user_context)
  prompt_parts <- c(prompt_parts, agent_instructions)
  
  # Joindre tous les parts
  final_prompt <- paste(prompt_parts, collapse = "\n")
  
  return(final_prompt)
}

# ============================================================================
# 5. TRACK CONVERSATION - Maintenir État Conversation
# ============================================================================

initialize_conversation <- function(user_context = NULL) {
  
  conversation <- list(
    messages = list(),
    turns = 0,
    context = user_context %||% create_user_context(),
    data_context = NULL,
    analysis_context = NULL,
    start_time = Sys.time(),
    current_agent = NULL,
    conversation_state = "idle"  # idle|analyzing|reporting|diagnosing
  )
  
  class(conversation) <- c("conversation_v3", "list")
  
  return(conversation)
}

add_message_to_conversation <- function(conversation, role, content, agent = NULL) {
  
  message <- list(
    role = role,  # user|assistant
    content = content,
    agent = agent,
    timestamp = Sys.time(),
    tokens_estimate = estimate_tokens(content)
  )
  
  conversation$messages <- c(conversation$messages, list(message))
  conversation$turns <- conversation$turns + 1
  if (!is.null(agent)) conversation$current_agent <- agent
  
  return(conversation)
}

get_conversation_summary <- function(conversation, max_messages = 5) {
  
  if (length(conversation$messages) == 0) {
    return(NULL)
  }
  
  # Prendre derniers messages
  recent_msgs <- tail(conversation$messages, max_messages)
  
  summary <- tibble(
    turn = seq_along(recent_msgs),
    role = map_chr(recent_msgs, ~.x$role),
    agent = map_chr(recent_msgs, ~.x$agent %||% "none"),
    preview = map_chr(recent_msgs, ~substr(.x$content, 1, 100)),
    timestamp = map(recent_msgs, ~.x$timestamp)
  )
  
  return(summary)
}

# ============================================================================
# 6. FONCTIONS UTILITAIRES DÉTECTION
# ============================================================================

detect_user_language <- function() {
  # Détection simple basée sur système d'exploitation
  lang_code <- Sys.getenv("LANG", "en_US.UTF-8")
  
  if (grepl("fr_FR|fr_FR.UTF-8", lang_code)) {
    return("french")
  } else if (grepl("es_ES|es_ES.UTF-8", lang_code)) {
    return("spanish")
  } else {
    return("english")
  }
}

detect_smart_indicators <- function(df) {
  
  smart_cols <- c("age_months", "height", "weight", "muac", "oedema",
                  "diarrhea_days", "sex", "vaccination_card")
  
  detected <- intersect(tolower(names(df)), tolower(smart_cols))
  return(detected)
}

detect_oms_indicators <- function(df) {
  
  oms_patterns <- c(
    "ha/" = "height_for_age",
    "wa/" = "weight_for_age",
    "wh/" = "weight_for_height",
    "z.score" = "zscore",
    "stunting|stunted" = "stunting",
    "wasting|wasted" = "wasting",
    "underweight" = "underweight"
  )
  
  detected <- c()
  for (pattern in names(oms_patterns)) {
    if (any(grepl(pattern, tolower(names(df))))) {
      detected <- c(detected, oms_patterns[pattern])
    }
  }
  
  return(unique(detected))
}

detect_outliers <- function(df) {
  
  numeric_cols <- select_if(df, is.numeric)
  if (ncol(numeric_cols) == 0) return(0)
  
  outlier_count <- 0
  for (col in numeric_cols) {
    Q1 <- quantile(col, 0.25, na.rm = TRUE)
    Q3 <- quantile(col, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    outliers <- col < (Q1 - 1.5*IQR) | col > (Q3 + 1.5*IQR)
    outlier_count <- outlier_count + sum(outliers, na.rm = TRUE)
  }
  
  return(outlier_count)
}

generate_data_suggestions <- function(df, smart_indicators, oms_indicators) {
  
  suggestions <- list()
  
  # Si données SMART détectées
  if (length(smart_indicators) > 0) {
    suggestions$analysis_ideas <- c(
      suggestions$analysis_ideas,
      "Calcul Z-scores OMS 2006",
      "Analyse prévalences malnutrition",
      "Contrôle qualité données SMART"
    )
  }
  
  # Si beaucoup de manquants
  if (mean(colSums(is.na(df)) / nrow(df) > 0.2)) {
    suggestions$data_quality_alert <- "⚠️ Plus de 20% données manquantes - considérer imputation"
  }
  
  # Suggestions analyses multivariées
  numeric_cols <- select_if(df, is.numeric)
  if (ncol(numeric_cols) >= 3) {
    suggestions$multivariate_ideas <- c(
      "ACP (Analyse en Composantes Principales)",
      "Clustering K-means",
      "Analyse corrélations"
    )
  }
  
  return(suggestions)
}

extract_methods <- function(analysis_history) {
  
  if (is.null(analysis_history) || length(analysis_history) == 0) {
    return(character(0))
  }
  
  methods <- map_chr(analysis_history, ~.x$method %||% "unknown")
  return(unique(methods))
}

extract_frequent_vars <- function(analysis_history) {
  
  if (is.null(analysis_history) || length(analysis_history) == 0) {
    return(character(0))
  }
  
  all_vars <- c()
  for (analysis in analysis_history) {
    all_vars <- c(all_vars, analysis$variables %||% character(0))
  }
  
  freq_table <- table(all_vars)
  return(names(sort(freq_table, decreasing = TRUE))[1:3])
}

extract_errors <- function(analysis_history) {
  
  if (is.null(analysis_history) || length(analysis_history) == 0) {
    return(character(0))
  }
  
  errors <- c()
  for (analysis in analysis_history) {
    if (!is.null(analysis$error)) {
      errors <- c(errors, analysis$error)
    }
  }
  
  return(unique(errors))
}

calculate_progression <- function(analysis_history) {
  
  if (is.null(analysis_history) || length(analysis_history) == 0) {
    return("beginner")
  }
  
  # Progression simple basée sur nombre analyses
  n <- length(analysis_history)
  
  if (n < 3) {
    return("beginner")
  } else if (n < 10) {
    return("intermediate")
  } else {
    return("advanced")
  }
}

get_agent_instructions <- function(agent_type, user_context) {
  
  instructions <- switch(
    agent_type,
    
    chat = sprintf(
      "\n\n🤖 AGENT INSTRUCTIONS (Chat):\n- Répondre en %s\n- Niveau expertise: %s\n- Focus: %s\n- Être pédagogue et clair",
      user_context$language %||% "english",
      user_context$expertise_level %||% "intermediate",
      user_context$specialization %||% "nutrition"
    ),
    
    analysis = "\n\n🤖 AGENT INSTRUCTIONS (Analysis):\n- Analyser données autonomement\n- Sélectionner meilleures méthodes\n- Valider hypothèses avant exécution\n- Interpréter résultats clairement",
    
    report = sprintf(
      "\n\n🤖 AGENT INSTRUCTIONS (Report):\n- Générer rapport style: %s\n- Format export: %s\n- Structurer: Context → Methods → Results → Discussion\n- Publication-ready",
      user_context$report_style %||% "technical",
      user_context$export_format %||% "html"
    ),
    
    diagnose = "\n\n🤖 AGENT INSTRUCTIONS (Diagnose):\n- Détecter erreurs utilisateur\n- Expliquer cause racine\n- Proposer 2-3 solutions\n- Éduquer pédagogiquement"
  )
  
  return(instructions)
}

estimate_tokens <- function(text) {
  # Estimation simple: ~4 chars = 1 token (Gemini)
  round(nchar(text) / 4)
}

# ============================================================================
# 7. EXPORT
# ============================================================================

# Initialiser contexte global si chargement du module
if (!exists("CONTEXT_MANAGER_V3", envir = .GlobalEnv)) {
  CONTEXT_MANAGER_V3 <- list(
    version = "3.0",
    initialized = TRUE,
    current_user = create_user_context(),
    current_conversation = initialize_conversation()
  )
}

cat("✅ Context Manager v3 loaded\n")
