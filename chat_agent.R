# ============================================================================
# Chat Agent v3 - Expert Support & Q&A (Nutrition, Épidémiologie, Sécurité Alimentaire)
# ============================================================================
# Agent intelligent pour répondre questions utilisateurs
# Spécialisé: Nutrition, Épidémiologie, Sécurité Alimentaire, Statistiques
# Multilingue: FR/EN/ES (auto-détecté)
# ============================================================================

library(tidyverse)

# Note: dépendances (config_api_secure_v2, gemini_api_v3, context_manager_v3,
# config_oms) sont sourcées par app_v2_final.R. Ne pas re-sourcer ici.

# ============================================================================
# 1. SYSTEM PROMPTS PAR DOMAINE
# ============================================================================

CHAT_SYSTEM_PROMPTS <- list(
  
  nutrition = "Tu es un expert en nutrition et malnutrition. Tu aides des professionnels de santé (médecins, nutritionnistes, agents terrain) à comprendre:
- Malnutrition aigüe/chronique (MAG/MAC)
- Micronutriments (fer, zinc, vitamine A, B12)
- MUAC (Middle Upper Arm Circumference)
- Indice de Masse Corporelle (IMC)
- Recommandations alimentaires
- Protocoles traitement malnutrition

Réponds de manière:
✓ Pédagogue et claire (explique concepts)
✓ Basée preuves (cite standards OMS/UNICEF)
✓ Contextualisée (adapte au contexte données si disponible)
✓ Pratique (donne exemples concrets)

Langue: Réponds dans la même langue que la question.",
  
  epidemiology = "Tu es un expert en épidémiologie et études SMART. Tu aides à comprendre:
- Indicateurs SMART (OMS standards)
- Designs études (transversales, cohortes, cas-témoins)
- Mesures association (RR, OR, IC95%)
- Biais et confounding
- Analyses stratifiées
- Tests d'hypothèse
- Puissance statistique

Réponds de manière:
✓ Scientifique (notation correcte)
✓ Accessible (pas de jargon inutile)
✓ Méthodique (décompose problèmes)
✓ Critique (signale limitations)

Langue: Réponds dans la même langue que la question.",
  
  food_security = "Tu es un expert en sécurité alimentaire. Tu connais:
- FCS (Food Consumption Score)
- RCSI (Reduced Coping Strategies Index)
- DiCES (Household Dietary Diversity Score)
- PAM/WFP standards
- Analyse vulnérabilité alimentaire
- Indicateurs chroniques/aigus

Réponds de manière:
✓ Juste (respecte méthodologie)
✓ Adaptée (tient compte contexte local)
✓ Actionnable (propositions interventions)
✓ Holistique (voir système alimentaire)

Langue: Réponds dans la même langue que la question.",
  
  statistics = "Tu es un expert en statistique et data science. Tu aides avec:
- Analyses descriptives (moyennes, distributions)
- Tests d'hypothèse (t-test, chi-squared, ANOVA)
- Régressions (linéaire, logistique, multiple)
- Réduction dimension (ACP, ACM, FAMD)
- Clustering (K-means, hiérarchique)
- Validation modèles (cross-validation, résidus)
- Visualisations appropriées

Réponds de manière:
✓ Rigoureuse (respecte hypothèses)
✓ Explicite (montre étapes)
✓ Critique (signale problèmes possibles)
✓ Visuelle (propose graphiques)

Langue: Réponds dans la même langue que la question."
)

# ============================================================================
# 2. INITIALIZE CHAT AGENT
# ============================================================================

initialize_chat_agent <- function(
  specialization = "nutrition",
  language = "french",
  user_context = NULL
) {
  
  agent <- list(
    type = "chat",
    specialization = specialization,
    language = language,
    system_prompt = CHAT_SYSTEM_PROMPTS[[specialization]],
    user_context = user_context,
    conversation = initialize_conversation(user_context),
    knowledge_base = build_knowledge_base(specialization),
    created = Sys.time(),
    messages_processed = 0,
    status = "ready"
  )
  
  class(agent) <- c("chat_agent", "list")
  
  cat("✅ Chat Agent Initialized\n")
  cat("   Specialization:", specialization, "\n")
  cat("   Language:", language, "\n")
  
  return(agent)
}

# ============================================================================
# 3. PROCESS USER QUESTION
# ============================================================================

process_user_question <- function(
  agent,
  user_question,
  data_context = NULL,
  auto_detect_domain = TRUE
) {
  
  # Détection langue si pas spécifiée
  question_language <- detect_question_language(user_question)
  
  # Auto-détection domaine si demandé
  if (auto_detect_domain) {
    best_domain <- auto_detect_domain_from_question(user_question)
    if (!is.null(best_domain) && best_domain != agent$specialization) {
      agent$specialization <- best_domain
      agent$system_prompt <- CHAT_SYSTEM_PROMPTS[[best_domain]]
    }
  }
  
  # Ajouter message à conversation
  agent$conversation <- add_message_to_conversation(
    agent$conversation,
    role = "user",
    content = user_question,
    agent = "chat"
  )
  
  # Construire prompt contextuel
  context_prompt <- build_agent_prompt(
    base_prompt = agent$system_prompt,
    user_context = agent$user_context,
    data_context = data_context,
    agent_type = "chat"
  )
  
  # Question avec contexte données si disponible
  full_prompt <- if (!is.null(data_context)) {
    sprintf(
      "%s\n\nCONTEXTE DONNÉES:\nDimensions: %d × %d\nVariables détectées: %s\n\nQUESTION UTILISATEUR:\n%s",
      context_prompt,
      data_context$dimensions$rows,
      data_context$dimensions$cols,
      paste(data_context$detected_indicators$smart, collapse = ", "),
      user_question
    )
  } else {
    sprintf("%s\n\nQUESTION:\n%s", context_prompt, user_question)
  }
  
  # Appel Gemini
  tryCatch({
    response <- call_gemini_api(
      prompt = full_prompt,
      config = GEMINI_CONFIG,
      max_tokens = 2000,
      temperature = 0.7,
      verbose = FALSE
    )
    
    # Enrichir réponse avec sources
    enriched_response <- enrich_response_with_sources(
      response,
      agent$specialization
    )
    
    # Ajouter réponse à conversation
    agent$conversation <- add_message_to_conversation(
      agent$conversation,
      role = "assistant",
      content = enriched_response,
      agent = "chat"
    )
    
    agent$messages_processed <- agent$messages_processed + 1
    
    return(list(
      success = TRUE,
      response = enriched_response,
      agent = agent
    ))
    
  }, error = function(e) {
    error_msg <- conditionMessage(e)
    return(list(
      success = FALSE,
      error = error_msg,
      agent = agent
    ))
  })
}

# ============================================================================
# 4. CONTEXTUALIZE ANSWER - Enrichir réponse avec contexte données
# ============================================================================

contextualize_answer_for_data <- function(
  agent,
  response,
  data_context
) {
  
  if (is.null(data_context)) {
    return(response)
  }
  
  # Ajouter insights spécifiques données
  insights <- generate_data_specific_insights(data_context)
  
  contextualized <- sprintf(
    "%s\n\n📊 **ADAPTÉ À VOS DONNÉES:**\n%s",
    response,
    paste(insights, collapse = "\n")
  )
  
  return(contextualized)
}

# ============================================================================
# 5. MULTILINGUAL SUPPORT
# ============================================================================

detect_question_language <- function(question_text) {
  
  # Patterns simples pour langues
  if (grepl("\\b(le|la|les|de|et|ou|qui|est|être)\\b", question_text, ignore.case = TRUE)) {
    return("french")
  } else if (grepl("\\b(el|la|los|de|y|que|ser|es)\\b", question_text, ignore.case = TRUE)) {
    return("spanish")
  } else {
    return("english")
  }
}

translate_prompt_to_language <- function(prompt, target_language) {
  
  # Demander à Gemini de traduire prompt system
  translation_request <- sprintf(
    "Translate this prompt to %s:\n\n%s",
    target_language,
    prompt
  )
  
  translated <- tryCatch({
    call_gemini_api(
      prompt = translation_request,
      max_tokens = 500,
      temperature = 0.3
    )
  }, error = function(e) {
    prompt  # Retourner original si erreur
  })
  
  return(translated)
}

# ============================================================================
# 6. EXPLAIN CONCEPTS - Explications Pédagogiques
# ============================================================================

explain_concept <- function(
  agent,
  concept,
  language = "french",
  detail_level = "intermediate"
) {
  
  detail_desc <- switch(detail_level,
    beginner = "pour un débutant, très simplement",
    intermediate = "niveau professionnel de santé",
    expert = "niveau recherche/publication"
  )
  
  explanation_prompt <- sprintf(
    "Explique le concept '%s' %s:\n\n1. Définition simple\n2. Contexte utilisation\n3. Exemple pratique\n4. Erreurs courantes à éviter\n\nLangue: %s",
    concept,
    detail_desc,
    language
  )
  
  response <- tryCatch({
    call_gemini_api(
      prompt = explanation_prompt,
      max_tokens = 1500,
      temperature = 0.7
    )
  }, error = function(e) {
    paste("❌ Erreur:", conditionMessage(e))
  })
  
  return(response)
}

# ============================================================================
# 7. REFERENCE OMS STANDARDS
# ============================================================================

reference_oms_standards <- function(topic, include_source = TRUE) {
  
  # Récupérer standards pertinents de config_oms.R
  standards <- switch(
    topic,
    malnutrition = list(
      title = "OMS Acute Malnutrition Standards",
      standard_muac_boys = "< 115 mm",
      standard_muac_girls = "< 115 mm",
      reference = "WHO 2009"
    ),
    zscore = list(
      title = "OMS Z-Score Reference (2006)",
      standard = "SD = Standard Deviations from median",
      thresholds = list(moderate = -2, severe = -3),
      reference = "WHO Child Growth Standards 2006"
    ),
    bmi = list(
      title = "OMS BMI Classification",
      underweight = "< 18.5",
      normal = "18.5-24.9",
      overweight = "25.0-29.9",
      obese = ">= 30.0",
      reference = "WHO 1995"
    )
  )
  
  formatted <- sprintf(
    "📋 %s\n%s\n\n%s",
    standards$title,
    paste(capture.output(str(standards)), collapse = "\n"),
    if (include_source) paste("Source:", standards$reference) else ""
  )
  
  return(formatted)
}

# ============================================================================
# 8. HELPER FUNCTIONS
# ============================================================================

build_knowledge_base <- function(specialization) {
  
  kb <- list(
    nutrition = list(
      key_terms = c("MAG", "MAC", "MUAC", "PEM", "micronutrient", "IMC"),
      organizations = c("OMS", "UNICEF", "PAM", "Save the Children"),
      standards = c("OMS Growth Standards", "SMART", "SPHERE")
    ),
    epidemiology = list(
      key_terms = c("RR", "OR", "IC95%", "p-value", "puissance", "biais"),
      designs = c("Transversale", "Cohorte", "Cas-témoins", "Essai randomisé"),
      standards = c("STROBE", "SMART", "Cochrane")
    ),
    food_security = list(
      key_terms = c("FCS", "RCSI", "DiCES", "HDDS", "CSI"),
      standards = c("PAM", "WFP", "FAO", "Sphere")
    )
  )
  
  return(kb[[specialization]] %||% kb$nutrition)
}

auto_detect_domain_from_question <- function(question) {
  
  nutrition_keywords <- c("malnutrition", "MUAC", "nutrition", "micronutrient", "MAG", "MAC", "allaitement", "alimentation")
  epidemio_keywords <- c("étude", "analyse", "statistique", "SMART", "prévalence", "significatif", "puissance", "biais")
  security_keywords <- c("sécurité alimentaire", "FCS", "RCSI", "DiCES", "faim", "accès")
  
  question_lower <- tolower(question)
  
  if (any(grepl(paste(nutrition_keywords, collapse = "|"), question_lower))) {
    return("nutrition")
  } else if (any(grepl(paste(epidemio_keywords, collapse = "|"), question_lower))) {
    return("epidemiology")
  } else if (any(grepl(paste(security_keywords, collapse = "|"), question_lower))) {
    return("food_security")
  } else {
    return(NULL)
  }
}

enrich_response_with_sources <- function(response, specialization) {
  
  sources <- switch(
    specialization,
    nutrition = "\n\n📚 **Sources Standards:**\n- OMS Standards de Croissance (2006)\n- Guidelines Nutrition OMS/UNICEF\n- Standards SMART",
    epidemiology = "\n\n📚 **Sources Standards:**\n- Guidelines SMART (OMS)\n- STROBE Guidelines\n- Cochrane Handbook",
    food_security = "\n\n📚 **Sources Standards:**\n- PAM/WFP Guidelines\n- Sphere Handbook\n- FAO Guidelines",
    "\n\n📚 **Pour en savoir plus:**\n- Consultez standards OMS\n- Voir documentations WFP/PAM"
  )
  
  enriched <- paste0(response, sources)
  return(enriched)
}

generate_data_specific_insights <- function(data_context) {
  
  insights <- c()
  
  # SMART indicators
  if (length(data_context$detected_indicators$smart) > 0) {
    insights <- c(insights,
      sprintf("✓ Données SMART détectées: %s", paste(data_context$detected_indicators$smart, collapse = ", ")),
      "→ Possibilité calcul Z-scores OMS et analyses prévalences"
    )
  }
  
  # Missing data
  if (data_context$data_quality$missing_percent > 10) {
    insights <- c(insights,
      sprintf("⚠️  %.1f%% données manquantes", data_context$data_quality$missing_percent),
      "→ Envisager analyse sensibilité et imputation"
    )
  }
  
  # Size
  if (data_context$dimensions$rows < 100) {
    insights <- c(insights,
      sprintf("📊 N = %d (petit échantillon)", data_context$dimensions$rows),
      "→ Prudence IC larges, puissance limitée"
    )
  }
  
  return(insights)
}

# ============================================================================
# 9. EXPORT
# ============================================================================

cat("✅ Chat Agent v3 loaded\n")
