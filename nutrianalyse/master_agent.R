# ============================================================================
# Master Agent v3 - Orchestrateur Multi-Agent Intelligent
# ============================================================================
# Route requêtes aux agents appropriés
# Coordonne analyses multi-agent
# Synthétise résultats
# Gère contexte & fallback strategies
# ============================================================================

library(tidyverse)

# Note: les dépendances (config_api_secure_v2, gemini_api_v3, context_manager_v3,
# chat_agent, analysis_agent, report_agent, diagnose_agent) sont sourcées
# par app_v2_final.R AVANT ce fichier. Ne pas re-sourcer ici pour éviter
# les conflits de définition de fonctions (call_gemini_api, gemini, etc.)

# ============================================================================
# 1. INITIALIZE MASTER AGENT
# ============================================================================

initialize_master_agent <- function(user_context = NULL) {
  
  # Initialiser tous les sub-agents
  agents <- list(
    chat = initialize_chat_agent(user_context = user_context),
    analysis = initialize_analysis_agent(user_context = user_context),
    report = initialize_report_agent(user_context = user_context),
    diagnose = initialize_diagnose_agent(user_context = user_context)
  )
  
  master <- list(
    type = "master",
    agents = agents,
    user_context = user_context,
    conversation = initialize_conversation(user_context),
    request_history = list(),
    workflow_state = "idle",  # idle|analyzing|reporting|diagnosing
    created = Sys.time(),
    requests_processed = 0,
    status = "ready"
  )
  
  class(master) <- c("master_agent", "list")
  
  cat("✅ Master Agent Initialized with 4 Sub-Agents\n")
  cat("   ├─ Chat Agent (Support & Q&A)\n")
  cat("   ├─ Analysis Agent (Analyses Autonomes)\n")
  cat("   ├─ Report Agent (Rapports Publication-Ready)\n")
  cat("   └─ Diagnose Agent (Diagnostic Erreurs)\n")
  
  return(master)
}

# ============================================================================
# 2. ROUTE REQUEST - Déterminer Agent(s) Approprié(s)
# ============================================================================

route_request <- function(request_text, master) {
  
  # Analyser demande avec Gemini pour meilleure compréhension
  routing_prompt <- sprintf(
    "Catégorise cette demande utilisateur parmi:\n1. CHAT (Questions, explications, support)\n2. ANALYSIS (Analyses données, statistiques, modèles)\n3. REPORT (Générer rapport, exporter résultats)\n4. DIAGNOSE (Erreur utilisateur, diagnostic)\n5. MULTI (Plusieurs de ces tâches)\n\nDemande: '%s'\n\nRéponds UNIQUEMENT: CHAT|ANALYSIS|REPORT|DIAGNOSE|MULTI",
    request_text
  )
  
  routing_decision <- tryCatch({
    call_gemini_api(
      prompt = routing_prompt,
      max_tokens = 50,
      temperature = 0.3
    )
  }, error = function(e) {
    auto_detect_agent_type(request_text)
  })
  
  # Parser réponse — prendre uniquement la première ligne (Gemini peut être verbeux)
  routing_decision <- strsplit(as.character(routing_decision), "\\n")[[1]][1]
  routing_decision <- toupper(trimws(routing_decision))
  # Extraire le premier mot-clé valide
  routing_decision <- regmatches(routing_decision, regexpr("CHAT|ANALYSIS|REPORT|DIAGNOSE|MULTI", routing_decision))
  if (length(routing_decision) == 0) routing_decision <- auto_detect_agent_type(request_text)
  
  routing <- list(
    requested = request_text,
    primary_agent = extract_primary_agent(routing_decision),
    secondary_agents = extract_secondary_agents(routing_decision),
    is_multi = grepl("MULTI", routing_decision),
    confidence = ifelse(routing_decision == "MULTI", 0.7, 0.9)
  )
  
  return(routing)
}

auto_detect_agent_type <- function(request_text) {
  
  request_lower <- tolower(request_text)
  
  if (grepl("error|problème|why|pourquoi|wrong|incorrect", request_lower)) {
    return("DIAGNOSE")
  } else if (grepl("report|export|generate|créer un|writing|rédiger|document", request_lower)) {
    return("REPORT")
  } else if (grepl("analys|test|acp|acm|regression|correlation|cluster|kmeans", request_lower)) {
    return("ANALYSIS")
  } else if (grepl("explain|comment|définit|what|qu'est|tell me|dis moi", request_lower)) {
    return("CHAT")
  } else {
    return("CHAT")  # Par défaut
  }
}

extract_primary_agent <- function(routing_decision) {
  
  agents <- strsplit(routing_decision, "\\|")[[1]]
  agents <- trimws(agents)
  
  if (length(agents) > 0) {
    return(tolower(agents[1]))
  }
  
  return("chat")
}

extract_secondary_agents <- function(routing_decision) {
  
  agents <- strsplit(routing_decision, "\\|")[[1]]
  agents <- trimws(agents)
  
  if (length(agents) > 1) {
    return(tolower(agents[2:length(agents)]))
  }
  
  return(character(0))
}

# ============================================================================
# 3. PROCESS REQUEST - Traiter Demande Multi-Agent
# ============================================================================

process_master_request <- function(
  master,
  user_request,
  df = NULL,
  data_context = NULL,
  analysis_results = NULL
) {
  
  tryCatch({
    
    # Étape 1: Router demande
    routing <- route_request(user_request, master)
    
    master$workflow_state <- "processing"
    
    # Étape 2: Exécuter agent primaire
    primary_response <- execute_agent(
      master,
      routing$primary_agent,
      user_request,
      df,
      data_context,
      analysis_results
    )
    
    responses <- list(
      primary = list(
        agent = routing$primary_agent,
        response = primary_response$response,
        success = primary_response$success
      )
    )
    
    # Étape 3: Exécuter agents secondaires si besoin
    if (length(routing$secondary_agents) > 0) {
      for (secondary_agent in routing$secondary_agents) {
        secondary_response <- execute_agent(
          master,
          secondary_agent,
          user_request,
          df,
          data_context,
          primary_response$result
        )
        
        responses[[secondary_agent]] <- list(
          agent = secondary_agent,
          response = secondary_response$response,
          success = secondary_response$success
        )
      }
    }
    
    # Étape 4: Synthétiser résultats
    synthesized <- synthesize_multi_agent_results(responses, routing)
    
    # Étape 5: Tracker requête
    master$request_history <- c(
      master$request_history,
      list(list(
        request = user_request,
        routing = routing,
        responses = responses,
        timestamp = Sys.time(),
        success = primary_response$success
      ))
    )
    
    master$requests_processed <- master$requests_processed + 1
    master$workflow_state <- "idle"
    
    # Mettre à jour agents dans master
    master$agents <- primary_response$agents
    
    return(list(
      success = TRUE,
      routing = routing,
      responses = responses,
      synthesized_response = synthesized,
      master = master
    ))
    
  }, error = function(e) {
    master$workflow_state <- "idle"
    return(list(
      success = FALSE,
      error = conditionMessage(e),
      master = master
    ))
  })
}

# ============================================================================
# 4. EXECUTE AGENT - Exécuter Agent Spécifique
# ============================================================================

execute_agent <- function(
  master,
  agent_name,
  user_request,
  df = NULL,
  data_context = NULL,
  previous_result = NULL
) {
  
  agent <- master$agents[[agent_name]]
  if (is.null(agent)) {
    return(list(success = FALSE, error = sprintf("Agent '%s' non trouvé", agent_name),
                agent = NULL, master = master))
  }
  
  result <- switch(
    agent_name,
    
    chat = execute_chat_agent(
      agent,
      user_request,
      data_context
    ),
    
    analysis = execute_analysis_agent(
      agent,
      user_request,
      df,
      data_context
    ),
    
    report = execute_report_agent(
      agent,
      user_request,
      data_context,
      previous_result
    ),
    
    diagnose = execute_diagnose_agent(
      agent,
      user_request,
      previous_result
    ),
    
    list(success = FALSE, error = "Unknown agent")
  )
  
  # Mettre à jour agent dans master
  master$agents[[agent_name]] <- result$agent
  
  return(result)
}

execute_chat_agent <- function(agent, request, data_context) {
  
  response_result <- process_user_question(
    agent,
    request,
    data_context
  )
  
  return(list(
    success = response_result$success,
    response = response_result$response %||% response_result$error,
    agents = agent,
    result = response_result
  ))
}

execute_analysis_agent <- function(agent, request, df, data_context) {
  
  # Parser demande d'analyse
  analysis_request <- analyze_request(agent, request, df)
  
  # Auto-sélectionner méthode
  method <- auto_select_method(
    df,
    analysis_request$plan$variables,
    "exploratory"
  )
  
  analysis_request$plan$method <- method
  
  # Exécuter analyse
  execution_result <- execute_analysis(agent, df, analysis_request)
  
  response <- if (execution_result$success) {
    paste(
      "✅ Analyse exécutée avec succès!",
      "Méthode:", method,
      "Variables:", paste(analysis_request$plan$variables, collapse = ", "),
      sep = "\n"
    )
  } else {
    execution_result$error
  }
  
  return(list(
    success = execution_result$success,
    response = response,
    agents = execution_result$agent,
    result = execution_result$result
  ))
}

execute_report_agent <- function(agent, request, data_context, analysis_results) {
  
  # Déterminer style rapport
  style <- detect_report_style(request)
  agent$report_style <- style
  
  # Générer rapport
  report_result <- generate_report(
    agent,
    data_context,
    analysis_results,
    research_question = request
  )
  
  response <- if (report_result$success) {
    paste(
      "✅ Rapport généré!",
      "Style:", agent$report_style,
      "Export:", report_result$export_path,
      sep = "\n"
    )
  } else {
    report_result$error
  }
  
  return(list(
    success = report_result$success,
    response = response,
    agents = report_result$agent,
    result = report_result
  ))
}

execute_diagnose_agent <- function(agent, request, previous_error) {
  
  # Lancer diagnostic complet
  diagnosis_result <- run_full_diagnosis(
    agent,
    error_message = previous_error %||% request,
    user_action = request
  )
  
  # Formater réponse
  response <- paste(
    "## 🔍 Diagnostic",
    diagnosis_result$diagnosis$root_cause,
    "\n## 💡 Solutions",
    diagnosis_result$diagnosis$solutions,
    sep = "\n\n"
  )
  
  return(list(
    success = TRUE,
    response = response,
    agents = diagnosis_result$agent,
    result = diagnosis_result$diagnosis
  ))
}

# ============================================================================
# 5. SYNTHESIZE RESULTS - Synthétiser Résultats Multi-Agent
# ============================================================================

synthesize_multi_agent_results <- function(responses, routing) {
  
  # Si seul agent principal
  if (length(responses) == 1 && !routing$is_multi) {
    return(responses$primary$response)
  }
  
  # Si multi-agents
  primary_response <- responses$primary$response
  
  secondary_texts <- c()
  for (agent_name in names(responses)[-1]) {
    secondary_texts <- c(
      secondary_texts,
      sprintf("**%s:** %s", toupper(agent_name), responses[[agent_name]]$response)
    )
  }
  
  synthesized <- paste(
    "## 📋 Résultats Multi-Agent\n\n",
    primary_response,
    "\n\n---\n\n",
    "## 📊 Résultats Additionnels\n\n",
    paste(secondary_texts, collapse = "\n\n"),
    sep = ""
  )
  
  return(synthesized)
}

# ============================================================================
# 6. CONTEXT SWITCHING - Gérer Transitions Entre Agents
# ============================================================================

handle_context_switch <- function(
  master,
  from_agent,
  to_agent,
  context_data = NULL
) {
  
  # Récupérer contexte de agent source
  source_context <- master$agents[[from_agent]]$conversation
  
  # Transférer contexte à agent destination
  master$agents[[to_agent]]$conversation <- source_context
  
  return(master)
}

# ============================================================================
# 7. FALLBACK STRATEGY - Si Agent Échoue
# ============================================================================

handle_agent_failure <- function(
  master,
  failed_agent,
  user_request,
  error_message
) {
  
  # Stratégie fallback
  fallback_plan <- list(
    chat = "diagnose",      # Chat échoue → Diagnose
    analysis = "chat",      # Analysis échoue → Chat (expliquer)
    report = "chat",        # Report échoue → Chat
    diagnose = "chat"       # Diagnose échoue → Chat (support)
  )
  
  fallback_agent <- fallback_plan[[failed_agent]]
  
  fallback_request <- sprintf(
    "Le %s agent a échoué avec erreur: '%s'. %s",
    failed_agent,
    error_message,
    if (failed_agent == "analysis") {
      "Peux-tu m'expliquer pourquoi?"
    } else if (failed_agent == "report") {
      "Comment puis-je générer un rapport?"
    } else {
      "Aide!"
    }
  )
  
  # Réessayer avec fallback agent
  retry_result <- execute_agent(
    master,
    fallback_agent,
    fallback_request
  )
  
  return(list(
    fallback_agent = fallback_agent,
    result = retry_result
  ))
}

# ============================================================================
# 8. PRINT MASTER STATUS
# ============================================================================

print_master_status <- function(master) {
  
  cat("\n")
  cat("╔═════════════════════════════════════════════════╗\n")
  cat("║  🤖 MASTER AGENT STATUS                        ║\n")
  cat("╠═════════════════════════════════════════════════╣\n")
  cat("│ Status:             ", master$status, "\n")
  cat("│ Workflow State:     ", master$workflow_state, "\n")
  cat("│ Requests Processed: ", master$requests_processed, "\n")
  cat("├─────────────────────────────────────────────────┤\n")
  cat("│ Sub-Agents:\n")
  for (agent_name in names(master$agents)) {
    agent <- master$agents[[agent_name]]
    cat(sprintf("│  ├─ %s: %d requests\n", toupper(agent_name), 
                if (agent$type == "chat") agent$messages_processed else
                if (agent$type == "analysis") agent$analyses_executed else 0))
  }
  cat("╚═════════════════════════════════════════════════╝\n")
}

# ============================================================================
# 9. HELPER FUNCTIONS
# ============================================================================

detect_report_style <- function(request_text) {
  
  request_lower <- tolower(request_text)
  
  if (grepl("executive|summary|1 page|one page", request_lower)) {
    return("executive")
  } else if (grepl("technical|detailed|5|10|pages", request_lower)) {
    return("technical")
  } else if (grepl("field|terrain|2|3|pages", request_lower)) {
    return("field")
  } else if (grepl("student|learn|education|assignment", request_lower)) {
    return("student")
  } else {
    return("technical")  # Par défaut
  }
}

# ============================================================================
# 10. EXPORT
# ============================================================================

cat("✅ Master Agent v3 loaded\n")
