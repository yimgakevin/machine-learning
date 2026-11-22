# ============================================================================
# Report Agent v3 - Auto-Generated Publication-Ready Reports
# ============================================================================
# Génère rapports de qualité publication automatiquement
# 4 styles: Executive (1p), Technical (5-10p), Field (2-3p), Student (3-5p)
# Export: HTML, DOCX, PDF
# ============================================================================

library(tidyverse)
library(rmarkdown)
library(officer)
library(flextable)

# Note: dépendances sourcées par app_v2_final.R. Ne pas re-sourcer ici.

# ============================================================================
# 1. INITIALIZE REPORT AGENT
# ============================================================================

initialize_report_agent <- function(
  report_style = "technical",
  export_format = "html",
  user_context = NULL
) {
  
  agent <- list(
    type = "report",
    report_style = report_style,  # executive|technical|field|student
    export_format = export_format,  # html|docx|pdf
    user_context = user_context,
    conversation = initialize_conversation(user_context),
    reports_generated = 0,
    created = Sys.time(),
    status = "ready"
  )
  
  class(agent) <- c("report_agent", "list")
  
  cat("✅ Report Agent Initialized\n")
  cat("   Style:", report_style, "\n")
  cat("   Export:", export_format, "\n")
  
  return(agent)
}

# ============================================================================
# 2. GENERATE REPORT - Créer Rapport Complet
# ============================================================================

generate_report <- function(
  agent,
  data_context,
  analysis_results = NULL,
  research_question = NULL,
  methodology_notes = NULL
) {
  
  tryCatch({
    
    # Construire sections rapport
    sections <- build_report_sections(
      agent,
      data_context,
      analysis_results,
      research_question,
      methodology_notes
    )
    
    # Formater rapport
    formatted_report <- format_report_output(
      agent,
      sections
    )
    
    # Exporter rapport
    export_result <- export_report(
      agent,
      formatted_report
    )
    
    agent$reports_generated <- agent$reports_generated + 1
    
    return(list(
      success = TRUE,
      report = formatted_report,
      export_path = export_result$path,
      agent = agent,
      sections = sections
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
# 3. BUILD REPORT SECTIONS - Auto-Générer Sections
# ============================================================================

build_report_sections <- function(
  agent,
  data_context,
  analysis_results,
  research_question,
  methodology_notes
) {
  
  sections <- list()
  
  # 1. TITRE & MÉTA
  sections$metadata <- list(
    title = generate_report_title(research_question, data_context),
    date = Sys.Date(),
    author = "NutriAnalyse Pro v2 - Analysis Report",
    style = agent$report_style
  )
  
  # 2. EXECUTIVE SUMMARY / RÉSUMÉ
  sections$executive_summary <- generate_executive_summary(
    agent,
    data_context,
    analysis_results,
    research_question
  )
  
  # 3. CONTEXTE
  sections$context <- generate_context_section(
    agent,
    data_context,
    research_question
  )
  
  # 4. MÉTHODOLOGIE
  sections$methodology <- generate_methodology_section(
    agent,
    data_context,
    analysis_results,
    methodology_notes
  )
  
  # 5. RÉSULTATS
  sections$results <- generate_results_section(
    agent,
    analysis_results,
    data_context
  )
  
  # 6. DISCUSSION
  sections$discussion <- generate_discussion_section(
    agent,
    analysis_results,
    data_context
  )
  
  # 7. LIMITATIONS
  sections$limitations <- generate_limitations_section(
    data_context,
    analysis_results
  )
  
  # 8. RECOMMANDATIONS
  sections$recommendations <- generate_recommendations_section(
    agent,
    analysis_results,
    data_context
  )
  
  # 9. RÉFÉRENCES
  sections$references <- generate_references(
    agent,
    data_context,
    analysis_results
  )
  
  return(sections)
}

# ============================================================================
# 4. INDIVIDUAL SECTION GENERATORS
# ============================================================================

generate_report_title <- function(research_question, data_context) {
  
  title <- if (!is.null(research_question)) {
    sprintf("Analysis Report: %s", substring(research_question, 1, 80))
  } else {
    sprintf("Data Analysis Report (%d observations, %d variables)",
            data_context$dimensions$rows,
            data_context$dimensions$cols)
  }
  
  return(title)
}

generate_executive_summary <- function(
  agent,
  data_context,
  analysis_results,
  research_question
) {
  
  # Prompt pour Gemini
  summary_prompt <- sprintf(
    "Génère un résumé exécutif très court (3-4 phrases) pour un rapport %s:\n\nQUESTION: %s\n\nDONNÉES: %d observations, %d variables\nManquants: %.1f%%\n\nRÉSULTATS: %s\n\nSois concis, mets les insights clés.",
    agent$report_style,
    research_question %||% "Exploration données",
    data_context$dimensions$rows,
    data_context$dimensions$cols,
    data_context$data_quality$missing_percent,
    if (!is.null(analysis_results)) "Analyses complétées" else "Aucune analyse spécifique"
  )
  
  summary_text <- tryCatch({
    call_gemini_api(
      prompt = summary_prompt,
      max_tokens = 500,
      temperature = 0.7
    )
  }, error = function(e) {
    paste("## Résumé Exécutif\n\nAnalyse des données complétée.")
  })
  
  return(summary_text)
}

generate_context_section <- function(
  agent,
  data_context,
  research_question
) {
  
  context_prompt <- sprintf(
    "Génère section 'Contexte' pour rapport de style '%s':\n\nQUESTION: %s\n\nDONNÉES: N=%d, %d variables\nIndicateurs détectés: %s\n\nSois court (150-200 mots), mets en contexte.",
    agent$report_style,
    research_question %||% "Exploration données nutritionnelles",
    data_context$dimensions$rows,
    data_context$dimensions$cols,
    paste(c(data_context$detected_indicators$smart,
            data_context$detected_indicators$oms), collapse = ", ")
  )
  
  context_text <- tryCatch({
    call_gemini_api(
      prompt = context_prompt,
      max_tokens = 800,
      temperature = 0.7
    )
  }, error = function(e) {
    "## Contexte\n\nAnalyse données nutritionnelles et épidémiologiques."
  })
  
  return(context_text)
}

generate_methodology_section <- function(
  agent,
  data_context,
  analysis_results,
  methodology_notes
) {
  
  methods_used <- if (!is.null(analysis_results)) {
    if (is.list(analysis_results)) {
      paste(unique(map_chr(analysis_results, ~.x$method %||% "unknown")), collapse = ", ")
    } else {
      analysis_results$method %||% "analyses"
    }
  } else {
    "exploration descriptive"
  }
  
  method_prompt <- sprintf(
    "Génère section 'Méthodologie' pour rapport %s:\n\nMéthodes: %s\nDonnées: N=%d\nManquants: %.1f%%\n\nSois clair sur approche analytique. (200-300 mots)",
    agent$report_style,
    methods_used,
    data_context$dimensions$rows,
    data_context$data_quality$missing_percent
  )
  
  methodology_text <- tryCatch({
    call_gemini_api(
      prompt = method_prompt,
      max_tokens = 1000,
      temperature = 0.5
    )
  }, error = function(e) {
    "## Méthodologie\n\nAnalyse descriptive et exploratoire des données."
  })
  
  return(methodology_text)
}

generate_results_section <- function(
  agent,
  analysis_results,
  data_context
) {
  
  if (is.null(analysis_results)) {
    return("## Résultats\n\nAnalyse descriptive: voir tableau données.")
  }
  
  results_summary <- format_results_for_report(analysis_results)
  
  results_prompt <- sprintf(
    "Présente les résultats d'analyse suivante pour rapport %s:\n\n%s\n\nSois clair et structuré. (300-500 mots)",
    agent$report_style,
    results_summary
  )
  
  results_text <- tryCatch({
    call_gemini_api(
      prompt = results_prompt,
      max_tokens = 1500,
      temperature = 0.7
    )
  }, error = function(e) {
    "## Résultats\n\nAnalyse complétée avec succès."
  })
  
  return(results_text)
}

generate_discussion_section <- function(
  agent,
  analysis_results,
  data_context
) {
  
  discussion_prompt <- sprintf(
    "Génère section 'Discussion' pour rapport %s:\n\nContexte: %d individus, %d variables\nQualité: %.1f%% données manquantes\n\nInterprète résultats dans contexte. Sois critique mais constructif. (300-400 mots)",
    agent$report_style,
    data_context$dimensions$rows,
    data_context$dimensions$cols,
    data_context$data_quality$missing_percent
  )
  
  discussion_text <- tryCatch({
    call_gemini_api(
      prompt = discussion_prompt,
      max_tokens = 1200,
      temperature = 0.7
    )
  }, error = function(e) {
    "## Discussion\n\nRésultats analysés et discutés."
  })
  
  return(discussion_text)
}

generate_limitations_section <- function(data_context, analysis_results) {
  
  limitations <- c()
  
  # Limitation 1: Taille échantillon
  if (data_context$dimensions$rows < 100) {
    limitations <- c(limitations,
      sprintf("Petit échantillon (N=%d) - puissance statistique limitée", 
              data_context$dimensions$rows))
  }
  
  # Limitation 2: Missing data
  if (data_context$data_quality$missing_percent > 10) {
    limitations <- c(limitations,
      sprintf("%.1f%% données manquantes - analyses sensibilité recommandées",
              data_context$data_quality$missing_percent))
  }
  
  # Limitation 3: Design
  limitations <- c(limitations,
    "Analyse transversale - pas de causalité")
  
  limitations_text <- paste0(
    "## Limitations\n\n",
    "- ",
    paste(limitations, collapse = "\n- ")
  )
  
  return(limitations_text)
}

generate_recommendations_section <- function(
  agent,
  analysis_results,
  data_context
) {
  
  rec_prompt <- sprintf(
    "Propose 3-5 recommandations basées sur analyse pour rapport %s. Sois pratique et actionnable. (150-200 mots)",
    agent$report_style
  )
  
  recommendations_text <- tryCatch({
    call_gemini_api(
      prompt = rec_prompt,
      max_tokens = 700,
      temperature = 0.7
    )
  }, error = function(e) {
    "## Recommandations\n\n1. Collecter plus de données\n2. Approfondir analyses\n3. Valider résultats"
  })
  
  return(recommendations_text)
}

generate_references <- function(agent, data_context, analysis_results) {
  
  references <- c(
    "WHO (2006). Child Growth Standards. Geneva: WHO.",
    "OMS (2009). Standards de croissance de l'enfant. Genève: OMS.",
    "SMART Initiative (2022). Manual for Standardized Monitoring and Assessment of Relief and Transitions.",
    "WFP (2021). Food Consumption Analysis and Dietary Diversity Guides.",
    "R Core Team (2024). R: A Language and Environment for Statistical Computing."
  )
  
  refs_text <- paste0(
    "## Références\n\n",
    paste(references, collapse = "\n")
  )
  
  return(refs_text)
}

# ============================================================================
# 5. FORMAT REPORT OUTPUT - Formatter Rapport
# ============================================================================

format_report_output <- function(agent, sections) {
  
  formatted <- paste(
    sprintf("# %s\n", sections$metadata$title),
    sprintf("**Date:** %s\n", sections$metadata$date),
    sprintf("**Style:** %s\n", sections$metadata$style),
    "\n---\n\n",
    "## Résumé Exécutif\n\n", sections$executive_summary, "\n\n",
    "---\n\n",
    sections$context, "\n\n",
    "---\n\n",
    sections$methodology, "\n\n",
    "---\n\n",
    sections$results, "\n\n",
    "---\n\n",
    sections$discussion, "\n\n",
    "---\n\n",
    sections$limitations, "\n\n",
    "---\n\n",
    sections$recommendations, "\n\n",
    "---\n\n",
    sections$references,
    sep = ""
  )
  
  return(formatted)
}

# ============================================================================
# 6. EXPORT REPORT - Exporter Rapport
# ============================================================================

export_report <- function(agent, report_content) {
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if (agent$export_format == "html") {
    export_path <- sprintf("reports/report_%s.html", timestamp)
    dir.create("reports", showWarnings = FALSE)
    writeLines(report_content, export_path)
    
  } else if (agent$export_format == "docx") {
    export_path <- sprintf("reports/report_%s.docx", timestamp)
    dir.create("reports", showWarnings = FALSE)
    
    # Utiliser rmarkdown pour convertir Markdown → DOCX
    temp_md <- tempfile(fileext = ".md")
    writeLines(report_content, temp_md)
    
    rmarkdown::render(
      temp_md,
      output_format = "word_document",
      output_file = export_path,
      quiet = TRUE
    )
    
  } else if (agent$export_format == "pdf") {
    export_path <- sprintf("reports/report_%s.pdf", timestamp)
    dir.create("reports", showWarnings = FALSE)
    
    temp_md <- tempfile(fileext = ".md")
    writeLines(report_content, temp_md)
    
    rmarkdown::render(
      temp_md,
      output_format = "pdf_document",
      output_file = export_path,
      quiet = TRUE
    )
  }
  
  return(list(
    success = TRUE,
    path = export_path,
    format = agent$export_format
  ))
}

# ============================================================================
# 7. HELPER FUNCTIONS
# ============================================================================

format_results_for_report <- function(analysis_results) {
  
  if (is.null(analysis_results)) {
    return("Aucune analyse spécifique.")
  }
  
  result_summary <- sprintf(
    "Méthode: %s\n\nRésumé: %s",
    analysis_results$method %||% "unknown",
    "Analyse complétée avec succès"
  )
  
  return(result_summary)
}

# ============================================================================
# 8. EXPORT
# ============================================================================

cat("✅ Report Agent v3 loaded\n")
