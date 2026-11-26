# ============================================================================
# Chatbot Server v3 — Gemini Direct + Recherche Approfondie
# ============================================================================
# 1. Analyse approfondie des données (structure, types, qualité)
# 2. Détection des indicateurs (SMART, OMS, sécurité alimentaire)
# 3. Propositions d'analyses basées sur le diagnostic
# 4. Boutons d'action cliquables pour exécuter

library(shiny)
library(shinyjs)
library(promises)
library(future)

# Utiliser un worker séparé pour les appels IA — évite de bloquer le thread Shiny
future::plan(future::multisession, workers = 2)

# ============================================================================
# MAPPING DES ONGLETS
# ============================================================================

TAB_MAP <- list(
  exploration   = list(tab = "explor",     label = "🔍 Exploration"),
  univariee     = list(tab = "uni",        label = "📊 Analyse univariée"),
  bivariee      = list(tab = "biv",        label = "📊 Analyse bivariée"),
  descriptif    = list(tab = "descoms",    label = "📋 Tableau descriptif OMS"),
  inferentielle = list(tab = "infer",      label = "🧪 Tests inférentiels"),
  acp           = list(tab = "rdim",       label = "🔬 ACP"),
  acm           = list(tab = "rdim",       label = "🔬 ACM"),
  famd          = list(tab = "rdim",       label = "🔬 FAMD"),
  rdim          = list(tab = "rdim",       label = "🔬 Réduction de dimension"),
  clustering    = list(tab = "clust",      label = "🔗 Clustering"),
  qualite       = list(tab = "qualite",    label = "✅ Qualité SMART"),
  prevalence    = list(tab = "prevalence", label = "📈 Prévalences SMART"),
  secu_alim     = list(tab = "secu_alim",  label = "🍽️ Sécurité alimentaire"),
  zscore        = list(tab = "zscore",     label = "📐 Z-scores OMS"),
  cartographie  = list(tab = "carto",      label = "🌍 Cartographie"),
  dplyr         = list(tab = "dplyr",      label = "🛠️ Manipulation dplyr")
)

# ============================================================================
# ANALYSE APPROFONDIE DES DONNÉES
# ============================================================================

analyze_data_deep <- function(df) {
  # Analyse complète d'un jeu de données pour diagnostiquer ses caractéristiques
  
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      status = "no_data",
      message = "❌ Aucune donnée chargée"
    ))
  }
  
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  # 1. Types de colonnes
  col_types <- sapply(df, function(x) class(x)[1])
  num_cols <- names(col_types[col_types %in% c("numeric", "integer", "double")])
  cat_cols <- names(col_types[col_types %in% c("character", "factor")])
  
  # 2. Valeurs manquantes
  na_summary <- colSums(is.na(df))
  pct_missing <- round(mean(na_summary) / n_rows * 100, 1)
  cols_with_na <- names(na_summary[na_summary > 0])
  
  # 3. Statistiques descriptives
  numeric_stats <- list()
  if (length(num_cols) > 0) {
    numeric_stats <- lapply(num_cols, function(col) {
      vals <- na.omit(df[[col]])
      list(
        mean = mean(vals),
        median = median(vals),
        sd = sd(vals),
        min = min(vals),
        max = max(vals),
        n = length(vals)
      )
    })
    names(numeric_stats) <- num_cols
  }
  
  # 4. Détection d'indicateurs SMART/OMS
  smart_keywords <- c("GAM", "SAM", "MAM", "malnutrition", "wasting", "stunting", 
                      "underweight", "WHZ", "HAZ", "WAZ", "MUAC", "age_months", "poids", "taille")
  oms_keywords <- c("WHZ", "HAZ", "WAZ", "zscore", "z_score", "z-score")
  secu_keywords <- c("FCS", "rCSI", "HDDS", "HHS", "food", "consumption", "coping")
  
  cols_lower <- tolower(names(df))
  
  smart_detected <- any(sapply(smart_keywords, function(kw) 
    any(grepl(tolower(kw), cols_lower))))
  oms_detected <- any(sapply(oms_keywords, function(kw) 
    any(grepl(tolower(kw), cols_lower))))
  secu_detected <- any(sapply(secu_keywords, function(kw) 
    any(grepl(tolower(kw), cols_lower))))
  
  # 5. Compléter l'analyse
  analysis <- list(
    dimensions = list(rows = n_rows, cols = n_cols),
    columns = list(
      numeric = num_cols,
      categorical = cat_cols,
      numeric_count = length(num_cols),
      categorical_count = length(cat_cols)
    ),
    quality = list(
      missing_percent = pct_missing,
      cols_with_missing = cols_with_na,
      missing_counts = na_summary[na_summary > 0],
      duplicates = sum(duplicated(df))
    ),
    detected_indicators = list(
      smart = smart_detected,
      oms = oms_detected,
      food_security = secu_detected
    ),
    numeric_summary = numeric_stats,
    status = "ok"
  )
  
  return(analysis)
}

# ============================================================================
# CONSTRUIRE LE RÉSUMÉ D'ANALYSE POUR GEMINI
# ============================================================================

build_analysis_summary <- function(analysis) {
  # Formater l'analyse approfondie pour envoyer à Gemini
  
  if (analysis$status != "ok") {
    return("❌ Aucune donnée disponible. Guide l'utilisateur pour importer un fichier.")
  }
  
  summary <- paste0(
    "══════ ANALYSE APPROFONDIE DES DONNÉES ══════\n",
    sprintf("📊 Dimensions : %d observations × %d colonnes\n", 
            analysis$dimensions$rows, analysis$dimensions$cols),
    "\n📈 STRUCTURE :\n",
    sprintf("  • Colonnes numériques (%d) : %s\n", 
            analysis$columns$numeric_count,
            if (analysis$columns$numeric_count > 0) paste(analysis$columns$numeric, collapse = ", ") else "aucune"),
    sprintf("  • Colonnes catégorielles (%d) : %s\n",
            analysis$columns$categorical_count,
            if (analysis$columns$categorical_count > 0) paste(analysis$columns$categorical, collapse = ", ") else "aucune"),
    
    "\n🔍 QUALITÉ :\n",
    sprintf("  • Valeurs manquantes : %.1f%% du jeu de données\n", analysis$quality$missing_percent),
    if (length(analysis$quality$cols_with_missing) > 0) {
      paste0("  • Colonnes affectées : ", 
             paste(names(analysis$quality$missing_counts), 
                   " (", analysis$quality$missing_counts, " NA)", collapse = ", "), "\n")
    } else {
      "  • Aucune valeur manquante ✅\n"
    },
    sprintf("  • Doublons : %d\n", analysis$quality$duplicates),
    
    "\n📋 INDICATEURS DÉTECTÉS :\n",
    sprintf("  • SMART/OMS nutrition : %s\n", if (analysis$detected_indicators$smart) "✅ OUI" else "❌ Non"),
    sprintf("  • Z-scores OMS : %s\n", if (analysis$detected_indicators$oms) "✅ OUI" else "❌ Non"),
    sprintf("  • Sécurité alimentaire : %s\n", if (analysis$detected_indicators$food_security) "✅ OUI" else "❌ Non"),
    
    if (length(analysis$numeric_summary) > 0) {
      paste0(
        "\n📊 STATISTIQUES NUMÉRIQUES :\n",
        paste(sapply(names(analysis$numeric_summary), function(col) {
          s <- analysis$numeric_summary[[col]]
          sprintf("  • %s : μ=%.2f, σ=%.2f, [%.2f, %.2f]", 
                  col, s$mean, s$sd, s$min, s$max)
        }), collapse = "\n"),
        "\n"
      )
    },
    "══════════════════════════════════════════\n"
  )
  
  return(summary)
}

# ============================================================================
# SYSTEM PROMPT AVEC INSTRUCTIONS DE RECHERCHE APPROFONDIE
# ============================================================================

SYSTEM_PROMPT_FR <- paste0(
"Tu es NutriAnalyse IA, expert en nutrition, épidémiologie, sécurité alimentaire et data science.
Tu es intégré dans une application Shiny d'analyse de données nutritionnelles.

TA MISSION :
1. Analyser en profondeur les données fournies
2. Identifier les indicateurs présents (SMART, OMS, sécurité alimentaire)
3. Proposer des analyses pertinentes et priorisées
4. Inclure des boutons d'action pour exécuter chaque analyse

INSTRUCTIONS POUR LA RECHERCHE APPROFONDIE :
- Examine TOUS les aspects des données : structure, qualité, types
- Détecte les patterns : valeurs manquantes, distributions, outliers
- Reconnais les domaines : c'est du SMART ? des Z-scores OMS ? de la sécurité alimentaire ?
- Propose des analyses ADAPTÉES au contexte détecté

PROPOSITIONS D'ANALYSES :
Pour chaque analyse suggérée, TOUJOURS inclure un bouton d'action avec ce format exact :
  [[ACTION:identifiant|Texte du bouton]]

Les identifiants valides sont :
", paste(sprintf("  - %s → onglet '%s'", names(TAB_MAP), sapply(TAB_MAP, function(x) x$label)), collapse = "\n"), "

EXEMPLES :
✅ 'Je recommande une ACP sur vos variables numériques. [[ACTION:acp|▶ Lancer ACP]]'
✅ 'Classez les individus avec un clustering K-means. [[ACTION:clustering|▶ Lancer clustering]]'
✅ 'Contrôlez la qualité SMART. [[ACTION:qualite|▶ Contrôle qualité]]'

REGLES :
1. Réponds TOUJOURS en français
2. Sois PROACTIF : ne pas attendre qu'on te demande, tu proposes
3. Cite les colonnes par leur NOM exact
4. Classe les analyses par PRIORITÉ (importance pour le domaine)
5. Utilise des emojis : 📊 📈 ⚠️ ✅ 💡 🔍
6. Chaque proposition = 1-2 lignes d'explication + 1 bouton [[ACTION:...]]
7. Maximum 5-7 propositions d'analyses

DOMAINES D'EXPERTISE :
- Malnutrition aigüe/chronique (GAM, SAM, MAM, stunting, wasting, underweight)
- Z-scores OMS 2006 (WHZ, HAZ, WAZ)
- Enquêtes SMART (plausibility check, flags, digit preference)
- Sécurité alimentaire (FCS, rCSI, HDDS, HHS)
- Statistiques (t-test, Chi², ANOVA, corrélation, régression)
- Réduction de dimension (ACP, ACM, FAMD)
- Clustering (K-means, CAH, DBSCAN)
- Cartographie nutritionnelle
")

# ============================================================================
# CHATBOT SERVER MODULE
# ============================================================================

chatbot_server <- function(id, df, col_mapping = NULL, context_manager = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── STATE ──────────────────────────────────────────────
    chat <- reactiveValues(
      history = list(),
      conversation = list(),
      is_processing = FALSE,
      data_analysis = NULL
    )
    
    # ── WELCOME ────────────────────────────────────────────
    observe({
      if (length(chat$history) == 0) {
        welcome <- paste0(
          "👋 **Bienvenue !** Je suis NutriAnalyse IA.\n\n",
          "📂 **Importez un jeu de données** via la sidebar.\n",
          "Je ferai une **analyse approfondie** et proposerai les meilleures analyses.\n\n",
          "💡 Cliquez sur **📊 Proposer analyse** quand vos données sont prêtes !"
        )
        chat$history <- list(list(role = "assistant", content = welcome, timestamp = Sys.time()))
        chat$conversation <- list(list(role = "model", parts = list(list(text = welcome))))
      }
    })
    
    # ── ANALYSE RÉACTIVE DES DONNÉES ───────────────────────
    deep_analysis <- reactive({
      d <- df()
      if (is.null(d)) return(NULL)
      analyze_data_deep(d)
    })
    
    analysis_summary <- reactive({
      analysis <- deep_analysis()
      if (is.null(analysis)) return(NULL)
      build_analysis_summary(analysis)
    })
    
    # ── RENDER MESSAGES ────────────────────────────────────
    output$messages_container <- renderUI({
      msgs <- chat$history
      if (length(msgs) == 0) return(NULL)
      
      tagList(lapply(seq_along(msgs), function(i) {
        msg <- msgs[[i]]
        if (msg$role == "assistant") {
          render_assistant_message(msg$content, msg$timestamp, i, ns)
        } else {
          render_message(msg$role, msg$content, msg$timestamp)
        }
      }))
    })
    
    # ── CALL GEMINI — MULTI-TOUR NATIF (comme SDK Python Colab) ────────────
    call_gemini_with_analysis <- function(user_text) {

      # System instruction = expertise + contexte données (injecté 1 fois, pas répété)
      analysis_text  <- analysis_summary()
      system_instr   <- if (!is.null(analysis_text)) {
        paste0(SYSTEM_PROMPT_FR, "\n\n", analysis_text)
      } else {
        SYSTEM_PROMPT_FR
      }

      # Historique structuré : derniers 20 turns (user/model alternés)
      # → envoyé dans contents[], pas concaténé en texte
      recent_history <- tail(chat$conversation, 20)

      # Ajouter le nouveau message utilisateur
      new_turn  <- list(role = "user", parts = list(list(text = user_text)))
      contents  <- c(recent_history, list(new_turn))

      call_gemini_multiturn(
        contents           = contents,
        system_instruction = system_instr,
        config             = GEMINI_CONFIG,
        max_tokens         = 2048,
        temperature        = 0.7
      )
    }
    
    # ── SEND MESSAGE ───────────────────────────────────────
    send_message <- function(user_text) {
      if (user_text == "" || chat$is_processing) return()
      
      chat$is_processing <- TRUE
      shinyjs::show("loading_indicator")
      
      chat$history <- c(chat$history, list(
        list(role = "user", content = user_text, timestamp = Sys.time())
      ))
      chat$conversation <- c(chat$conversation, list(
        list(role = "user", parts = list(list(text = user_text)))
      ))
      
      updateTextInput(session, "user_input", value = "")

      # ── Capturer les données nécessaires hors contexte réactif ─────────────
      # (les futures s'exécutent dans un worker séparé sans accès aux réactifs)
      contents_snap     <- chat$conversation
      system_instr_snap <- {
        at <- analysis_summary()
        if (!is.null(at)) paste0(SYSTEM_PROMPT_FR, "\n\n", at) else SYSTEM_PROMPT_FR
      }
      api_key_snap  <- GEMINI_CONFIG$api_key
      model_snap    <- GEMINI_CONFIG$model
      timeout_snap  <- GEMINI_CONFIG$timeout

      # ── Fonction interne d'appel Gemini (synchrone) ──────────────────────
      .call_gemini <- function() {
        url <- sprintf(
          "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent",
          model_snap
        )
        new_turn <- list(role = "user", parts = list(list(text = user_text)))
        contents <- c(contents_snap, list(new_turn))

        payload <- list(
          systemInstruction = list(parts = list(list(text = system_instr_snap))),
          contents          = contents,
          generationConfig  = list(
            maxOutputTokens = 2048L,
            temperature     = 0.7,
            topP            = 0.95
          ),
          safetySettings = list(
            list(category = "HARM_CATEGORY_HATE_SPEECH",       threshold = "BLOCK_NONE"),
            list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT", threshold = "BLOCK_NONE"),
            list(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE"),
            list(category = "HARM_CATEGORY_HARASSMENT",        threshold = "BLOCK_NONE")
          )
        )

        resp <- httr::POST(
          url,
          httr::add_headers("Content-Type" = "application/json"),
          query = list(key = api_key_snap),
          body  = jsonlite::toJSON(payload, auto_unbox = TRUE),
          httr::timeout(timeout_snap)
        )

        if (httr::status_code(resp) != 200L)
          stop(sprintf("API Error (%d)", httr::status_code(resp)))

        result <- jsonlite::fromJSON(
          httr::content(resp, as = "text", encoding = "UTF-8"),
          simplifyVector = FALSE
        )
        result$candidates[[1]]$content$parts[[1]]$text
      }

      # ── Callback succès ─────────────────────────────────────────────────
      .on_success <- function(response) {
        chat$history <- c(chat$history, list(
          list(role = "assistant", content = response, timestamp = Sys.time())
        ))
        chat$conversation <- c(
          chat$conversation,
          list(list(role = "user",  parts = list(list(text = user_text)))),
          list(list(role = "model", parts = list(list(
            text = gsub("\\[\\[ACTION:[^]]*\\]\\]", "", response)
          ))))
        )
        chat$is_processing <- FALSE
        shinyjs::hide("loading_indicator")
      }

      # ── Callback erreur ─────────────────────────────────────────────────
      .on_error <- function(err) {
        msg <- if (inherits(err, "condition")) conditionMessage(err) else as.character(err)
        friendly <- if (grepl("timeout|connexion", msg, ignore.case = TRUE)) {
          "\u26a0\ufe0f Connexion \u00e9chou\u00e9e. V\u00e9rifiez votre connexion internet."
        } else if (grepl("401|403|api", msg, ignore.case = TRUE)) {
          "\U0001f511 Erreur d'authentification API. V\u00e9rifiez votre cl\u00e9 Gemini."
        } else if (grepl("429|rate", msg, ignore.case = TRUE)) {
          "\u23f1\ufe0f Limite de requ\u00eates atteinte. Attendez quelques secondes."
        } else {
          sprintf("\u274c Erreur IA : %s", msg)
        }
        chat$history <- c(chat$history, list(
          list(role = "assistant", content = friendly, timestamp = Sys.time())
        ))
        chat$is_processing <- FALSE
        shinyjs::hide("loading_indicator")
      }

      # ── Tentative asynchrone (future_promise) avec fallback synchrone ───
      tryCatch({
        promises::future_promise({ .call_gemini() }) %...>%
          .on_success %...!%
          .on_error
      }, error = function(e) {
        # Fallback synchrone si future_promise échoue
        tryCatch({
          response <- .call_gemini()
          .on_success(response)
        }, error = function(e2) {
          .on_error(e2)
        })
      })

      NULL
    }
    
    # ── TRIGGERS ───────────────────────────────────────────
    
    observeEvent(input$send_button, {
      send_message(trimws(input$user_input %||% ""))
    })
    
    observeEvent(input$quick_analysis, {
      d <- df()
      if (is.null(d) || nrow(d) == 0) {
        showNotification("📂 Importez d'abord un jeu de données", type = "warning")
        return()
      }
      
      auto_prompt <- paste0(
        "J'ai chargé un jeu de données (", nrow(d), " obs × ", ncol(d), " colonnes). ",
        "Fais une analyse APPROFONDIE et propose les meilleures analyses à effectuer. ",
        "Sois spécifique : cite les noms des colonnes, indique pourquoi chaque analyse est pertinente, ",
        "et ajoute un bouton d'exécution pour chacune. Classe par priorité (max 5 propositions)."
      )
      
      send_message(auto_prompt)
    })
    
    observeEvent(input$quick_report, {
      d <- df()
      if (is.null(d) || nrow(d) == 0) {
        showNotification("📂 Importez d'abord un jeu de données", type = "warning")
        return()
      }
      send_message("Résume complètement mes données et propose les analyses clés.")
    })
    
    observeEvent(input$clear_chat, {
      chat$history <- list(list(
        role = "assistant",
        content = "🔄 Conversation effacée.",
        timestamp = Sys.time()
      ))
      chat$conversation <- list()
    })
    
    return(list(
      messages = reactive(chat$history),
      data_analysis = reactive(deep_analysis())
    ))
  })
}

cat("✅ Chatbot Server v3 loaded\n")
