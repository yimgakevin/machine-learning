# ============================================================================
# Chatbot Server v4.1 — IA Expert : Analyse réelle des données via code R
# ============================================================================
# AMÉLIORATIONS v4.1 :
#   - Fix regex extraction code R (multi-lignes)
#   - Suppression double appel API (interprétation intégrée)
#   - System prompt enrichi + structured output
#   - Meilleure gestion contexte/tokens
#   - Retry intelligent + meilleur error handling
# ============================================================================

library(shiny)
library(shinyjs)
library(promises)
library(future)
library(ggplot2)
library(tidyverse)
if (!requireNamespace("knitr", quietly = TRUE)) {
  message("\u26a0\ufe0f Package knitr recommandé pour les tableaux dans le chat")
}
if (!requireNamespace("base64enc", quietly = TRUE)) {
  message("\u26a0\ufe0f Package base64enc requis pour les graphiques dans le chat. install.packages('base64enc')")
}

# Async léger via later (évite les problèmes de sérialisation Shiny + multisession)
if (!requireNamespace("later", quietly = TRUE)) {
  message("⚠️ Package 'later' requis pour l'async du chatbot. install.packages('later')")
}

# ============================================================================
# MAPPING DES ONGLETS (conservé pour navigation)
# ============================================================================

TAB_MAP <- list(
  exploration   = list(tab = "explor",     label = "\U0001f50d Exploration"),
  univariee     = list(tab = "uni",        label = "\U0001f4ca Analyse univariée"),
  bivariee      = list(tab = "biv",        label = "\U0001f4ca Analyse bivariée"),
  descriptif    = list(tab = "descoms",    label = "\U0001f4cb Tableau descriptif OMS"),
  inferentielle = list(tab = "infer",      label = "\U0001f9ea Tests inférentiels"),
  acp           = list(tab = "rdim",       label = "\U0001f52c ACP"),
  acm           = list(tab = "rdim",       label = "\U0001f52c ACM"),
  famd          = list(tab = "rdim",       label = "\U0001f52c FAMD"),
  rdim          = list(tab = "rdim",       label = "\U0001f52c Réduction de dimension"),
  clustering    = list(tab = "clust",      label = "\U0001f517 Clustering"),
  qualite       = list(tab = "qualite",    label = "\u2705 Qualité SMART"),
  prevalence    = list(tab = "prevalence", label = "\U0001f4c8 Prévalences SMART"),
  secu_alim     = list(tab = "secu_alim",  label = "\U0001f37d\ufe0f Sécurité alimentaire"),
  zscore        = list(tab = "zscore",     label = "\U0001f4d0 Z-scores OMS"),
  cartographie  = list(tab = "carto",      label = "\U0001f30d Cartographie"),
  dplyr         = list(tab = "dplyr",      label = "\U0001f6e0\ufe0f Manipulation dplyr")
)

# ============================================================================
# 1. CONSTRUCTION DU CONTEXTE RICHE (optimisé v4.1)
# ============================================================================

build_rich_context <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  n <- nrow(df)
  p <- ncol(df)

  # Types de colonnes
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  date_cols <- names(df)[vapply(df, function(x) inherits(x, c("Date", "POSIXct")), logical(1))]
  bool_cols <- names(df)[vapply(df, is.logical, logical(1))]

  # Info par colonne (type, NA, n_distinct)
  col_info <- vapply(names(df), function(col) {
    x <- df[[col]]
    sprintf("%s [%s, %d uniques, %d NA (%.0f%%)]",
            col, class(x)[1], length(unique(na.omit(x))),
            sum(is.na(x)), sum(is.na(x)) / n * 100)
  }, character(1))

  # Statistiques numériques (concises)
  num_stats <- if (length(num_cols) > 0) {
    vapply(num_cols[seq_len(min(20, length(num_cols)))], function(col) {
      x <- na.omit(df[[col]])
      if (length(x) == 0) return("toutes valeurs manquantes")
      sprintf("mean=%.2f, sd=%.2f, min=%.2f, med=%.2f, max=%.2f, NA=%d",
              mean(x), sd(x), min(x), median(x), max(x), sum(is.na(df[[col]])))
    }, character(1))
  } else character(0)

  # Catégorielles : top valeurs (limité)
  cat_stats <- if (length(cat_cols) > 0) {
    vapply(cat_cols[seq_len(min(10, length(cat_cols)))], function(col) {
      tbl <- sort(table(df[[col]], useNA = "ifany"), decreasing = TRUE)
      top <- head(tbl, 5)
      paste(sprintf("%s(%d)", names(top), as.integer(top)), collapse = ", ")
    }, character(1))
  } else character(0)

  # Échantillon compact (3 lignes)
  sample_rows <- tryCatch({
    idx <- unique(c(1:min(3, n)))
    capture.output(print(df[idx, , drop = FALSE], max = 300, width = 100))
  }, error = function(e) "Impossible d'afficher")

  # Corrélations fortes
  cor_text <- ""
  if (length(num_cols) >= 2 && length(num_cols) <= 50) {
    tryCatch({
      cm <- cor(df[num_cols], use = "pairwise.complete.obs")
      diag(cm) <- NA
      top_idx <- which(abs(cm) > 0.5, arr.ind = TRUE)
      if (nrow(top_idx) > 0) {
        pairs <- unique(t(apply(top_idx, 1, sort)))
        top_cors <- vapply(seq_len(min(8, nrow(pairs))), function(i) {
          sprintf("  %s <-> %s : r=%.2f", rownames(cm)[pairs[i, 1]],
                  colnames(cm)[pairs[i, 2]], cm[pairs[i, 1], pairs[i, 2]])
        }, character(1))
        cor_text <- paste0("\nCORRÉLATIONS NOTABLES (|r|>0.5):\n", paste(top_cors, collapse = "\n"))
      }
    }, error = function(e) {})
  }

  # Détection domaines
  cols_lower <- tolower(names(df))
  all_lower <- paste(cols_lower, collapse = " ")
  smart <- grepl("gam|sam|mam|wasting|stunting|underweight|whz|haz|waz|muac|pb|poids|taille|weight|height", all_lower)
  fcs <- grepl("fcs|rcsi|hdds|hhs|food|coping", all_lower)
  epi <- grepl("incidence|mortality|morbidity|case|death|outbreak", all_lower)
  zscore <- grepl("whz|haz|waz|zscore|z_score|z.score", all_lower)

  domain_text <- paste0(
    "DOMAINES DÉTECTÉS :",
    if (smart) " NUTRITION/SMART" else "",
    if (zscore) " Z-SCORES" else "",
    if (fcs) " SÉCURITÉ_ALIMENTAIRE" else "",
    if (epi) " ÉPIDÉMIOLOGIE" else "",
    if (!smart && !fcs && !epi) " DONNÉES_GÉNÉRALES" else ""
  )

  # Assembler le contexte
  ctx <- paste0(
    "═══ DONNÉES CHARGÉES ═══\n",
    sprintf("Dimensions : %d observations × %d colonnes\n", n, p),
    sprintf("Numériques (%d): %s\n", length(num_cols), paste(num_cols, collapse = ", ")),
    sprintf("Catégorielles (%d): %s\n", length(cat_cols), paste(cat_cols, collapse = ", ")),
    if (length(date_cols) > 0) sprintf("Dates (%d): %s\n", length(date_cols), paste(date_cols, collapse = ", ")) else "",
    if (length(bool_cols) > 0) sprintf("Booléens (%d): %s\n", length(bool_cols), paste(bool_cols, collapse = ", ")) else "",

    "\nSCHÉMA DÉTAILLÉ:\n", paste(col_info, collapse = "\n"),

    if (length(num_stats) > 0) paste0(
      "\n\nSTATISTIQUES NUMÉRIQUES:\n",
      paste(sprintf("  %s : %s", names(num_stats), num_stats), collapse = "\n")
    ) else "",

    if (length(cat_stats) > 0) paste0(
      "\n\nVALEURS CATÉGORIELLES:\n",
      paste(sprintf("  %s : %s", names(cat_stats), cat_stats), collapse = "\n")
    ) else "",

    cor_text,
    "\n\n", domain_text,

    "\n\nÉCHANTILLON:\n", paste(sample_rows, collapse = "\n"),
    "\n═══════════════════════\n"
  )

  return(ctx)
}

# ============================================================================
# 2. SYSTEM PROMPT — IA EXPERT DATA SCIENTIST (v4.1 amélioré)
# ============================================================================

SYSTEM_PROMPT_EXPERT <- "Tu es NutriAnalyse IA Expert, un data scientist de niveau senior spécialisé en nutrition, épidémiologie, sécurité alimentaire et biostatistique.

═══ RÈGLE ANTI-HALLUCINATION (CRITIQUE) ═══
⚠️ Tu NE CONNAIS PAS les données de l'utilisateur à l'avance.
⚠️ Tu n'as AUCUNE mémoire de sessions précédentes.
⚠️ Tu ne dois JAMAIS inventer, deviner ou supposer :
   - Le nombre d'observations ou de variables
   - Les noms de colonnes (utilise UNIQUEMENT ceux fournis dans le contexte)
   - Les valeurs, moyennes, proportions ou résultats statistiques
   - Le type de données (nutrition, FCS, etc.) sauf si le contexte le confirme
⚠️ Si aucune donnée n'est chargée (pas de section DONNÉES CHARGÉES dans le contexte),
   dis simplement : 'Veuillez d'abord importer un jeu de données.'
⚠️ JAMAIS de chiffres inventés. Si tu ne sais pas, dis-le.

═══ TON RÔLE ═══
Tu analyses RÉELLEMENT les données via du code R exécuté automatiquement.
Le code que tu écris sera exécuté et les VRAIS résultats seront affichés à l'utilisateur.
Tu ne dois donc PAS prédire ou anticiper les résultats dans ton texte.

═══ COMMENT TU TRAVAILLES ═══
1. L'utilisateur pose une question sur ses données
2. Tu consultes le CONTEXTE fourni (schéma, colonnes, stats) pour connaître la structure réelle
3. Tu génères du CODE R adapté aux colonnes RÉELLES du jeu de données
4. Le code est exécuté automatiquement — les résultats sont affichés à l'utilisateur
5. Tu fournis une EXPLICATION MÉTHODOLOGIQUE (pas les résultats que tu ne connais pas encore)

═══ RÈGLES DE GÉNÉRATION DE CODE R ═══
1. TOUJOURS mettre le code dans un SEUL bloc ```r ... ``` (un seul bloc, pas plusieurs)
2. Les données sont dans l'objet `df` — NE PAS utiliser d'autre nom
3. NE PAS utiliser library() — les packages sont déjà chargés (tidyverse, ggplot2, etc.)
4. Pour les graphiques : assigner à `p` → p <- ggplot(...)
   - Style professionnel : theme_minimal() + labs() avec titres clairs en français
   - Couleurs sobres : scale_fill_brewer ou scale_color_viridis_d
5. Pour les tableaux : assigner à `tbl` → tbl <- df |> group_by(...) |> summarise(...)
6. Pour les résultats texte : utiliser cat() ou print()
7. UTILISER UNIQUEMENT les noms de colonnes listés dans le contexte — NE PAS deviner
8. TOUJOURS gérer les NA avec na.rm=TRUE
9. Code CONCIS et EFFICACE (max 40 lignes)
10. Ne pas utiliser de fonctions dangereuses (system, file.remove, etc.)

═══ STRUCTURE DE RÉPONSE ═══

QUAND TU GÉNÈRES DU CODE (question sur les données) :
1. **Introduction** (1-2 phrases) : ce que tu vas analyser
2. **Code R** dans un bloc ```r ... ```
3. **Explication méthodologique** : explique la MÉTHODE utilisée et comment LIRE les résultats
   - Quels seuils de référence s'appliquent (ex: OMS si pertinent)
   - Comment interpréter les graphiques/tableaux qui seront produits
   - NE PAS écrire de chiffres — les vrais résultats apparaîtront automatiquement

QUAND TU NE GÉNÈRES PAS DE CODE (question conceptuelle) :
Réponds directement en texte structuré avec ton expertise.

QUAND AUCUNE DONNÉE N'EST CHARGÉE :
Ne génère PAS de code. Dis à l'utilisateur d'importer ses données d'abord.

═══ EXPERTISE DISPONIBLE ═══
- Malnutrition : GAM, SAM, MAM, stunting, wasting, underweight, MUAC, œdèmes
- Z-scores OMS 2006 : WHZ, HAZ, WAZ, BAZ — seuils -2/-3 SD
- Enquêtes SMART : plausibility checks, digit preference, SMART flags, DEFF
- Sécurité alimentaire : FCS (seuils 0-21/21.5-35/35.5+), rCSI, HDDS, HHS, FIES
- Épidémiologie : incidence, prévalence, mortalité, OR/RR, IC 95%
- Statistiques : tests paramétriques/non-paramétriques, régressions, ACP/ACM/FAMD, clustering
- Seuils OMS malnutrition : <5% acceptable, 5-9.9% alerte, 10-14.9% sérieux, 15-29.9% critique, ≥30% urgence

═══ RÈGLES ABSOLUES ═══
1. TOUJOURS en français
2. NE JAMAIS INVENTER DE DONNÉES — c'est la règle la plus importante
3. Utiliser UNIQUEMENT les colonnes qui existent dans le contexte fourni
4. HONNÊTE : si les données ne permettent pas de répondre, dis-le clairement
5. CRITIQUE : signale problèmes de qualité, biais, limites
6. PROACTIF : suggère des analyses complémentaires pertinentes

═══ NAVIGATION MODULES ═══
À la FIN de CHAQUE réponse qui contient du code R, ajoute EXACTEMENT UN bouton
vers le module le plus pertinent, sur sa propre ligne, format :
  [[ACTION:identifiant|Texte du bouton]]
Identifiants valides : prevalence, carto, zscore, secu_alim, uni, biv, infer,
epidemio, mortalite, anemie, iycf, wash, paludisme, qualite, rapport, predictif.
Exemple : [[ACTION:prevalence|📊 Aller au module Prévalence]]
Le bouton est un COMPLÉMENT — il ne remplace jamais l'analyse.
Pour une question conceptuelle sans code, le bouton est optionnel.
"

# ============================================================================
# 3. EXÉCUTION SÉCURISÉE DE CODE R (v4.1 amélioré)
# ============================================================================

safe_execute_r <- function(code, df, timeout_sec = 45) {
  # Vérifier commandes dangereuses
  dangerous <- c("system", "system2", "file.remove", "file.delete", "unlink",
                 "shell", "shell.exec", "Sys.setenv", "quit", "q()",
                 "install.packages", "remove.packages", "download.file",
                 "source", "eval(parse", "readLines", "writeLines",
                 "file.create", "dir.create", "setwd", "rm(list")

  code_lower <- tolower(code)
  for (cmd in dangerous) {
    if (grepl(cmd, code_lower, fixed = TRUE)) {
      return(list(success = FALSE, output = NULL, plot_base64 = NULL,
                  warnings = NULL, error = sprintf("Commande interdite : %s", cmd)))
    }
  }

  # Créer environnement sandboxé
  sandbox <- new.env(parent = globalenv())
  sandbox$df <- df

  result <- list(success = FALSE, output = NULL, plot_base64 = NULL,
                 warnings = character(0), error = NULL, tbl_html = NULL)

  plot_file <- tempfile(fileext = ".png")

  tryCatch({
    # Capturer sortie texte + exécuter le code
    text_output <- NULL
    exec_output <- withCallingHandlers(
      withTimeout({
        text_output <<- capture.output({
          .last_val <- eval(parse(text = code), envir = sandbox)
        })
        .last_val
      }, timeout = timeout_sec, onTimeout = "error"),
      warning = function(w) {
        result$warnings <<- c(result$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    # Chercher un ggplot (dans output, sandbox$p, ou dernière valeur)
    gg <- NULL
    if (inherits(exec_output, "ggplot") || inherits(exec_output, "gg")) {
      gg <- exec_output
    } else if (exists("p", envir = sandbox) && (inherits(sandbox$p, "ggplot") || inherits(sandbox$p, "gg"))) {
      gg <- sandbox$p
    }

    # Rendre le ggplot en PNG si trouvé
    plot_b64 <- NULL
    if (!is.null(gg)) {
      tryCatch({
        grDevices::png(plot_file, width = 900, height = 550, res = 130, bg = "white")
        print(gg)
        grDevices::dev.off()
        if (file.exists(plot_file) && file.info(plot_file)$size > 500) {
          if (requireNamespace("base64enc", quietly = TRUE)) {
            raw <- readBin(plot_file, "raw", file.info(plot_file)$size)
            plot_b64 <- base64enc::base64encode(raw)
          }
        }
      }, error = function(e) {
        tryCatch(grDevices::dev.off(), error = function(e2) NULL)
      })
    }

    # Chercher un data.frame tbl pour affichage tableau
    tbl_html <- NULL
    if (exists("tbl", envir = sandbox) && is.data.frame(sandbox$tbl)) {
      tbl_html <- tryCatch({
        knitr::kable(head(sandbox$tbl, 30), format = "html",
                     table.attr = 'class="table table-sm table-striped" style="font-size:11px;"')
      }, error = function(e) NULL)
    }

    result$success <- TRUE
    result$output <- if (!is.null(text_output) && length(text_output) > 0)
      paste(head(text_output, 80), collapse = "\n") else NULL
    result$plot_base64 <- plot_b64
    result$tbl_html <- tbl_html

  }, error = function(e) {
    result$error <<- conditionMessage(e)
    tryCatch(grDevices::dev.off(), error = function(e2) NULL)
  })

  # Cleanup
  tryCatch(file.remove(plot_file), error = function(e) NULL)

  return(result)
}

# Timeout helper
withTimeout <- function(expr, timeout = 45, onTimeout = "error") {
  if (requireNamespace("R.utils", quietly = TRUE)) {
    R.utils::withTimeout(expr, timeout = timeout, onTimeout = onTimeout)
  } else {
    expr
  }
}

# ============================================================================
# 4. EXTRACTION DE CODE R — FIX CRITIQUE v4.1 (regex multi-lignes)
# ============================================================================

extract_r_code <- function(response_text) {
  if (is.null(response_text) || !nzchar(response_text)) return(NULL)

  # (?s) = DOTALL : le point matche aussi les sauts de ligne
  # On cherche ```r ou ```R suivi de code puis ```
  pattern <- "(?s)```[rR]\\s*\\n(.*?)\\n?```"
  matches <- regmatches(response_text, gregexpr(pattern, response_text, perl = TRUE))[[1]]

  if (length(matches) == 0) {
    # Fallback : essayer sans le flag de langue
    pattern2 <- "(?s)```\\s*\\n(.*?)\\n?```"
    matches <- regmatches(response_text, gregexpr(pattern2, response_text, perl = TRUE))[[1]]
  }

  if (length(matches) == 0) return(NULL)

  # Extraire le contenu entre les balises
  codes <- gsub("(?s)```[rR]?\\s*\\n", "", matches, perl = TRUE)
  codes <- gsub("\\n?```$", "", codes, perl = TRUE)
  codes <- trimws(codes)

  # Filtrer les blocs vides
  codes <- codes[nzchar(codes)]
  if (length(codes) == 0) return(NULL)

  paste(codes, collapse = "\n\n")
}

# ============================================================================
# 5. MODULE SERVER PRINCIPAL (v4.1 — amélioré)
# ============================================================================

chatbot_server <- function(id, df, col_mapping = NULL, context_manager = NULL) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── STATE ──────────────────────────────────────────────
    chat <- reactiveValues(
      history = list(),
      conversation = list(),
      is_processing = FALSE,
      data_context = NULL,
      last_context_hash = NULL  # Cache du contexte
    )

    # ── WELCOME ────────────────────────────────────────────
    observe({
      if (length(chat$history) == 0) {
        welcome <- paste0(
          "\U0001f44b **Bienvenue !** Je suis NutriAnalyse IA Expert.\n\n",
          "\U0001f4ca **Je suis un vrai data scientist** — je génère et exécute du code R ",
          "pour analyser vos données en temps réel.\n\n",
          "Chargez vos données et posez-moi n'importe quelle question :\n\n",
          "- *\"Quelle est la prévalence de la malnutrition aiguë ?\"*\n",
          "- *\"Montre-moi la distribution des z-scores WHZ\"*\n",
          "- *\"Compare la malnutrition par région\"*\n",
          "- *\"Y a-t-il une corrélation entre FCS et rCSI ?\"*\n",
          "- *\"Fais une ACP sur mes variables numériques\"*\n",
          "- *\"Vérifie la qualité de mes données SMART\"*\n\n",
          "\U0001f4a1 Cliquez sur **\U0001f4ca Analyser mes données** pour un diagnostic complet !"
        )
        chat$history <- list(list(role = "assistant", content = welcome,
                                  timestamp = Sys.time(), type = "text"))
        chat$conversation <- list(list(role = "model", parts = list(list(text = welcome))))
      }
    })

    # ── CONTEXTE RÉACTIF DES DONNÉES (avec cache) ──────────
    rich_context <- reactive({
      d <- df()
      if (is.null(d)) return(NULL)

      # Cache basé sur hash des dimensions + noms de colonnes
      current_hash <- digest::digest(list(nrow(d), ncol(d), names(d)), algo = "md5")
      if (!is.null(chat$last_context_hash) && chat$last_context_hash == current_hash &&
          !is.null(chat$data_context)) {
        return(chat$data_context)
      }

      ctx <- build_rich_context(d)
      chat$data_context <- ctx
      chat$last_context_hash <- current_hash
      ctx
    })

    # ── RENDER MESSAGES ────────────────────────────────────
    output$messages_container <- renderUI({
      msgs <- chat$history
      if (length(msgs) == 0) return(NULL)

      tagList(lapply(seq_along(msgs), function(i) {
        msg <- msgs[[i]]
        if (msg$role == "assistant") {
          render_expert_message(msg, i, ns)
        } else {
          render_message(msg$role, msg$content, msg$timestamp)
        }
      }))
    })

    # ── Indicateur de chargement (sécurisé hors contexte Shiny) ──
    # shinyjs::show/hide échouent dans un callback later::later
    # ("could not find the Shiny session object") — on envoie le message
    # custom directement via la session du module.
    loading_show <- function() {
      tryCatch(
        session$sendCustomMessage(
          "shinyjs-show", list(id = session$ns("loading_indicator"))),
        error = function(e) NULL)
    }
    loading_hide <- function() {
      tryCatch(
        session$sendCustomMessage(
          "shinyjs-hide", list(id = session$ns("loading_indicator"))),
        error = function(e) NULL)
    }

    # ── APPEL GEMINI + EXÉCUTION CODE R (v4.2 — later async + RAG) ──
    process_question <- function(user_text) {
      if (is.null(user_text) || user_text == "") return()

      # Ajouter message utilisateur (toujours, même hors domaine)
      chat$history <- c(chat$history, list(
        list(role = "user", content = user_text, timestamp = Sys.time(), type = "text")
      ))
      chat$conversation <- c(chat$conversation, list(
        list(role = "user", parts = list(list(text = user_text)))
      ))

      # ── Filtre domaine (soft) — AVANT le garde is_processing pour que
      #    toute question hors domaine reçoive toujours une réponse ──────
      domain_ok <- tryCatch(rag_is_in_domain(user_text), error = function(e) TRUE)
      if (!domain_ok) {
        chat$history <- c(chat$history, list(list(
          role = "assistant",
          content = paste0(
            "\U0001f3af **Je suis spécialisé en :** nutrition, malnutrition, sécurité alimentaire, ",
            "épidémiologie et biostatistique.\n\n",
            "Votre question semble en dehors de ces domaines. Posez-moi une question sur les ",
            "**indicateurs nutritionnels** (MAG, z-scores, MUAC, FCS, rCSI…), les ",
            "**analyses de données de santé publique** ou l'**interprétation de modèles statistiques** ",
            "dans ce contexte. Je serai ravi d'aider !"
          ),
          timestamp = Sys.time(), type = "text"
        )))
        return()
      }

      if (chat$is_processing) return()

      chat$is_processing <- TRUE
      loading_show()

      updateTextInput(session, "user_input", value = "")

      # Snapshots — tous objets R sérialisables (aucun objet Shiny/réactif)
      context_snap      <- rich_context()
      conversation_snap <- chat$conversation
      api_key_snap      <- GEMINI_CONFIG$api_key
      model_snap        <- GEMINI_CONFIG$model
      timeout_snap      <- max(GEMINI_CONFIG$timeout, 60)
      df_snap           <- df()

      # ── RAG : récupérer les références pertinentes avec métadonnées ──
      rag_result <- tryCatch(
        rag_retrieve_with_meta(user_text, top_k = 4),
        error = function(e) list(context = "", sources = list())
      )
      rag_ctx     <- rag_result$context
      rag_sources <- rag_result$sources   # liste de {id, domain, label, source}

      # Construire system instruction = expert prompt + RAG + contexte données
      sys_prompt <- SYSTEM_PROMPT_EXPERT
      if (nzchar(rag_ctx)) {
        sys_prompt <- paste0(sys_prompt, "\n\n", rag_ctx)
      }
      if (!is.null(context_snap)) {
        sys_prompt <- paste0(sys_prompt, "\n\n", context_snap)
      }

      # ── Fonction d'appel Gemini (v4.1 — robuste) ──────────
      .call_gemini <- function() {
        url <- sprintf(
          "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent",
          model_snap
        )

        # Garder les 16 derniers messages pour le contexte
        contents <- tail(conversation_snap, 16)

        payload <- list(
          systemInstruction = list(parts = list(list(text = sys_prompt))),
          contents = contents,
          generationConfig = list(
            maxOutputTokens = 8192L,
            temperature = 0.35,
            topP = 0.92,
            topK = 40L
          ),
          safetySettings = list(
            list(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
            list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT", threshold = "BLOCK_NONE"),
            list(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE"),
            list(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE")
          )
        )

        resp <- httr::POST(
          url,
          httr::add_headers("Content-Type" = "application/json"),
          query = list(key = api_key_snap),
          body = jsonlite::toJSON(payload, auto_unbox = TRUE),
          httr::timeout(timeout_snap)
        )

        status <- httr::status_code(resp)

        if (status != 200L) {
          err_body <- tryCatch(
            jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)$error$message,
            error = function(e) "Erreur inconnue"
          )

          # Retry sur 429 (rate limit)
          if (status == 429L) {
            Sys.sleep(3)
            resp2 <- httr::POST(
              url,
              httr::add_headers("Content-Type" = "application/json"),
              query = list(key = api_key_snap),
              body = jsonlite::toJSON(payload, auto_unbox = TRUE),
              httr::timeout(timeout_snap)
            )
            if (httr::status_code(resp2) == 200L) {
              resp <- resp2
              status <- 200L
            } else {
              stop(sprintf("API Error (%d): %s", status, err_body))
            }
          } else {
            stop(sprintf("API Error (%d): %s", status, err_body))
          }
        }

        result <- jsonlite::fromJSON(
          httr::content(resp, as = "text", encoding = "UTF-8"),
          simplifyVector = FALSE
        )

        candidates <- result$candidates
        if (is.null(candidates) || length(candidates) == 0) {
          finish <- tryCatch(result$promptFeedback$blockReason, error = function(e) "UNKNOWN")
          stop(sprintf("Réponse bloquée par Gemini (raison: %s)", finish))
        }
        parts <- candidates[[1]]$content$parts
        if (is.null(parts) || length(parts) == 0) stop("Pas de contenu dans la réponse")
        parts[[1]]$text
      }

      # ── Succès (v4.1 — interprétation intégrée, pas de 2e appel) ──
      .on_success <- function(ai_response) {
        # Extraire et exécuter le code R si présent
        r_code <- extract_r_code(ai_response)

        if (!is.null(r_code) && !is.null(df_snap)) {
          # Exécuter le code R
          exec_result <- safe_execute_r(r_code, df_snap)

          # Construire le message enrichi
          msg <- list(
            role = "assistant",
            content = ai_response,
            timestamp = Sys.time(),
            type = "analysis",
            code = r_code,
            exec_success = exec_result$success,
            exec_output = exec_result$output,
            exec_plot = exec_result$plot_base64,
            exec_table = exec_result$tbl_html,
            exec_error = exec_result$error,
            exec_warnings = exec_result$warnings,
            rag_sources = rag_sources   # ← sources RAG pour affichage UI
          )

          # v4.1 : Si le code a échoué, faire un appel de correction
          if (!exec_result$success && !is.null(exec_result$error)) {
            correction_msg <- tryCatch({
              error_text <- exec_result$error
              # Demander à Gemini de corriger
              fix_prompt <- sprintf(
                "Le code R suivant a produit une erreur:\n\nCODE:\n```r\n%s\n```\n\nERREUR: %s\n\nCorrige le code R. Mets le code corrigé dans un bloc ```r ... ```. Explique brièvement la correction.",
                r_code, substr(error_text, 1, 500)
              )

              # Appel de correction
              fix_contents <- list(
                list(role = "user", parts = list(list(text = fix_prompt)))
              )
              fix_payload <- list(
                systemInstruction = list(parts = list(list(text = sys_prompt))),
                contents = fix_contents,
                generationConfig = list(
                  maxOutputTokens = 4096L,
                  temperature = 0.2,
                  topP = 0.9
                ),
                safetySettings = list(
                  list(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
                  list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT", threshold = "BLOCK_NONE"),
                  list(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE"),
                  list(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE")
                )
              )

              fix_url <- sprintf(
                "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent",
                model_snap
              )

              fix_resp <- httr::POST(
                fix_url,
                httr::add_headers("Content-Type" = "application/json"),
                query = list(key = api_key_snap),
                body = jsonlite::toJSON(fix_payload, auto_unbox = TRUE),
                httr::timeout(timeout_snap)
              )

              if (httr::status_code(fix_resp) == 200L) {
                fix_result <- jsonlite::fromJSON(
                  httr::content(fix_resp, as = "text", encoding = "UTF-8"),
                  simplifyVector = FALSE
                )
                fix_text <- fix_result$candidates[[1]]$content$parts[[1]]$text

                # Extraire le code corrigé
                fixed_code <- extract_r_code(fix_text)
                if (!is.null(fixed_code)) {
                  # Ré-exécuter le code corrigé
                  exec_result2 <- safe_execute_r(fixed_code, df_snap)
                  if (exec_result2$success) {
                    msg$code <<- fixed_code
                    msg$exec_success <<- TRUE
                    msg$exec_output <<- exec_result2$output
                    msg$exec_plot <<- exec_result2$plot_base64
                    msg$exec_table <<- exec_result2$tbl_html
                    msg$exec_error <<- NULL
                    msg$exec_warnings <<- exec_result2$warnings
                    msg$content <<- paste0(ai_response, "\n\n*\u2699\ufe0f Code auto-corrigé et ré-exécuté avec succès.*")
                  }
                }
              }
              NULL
            }, error = function(e) NULL)
          }

        } else {
          # Pas de code R — réponse textuelle simple
          msg <- list(
            role = "assistant",
            content = ai_response,
            timestamp = Sys.time(),
            type = "text",
            rag_sources = rag_sources   # ← sources RAG pour affichage UI
          )
        }

        chat$history <- c(chat$history, list(msg))
        chat$conversation <- c(
          chat$conversation,
          list(list(role = "model", parts = list(list(
            text = gsub("\\[\\[ACTION:[^]]*\\]\\]", "", ai_response)
          ))))
        )
        chat$is_processing <- FALSE
        loading_hide()
      }

      # ── Erreur ──────────────────────────────────────────
      .on_error <- function(err) {
        msg_text <- if (inherits(err, "condition")) conditionMessage(err) else as.character(err)
        friendly <- if (grepl("timeout|connexion|timed.out", msg_text, ignore.case = TRUE)) {
          "\u26a0\ufe0f Connexion échouée ou timeout. Vérifiez votre connexion internet et réessayez."
        } else if (grepl("401|403|api.key|PERMISSION", msg_text, ignore.case = TRUE)) {
          "\U0001f511 Erreur d'authentification API. Vérifiez votre clé Gemini dans config_api_secure_v2.R."
        } else if (grepl("429|rate|quota|RESOURCE_EXHAUSTED", msg_text, ignore.case = TRUE)) {
          "\u23f1\ufe0f Limite de requêtes atteinte. Attendez 30 secondes et réessayez."
        } else if (grepl("bloqu|safety|BLOCK", msg_text, ignore.case = TRUE)) {
          "\U0001f6ab Réponse bloquée par le filtre de sécurité. Reformulez votre question."
        } else {
          sprintf("\u274c Erreur IA : %s", substr(msg_text, 1, 300))
        }
        chat$history <- c(chat$history, list(
          list(role = "assistant", content = friendly, timestamp = Sys.time(), type = "error")
        ))
        chat$is_processing <- FALSE
        loading_hide()
      }

      # ── Exécution via later::later (delay=0) ─────────────────────
      # L'UI (loading indicator) est flushée avant l'appel API grâce à delay=0.
      # L'appel API bloque ~3-5s dans le même processus R (fiable, pas de
      # problème de sérialisation contrairement à future::multisession).
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() {
          shiny::isolate({
            tryCatch({
              ai_response <- .call_gemini()
              .on_success(ai_response)
            }, error = function(e) {
              .on_error(e)
            })
          })
        }, delay = 0)
      } else {
        # Fallback synchrone si later non disponible
        tryCatch({
          ai_response <- .call_gemini()
          .on_success(ai_response)
        }, error = function(e) {
          .on_error(e)
        })
      }

      NULL
    }

    # ── TRIGGERS ───────────────────────────────────────────

    observeEvent(input$send_button, {
      process_question(trimws(input$user_input %||% ""))
    })

    observeEvent(input$user_input_enter, {
      process_question(trimws(input$user_input %||% ""))
    })

    observeEvent(input$quick_analysis, {
      d <- df()
      if (is.null(d) || nrow(d) == 0) {
        showNotification("\U0001f4c2 Importez d'abord un jeu de données", type = "warning")
        return()
      }

      n_num <- sum(vapply(d, is.numeric, logical(1)))
      n_cat <- sum(vapply(d, function(x) is.character(x) || is.factor(x), logical(1)))

      auto_prompt <- paste0(
        "Fais un diagnostic complet de mon jeu de données (",
        nrow(d), " obs × ", ncol(d), " cols, ",
        n_num, " numériques, ", n_cat, " catégorielles). ",
        "Génère du code R pour : ",
        "1) Un résumé statistique des variables clés (summary + NA), ",
        "2) Un graphique pertinent qui montre la distribution ou les relations les plus importantes. ",
        "Interprète les résultats en détail dans le contexte nutrition/santé publique si applicable. ",
        "Signale tout problème de qualité des données."
      )
      process_question(auto_prompt)
    })

    observeEvent(input$quick_report, {
      d <- df()
      if (is.null(d) || nrow(d) == 0) {
        showNotification("\U0001f4c2 Importez d'abord un jeu de données", type = "warning")
        return()
      }
      process_question(
        "Génère un rapport synthétique : statistiques descriptives essentielles, qualité des données, et les patterns principaux. Inclus un tableau résumé et un graphique informatif. Fais une interprétation complète."
      )
    })

    observeEvent(input$clear_chat, {
      chat$history <- list(list(
        role = "assistant",
        content = "\U0001f504 Conversation effacée. Posez une nouvelle question !",
        timestamp = Sys.time(),
        type = "text"
      ))
      chat$conversation <- list()
      chat$data_context <- NULL
      chat$last_context_hash <- NULL
    })

    return(list(
      messages = reactive(chat$history),
      data_analysis = reactive(rich_context())
    ))
  })
}

cat("\u2705 Chatbot Server v4.2 Expert loaded (later-async + RAG + domain-filter)\n")
