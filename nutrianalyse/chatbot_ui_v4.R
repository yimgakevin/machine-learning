# ============================================================================
# Chatbot UI v4 — Interface Expert avec support graphiques/tableaux/code
# ============================================================================

library(shiny)
library(shinyjs)

# ============================================================================
# 1. CHATBOT UI MODULE
# ============================================================================

chatbot_ui <- function(id, width = "100%", height = "450px") {

  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML(get_chatbot_css_v4()))),

    # Script pour Enter key + auto-scroll + auto-focus
    tags$script(HTML(sprintf("
      $(document).on('keypress', '#%s', function(e) {
        if (e.which == 13 && !e.shiftKey) {
          e.preventDefault();
          Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
        }
      });
      // Auto-scroll vers le bas quand nouveau message
      $(document).on('shiny:value', function(e) {
        if (e.name && e.name.indexOf('messages_container') > -1) {
          setTimeout(function() {
            var container = document.getElementById('%s');
            if (container) container.scrollTop = container.scrollHeight;
          }, 150);
        }
      });
    ", ns("user_input"), ns("user_input_enter"), ns("messages_scroll")))),

    div(
      class = "chatbot-container",
      style = sprintf("width: %s;", width),

      # Header
      div(
        class = "chatbot-header",
        tags$span(class = "chatbot-title", "\U0001f9e0 NutriAnalyse IA Expert"),
        tags$span(class = "chatbot-badge", "Data Scientist")
      ),

      # Zone messages
      div(
        class = "chatbot-messages",
        id = ns("messages_scroll"),
        style = sprintf("height: %s; overflow-y: auto;", height),
        uiOutput(ns("messages_container"))
      ),

      # Indicateur de chargement
      hidden(
        div(
          id = ns("loading_indicator"),
          class = "loading-indicator",
          div(class = "typing-dots",
              tags$span(), tags$span(), tags$span()
          ),
          tags$span("\U0001f9ea Analyse en cours...",
                    style = "margin-left: 8px; font-size: 11px; color: #666;")
        )
      ),

      # Zone de saisie
      div(
        class = "chatbot-input-area",
        div(
          class = "input-group",
          style = "display: flex; gap: 4px; align-items: flex-end;",
          textInput(ns("user_input"), label = NULL,
                    placeholder = "Posez votre question sur les données...",
                    width = "100%"),
          actionButton(ns("send_button"), label = NULL,
                       icon = icon("paper-plane"),
                       class = "btn btn-primary btn-sm",
                       style = "height: 38px; min-width: 42px;")
        )
      ),

      # Boutons rapides
      div(
        class = "chatbot-quick-actions",
        actionButton(ns("quick_analysis"), "\U0001f4ca Analyser mes données",
                     class = "btn btn-outline-primary btn-xs chatbot-quick-btn"),
        actionButton(ns("quick_report"), "\U0001f4c4 Rapport synthétique",
                     class = "btn btn-outline-success btn-xs chatbot-quick-btn"),
        actionButton(ns("clear_chat"), "\U0001f5d1\ufe0f Effacer",
                     class = "btn btn-outline-danger btn-xs chatbot-quick-btn")
      )
    )
  )
}

# ============================================================================
# 2. MESSAGE RENDERING — Expert Messages avec code/plots/tables
# ============================================================================

render_expert_message <- function(msg, msg_index = 1, ns = NS(NULL)) {
  content <- msg$content
  msg_type <- msg$type %||% "text"

  # Extraire les boutons [[ACTION:id|Texte]]
  action_pattern <- "\\[\\[ACTION:([^|\\]]+)\\|([^\\]]+)\\]\\]"
  actions <- tryCatch(
    regmatches(content, gregexpr(action_pattern, content, perl = TRUE))[[1]],
    error = function(e) character(0)
  )

  # Nettoyer le texte des balises action et code
  clean_text <- gsub(action_pattern, "", content, perl = TRUE)
  # Retirer les blocs de code R (ils seront affichés séparément avec résultats)
  # (?s) = DOTALL pour matcher les sauts de ligne dans le code
  display_text <- gsub("(?s)```[rR]?\\s*\\n.*?```", "", clean_text, perl = TRUE)
  display_text <- trimws(display_text)

  # Construire les boutons d'action
  action_buttons <- if (length(actions) > 0) {
    btn_list <- lapply(seq_along(actions), function(j) {
      m <- regmatches(actions[j],
                      regexec(action_pattern, actions[j], perl = TRUE))[[1]]
      tab_id <- m[2]
      btn_lbl <- m[3]
      btn_id <- ns("goto_tab")
      tags$button(
        class = "btn btn-sm btn-outline-primary ai-action-btn",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
          btn_id, tab_id
        ),
        HTML(btn_lbl)
      )
    })
    tags$div(class = "ai-action-buttons", tagList(btn_list))
  } else NULL

  # Contenu principal
  content_parts <- list()

  # Texte principal (markdown converti en HTML)
  if (nzchar(display_text)) {
    content_parts <- c(content_parts, list(
      div(class = "chat-text", HTML(markdown_to_html_v4(display_text)))
    ))
  }

  # Si c'est une analyse avec résultats d'exécution
  if (msg_type == "analysis") {

    # Code R exécuté (collapsible)
    if (!is.null(msg$code)) {
      content_parts <- c(content_parts, list(
        tags$details(
          class = "code-details",
          tags$summary(class = "code-summary", "\U0001f4bb Code R exécuté"),
          tags$pre(class = "code-block", tags$code(msg$code))
        )
      ))
    }

    # Erreur d'exécution
    if (!isTRUE(msg$exec_success) && !is.null(msg$exec_error)) {
      content_parts <- c(content_parts, list(
        div(class = "exec-error",
            tags$strong("\u26a0\ufe0f Erreur d'exécution : "),
            tags$span(msg$exec_error))
      ))
    }

    # Sortie texte — VRAIS résultats d'exécution (toujours ouverts et visibles)
    if (isTRUE(msg$exec_success) && !is.null(msg$exec_output) && nzchar(msg$exec_output)) {
      content_parts <- c(content_parts, list(
        div(class = "exec-results-real",
          tags$div(class = "exec-results-header",
            style = "background:#e8f5e9;padding:6px 10px;border-radius:6px 6px 0 0;font-weight:600;color:#2e7d32;font-size:13px;",
            "\u2705 Résultats réels (données exécutées)"
          ),
          tags$pre(class = "output-block",
            style = "margin-top:0;border-radius:0 0 6px 6px;border-top:none;",
            msg$exec_output)
        )
      ))
    }

    # Tableau HTML
    if (isTRUE(msg$exec_success) && !is.null(msg$exec_table)) {
      content_parts <- c(content_parts, list(
        div(class = "exec-table", HTML(msg$exec_table))
      ))
    }

    # Graphique
    if (isTRUE(msg$exec_success) && !is.null(msg$exec_plot)) {
      content_parts <- c(content_parts, list(
        div(class = "exec-plot",
            tags$img(
              src = paste0("data:image/png;base64,", msg$exec_plot),
              style = "max-width: 100%; border-radius: 6px; margin: 8px 0;",
              alt = "Graphique d'analyse"
            ))
      ))
    }

    # Warnings
    if (length(msg$exec_warnings) > 0) {
      content_parts <- c(content_parts, list(
        div(class = "exec-warnings",
            tags$small(paste("\u26a0\ufe0f", paste(msg$exec_warnings, collapse = " | "))))
      ))
    }

    # Interprétation IA des résultats
    if (!is.null(msg$interpretation)) {
      content_parts <- c(content_parts, list(
        div(class = "interpretation-block",
            tags$div(class = "interpretation-header", "\U0001f9e0 Interprétation Expert"),
            div(class = "interpretation-text",
                HTML(markdown_to_html_v4(msg$interpretation)))
        )
      ))
    }
  }

  # ── Indicateur RAG — sources scientifiques utilisées ─────────────
  if (!is.null(msg$rag_sources) && length(msg$rag_sources) > 0) {
    domain_colors <- c(
      malnutrition  = "#c9161d",
      food_security = "#e67e22",
      epidemiology  = "#008DC9",
      statistics    = "#27ae60"
    )
    domain_icons <- c(
      malnutrition  = "\U0001f9ec",
      food_security = "\U0001f33e",
      epidemiology  = "\U0001f52c",
      statistics    = "\U0001f4ca"
    )

    badges <- lapply(msg$rag_sources, function(src) {
      col   <- domain_colors[[src$domain]] %||% "#607d8b"
      icn   <- domain_icons[[src$domain]]  %||% "\U0001f4d6"
      label <- paste(icn, tools::toTitleCase(src$label))
      tags$span(
        class = "rag-badge",
        title = src$source,          # tooltip au survol
        style = sprintf(
          "background:%s22;border:1px solid %s;color:%s;",
          col, col, col
        ),
        label
      )
    })

    content_parts <- c(content_parts, list(
      tags$div(
        class = "rag-sources-block",
        tags$span(class = "rag-sources-label", "\U0001f4da Références RAG :"),
        tagList(badges)
      )
    ))
  }

  # Ajouter boutons d'action
  if (!is.null(action_buttons)) {
    content_parts <- c(content_parts, list(action_buttons))
  }

  # Bulle complète
  div(
    class = "chat-msg chat-msg-ai",
    div(class = "chat-avatar", icon("robot")),
    div(
      class = "chat-bubble chat-bubble-expert",
      if (!is.null(msg$timestamp))
        tags$small(class = "chat-time", format(msg$timestamp, "%H:%M")),
      tagList(content_parts)
    )
  )
}

render_message <- function(role, content, timestamp = NULL) {
  is_user <- role == "user"
  div(
    class = paste("chat-msg", if (is_user) "chat-msg-user" else "chat-msg-ai"),
    div(class = "chat-avatar",
        if (is_user) icon("user") else icon("robot")),
    div(
      class = paste("chat-bubble", if (is_user) "chat-bubble-user" else ""),
      if (!is.null(timestamp))
        tags$small(class = "chat-time", format(timestamp, "%H:%M")),
      div(class = "chat-text", HTML(markdown_to_html_v4(content)))
    )
  )
}

# ============================================================================
# 3. MARKDOWN TO HTML (amélioré)
# ============================================================================

markdown_to_html_v4 <- function(text) {
  if (is.null(text) || !nzchar(text)) return("")

  html <- text

  # Code blocks
  html <- gsub("```[a-zA-Z]*\n(.*?)\n```", "<pre class='md-code'><code>\\1</code></pre>", html, perl = TRUE)
  # Inline code
  html <- gsub("`([^`]+)`", "<code class='md-inline-code'>\\1</code>", html)
  # Bold
  html <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", html)
  # Italic
  html <- gsub("\\*(.*?)\\*", "<em>\\1</em>", html)
  # Headers
  html <- gsub("(?m)^### (.*?)$", "<h5 class='md-h5'>\\1</h5>", html, perl = TRUE)
  html <- gsub("(?m)^## (.*?)$", "<h4 class='md-h4'>\\1</h4>", html, perl = TRUE)
  html <- gsub("(?m)^# (.*?)$", "<h3 class='md-h3'>\\1</h3>", html, perl = TRUE)
  # Numbered lists
  html <- gsub("(?m)^(\\d+)\\. (.*?)$", "<li class='md-li-num'>\\2</li>", html, perl = TRUE)
  # Bullet lists
  html <- gsub("(?m)^- (.*?)$", "<li class='md-li'>\\1</li>", html, perl = TRUE)
  # Line breaks
  html <- gsub("\n\n", "<br/><br/>", html)
  html <- gsub("\n", "<br/>", html)

  return(html)
}

# ============================================================================
# 4. CSS — Thème Expert
# ============================================================================

get_chatbot_css_v4 <- function() {
  "
  .chatbot-container {
    background: #fff;
    border-radius: 10px;
    border: 1px solid #e2e8f0;
    overflow: hidden;
    box-shadow: 0 2px 8px rgba(0,0,0,.08);
  }
  .chatbot-header {
    background: linear-gradient(135deg, #00426A, #008DC9);
    color: white;
    padding: 10px 14px;
    display: flex;
    align-items: center;
    justify-content: space-between;
  }
  .chatbot-title { font-weight: 700; font-size: 13px; }
  .chatbot-badge {
    background: rgba(255,255,255,.2);
    padding: 2px 8px;
    border-radius: 10px;
    font-size: 10px;
    font-weight: 600;
  }
  .chatbot-messages {
    background: #f8f9fb;
    padding: 12px;
  }
  .chat-msg {
    display: flex;
    gap: 8px;
    margin-bottom: 12px;
    animation: fadeIn .3s ease;
  }
  .chat-msg-user { flex-direction: row-reverse; }
  .chat-bubble-user {
    background: #008DC9 !important;
    color: white !important;
    border-radius: 14px 4px 14px 14px !important;
  }
  .chat-bubble-expert {
    background: white;
    color: #333;
    border: 1px solid #e0e0e0;
    border-radius: 4px 14px 14px 14px;
  }
  .chat-avatar {
    width: 30px; height: 30px; min-width: 30px;
    display: flex; align-items: center; justify-content: center;
    border-radius: 50%;
    font-size: 13px;
  }
  .chat-msg-user .chat-avatar { background: rgba(0,141,201,.1); color: #008DC9; }
  .chat-msg-ai .chat-avatar { background: rgba(40,167,69,.1); color: #28a745; }
  .chat-bubble {
    max-width: 90%;
    padding: 10px 14px;
    font-size: 12px;
    line-height: 1.6;
    word-wrap: break-word;
  }
  .chat-time { font-size: 9px; color: #aaa; display: block; margin-bottom: 3px; }

  /* Code blocks */
  .code-details { margin: 8px 0; }
  .code-summary {
    cursor: pointer;
    font-size: 11px;
    color: #666;
    padding: 4px 8px;
    background: #f0f4f8;
    border-radius: 4px;
    user-select: none;
  }
  .code-summary:hover { background: #e2e8f0; }
  .code-block {
    background: #1e293b;
    color: #e2e8f0;
    padding: 10px 12px;
    border-radius: 6px;
    font-size: 11px;
    overflow-x: auto;
    margin-top: 4px;
    line-height: 1.5;
  }

  /* Output blocks */
  .output-details { margin: 6px 0; }
  .output-summary {
    cursor: pointer;
    font-size: 11px;
    color: #0369a1;
    padding: 4px 8px;
    background: #f0f9ff;
    border-radius: 4px;
    font-weight: 600;
  }
  .output-block {
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    padding: 8px 10px;
    border-radius: 4px;
    font-size: 10px;
    overflow-x: auto;
    max-height: 200px;
    overflow-y: auto;
    margin-top: 4px;
  }

  /* Exec results */
  .exec-error {
    background: #fef2f2;
    border: 1px solid #fecaca;
    color: #991b1b;
    padding: 6px 10px;
    border-radius: 4px;
    font-size: 11px;
    margin: 6px 0;
  }
  .exec-table {
    overflow-x: auto;
    margin: 8px 0;
    border-radius: 4px;
    border: 1px solid #e2e8f0;
  }
  .exec-table table { margin: 0 !important; }
  .exec-plot { margin: 8px 0; text-align: center; }
  .exec-warnings {
    color: #92400e;
    font-size: 10px;
    margin: 4px 0;
    padding: 3px 8px;
    background: #fffbeb;
    border-radius: 3px;
  }

  /* Interpretation */
  .interpretation-block {
    background: linear-gradient(135deg, #f0fdf4, #ecfdf5);
    border: 1px solid #86efac;
    border-radius: 6px;
    padding: 10px 12px;
    margin: 8px 0;
  }
  .interpretation-header {
    font-weight: 700;
    font-size: 12px;
    color: #166534;
    margin-bottom: 6px;
  }
  .interpretation-text {
    font-size: 12px;
    line-height: 1.6;
    color: #14532d;
  }

  /* Action buttons */
  .ai-action-buttons {
    margin-top: 8px;
    display: flex;
    flex-wrap: wrap;
    gap: 4px;
  }
  .ai-action-btn {
    border: 1px solid #008DC9 !important;
    color: #008DC9 !important;
    background: white !important;
    border-radius: 14px !important;
    font-size: 10px !important;
    padding: 3px 10px !important;
    transition: all .15s;
    line-height: 1.3;
  }
  .ai-action-btn:hover {
    background: #008DC9 !important;
    color: white !important;
  }

  /* Input area */
  .chatbot-input-area {
    padding: 10px 12px;
    border-top: 1px solid #e2e8f0;
    background: white;
  }
  .chatbot-input-area .form-group { margin-bottom: 0 !important; }
  .chatbot-input-area .form-control {
    font-size: 12px !important;
    padding: 8px 12px !important;
    height: 38px !important;
    border-radius: 8px !important;
  }

  /* Quick actions */
  .chatbot-quick-actions {
    display: flex;
    gap: 4px;
    flex-wrap: wrap;
    padding: 6px 12px 10px;
    background: white;
  }
  .chatbot-quick-btn {
    font-size: 10px !important;
    padding: 3px 8px !important;
    border-radius: 12px !important;
  }

  /* Loading */
  .loading-indicator {
    padding: 10px 14px;
    display: flex;
    align-items: center;
  }
  .typing-dots {
    display: flex;
    gap: 3px;
  }
  .typing-dots span {
    width: 6px; height: 6px;
    background: #008DC9;
    border-radius: 50%;
    animation: typing 1.4s infinite both;
  }
  .typing-dots span:nth-child(2) { animation-delay: 0.2s; }
  .typing-dots span:nth-child(3) { animation-delay: 0.4s; }
  @keyframes typing {
    0%, 80%, 100% { opacity: 0.3; transform: scale(0.8); }
    40% { opacity: 1; transform: scale(1); }
  }

  /* Markdown helpers */
  .md-h3 { font-size: 14px; font-weight: 700; margin: 8px 0 4px; color: #1e293b; }
  .md-h4 { font-size: 13px; font-weight: 700; margin: 6px 0 3px; color: #334155; }
  .md-h5 { font-size: 12px; font-weight: 700; margin: 4px 0 2px; color: #475569; }
  .md-li, .md-li-num { margin-left: 16px; margin-bottom: 2px; }
  .md-inline-code {
    background: rgba(0,0,0,.06);
    padding: 1px 4px;
    border-radius: 3px;
    font-size: 11px;
  }
  .md-code {
    background: rgba(0,0,0,.06);
    padding: 8px;
    border-radius: 4px;
    overflow-x: auto;
    font-size: 11px;
  }

  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(4px); }
    to { opacity: 1; transform: translateY(0); }
  }

  /* ── RAG Sources indicator ── */
  .rag-sources-block {
    margin-top: 10px;
    padding: 6px 8px;
    background: #f8f9fb;
    border-top: 1px solid #e8ecf0;
    border-radius: 0 0 6px 6px;
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 5px;
  }
  .rag-sources-label {
    font-size: 10px;
    font-weight: 700;
    color: #888;
    letter-spacing: 0.3px;
    white-space: nowrap;
    margin-right: 2px;
  }
  .rag-badge {
    display: inline-flex;
    align-items: center;
    padding: 2px 8px;
    border-radius: 10px;
    font-size: 10px;
    font-weight: 600;
    cursor: help;
    transition: opacity .15s;
    white-space: nowrap;
  }
  .rag-badge:hover { opacity: .75; }
  "
}

cat("\u2705 Chatbot UI v4 Expert loaded\n")
