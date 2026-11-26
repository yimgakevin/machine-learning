# ============================================================================
# Chatbot UI v3 - Interface Chat IA pour sidebar (corrigée)
# ============================================================================

library(shiny)
library(shinyjs)

# ============================================================================
# 1. CHATBOT UI MODULE
# ============================================================================

chatbot_ui <- function(id, width = "100%", height = "400px") {
  
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(HTML(get_chatbot_css()))),
    
    div(
      class = "chatbot-container",
      style = sprintf("width: %s;", width),
      
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
          tags$span(class = "spinner-dots", "●●●"),
          tags$span("IA en réflexion...", style = "margin-left: 6px; font-size: 11px; color: #888;")
        )
      ),
      
      # Zone de saisie — textInput Shiny (pas tags$input)
      div(
        class = "chatbot-input-area",
        div(
          class = "input-group",
          style = "display: flex; gap: 4px;",
          textInput(ns("user_input"), label = NULL,
                    placeholder = "Posez votre question...",
                    width = "100%"),
          actionButton(ns("send_button"), label = NULL,
                       icon = icon("paper-plane"),
                       class = "btn btn-primary btn-sm",
                       style = "height: 38px; margin-top: 0px;")
        )
      ),
      
      # Boutons rapides
      div(
        class = "chatbot-quick-actions",
        style = "display: flex; gap: 4px; flex-wrap: wrap; margin-top: 6px;",
        actionButton(ns("quick_analysis"), "📊 Proposer analyse",
                     class = "btn btn-outline-primary btn-xs",
                     style = "font-size: 10px; padding: 2px 6px;"),
        actionButton(ns("quick_report"), "📄 Rapport",
                     class = "btn btn-outline-success btn-xs",
                     style = "font-size: 10px; padding: 2px 6px;"),
        actionButton(ns("clear_chat"), "🗑️ Effacer",
                     class = "btn btn-outline-danger btn-xs",
                     style = "font-size: 10px; padding: 2px 6px;")
      )
    )
  )
}

# ============================================================================
# 2. MESSAGE RENDERING
# ============================================================================

# Rendu d'un message assistant avec boutons d'action [[ACTION:id|Texte]]
render_assistant_message <- function(content, timestamp = NULL, msg_index = 1, ns = NS(NULL)) {

  # Extraire les boutons [[ACTION:id|Texte]]
  action_pattern <- "\\[\\[ACTION:([^|\\]]+)\\|([^\\]]+)\\]\\]"
  actions <- regmatches(content, gregexpr(action_pattern, content, perl = TRUE))[[1]]

  # Nettoyer le texte (supprimer les balises action)
  clean_text <- gsub(action_pattern, "", content, perl = TRUE)
  clean_text <- trimws(clean_text)

  # Construire les boutons d'action
  action_buttons <- if (length(actions) > 0) {
    btn_list <- lapply(seq_along(actions), function(j) {
      m <- regmatches(actions[j],
                      regexec(action_pattern, actions[j], perl = TRUE))[[1]]
      tab_id  <- m[2]
      btn_lbl <- m[3]
      # inputId global transmis au server via updateTabItems
      btn_id  <- ns(paste0("goto_tab"))
      tags$button(
        class   = "btn btn-sm btn-outline-primary ai-action-btn",
        style   = "font-size:11px;margin:2px 3px 2px 0;padding:3px 8px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
          btn_id, tab_id
        ),
        HTML(btn_lbl)
      )
    })
    tags$div(
      style = "margin-top:6px; display:flex; flex-wrap:wrap; gap:2px;",
      tagList(btn_list)
    )
  } else NULL

  # Bulle assistante complète
  div(
    class = "chat-msg chat-msg-ai",
    div(class = "chat-avatar", icon("robot")),
    div(
      class = "chat-bubble",
      if (!is.null(timestamp))
        tags$small(class = "chat-time", format(timestamp, "%H:%M")),
      div(class = "chat-text", HTML(markdown_to_html(clean_text))),
      action_buttons
    )
  )
}

render_message <- function(role, content, timestamp = NULL) {
  
  is_user <- role == "user"
  
  div(
    class = paste("chat-msg", if (is_user) "chat-msg-user" else "chat-msg-ai"),
    
    # Avatar
    div(
      class = "chat-avatar",
      if (is_user) icon("user") else icon("robot")
    ),
    
    # Contenu
    div(
      class = "chat-bubble",
      if (!is.null(timestamp)) {
        tags$small(class = "chat-time", format(timestamp, "%H:%M"))
      },
      div(class = "chat-text", HTML(markdown_to_html(content)))
    )
  )
}

# ============================================================================
# 3. CSS
# ============================================================================

get_chatbot_css <- function() {
  "
  .chatbot-container {
    background: #fff;
    border-radius: 8px;
    border: 1px solid rgba(255,255,255,.15);
    overflow: hidden;
  }
  .chatbot-messages {
    background: #f8f9fa;
    padding: 10px;
  }
  .chat-msg {
    display: flex;
    gap: 8px;
    margin-bottom: 10px;
    animation: fadeIn .3s ease;
  }
  .chat-msg-user { flex-direction: row-reverse; }
  .chat-msg-user .chat-bubble {
    background: #008DC9;
    color: white;
    border-radius: 12px 4px 12px 12px;
  }
  .chat-msg-ai .chat-bubble {
    background: white;
    color: #333;
    border: 1px solid #e0e0e0;
    border-radius: 4px 12px 12px 12px;
  }
  .chat-avatar {
    width: 28px;
    height: 28px;
    min-width: 28px;
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 50%;
    background: rgba(0,141,201,.1);
    font-size: 13px;
    color: #008DC9;
  }
  .chat-msg-ai .chat-avatar { color: #28a745; background: rgba(40,167,69,.1); }
  .chat-bubble {
    max-width: 85%;
    padding: 8px 12px;
    font-size: 12px;
    line-height: 1.5;
    word-wrap: break-word;
  }
  .chat-bubble code {
    background: rgba(0,0,0,.06);
    padding: 1px 4px;
    border-radius: 3px;
    font-size: 11px;
  }
  .chat-bubble pre {
    background: rgba(0,0,0,.08);
    padding: 8px;
    border-radius: 4px;
    overflow-x: auto;
    margin: 6px 0;
    font-size: 11px;
  }
  .chat-time { font-size: 9px; color: #aaa; display: block; margin-bottom: 2px; }
  .loading-indicator {
    padding: 8px 12px;
    font-size: 12px;
    color: #888;
    display: flex;
    align-items: center;
  }
  .spinner-dots {
    animation: blink 1.4s infinite both;
    font-size: 14px;
    color: #008DC9;
  }
  @keyframes blink {
    0%, 80%, 100% { opacity: 0; }
    40% { opacity: 1; }
  }
  .chatbot-input-area {
    padding: 8px 10px;
    border-top: 1px solid #eee;
    background: white;
  }
  .chatbot-input-area .form-group { margin-bottom: 0 !important; }
  .chatbot-input-area .form-control {
    font-size: 12px !important;
    padding: 6px 10px !important;
    height: 38px !important;
  }
  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(6px); }
    to { opacity: 1; transform: translateY(0); }
  }
  .ai-action-btn {
    border: 1px solid #008DC9 !important;
    color: #008DC9 !important;
    background: white !important;
    border-radius: 12px !important;
    transition: all .15s;
    line-height: 1.2;
  }
  .ai-action-btn:hover {
    background: #008DC9 !important;
    color: white !important;
  }
  "
}

# ============================================================================
# 4. MARKDOWN TO HTML
# ============================================================================

markdown_to_html <- function(markdown_text) {
  
  html <- markdown_text
  
  # Code blocks
  html <- gsub("```[a-zA-Z]*\n(.*?)\n```", "<pre><code>\\1</code></pre>", html, perl = TRUE)
  
  # Inline code
  html <- gsub("`([^`]+)`", "<code>\\1</code>", html)
  
  # Bold
  html <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", html)
  
  # Italic
  html <- gsub("\\*(.*?)\\*", "<em>\\1</em>", html)
  
  # Headers
  html <- gsub("(?m)^### (.*?)$", "<h5>\\1</h5>", html, perl = TRUE)
  html <- gsub("(?m)^## (.*?)$", "<h4>\\1</h4>", html, perl = TRUE)
  html <- gsub("(?m)^# (.*?)$", "<h3>\\1</h3>", html, perl = TRUE)
  
  # Lists
  html <- gsub("(?m)^- (.*?)$", "<li>\\1</li>", html, perl = TRUE)
  
  # Line breaks
  html <- gsub("\n\n", "<br/><br/>", html)
  html <- gsub("\n", "<br/>", html)
  
  return(html)
}

cat("✅ Chatbot UI v3 loaded\n")
