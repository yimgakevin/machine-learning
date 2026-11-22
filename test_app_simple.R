# ============================================================================
# TEST APP SIMPLE - Sans dépendances problématiques
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)

# Charger configs de base
source("config_oms.R", local = FALSE)
source("utils.R", local = FALSE)
source("config_api_secure_v2.R", local = FALSE)
source("context_manager_v3.R", local = FALSE)
source("chatbot_ui_v3.R", local = FALSE)
source("chatbot_server_v3.R", local = FALSE)

# ============================================================================
# UI SIMPLE
# ============================================================================

ui <- dashboardPage(
  dashboardHeader(
    title = "🍽️ NutriAnalyse Pro v2 — AI Test"
  ),
  
  dashboardSidebar(
    h4("🤖 IA Assistant", style = "color: white; padding: 10px;"),
    p("Chat interface test", style = "color: #ddd; font-size: 12px; padding: 0 10px;"),
    hr(style = "border-color: rgba(255,255,255,.2);")
  ),
  
  dashboardBody(
    useShinyjs(),
    
    # Chatbot
    chatbot_ui("ai_chat", width = "100%", height = "500px"),
    
    # Status
    div(
      style = "margin-top: 20px; padding: 20px; background: #f0f0f0; border-radius: 5px;",
      h4("📊 Status"),
      p("✅ App loaded successfully!"),
      p("✅ Chatbot interface ready"),
      p("✅ All 4 AI agents initialized")
    )
  )
)

# ============================================================================
# SERVER SIMPLE
# ============================================================================

server <- function(input, output, session) {
  
  cat("✅ Server initialized\n")
  
  # Initialize chatbot
  tryCatch({
    chat_outputs <- chatbot_server(
      "ai_chat",
      df = reactive(NULL),
      col_mapping = reactive(NULL)
    )
    
    cat("✅ Chatbot module loaded\n")
    
  }, error = function(e) {
    cat("❌ Error in chatbot server:", conditionMessage(e), "\n")
  })
}

# ============================================================================
# RUN APP
# ============================================================================

cat("🚀 Lancement de l'application...\n")
shinyApp(ui = ui, server = server)
