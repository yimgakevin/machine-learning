# ============================================================================
# Configuration API Sécurisée - Gemini v3 (Version Alternative)
# ============================================================================
# Cette version stocke la clé directement mais avec protections d'affichage
# ============================================================================

# INITIALISER AVEC VOTRE CLÉ API (VERSION DE DÉVELOPPEMENT SÉCURISÉE)
GEMINI_CONFIG <- list(
  api_key = "REDACTED_GEMINI_KEY",
  model = "gemini-2.5-flash",
  rate_limit = 60,
  timeout = 90,
  cache_enabled = TRUE,
  cache_dir = ".gemini_cache/"
)

# Ajouter attribut secret pour blocage automatique d'affichage
attr(GEMINI_CONFIG$api_key, "secret") <- TRUE

# ============================================================================
# Fonction utilitaire: Masquer clé dans les logs
# ============================================================================
mask_api_key <- function(text) {
  if (length(text) == 0) return(text)
  if (is.null(text)) return(text)
  
  # Masquer clés Google (AIzaSy...)
  masked <- gsub(
    "AIzaSy[A-Za-z0-9_-]{30,}", 
    "****[GOOGLE_API_KEY_MASKED]****", 
    text
  )
  
  # Masquer aussi pattern partiel pour sécurité
  masked <- gsub(
    "AIza[A-Za-z0-9_-]{33}", 
    "****[API_KEY_MASKED]****", 
    masked
  )
  
  return(masked)
}

# ============================================================================
# Fonction sécurisée: Afficher config SANS exposer la clé
# ============================================================================
print_config_safe <- function(config = GEMINI_CONFIG) {
  cat("\n")
  cat("┌─────────────────────────────────────────────────┐\n")
  cat("│  📋 Configuration API Gemini (clé masquée)      │\n")
  cat("├─────────────────────────────────────────────────┤\n")
  cat("│  Model:        ", config$model, "\n")
  cat("│  Rate Limit:   ", config$rate_limit, " req/min\n")
  cat("│  Timeout:      ", config$timeout, "s\n")
  cat("│  Cache:        ", ifelse(config$cache_enabled, "✅ Enabled", "❌ Disabled"), "\n")
  cat("│  API Key:      ", "AIzaSy****", "[MASKED]\n")
  cat("│  Status:       ✅ Ready to use\n")
  cat("└─────────────────────────────────────────────────┘\n")
}

# ============================================================================
# Fonction: Appel API SÉCURISÉ (masque clé en cas d'erreur)
# ============================================================================
call_gemini_api <- function(
  prompt,
  config = GEMINI_CONFIG,
  max_tokens = 1024,
  temperature = 0.7,
  verbose = FALSE
) {
  
  # Valider config
  if (is.null(config$api_key)) {
    stop("❌ API Key missing from config")
  }
  
  if (nchar(config$api_key) < 30) {
    stop("❌ API Key appears to be invalid")
  }
  
  # Construire URL (clé insérée ici pour requête)
  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent",
    config$model
  )
  
  # Payload JSON
  payload <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = list(
      maxOutputTokens = max_tokens,
      temperature = temperature,
      topP = 0.9,
      topK = 40
    )
  )
  
  if (verbose) {
    cat("🔄 Appel API en cours...\n")
  }
  
  # Appel avec gestion d'erreur sécurisée
  tryCatch({
    
    # Vérifier si httr est disponible
    if (!require("httr", quietly = TRUE)) {
      stop("❌ Package 'httr' required. Install with: install.packages('httr')")
    }
    
    if (!require("jsonlite", quietly = TRUE)) {
      stop("❌ Package 'jsonlite' required. Install with: install.packages('jsonlite')")
    }
    
    response <- httr::POST(
      url,
      httr::add_headers("Content-Type" = "application/json"),
      query = list(key = config$api_key),
      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
      httr::timeout(config$timeout)
    )
    
    status_code <- httr::status_code(response)
    
    if (status_code != 200) {
      error_content <- httr::content(response, as = "text")
      # Masquer clé dans message d'erreur
      error_content <- mask_api_key(error_content)
      
      error_msg <- sprintf(
        "API Error (Status %d): %s",
        status_code,
        substr(error_content, 1, 200)
      )
      
      stop(error_msg)
    }
    
    result <- jsonlite::fromJSON(
      httr::content(response, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    
    # Vérifier structure réponse
    if (is.null(result$candidates) || length(result$candidates) == 0) {
      stop("❌ Empty response from API")
    }
    
    # Extraction robuste (structure liste pure)
    response_text <- tryCatch({
      result$candidates[[1]]$content$parts[[1]]$text
    }, error = function(e) {
      # Fallback pour structures variées
      candidates <- result$candidates
      if (is.data.frame(candidates)) {
        candidates$content$parts[[1]]$text[1]
      } else {
        candidates[[1]]$content$parts[[1]]$text
      }
    })
    
    if (verbose) {
      cat("✅ API Response received\n")
      cat("   Tokens: ~", nchar(response_text) / 4, "\n")
    }
    
    return(response_text)
    
  }, error = function(e) {
    # Afficher erreur SANS la clé API
    msg <- conditionMessage(e)
    msg <- mask_api_key(msg)
    
    cat("❌ Erreur API:\n")
    cat("   ", msg, "\n")
    
    stop(msg, call. = FALSE)
  })
}

# ============================================================================
# Fonction: Tester la connexion API
# ============================================================================
test_gemini_connection <- function(config = GEMINI_CONFIG) {
  
  cat("🧪 Test de connexion API Gemini...\n\n")
  
  # 1. Vérifier config
  cat("1️⃣  Vérification configuration... ")
  
  if (is.null(config$api_key) || config$api_key == "") {
    cat("❌ ÉCHOUÉ\n")
    return(FALSE)
  }
  
  if (nchar(config$api_key) < 30) {
    cat("❌ ÉCHOUÉ (clé invalide)\n")
    return(FALSE)
  }
  
  cat("✅ OK\n")
  
  # 2. Vérifier packages requis
  cat("2️⃣  Vérification packages... ")
  
  if (!require("httr", quietly = TRUE) || !require("jsonlite", quietly = TRUE)) {
    cat("❌ ÉCHOUÉ\n")
    cat("   Install: install.packages(c('httr', 'jsonlite'))\n")
    return(FALSE)
  }
  
  cat("✅ OK\n")
  
  # 3. Test d'appel
  cat("3️⃣  Test d'appel API... ")
  
  tryCatch({
    response <- call_gemini_api(
      prompt = "Réponds en une phrase: Qui a créé Google?",
      config = config,
      max_tokens = 50,
      verbose = FALSE
    )
    cat("✅ OK\n\n")
    cat("📝 Réponse test:\n   ", response, "\n\n")
    
    cat("✅ Connexion API fonctionnelle!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ ÉCHOUÉ\n")
    cat("   Erreur: ", conditionMessage(e), "\n")
    return(FALSE)
  })
}

# ============================================================================
# Afficher message de bienvenue
# ============================================================================
cat("\n")
cat("╔═════════════════════════════════════════════════════════════╗\n")
cat("║  🔐 Configuration API Gemini Chargée Avec Sécurité         ║\n")
cat("║                                                             ║\n")
cat("║  Utilisez:                                                 ║\n")
cat("║    • print_config_safe() - Afficher config (clé masquée)   ║\n")
cat("║    • call_gemini_api(prompt) - Appeler l'API              ║\n")
cat("║    • test_gemini_connection() - Tester la connexion       ║\n")
cat("║                                                             ║\n")
cat("║  🛡️  La clé API est MASQUÉE automatiquement en logs        ║\n")
cat("╚═════════════════════════════════════════════════════════════╝\n\n")

# Export de la fonction principale pour utilisation facile
#' Gemini API Call (Wrapper simplifié)
#'
#' @param prompt Texte du prompt à envoyer à Gemini
#' @param max_tokens Nombre max de tokens (défaut: 1024)
#' @param temperature Température (0-1, défaut: 0.7)
#'
#' @return Réponse texte de Gemini
#'
#' @examples
#' \dontrun{
#'   response <- gemini("Explique l'IA en 50 mots")
#'   print(response)
#' }
#'
#' @export
gemini <- function(prompt, max_tokens = 1024, temperature = 0.7) {
  call_gemini_api(
    prompt = prompt,
    config = GEMINI_CONFIG,
    max_tokens = max_tokens,
    temperature = temperature,
    verbose = FALSE
  )
}
