# ============================================================================
# Gemini API v3 — Architecture Multi-Tour Natif (équivalent Python SDK Colab)
# ============================================================================
# Reproduit exactement le comportement du SDK google-generativeai en Python :
#   model = GenerativeModel(model_name=..., system_instruction=...)
#   chat  = model.start_chat(history=[])
#   chat.send_message("...")
# ============================================================================

library(httr)
library(jsonlite)

# GEMINI_CONFIG doit être chargé avant ce fichier (via config_api_secure_v2.R)
if (!exists("GEMINI_CONFIG")) {
  message("⚠️ GEMINI_CONFIG non trouvé — tentative de chargement config_api_secure_v2.R")
  tryCatch(source("config_api_secure_v2.R"), error = function(e) {
    stop("❌ Impossible de charger la configuration API Gemini")
  })
}

# ============================================================================
# 1. RATE LIMITER & CACHE
# ============================================================================

GEMINI_CACHE <- list(responses = list(), size_mb = 0, max_size_mb = 50, hits = 0, misses = 0)
RATE_LIMITER <- list(calls_per_minute = 0, last_reset = Sys.time())

.check_rate_limit <- function(max_per_minute = 60) {
  now <- Sys.time()
  if (as.numeric(now - RATE_LIMITER$last_reset, units = "secs") > 60) {
    RATE_LIMITER$calls_per_minute <<- 0
    RATE_LIMITER$last_reset       <<- now
  }
  if (RATE_LIMITER$calls_per_minute >= max_per_minute) {
    wait <- 60 - as.numeric(now - RATE_LIMITER$last_reset, units = "secs")
    stop(sprintf("⏱️  Rate limit atteint — attendre %.0f secondes", wait))
  }
  RATE_LIMITER$calls_per_minute <<- RATE_LIMITER$calls_per_minute + 1
  invisible(TRUE)
}

.cache_key <- function(contents, max_tokens, temperature) {
  digest::digest(list(contents, max_tokens, round(temperature, 1)), algo = "md5")
}

.get_cache <- function(key) {
  cached <- GEMINI_CACHE$responses[[key]]
  if (is.null(cached)) return(NULL)
  if (as.numeric(Sys.time() - cached$ts, units = "mins") > 60) {
    GEMINI_CACHE$responses[[key]] <<- NULL
    return(NULL)
  }
  GEMINI_CACHE$hits <<- GEMINI_CACHE$hits + 1
  cached$text
}

.set_cache <- function(key, text) {
  GEMINI_CACHE$responses[[key]] <<- list(text = text, ts = Sys.time())
  GEMINI_CACHE$misses            <<- GEMINI_CACHE$misses + 1
}

# ============================================================================
# 2. FONCTION CŒUR — APPEL MULTI-TOUR NATIF
# ============================================================================
# Équivalent exact de la requête REST que le SDK Python envoie sous le capot.
#
# Arguments :
#   contents          — liste de turns : list(list(role="user", parts=list(list(text="..."))))
#   system_instruction— chaîne (string) ou NULL ; équivalent system_instruction= du SDK
#   config            — GEMINI_CONFIG
#   max_tokens        — entier
#   temperature       — numérique 0-1
#   use_cache         — booléen

call_gemini_multiturn <- function(
  contents,
  system_instruction = NULL,
  config             = GEMINI_CONFIG,
  max_tokens         = 2048,
  temperature        = 0.7,
  use_cache          = FALSE
) {
  .check_rate_limit()

  if (use_cache) {
    key    <- .cache_key(contents, max_tokens, temperature)
    cached <- .get_cache(key)
    if (!is.null(cached)) return(cached)
  }

  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent",
    config$model
  )

  # Payload — identique à ce que le SDK Python génère
  payload <- list(
    contents = contents,
    generationConfig = list(
      maxOutputTokens = max_tokens,
      temperature     = temperature,
      topP            = 0.95,
      topK            = 40
    ),
    safetySettings = list(
      list(category = "HARM_CATEGORY_HATE_SPEECH",        threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT",  threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_DANGEROUS_CONTENT",  threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_HARASSMENT",         threshold = "BLOCK_NONE")
    )
  )

  # systemInstruction — champ dédié (comme SDK Python), pas dans contents
  if (!is.null(system_instruction) && nchar(system_instruction) > 0) {
    payload$systemInstruction <- list(
      parts = list(list(text = system_instruction))
    )
  }

  response <- tryCatch(
    httr::POST(
      url,
      httr::add_headers("Content-Type" = "application/json"),
      query = list(key = config$api_key),
      body  = jsonlite::toJSON(payload, auto_unbox = TRUE),
      httr::timeout(config$timeout)
    ),
    error = function(e) stop(sprintf("❌ Connexion échouée: %s", e$message))
  )

  status <- httr::status_code(response)
  if (status != 200) {
    err <- mask_api_key(httr::content(response, as = "text", encoding = "UTF-8"))
    stop(sprintf("❌ API Error (%d): %s", status, substr(err, 1, 300)))
  }

  result <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE   # ← CRITIQUE : conserve la structure liste pure
  )

  # Extraction robuste (structure liste, pas data.frame)
  text <- tryCatch({
    candidates <- result$candidates
    if (is.null(candidates) || length(candidates) == 0) stop("No candidates")
    candidate <- candidates[[1]]
    parts <- candidate$content$parts
    if (is.null(parts) || length(parts) == 0) stop("No parts")
    parts[[1]]$text
  }, error = function(e) NULL)

  if (is.null(text) || length(text) == 0 || !nzchar(text)) {
    finish <- tryCatch(result$candidates[[1]]$finishReason, error = function(e) "UNKNOWN")
    blocked <- tryCatch(result$promptFeedback$blockReason, error = function(e) NULL)
    msg <- sprintf("❌ Réponse vide de Gemini (finishReason: %s)", finish)
    if (!is.null(blocked)) msg <- paste0(msg, sprintf(" | blockReason: %s", blocked))
    stop(msg)
  }

  if (use_cache) .set_cache(key, text)

  return(text)
}

# ============================================================================
# 3. OBJET CHAT — équivalent model.start_chat() du SDK Python
# ============================================================================
# Usage :
#   chat <- gemini_start_chat(system_instruction = "Tu es un expert en nutrition")
#   reponse <- gemini_send_message(chat, "Analyse ces données")
#   chat     <- reponse$chat   # Chat mis à jour avec l'historique

gemini_start_chat <- function(
  system_instruction = NULL,
  config             = GEMINI_CONFIG,
  max_tokens         = 2048,
  temperature        = 0.7
) {
  structure(
    list(
      system_instruction = system_instruction,
      history            = list(),   # list de turns user/model
      config             = config,
      max_tokens         = max_tokens,
      temperature        = temperature,
      n_turns            = 0L,
      created            = Sys.time()
    ),
    class = c("gemini_chat", "list")
  )
}

gemini_send_message <- function(chat, message, use_cache = FALSE) {
  stopifnot(inherits(chat, "gemini_chat"))

  # Ajouter le message utilisateur à l'historique
  new_turn   <- list(role = "user", parts = list(list(text = message)))
  contents   <- c(chat$history, list(new_turn))

  response_text <- call_gemini_multiturn(
    contents           = contents,
    system_instruction = chat$system_instruction,
    config             = chat$config,
    max_tokens         = chat$max_tokens,
    temperature        = chat$temperature,
    use_cache          = use_cache
  )

  # Mettre à jour l'historique (user + model)
  model_turn   <- list(role = "model", parts = list(list(text = response_text)))
  chat$history <- c(contents, list(model_turn))
  chat$n_turns <- chat$n_turns + 1L

  list(text = response_text, chat = chat)
}

# Accéder à l'historique formaté
gemini_chat_history <- function(chat) {
  lapply(chat$history, function(turn) {
    list(
      role    = turn$role,
      message = turn$parts[[1]]$text
    )
  })
}

# Réinitialiser l'historique (garde system_instruction)
gemini_reset_chat <- function(chat) {
  chat$history  <- list()
  chat$n_turns  <- 0L
  chat$created  <- Sys.time()
  chat
}

# ============================================================================
# 4. WRAPPER SIMPLE (rétrocompatibilité avec l'ancien call_gemini_api)
# ============================================================================
call_gemini_api <- function(
  prompt,
  config      = GEMINI_CONFIG,
  max_tokens  = 1024,
  temperature = 0.7,
  use_cache   = FALSE,
  verbose     = FALSE
) {
  contents <- list(list(role = "user", parts = list(list(text = prompt))))
  text <- call_gemini_multiturn(
    contents    = contents,
    config      = config,
    max_tokens  = max_tokens,
    temperature = temperature,
    use_cache   = use_cache
  )
  if (verbose) cat(sprintf("✅ Réponse reçue (~%d tokens)\n", round(nchar(text) / 4)))
  text
}

# Alias court (même usage que gemini("...") en Python)
gemini <- function(prompt, max_tokens = 1024, temperature = 0.7) {
  call_gemini_api(prompt, max_tokens = max_tokens, temperature = temperature)
}

# ============================================================================
# 5. STATISTIQUES
# ============================================================================
print_gemini_stats <- function() {
  total <- GEMINI_CACHE$hits + GEMINI_CACHE$misses
  rate  <- if (total > 0) round(GEMINI_CACHE$hits / total * 100, 1) else 0
  cat(sprintf(
    "\n📊 Gemini Stats — Cache: %d hits / %d req (%.1f%%) | Rate: %d/60 req/min\n",
    GEMINI_CACHE$hits, total, rate, RATE_LIMITER$calls_per_minute
  ))
}

# ============================================================================
# 6. INIT SESSION (rétrocompatibilité)
# ============================================================================
init_gemini_session <- function(config = GEMINI_CONFIG, cache_enabled = TRUE) {
  session <- list(
    config         = config,
    cache_enabled  = cache_enabled,
    session_id     = paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    created        = Sys.time(),
    api_calls      = 0L,
    cache_hits     = 0L,
    errors         = character(0),
    status         = "initialized"
  )
  class(session) <- c("gemini_session", "list")
  cat("✅ Gemini Session Initialized | Model:", config$model, "\n")
  session
}

if (!exists("GEMINI_SESSION", envir = .GlobalEnv)) {
  GEMINI_SESSION <- init_gemini_session()
}

cat("✅ Gemini API v3 loaded (multi-tour natif)\n")
