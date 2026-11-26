# ============================================================================
# safe_eval.R — Sandbox sécurisé pour eval(parse()) dans l'app
# ============================================================================
# Bloque les fonctions système dangereuses, n'autorise que les opérations
# légitimes d'analyse de données.
# ============================================================================

# Fonctions interdites — toute tentative lève une erreur
BLOCKED_FUNCTIONS <- c(
  "system", "system2", "shell", "shell.exec",
  "unlink", "file.remove", "file.create", "dir.create",
  "Sys.setenv", "Sys.setlocale", "setwd",
  "source", "load", "loadNamespace", "requireNamespace",
  "library", "require", "install.packages",
  "readLines", "writeLines", "readRDS", "saveRDS",
  "read.csv", "write.csv", "read.table", "write.table",
  ".Call", ".External", ".C", ".Fortran",
  "tryCatch", "try", "withCallingHandlers",  # éviter les contournements
  "parse", "eval",                             # pas d'eval récursif
  "Recall", "do.call"                          # limiter les constructions complexes
)

# Pattern regex pour détecter les tentatives d'appels dangereux
BLOCKED_PATTERN <- paste0(
  "(", paste(gsub("\\.", "\\\\.", BLOCKED_FUNCTIONS), collapse = "|"), ")\\s*\\("
)

# ── Fonction principale ──────────────────────────────────────────────────────

safe_eval_expr <- function(expr_text, data = NULL, context = "expression") {

  if (is.null(expr_text) || !nzchar(trimws(expr_text))) {
    stop("Expression vide.")
  }

  # 1. Longueur maximale
  if (nchar(expr_text) > 500) {
    stop("❌ Expression trop longue (max 500 caractères).")
  }

  # 2. Détecter les fonctions bloquées
  if (grepl(BLOCKED_PATTERN, expr_text, perl = TRUE)) {
    matched <- regmatches(expr_text,
                          regexpr(BLOCKED_PATTERN, expr_text, perl = TRUE))
    stop(sprintf("❌ Fonction interdite détectée : '%s'", matched))
  }

  # 3. Pas de backticks système ou d'accès aux objets globaux sensibles
  if (grepl("`[^`]+`|\\$\\$|@[A-Z]", expr_text)) {
    stop("❌ Syntaxe non autorisée.")
  }

  # 4. Tenter le parse pour valider la syntaxe R
  parsed <- tryCatch(
    parse(text = expr_text, keep.source = FALSE),
    error = function(e) stop(sprintf("❌ Syntaxe R invalide : %s", e$message))
  )

  # 5. Évaluer dans un environnement restreint
  env <- if (!is.null(data)) {
    list2env(as.list(data), parent = baseenv())
  } else {
    new.env(parent = baseenv())
  }

  tryCatch(
    eval(parsed, envir = env),
    error   = function(e) stop(sprintf("❌ Erreur dans %s : %s", context, e$message)),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

# ── Wrapper pour mutate dplyr ────────────────────────────────────────────────

safe_mutate <- function(df, new_col_name, expr_text) {

  # Valider le nom de la nouvelle colonne
  if (!grepl("^[a-zA-Z][a-zA-Z0-9_.]*$", new_col_name)) {
    stop("❌ Nom de colonne invalide. Utilisez lettres, chiffres, _ ou .")
  }

  result_vec <- safe_eval_expr(expr_text, data = df, context = "mutate")

  if (length(result_vec) != nrow(df) && length(result_vec) != 1L) {
    stop(sprintf(
      "❌ L'expression retourne %d valeurs pour %d lignes.",
      length(result_vec), nrow(df)
    ))
  }

  df[[new_col_name]] <- result_vec
  df
}

cat("✅ safe_eval.R chargé — sandbox sécurisé actif\n")
