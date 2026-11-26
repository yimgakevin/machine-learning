#!/usr/bin/env Rscript
# Validation du code pour les erreurs courantes

cat("\n🔍 VALIDATION DU CODE\n")
cat("═════════════════════════════════════════════════════════════════\n\n")

errors <- 0

# 1. Vérifier NULL dans app_v2_final.R
cat("1️⃣  Vérification des 'if' sur input (NULL check)...\n")

app_content <- readLines("app_v2_final.R")
problem_lines <- grep('if\\s*\\(input\\$.*\\s*[!=<>]', app_content, value = TRUE)
problem_lines <- problem_lines[!grepl("!is.null", problem_lines)]
problem_lines <- problem_lines[!grepl("length(", problem_lines)]

if (length(problem_lines) > 0) {
  cat("   ⚠️  Potentiels problèmes trouvés:\n")
  for (i in seq_along(problem_lines[1:min(5, length(problem_lines))])) {
    cat("      ", substr(problem_lines[i], 1, 70), "\n")
  }
  errors <- errors + length(problem_lines)
} else {
  cat("   ✅ OK\n")
}

# 2. Vérifier carto_country_selector.R
cat("\n2️⃣  Vérification carto_country_selector.R...\n")

carto_content <- readLines("carto_country_selector.R")
carto_problems <- grep('if\\s*\\(input\\$', carto_content, value = TRUE)
carto_problems <- carto_problems[!grepl("!is.null", carto_problems)]
carto_problems <- carto_problems[!grepl("length(", carto_problems)]

if (length(carto_problems) > 0) {
  cat("   ⚠️  Potentiels problèmes trouvés:\n")
  for (i in seq_along(carto_problems[1:min(5, length(carto_problems))])) {
    cat("      ", substr(carto_problems[i], 1, 70), "\n")
  }
  errors <- errors + length(carto_problems)
} else {
  cat("   ✅ OK\n")
}

# 3. Résumé
cat("\n")
cat("═════════════════════════════════════════════════════════════════\n")
if (errors == 0) {
  cat("\n✅ VALIDATION RÉUSSIE - Aucun problème détecté\n\n")
} else {
  cat("\n⚠️  ", errors, " potentiels problèmes à vérifier\n\n")
}
