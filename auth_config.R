# ============================================================================
# auth_config.R — Gestion des utilisateurs NutriAnalyse Pro v2
# ============================================================================
# Tous les utilisateurs authentifiés disposent des mêmes droits.
# Il n'y a plus de rôles ni de permissions différenciées.
# ============================================================================

library(shinyauthr)
library(sodium)

# ----------------------------------------------------------------------------
# BASE D'UTILISATEURS
# Deux modes : config/users.rds (recommandé, généré par tools/manage_users.R)
# ou comptes de démonstration ci-dessous (refusés en production).
# Les mots de passe sont hachés avec sodium — JAMAIS en clair.
#
# Pour générer un nouveau hash : sodium::password_store("MonMotDePasse")
# ----------------------------------------------------------------------------

.auth_users_file <- Sys.getenv("NUTRI_USERS_FILE", unset = "config/users.rds")
.auth_demo_mode  <- !file.exists(.auth_users_file)

if (.auth_demo_mode) {
  if (identical(Sys.getenv("NUTRI_ENV"), "production")) {
    stop("Aucun fichier utilisateurs trouvé (", .auth_users_file, "). ",
         "Générez-le avec : Rscript tools/manage_users.R add <user> <nom>",
         call. = FALSE)
  }
  USER_BASE <- data.frame(
    user     = c("yimga", "analyste", "lecteur"),
    password = c(
      "$7$C6..../....HCGlCuPa11r2pcKF9yYQeV0Qp.y9cT8i9ZPOKAmyNh6$YrF.QCEdAsv7SO9cMssRkUSxFI6As3CktQngTY/lVm8",
      "$7$C6..../....MmYwsIoIPqyyd3za5Aly5qqsbPwYmDxRmYctrIx0JY6$wC9PFakoGGp59UYOzkPifc6p5GdVx3Q61EuT7mbt6/6",
      "$7$C6..../....ZSVpK8LtuiR/RJ3tDLRC/S1hZsWpDkGCMgsFXZS/GmA$cHj9T4n9ZzZJ.sPCrXb82bNgIHx9uISnhOghj1W4Q9A"
    ),
    name     = c("YIMGA WANDJI KEVIN", "Analyste DSAMID", "Lecteur"),
    stringsAsFactors = FALSE
  )
  message("⚠️  Auth en mode démonstration — créez config/users.rds avec tools/manage_users.R")
} else {
  USER_BASE <- readRDS(.auth_users_file)
  .required <- c("user", "password", "name")
  .missing  <- setdiff(.required, names(USER_BASE))
  if (length(.missing) > 0) {
    stop("Fichier utilisateurs invalide (", .auth_users_file, ") : colonnes manquantes ",
         paste(.missing, collapse = ", "), call. = FALSE)
  }
  # Compatibilité avec les anciens fichiers contenant une colonne role.
  USER_BASE <- as.data.frame(USER_BASE[, .required, drop = FALSE],
                             stringsAsFactors = FALSE)
  USER_BASE$user     <- as.character(USER_BASE$user)
  USER_BASE$password <- as.character(USER_BASE$password)
  USER_BASE$name     <- as.character(USER_BASE$name)
  if (anyDuplicated(USER_BASE$user)) {
    stop("Fichier utilisateurs invalide : identifiants en double.", call. = FALSE)
  }
}

cat("✅ Auth config chargé —", nrow(USER_BASE), "utilisateurs",
    if (.auth_demo_mode) "(mode démonstration)" else paste0("(", .auth_users_file, ")"), "\n")
