# ============================================================================
# auth_config.R — Gestion des utilisateurs et rôles NutriAnalyse Pro v2
# ============================================================================
# Rôles disponibles :
#   "admin"    — accès complet + gestion utilisateurs
#   "analyste" — accès analyses + IA + cartographie (pas admin)
#   "lecteur"  — accès lecture seule + export uniquement
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
         "Générez-le avec : Rscript tools/manage_users.R add <user> <role> <nom>",
         call. = FALSE)
  }
  USER_BASE <- data.frame(
    user     = c("yimga", "analyste", "lecteur"),
    password = c(
      "$7$C6..../....HCGlCuPa11r2pcKF9yYQeV0Qp.y9cT8i9ZPOKAmyNh6$YrF.QCEdAsv7SO9cMssRkUSxFI6As3CktQngTY/lVm8",
      "$7$C6..../....MmYwsIoIPqyyd3za5Aly5qqsbPwYmDxRmYctrIx0JY6$wC9PFakoGGp59UYOzkPifc6p5GdVx3Q61EuT7mbt6/6",
      "$7$C6..../....ZSVpK8LtuiR/RJ3tDLRC/S1hZsWpDkGCMgsFXZS/GmA$cHj9T4n9ZzZJ.sPCrXb82bNgIHx9uISnhOghj1W4Q9A"
    ),
    role     = c("admin", "analyste", "lecteur"),
    name     = c("YIMGA WANDJI KEVIN", "Analyste DSAMID", "Lecteur"),
    stringsAsFactors = FALSE
  )
  message("⚠️  Auth en mode démonstration — créez config/users.rds avec tools/manage_users.R")
} else {
  USER_BASE <- readRDS(.auth_users_file)
  .required <- c("user", "password", "role", "name")
  .missing  <- setdiff(.required, names(USER_BASE))
  if (length(.missing) > 0) {
    stop("Fichier utilisateurs invalide (", .auth_users_file, ") : colonnes manquantes ",
         paste(.missing, collapse = ", "), call. = FALSE)
  }
  USER_BASE <- as.data.frame(USER_BASE, stringsAsFactors = FALSE)
  USER_BASE$user     <- as.character(USER_BASE$user)
  USER_BASE$password <- as.character(USER_BASE$password)
  USER_BASE$role     <- as.character(USER_BASE$role)
  USER_BASE$name     <- as.character(USER_BASE$name)
  if (anyDuplicated(USER_BASE$user)) {
    stop("Fichier utilisateurs invalide : identifiants en double.", call. = FALSE)
  }
}

# ----------------------------------------------------------------------------
# PERMISSIONS PAR RÔLE
# ----------------------------------------------------------------------------

ROLE_PERMISSIONS <- list(
  admin = list(
    tabs        = "all",         # Tous les onglets
    can_export  = TRUE,
    can_delete  = TRUE,
    can_ia      = TRUE,
    can_carto   = TRUE,
    label       = "🔑 Administrateur",
    color       = "#c9161d"
  ),
  analyste = list(
    tabs        = c("accueil","import","explor","dplyr","uni","biv","descoms",
                    "infer","rdim","clust","qualite","prevalence","secu_alim",
                    "zscore","oms","carto","epidemio","survie","predictif",
                    "rapport","exec_dash","qualite_auto","temporal_tests",
                    "advanced_forecasting","advanced_geolocation",
                    "advanced_nlp","advanced_benchmarking","about"),
    can_export  = TRUE,
    can_delete  = FALSE,
    can_ia      = TRUE,
    can_carto   = TRUE,
    label       = "📊 Analyste",
    color       = "#008DC9"
  ),
  lecteur = list(
    tabs        = c("accueil","explor","uni","biv","descoms","zscore",
                    "oms","rapport","about"),
    can_export  = TRUE,
    can_delete  = FALSE,
    can_ia      = FALSE,     # pas d'accès à l'IA
    can_carto   = FALSE,
    label       = "👁️ Lecteur",
    color       = "#4dac26"
  )
)

# Validation des rôles après définition des permissions
.invalid_roles <- setdiff(USER_BASE$role, names(ROLE_PERMISSIONS))
if (length(.invalid_roles) > 0) {
  stop("Rôles inconnus dans la base utilisateurs : ",
       paste(.invalid_roles, collapse = ", "), call. = FALSE)
}

# ----------------------------------------------------------------------------
# HELPER — vérifier si un utilisateur a accès à un onglet
# ----------------------------------------------------------------------------

user_can_access_tab <- function(role, tab) {
  perms <- ROLE_PERMISSIONS[[role]]
  if (is.null(perms)) return(FALSE)
  if (identical(perms$tabs, "all")) return(TRUE)
  tab %in% perms$tabs
}

user_has_permission <- function(role, perm) {
  perms <- ROLE_PERMISSIONS[[role]]
  if (is.null(perms)) return(FALSE)
  isTRUE(perms[[perm]])
}

cat("✅ Auth config chargé —", nrow(USER_BASE), "utilisateurs,", length(ROLE_PERMISSIONS), "rôles",
    if (.auth_demo_mode) "(mode démonstration)" else paste0("(", .auth_users_file, ")"), "\n")
