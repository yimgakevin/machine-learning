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
# Modifier ici pour ajouter / supprimer des utilisateurs.
# Les mots de passe sont hachés avec sodium — JAMAIS en clair.
#
# Pour générer un nouveau hash : sodium::password_store("MonMotDePasse")
# ----------------------------------------------------------------------------

USER_BASE <- data.frame(
  user     = c("yimga",                        "analyste",              "lecteur"),
  password = c(
    sodium::password_store("Admin2026!"),   # ← À changer avant déploiement
    sodium::password_store("Analyste2026!"),
    sodium::password_store("Lecteur2026!")
  ),
  role     = c("admin",                         "analyste",              "lecteur"),
  name     = c("YIMGA WANDJI KEVIN",            "Analyste DSAMID",       "Lecteur"),
  stringsAsFactors = FALSE
)

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

cat("✅ Auth config chargé —", nrow(USER_BASE), "utilisateurs,", length(ROLE_PERMISSIONS), "rôles\n")
