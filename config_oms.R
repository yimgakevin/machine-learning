# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  config_oms.R — Configuration centralisée des seuils OMS/UNICEF/PAM        ║
# ║  Tous les seuils sont définis ICI et utilisés par tous les modules         ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Seuils de sévérité OMS (MAG / GAM) ──────────────────────────────────────
# Source: WHO/UNICEF Technical Advisory Group on Nutrition Monitoring (2018)
OMS_SEUILS <- list(
  GAM = list(
    acceptable = 5,
    alerte     = 10,
    serieux    = 15,
    critique   = 30,
    labels     = c("Acceptable", "Alerte", "Sérieux", "Critique", "Urgence max"),
    colors     = c("#2E7D52", "#D4A843", "#D98B14", "#BF3636", "#5C1A1A")
  ),
  SAM = list(
    acceptable = 1,
    alerte     = 2,
    serieux    = 5,
    critique   = 10,
    labels     = c("Acceptable", "Alerte", "Sérieux", "Critique", "Urgence max"),
    colors     = c("#2E7D52", "#D4A843", "#D98B14", "#BF3636", "#5C1A1A")
  ),
  MAM = list(
    acceptable = 3,
    alerte     = 8,
    serieux    = 12,
    critique   = 20,
    labels     = c("Acceptable", "Alerte", "Sérieux", "Critique", "Urgence max"),
    colors     = c("#2E7D52", "#D4A843", "#D98B14", "#BF3636", "#5C1A1A")
  ),
  Stunting = list(
    acceptable = 20,
    alerte     = 30,
    serieux    = 40,
    critique   = 60,
    labels     = c("Acceptable", "Alerte", "Sérieux", "Critique", "Urgence max"),
    colors     = c("#2E7D52", "#D4A843", "#D98B14", "#BF3636", "#5C1A1A")
  ),
  Underweight = list(
    acceptable = 10,
    alerte     = 20,
    serieux    = 30,
    critique   = 40,
    labels     = c("Acceptable", "Alerte", "Sérieux", "Critique", "Urgence max"),
    colors     = c("#2E7D52", "#D4A843", "#D98B14", "#BF3636", "#5C1A1A")
  ),
  MUAC_GAM = list(
    acceptable = 5,
    alerte     = 10,
    serieux    = 15,
    critique   = 20,
    labels     = c("Acceptable", "Alerte", "Sérieux", "Critique", "Urgence max"),
    colors     = c("#2E7D52", "#D4A843", "#D98B14", "#BF3636", "#5C1A1A")
  )
)

# ── Couleurs institutionnelles ────────────────────────────────────────────────
COULEURS <- list(
  bleu_oms    = "#0077B6",
  bleu_fonce  = "#0B3954",
  bleu_moyen  = "#0C4A6E",
  teal        = "#00838F",
  or          = "#D4A843",
  rouge       = "#BF3636",
  rouge_fonce = "#5C1A1A",
  orange      = "#D98B14",
  jaune       = "#D4A843",
  vert        = "#2E7D52",
  vert_fonce  = "#1B5E3B",
  violet      = "#6B3FA0",
  gris        = "#CBD5E1",
  gris_fonce  = "#4A5568",
  surface     = "#F4F6FB"
)

# ── Z-score thresholds OMS 2006 ──────────────────────────────────────────────
ZSCORE_SEUILS <- list(
  severe   = -3,
  moderate = -2,
  normal   = 0
)

# ── Helper: classifier une valeur selon les seuils OMS ────────────────────────
classify_oms <- function(value, indicator = "GAM") {
  s <- OMS_SEUILS[[indicator]]
  if (is.null(s)) s <- OMS_SEUILS$GAM

  dplyr::case_when(
    value >= s$critique  ~ s$labels[5],
    value >= s$serieux   ~ s$labels[4],
    value >= s$alerte    ~ s$labels[3],
    value >= s$acceptable ~ s$labels[2],
    TRUE                 ~ s$labels[1]
  )
}

# ── Helper: couleur OMS pour une valeur ───────────────────────────────────────
color_oms <- function(value, indicator = "GAM") {
  s <- OMS_SEUILS[[indicator]]
  if (is.null(s)) s <- OMS_SEUILS$GAM

  dplyr::case_when(
    value >= s$critique   ~ s$colors[5],
    value >= s$serieux    ~ s$colors[4],
    value >= s$alerte     ~ s$colors[3],
    value >= s$acceptable ~ s$colors[2],
    TRUE                  ~ s$colors[1]
  )
}

# ── Helper: seuil critique pour un indicateur ─────────────────────────────────
seuil_critique <- function(indicator = "GAM") {
  s <- OMS_SEUILS[[indicator]]
  if (is.null(s)) return(15)
  s$serieux
}

# ── Limites de performance ────────────────────────────────────────────────────
PERF_LIMITS <- list(
  max_gps_points  = 50000L,
  max_heat_points = 100000L,
  gadm_timeout    = 30L,
  gadm_retries    = 2L
)

cat("config_oms.R chargé —", length(OMS_SEUILS), "indicateurs configurés\n")
