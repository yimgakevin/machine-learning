# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_qualite_smart.R — Contrôle Qualité SMART (ENA-equivalent)         ║
# ║  SMART Flags · Plausibility Score · Digit Preference · Age Heaping        ║
# ║  SD par équipe · Complétude · Score global de qualité                     ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ══════════════════════════════════════════════════════════════════════════════
# 1. SMART FLAGS — Détection des valeurs aberrantes anthropométriques
# ══════════════════════════════════════════════════════════════════════════════

# Seuils SMART/OMS pour les flags (WHO Flags)
SMART_FLAG_LIMITS <- list(
  WHZ  = list(min = -5, max = 5,  label = "Poids-pour-Taille (WHZ)"),
  HAZ  = list(min = -6, max = 6,  label = "Taille-pour-Âge (HAZ)"),
  WAZ  = list(min = -6, max = 5,  label = "Poids-pour-Âge (WAZ)"),
  MUAC = list(min = 80, max = 220, label = "MUAC (mm)")
)

# Fonction : détecter les SMART flags dans les données
detect_smart_flags <- function(d, col_whz = NULL, col_haz = NULL,
                                col_waz = NULL, col_muac = NULL) {
  n <- nrow(d)
  flags <- tibble(.row = seq_len(n), flag_whz = FALSE, flag_haz = FALSE,
                  flag_waz = FALSE, flag_muac = FALSE)

  if (!is.null(col_whz) && col_whz %in% names(d)) {
    v <- d[[col_whz]]
    flags$flag_whz <- !is.na(v) & (v < SMART_FLAG_LIMITS$WHZ$min | v > SMART_FLAG_LIMITS$WHZ$max)
  }
  if (!is.null(col_haz) && col_haz %in% names(d)) {
    v <- d[[col_haz]]
    flags$flag_haz <- !is.na(v) & (v < SMART_FLAG_LIMITS$HAZ$min | v > SMART_FLAG_LIMITS$HAZ$max)
  }
  if (!is.null(col_waz) && col_waz %in% names(d)) {
    v <- d[[col_waz]]
    flags$flag_waz <- !is.na(v) & (v < SMART_FLAG_LIMITS$WAZ$min | v > SMART_FLAG_LIMITS$WAZ$max)
  }
  if (!is.null(col_muac) && col_muac %in% names(d)) {
    v <- d[[col_muac]]
    flags$flag_muac <- !is.na(v) & (v < SMART_FLAG_LIMITS$MUAC$min | v > SMART_FLAG_LIMITS$MUAC$max)
  }

  flags$any_flag <- flags$flag_whz | flags$flag_haz | flags$flag_waz | flags$flag_muac
  flags$n_flags  <- rowSums(flags[, c("flag_whz", "flag_haz", "flag_waz", "flag_muac")])
  flags
}

# Résumé des flags
summarise_flags <- function(flags, n_total) {
  tibble(
    Indicateur = c("WHZ hors [-5;+5]", "HAZ hors [-6;+6]",
                   "WAZ hors [-6;+5]", "MUAC hors [80;220]",
                   "Au moins 1 flag", "Total observations"),
    `N flaggé` = c(sum(flags$flag_whz), sum(flags$flag_haz),
                   sum(flags$flag_waz), sum(flags$flag_muac),
                   sum(flags$any_flag), n_total),
    `% flaggé` = round(c(sum(flags$flag_whz), sum(flags$flag_haz),
                         sum(flags$flag_waz), sum(flags$flag_muac),
                         sum(flags$any_flag), n_total) / n_total * 100, 2),
    Seuil = c("< 2.5%", "< 2.5%", "< 2.5%", "< 2.5%",
              "< 7.5%", "—"),
    Qualité = c(
      score_quality_pct(sum(flags$flag_whz) / n_total * 100, 2.5),
      score_quality_pct(sum(flags$flag_haz) / n_total * 100, 2.5),
      score_quality_pct(sum(flags$flag_waz) / n_total * 100, 2.5),
      score_quality_pct(sum(flags$flag_muac) / n_total * 100, 2.5),
      score_quality_pct(sum(flags$any_flag) / n_total * 100, 7.5),
      "—"
    )
  )
}

score_quality_pct <- function(pct, seuil) {
  if (pct <= seuil * 0.5) "🟢 Excellent"
  else if (pct <= seuil) "🟡 Acceptable"
  else if (pct <= seuil * 1.5) "🟠 Problématique"
  else "🔴 Sérieux"
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. DIGIT PREFERENCE — Préférence pour certains chiffres terminaux
# ══════════════════════════════════════════════════════════════════════════════

# DPS = Digit Preference Score (méthode SMART/ENA)
# Formule : DPS = sum(|obs_i - 10|) / 5  pour chiffres 0-9
calc_digit_preference <- function(x, label = "Variable") {
  x_clean <- na.omit(x)
  if (length(x_clean) < 10) {
    return(list(
      label = label, dps = NA, quality = "N/A",
      freq = NULL, detail = "Trop peu de valeurs"
    ))
  }

  # Extraire le dernier chiffre
  # Pour les décimaux : prendre le dernier chiffre significatif
  last_digit <- abs(round(x_clean * 10, 0)) %% 10
  freq <- table(factor(last_digit, levels = 0:9))
  pct  <- as.numeric(freq) / sum(freq) * 100

  # DPS = somme |obs% - 10%| / 5
  dps <- sum(abs(pct - 10)) / 5

  quality <- if (dps <= 2) "🟢 Excellent (DPS ≤ 2)"
             else if (dps <= 6) "🟡 Acceptable (DPS 2-6)"
             else if (dps <= 12) "🟠 Problématique (DPS 6-12)"
             else "🔴 Sérieux (DPS > 12)"

  list(
    label   = label,
    dps     = round(dps, 2),
    quality = quality,
    freq    = tibble(
      Chiffre     = 0:9,
      N           = as.integer(freq),
      `%`         = round(pct, 1),
      `Attendu %` = 10.0,
      Écart       = round(pct - 10, 1)
    ),
    detail  = paste0("DPS = ", round(dps, 2), " — ", quality)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. AGE HEAPING — Accumulation sur certains âges (6, 12, 18, 24, 36, 48)
# ══════════════════════════════════════════════════════════════════════════════

calc_age_heaping <- function(age_months) {
  age_clean <- na.omit(age_months)
  if (length(age_clean) < 20) {
    return(list(
      index = NA, quality = "N/A",
      freq = NULL, detail = "Trop peu de valeurs"
    ))
  }

  # Âges ronds : multiples de 6 (6, 12, 18, 24, 30, 36, 42, 48, 54)
  is_round <- age_clean %% 6 == 0
  pct_round <- sum(is_round) / length(age_clean) * 100

  # Age Ratio Index : ratio observé / attendu pour les multiples de 6
  # Dans 6-59 mois, les multiples de 6 sont : 6,12,18,24,30,36,42,48,54 = 9 valeurs sur 54
  expected_pct <- 9 / 54 * 100  # ~16.7%
  age_ratio <- pct_round / expected_pct

  quality <- if (age_ratio <= 1.3) "🟢 Excellent (ratio ≤ 1.3)"
             else if (age_ratio <= 2.0) "🟡 Acceptable (ratio 1.3-2.0)"
             else if (age_ratio <= 3.0) "🟠 Problématique (ratio 2.0-3.0)"
             else "🔴 Sérieux (ratio > 3.0)"

  # Distribution par mois
  age_tbl <- tibble(
    `Âge (mois)` = as.integer(names(table(age_clean))),
    N = as.integer(table(age_clean))
  )

  list(
    index       = round(age_ratio, 2),
    pct_round   = round(pct_round, 1),
    expected_pct = round(expected_pct, 1),
    quality     = quality,
    freq        = age_tbl,
    detail      = paste0("Ratio = ", round(age_ratio, 2),
                         " (", round(pct_round, 1), "% sur multiples de 6 vs ",
                         round(expected_pct, 1), "% attendu) — ", quality)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. ÉCART-TYPE PAR ÉQUIPE — Détection des équipes qui mesurent mal
# ══════════════════════════════════════════════════════════════════════════════

calc_sd_by_team <- function(d, col_zscore, col_team) {
  if (is.null(col_zscore) || is.null(col_team)) return(NULL)
  if (!col_zscore %in% names(d) || !col_team %in% names(d)) return(NULL)

  d_clean <- d |>
    filter(!is.na(.data[[col_zscore]]), !is.na(.data[[col_team]])) |>
    group_by(Équipe = .data[[col_team]]) |>
    summarise(
      N     = n(),
      Moy   = round(mean(.data[[col_zscore]], na.rm = TRUE), 3),
      SD    = round(sd(.data[[col_zscore]], na.rm = TRUE), 3),
      Min   = round(min(.data[[col_zscore]], na.rm = TRUE), 2),
      Max   = round(max(.data[[col_zscore]], na.rm = TRUE), 2),
      .groups = "drop"
    )

  # SD global pour comparaison
  sd_global <- sd(d[[col_zscore]], na.rm = TRUE)

  # SD attendu pour WHZ : ~1.0 (population de référence OMS)
  # Seuils SMART : SD < 0.8 = suspicieux, SD > 1.2 = suspicieux
  d_clean <- d_clean |>
    mutate(
      Qualité = case_when(
        SD >= 0.8 & SD <= 1.2 ~ "🟢 OK",
        SD >= 0.7 & SD <= 1.3 ~ "🟡 Attention",
        TRUE                  ~ "🔴 Problème"
      ),
      `Écart vs global` = round(SD - sd_global, 3)
    )

  list(
    table     = d_clean,
    sd_global = round(sd_global, 3),
    quality   = if (all(d_clean$SD >= 0.8 & d_clean$SD <= 1.2)) "🟢 Toutes les équipes OK"
                else if (any(d_clean$SD < 0.7 | d_clean$SD > 1.3)) "🔴 Problème détecté"
                else "🟡 Vigilance requise"
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. COMPLÉTUDE DES DONNÉES — Taux de remplissage par variable clé
# ══════════════════════════════════════════════════════════════════════════════

calc_completude <- function(d, vars_cles = NULL) {
  if (is.null(vars_cles)) vars_cles <- names(d)
  vars_cles <- intersect(vars_cles, names(d))

  map_dfr(vars_cles, function(v) {
    n_total <- nrow(d)
    n_na    <- sum(is.na(d[[v]]))
    n_valid <- n_total - n_na
    pct     <- round(n_valid / n_total * 100, 1)

    tibble(
      Variable    = v,
      `N valide`  = n_valid,
      `N manquant`= n_na,
      `N total`   = n_total,
      `% complet` = pct,
      Qualité     = case_when(
        pct >= 95 ~ "🟢 Excellent",
        pct >= 80 ~ "🟡 Acceptable",
        pct >= 60 ~ "🟠 Problématique",
        TRUE      ~ "🔴 Sérieux"
      )
    )
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. SCORE DE PLAUSIBILITÉ GLOBAL — Inspiré ENA for SMART
# ══════════════════════════════════════════════════════════════════════════════
#
# Le Plausibility Check de SMART attribue des pénalités (0=bon, 1-4=warning,
# 5+=problème) sur 12 critères. Score final 0-100 :
#   0-9    = Excellent
#   10-14  = Bon
#   15-24  = Acceptable
#   ≥ 25   = Problématique
#
# Critères implémentés :
#   1. % flags SMART (WHZ hors ±5)
#   2. Écart-type global du WHZ
#   3. Moyenne globale du WHZ
#   4. Digit preference (poids ou taille)
#   5. Age heaping (préférence âges ronds)
#   6. Sexe ratio (garçons/filles ~ 1.0)
#   7. Complétude des mesures anthropo
#   8. Skewness (asymétrie) du WHZ
#   9. Kurtosis (aplatissement) du WHZ
#  10. SD par équipe (homogénéité)

calc_plausibility_score <- function(d, col_whz = NULL, col_haz = NULL,
                                     col_waz = NULL, col_muac = NULL,
                                     col_sex = NULL, col_age = NULL,
                                     col_weight = NULL, col_height = NULL,
                                     col_team = NULL) {
  scores <- list()
  total_penalty <- 0

  n_total <- nrow(d)

  # ── 1. Flags SMART (WHZ) ──
  if (!is.null(col_whz) && col_whz %in% names(d)) {
    whz <- d[[col_whz]]
    whz_clean <- na.omit(whz)
    n_flags <- sum(whz_clean < -5 | whz_clean > 5)
    pct_flags <- n_flags / length(whz_clean) * 100

    pen <- if (pct_flags <= 2.5) 0
           else if (pct_flags <= 5.0) 5
           else if (pct_flags <= 7.5) 10
           else 20

    scores$flags <- list(
      critere = "1. Flags WHZ hors [-5;+5]",
      valeur  = paste0(round(pct_flags, 2), "%"),
      seuil   = "≤ 2.5%",
      penalty = pen,
      qualite = if (pen == 0) "🟢" else if (pen <= 5) "🟡" else if (pen <= 10) "🟠" else "🔴"
    )
    total_penalty <- total_penalty + pen

    # ── 2. SD du WHZ ──
    sd_whz <- sd(whz_clean)
    pen_sd <- if (sd_whz >= 0.8 & sd_whz <= 1.2) 0
              else if (sd_whz >= 0.7 & sd_whz <= 1.3) 5
              else 10

    scores$sd_whz <- list(
      critere = "2. Écart-type WHZ",
      valeur  = round(sd_whz, 3),
      seuil   = "0.8 - 1.2",
      penalty = pen_sd,
      qualite = if (pen_sd == 0) "🟢" else if (pen_sd <= 5) "🟡" else "🔴"
    )
    total_penalty <- total_penalty + pen_sd

    # ── 3. Moyenne WHZ ──
    mean_whz <- mean(whz_clean)
    pen_mean <- if (abs(mean_whz) <= 0.5) 0
                else if (abs(mean_whz) <= 1.0) 2
                else 5

    scores$mean_whz <- list(
      critere = "3. Moyenne WHZ",
      valeur  = round(mean_whz, 3),
      seuil   = "± 0.5 de la moyenne attendue",
      penalty = pen_mean,
      qualite = if (pen_mean == 0) "🟢" else if (pen_mean <= 2) "🟡" else "🔴"
    )
    total_penalty <- total_penalty + pen_mean

    # ── 8. Skewness (asymétrie) ──
    skew <- tryCatch({
      n <- length(whz_clean)
      m3 <- mean((whz_clean - mean(whz_clean))^3)
      s3 <- sd(whz_clean)^3
      m3 / s3
    }, error = function(e) NA)

    if (!is.na(skew)) {
      pen_skew <- if (abs(skew) <= 0.5) 0
                  else if (abs(skew) <= 1.0) 2
                  else 5

      scores$skewness <- list(
        critere = "8. Asymétrie (Skewness) WHZ",
        valeur  = round(skew, 3),
        seuil   = "|skew| ≤ 0.5",
        penalty = pen_skew,
        qualite = if (pen_skew == 0) "🟢" else if (pen_skew <= 2) "🟡" else "🔴"
      )
      total_penalty <- total_penalty + pen_skew
    }

    # ── 9. Kurtosis ──
    kurt <- tryCatch({
      n <- length(whz_clean)
      m4 <- mean((whz_clean - mean(whz_clean))^4)
      s4 <- sd(whz_clean)^4
      m4 / s4 - 3  # excès de kurtosis
    }, error = function(e) NA)

    if (!is.na(kurt)) {
      pen_kurt <- if (abs(kurt) <= 1.0) 0
                  else if (abs(kurt) <= 2.0) 2
                  else 5

      scores$kurtosis <- list(
        critere = "9. Kurtosis (excès) WHZ",
        valeur  = round(kurt, 3),
        seuil   = "|kurt| ≤ 1.0",
        penalty = pen_kurt,
        qualite = if (pen_kurt == 0) "🟢" else if (pen_kurt <= 2) "🟡" else "🔴"
      )
      total_penalty <- total_penalty + pen_kurt
    }
  }

  # ── 4. Digit preference (poids) ──
  dp_var <- col_weight %||% col_height
  if (!is.null(dp_var) && dp_var %in% names(d)) {
    dp <- calc_digit_preference(d[[dp_var]], dp_var)
    pen_dp <- if (is.na(dp$dps)) 0
              else if (dp$dps <= 2) 0
              else if (dp$dps <= 6) 2
              else if (dp$dps <= 12) 5
              else 10

    scores$digit_pref <- list(
      critere = paste0("4. Digit preference (", dp_var, ")"),
      valeur  = if (!is.na(dp$dps)) round(dp$dps, 2) else "N/A",
      seuil   = "DPS ≤ 6",
      penalty = pen_dp,
      qualite = if (pen_dp == 0) "🟢" else if (pen_dp <= 2) "🟡" else if (pen_dp <= 5) "🟠" else "🔴"
    )
    total_penalty <- total_penalty + pen_dp
  }

  # ── 5. Age heaping ──
  if (!is.null(col_age) && col_age %in% names(d)) {
    ah <- calc_age_heaping(d[[col_age]])
    pen_age <- if (is.na(ah$index)) 0
               else if (ah$index <= 1.3) 0
               else if (ah$index <= 2.0) 2
               else if (ah$index <= 3.0) 5
               else 10

    scores$age_heap <- list(
      critere = "5. Age heaping (multiples de 6)",
      valeur  = if (!is.na(ah$index)) round(ah$index, 2) else "N/A",
      seuil   = "Ratio ≤ 2.0",
      penalty = pen_age,
      qualite = if (pen_age == 0) "🟢" else if (pen_age <= 2) "🟡" else if (pen_age <= 5) "🟠" else "🔴"
    )
    total_penalty <- total_penalty + pen_age
  }

  # ── 6. Sex ratio ──
  if (!is.null(col_sex) && col_sex %in% names(d)) {
    sex_vals <- tolower(as.character(d[[col_sex]]))
    n_m <- sum(sex_vals %in% c("m", "male", "masculin", "garçon", "garcon", "boy", "1"))
    n_f <- sum(sex_vals %in% c("f", "female", "féminin", "feminin", "fille", "girl", "2", "0"))
    sex_ratio <- if (n_f > 0) n_m / n_f else NA

    pen_sex <- if (is.na(sex_ratio)) 0
               else if (sex_ratio >= 0.85 & sex_ratio <= 1.15) 0
               else if (sex_ratio >= 0.75 & sex_ratio <= 1.25) 2
               else 5

    scores$sex_ratio <- list(
      critere = "6. Ratio garçons/filles",
      valeur  = if (!is.na(sex_ratio)) paste0(round(sex_ratio, 2),
                  " (", n_m, "G / ", n_f, "F)") else "N/A",
      seuil   = "0.85 - 1.15",
      penalty = pen_sex,
      qualite = if (pen_sex == 0) "🟢" else if (pen_sex <= 2) "🟡" else "🔴"
    )
    total_penalty <- total_penalty + pen_sex
  }

  # ── 7. Complétude anthropo ──
  anthro_vars <- c(col_whz, col_haz, col_waz, col_muac)
  anthro_vars <- anthro_vars[!is.null(anthro_vars) & anthro_vars %in% names(d)]
  if (length(anthro_vars) > 0) {
    pct_complete <- mean(sapply(anthro_vars, function(v) {
      sum(!is.na(d[[v]])) / nrow(d) * 100
    }))
    pen_compl <- if (pct_complete >= 95) 0
                 else if (pct_complete >= 80) 2
                 else if (pct_complete >= 60) 5
                 else 10

    scores$completude <- list(
      critere = "7. Complétude des mesures anthropo",
      valeur  = paste0(round(pct_complete, 1), "%"),
      seuil   = "≥ 95%",
      penalty = pen_compl,
      qualite = if (pen_compl == 0) "🟢" else if (pen_compl <= 2) "🟡" else if (pen_compl <= 5) "🟠" else "🔴"
    )
    total_penalty <- total_penalty + pen_compl
  }

  # ── 10. SD par équipe ──
  if (!is.null(col_team) && !is.null(col_whz) &&
      col_team %in% names(d) && col_whz %in% names(d)) {
    sd_teams <- d |>
      filter(!is.na(.data[[col_whz]]), !is.na(.data[[col_team]])) |>
      group_by(.data[[col_team]]) |>
      summarise(sd_t = sd(.data[[col_whz]], na.rm = TRUE), .groups = "drop")

    n_bad <- sum(sd_teams$sd_t < 0.7 | sd_teams$sd_t > 1.3, na.rm = TRUE)
    pen_team <- if (n_bad == 0) 0
                else if (n_bad == 1) 5
                else 10

    scores$sd_team <- list(
      critere = "10. Homogénéité SD par équipe",
      valeur  = paste0(n_bad, " équipe(s) hors [0.7-1.3]"),
      seuil   = "0 équipe hors norme",
      penalty = pen_team,
      qualite = if (pen_team == 0) "🟢" else if (pen_team <= 5) "🟡" else "🔴"
    )
    total_penalty <- total_penalty + pen_team
  }

  # ── SCORE GLOBAL ──
  # Normaliser sur 100 (max théorique ~80 pénalités)
  # Score SMART : 0 = parfait, plus c'est haut plus c'est mauvais
  score_global <- min(100, total_penalty)

  classification <- if (score_global <= 9) "🟢 EXCELLENT (0-9)"
                    else if (score_global <= 14) "🟢 BON (10-14)"
                    else if (score_global <= 24) "🟡 ACCEPTABLE (15-24)"
                    else "🔴 PROBLÉMATIQUE (≥ 25)"

  list(
    scores        = scores,
    total_penalty = total_penalty,
    score_global  = score_global,
    classification = classification,
    n_criteres    = length(scores)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 7. RENDU HTML — Score de plausibilité (jauge + tableau)
# ══════════════════════════════════════════════════════════════════════════════

render_plausibility_html <- function(plaus) {
  if (is.null(plaus)) return(NULL)

  # Couleur selon score
  col <- if (plaus$score_global <= 9) "#27ae60"
         else if (plaus$score_global <= 14) "#2ecc71"
         else if (plaus$score_global <= 24) "#f39c12"
         else "#e74c3c"

  # Tableau des critères
  criteria_html <- paste(sapply(plaus$scores, function(s) {
    paste0(
      "<tr>",
      "<td style='padding:6px 10px;'>", s$qualite, "</td>",
      "<td style='padding:6px 10px;font-weight:600;'>", s$critere, "</td>",
      "<td style='padding:6px 10px;text-align:center;'>", s$valeur, "</td>",
      "<td style='padding:6px 10px;text-align:center;color:#888;'>", s$seuil, "</td>",
      "<td style='padding:6px 10px;text-align:center;font-weight:700;",
      "color:", if (s$penalty == 0) "#27ae60" else if (s$penalty <= 2) "#f39c12" else "#e74c3c",
      ";'>", s$penalty, "</td>",
      "</tr>"
    )
  }), collapse = "")

  tagList(
    # Score global
    tags$div(
      style = paste0(
        "text-align:center;padding:20px;margin-bottom:16px;",
        "background:linear-gradient(135deg,", col, ",", col, "22);",
        "border:2px solid ", col, ";border-radius:12px;"
      ),
      tags$div(style = "font-size:11px;text-transform:uppercase;letter-spacing:1px;color:#555;",
               "SCORE DE PLAUSIBILITÉ SMART"),
      tags$div(style = paste0("font-size:56px;font-weight:800;color:", col, ";line-height:1.1;"),
               plaus$score_global),
      tags$div(style = paste0("font-size:16px;font-weight:700;color:", col, ";margin-top:4px;"),
               plaus$classification),
      tags$div(style = "font-size:12px;color:#888;margin-top:6px;",
               paste0(plaus$n_criteres, " critères évalués — Pénalité totale : ",
                      plaus$total_penalty, " points"))
    ),

    # Échelle de référence
    tags$div(
      style = "display:flex;gap:8px;justify-content:center;margin-bottom:16px;",
      tags$span(style = "background:#27ae60;color:white;padding:3px 10px;border-radius:4px;font-size:11px;",
                "0-9 Excellent"),
      tags$span(style = "background:#2ecc71;color:white;padding:3px 10px;border-radius:4px;font-size:11px;",
                "10-14 Bon"),
      tags$span(style = "background:#f39c12;color:white;padding:3px 10px;border-radius:4px;font-size:11px;",
                "15-24 Acceptable"),
      tags$span(style = "background:#e74c3c;color:white;padding:3px 10px;border-radius:4px;font-size:11px;",
                "≥25 Problématique")
    ),

    # Tableau des critères
    tags$div(
      style = "overflow-x:auto;",
      HTML(paste0(
        "<table style='width:100%;border-collapse:collapse;font-size:12px;'>",
        "<thead><tr style='background:#f0f8ff;border-bottom:2px solid #008DC9;'>",
        "<th style='padding:8px 10px;text-align:left;'>", "</th>",
        "<th style='padding:8px 10px;text-align:left;'>Critère</th>",
        "<th style='padding:8px 10px;text-align:center;'>Valeur</th>",
        "<th style='padding:8px 10px;text-align:center;'>Seuil</th>",
        "<th style='padding:8px 10px;text-align:center;'>Pénalité</th>",
        "</tr></thead><tbody>",
        criteria_html,
        "<tr style='border-top:2px solid #333;font-weight:700;background:#f8f9fa;'>",
        "<td style='padding:8px 10px;'>", plaus$classification, "</td>",
        "<td style='padding:8px 10px;' colspan='3'>SCORE TOTAL</td>",
        "<td style='padding:8px 10px;text-align:center;font-size:16px;color:",
        col, ";'>", plaus$total_penalty, "</td>",
        "</tr></tbody></table>"
      ))
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 8. GRAPHIQUES DE QUALITÉ
# ══════════════════════════════════════════════════════════════════════════════

# Histogramme du WHZ avec zones de flags
plot_whz_flags <- function(d, col_whz) {
  if (is.null(col_whz) || !col_whz %in% names(d)) return(plotly_empty())
  whz <- na.omit(d[[col_whz]])
  if (length(whz) < 5) return(plotly_empty())

  df_plot <- tibble(WHZ = whz,
                    Flag = ifelse(whz < -5 | whz > 5, "Flag", "OK"))

  p <- ggplot(df_plot, aes(x = WHZ, fill = Flag)) +
    geom_histogram(bins = 60, alpha = 0.85, color = "white", linewidth = 0.2) +
    geom_vline(xintercept = c(-5, 5), linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
    geom_vline(xintercept = c(-3, -2, 2, 3), linetype = "dotted", color = "#888", linewidth = 0.5) +
    annotate("text", x = -5, y = Inf, label = "Flag -5", vjust = 2, hjust = 1.1,
             color = "#e74c3c", size = 3, fontface = "bold") +
    annotate("text", x = 5, y = Inf, label = "Flag +5", vjust = 2, hjust = -0.1,
             color = "#e74c3c", size = 3, fontface = "bold") +
    annotate("rect", xmin = -5, xmax = -Inf, ymin = -Inf, ymax = Inf,
             fill = "#e74c3c", alpha = 0.08) +
    annotate("rect", xmin = 5, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "#e74c3c", alpha = 0.08) +
    scale_fill_manual(values = c("OK" = "#008DC9", "Flag" = "#e74c3c")) +
    labs(x = "WHZ (score Z poids-pour-taille)", y = "Fréquence",
         title = "Distribution WHZ avec zones de flags SMART",
         subtitle = paste0("n=", length(whz), " | Flags: ",
                           sum(whz < -5 | whz > 5), " (",
                           round(sum(whz < -5 | whz > 5) / length(whz) * 100, 1), "%)")) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
  ggplotly(p)
}

# Barplot digit preference
plot_digit_preference <- function(dp_result) {
  if (is.null(dp_result$freq)) return(plotly_empty())

  df_plot <- dp_result$freq |>
    mutate(
      Couleur = case_when(
        abs(Écart) <= 2 ~ "Normal",
        abs(Écart) <= 5 ~ "Attention",
        TRUE ~ "Problème"
      )
    )

  p <- ggplot(df_plot, aes(x = factor(Chiffre), y = `%`, fill = Couleur,
                            text = paste0("Chiffre ", Chiffre, "\n",
                                          N, " obs (", `%`, "%)\nÉcart: ",
                                          Écart, " pts"))) +
    geom_col(alpha = 0.85, width = 0.7) +
    geom_hline(yintercept = 10, linetype = "dashed", color = "#333", linewidth = 0.8) +
    annotate("text", x = 10.5, y = 10, label = "Attendu 10%", hjust = 0,
             size = 3, color = "#555") +
    scale_fill_manual(values = c("Normal" = "#008DC9", "Attention" = "#f39c12",
                                  "Problème" = "#e74c3c")) +
    labs(x = "Dernier chiffre", y = "Fréquence (%)",
         title = paste0("Digit Preference — ", dp_result$label),
         subtitle = paste0("DPS = ", dp_result$dps, " — ", dp_result$quality)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
  ggplotly(p, tooltip = "text")
}

# Pyramide des âges
plot_age_distribution <- function(d, col_age) {
  if (is.null(col_age) || !col_age %in% names(d)) return(plotly_empty())
  age <- na.omit(d[[col_age]])
  if (length(age) < 10) return(plotly_empty())

  df_plot <- tibble(Age = as.integer(round(age)),
                    Multiple6 = (round(age) %% 6 == 0))

  p <- ggplot(df_plot, aes(x = Age, fill = Multiple6)) +
    geom_histogram(binwidth = 1, alpha = 0.85, color = "white", linewidth = 0.2) +
    scale_fill_manual(values = c("FALSE" = "#008DC9", "TRUE" = "#e74c3c"),
                      labels = c("FALSE" = "Autre", "TRUE" = "Multiple de 6")) +
    labs(x = "Âge (mois)", y = "Fréquence",
         title = "Distribution des âges — Détection Age Heaping",
         fill = "") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
  ggplotly(p)
}

# Barplot SD par équipe
plot_sd_teams <- function(sd_result, zscore_label = "WHZ") {
  if (is.null(sd_result) || is.null(sd_result$table)) return(plotly_empty())

  df_plot <- sd_result$table

  p <- ggplot(df_plot, aes(x = reorder(Équipe, SD), y = SD, fill = Qualité,
                            text = paste0("Équipe: ", Équipe, "\nN=", N,
                                          "\nSD=", SD, "\nMoyenne=", Moy))) +
    geom_col(alpha = 0.85, width = 0.6) +
    geom_hline(yintercept = c(0.8, 1.2), linetype = "dashed", color = "#27ae60", linewidth = 0.6) +
    geom_hline(yintercept = c(0.7, 1.3), linetype = "dotted", color = "#e74c3c", linewidth = 0.6) +
    geom_hline(yintercept = sd_result$sd_global, color = "#333", linewidth = 0.8) +
    annotate("text", x = 0.5, y = sd_result$sd_global, label = paste0("SD global = ", sd_result$sd_global),
             hjust = 0, vjust = -0.5, size = 3, fontface = "bold") +
    scale_fill_manual(values = c("🟢 OK" = "#27ae60", "🟡 Attention" = "#f39c12",
                                  "🔴 Problème" = "#e74c3c")) +
    labs(x = "Équipe", y = paste0("SD (", zscore_label, ")"),
         title = paste0("Écart-type ", zscore_label, " par équipe"),
         subtitle = "Bandes vertes = [0.8-1.2] | Pointillés rouges = [0.7-1.3]") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))
  ggplotly(p, tooltip = "text")
}

cat("module_qualite_smart.R chargé — Contrôle qualité SMART complet\n")
