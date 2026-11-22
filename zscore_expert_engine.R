# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  zscore_expert_engine.R — Moteur Expert OMS/UNICEF                          ║
# ║  Narrative Analytics · Quality Scoring · Anomaly Detection · Benchmarking    ║
# ║  Ceci est le cœur "pensée d'expert" du module Z-scores                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ══════════════════════════════════════════════════════════════════════════════
# 1. SPARKLINE GENERATOR — Micro-visualisations pour les KPI cards
# ══════════════════════════════════════════════════════════════════════════════

#' Générer une sparkline (mini-densité) pour une KPI card
#' @param z_vals Vecteur de Z-scores
#' @param couleur Couleur de remplissage
#' @param width Largeur en pixels
#' @param height Hauteur en pixels
#' @return ggplot object (très petit, pas d'axes)
gen_sparkline_z <- function(z_vals, couleur = "#1976d2", width = 100, height = 35) {
  z <- z_vals[!is.na(z_vals)]
  if (length(z) < 5) return(NULL)

  # Calculer densité
  density_obj <- density(z, bw = "SJ", n = 512)
  density_df <- tibble::tibble(x = density_obj$x, y = density_obj$y)

  # Restreindre à la plage pertinente (-5 à 5)
  density_df <- density_df |> filter(x >= -5, x <= 5)

  p <- ggplot(density_df, aes(x = x, y = y)) +
    geom_area(fill = couleur, alpha = 0.7) +
    geom_line(color = couleur, linewidth = 0.8) +
    # Zones de sévérité (très discrètes)
    annotate("rect", xmin = -5, xmax = -3, ymin = 0, ymax = Inf, fill = "#d32f2f", alpha = 0.03) +
    annotate("rect", xmin = -3, xmax = -2, ymin = 0, ymax = Inf, fill = "#f57c00", alpha = 0.03) +
    # Ligne médiane
    geom_vline(xintercept = 0, linetype = "dashed", color = "#666", alpha = 0.3, linewidth = 0.5) +
    scale_x_continuous(limits = c(-5, 5)) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  p
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. QUALITY SCORING EXPERT — 10 critères OMS standardisés
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer le score de qualité expert multi-critère (0-100)
#'
#' @param d Data frame avec Z-scores calculés
#' @param col_whz, col_haz, col_waz Noms des colonnes Z-score
#' @param col_age, col_sexe, col_team Noms des colonnes contexte
#'
#' @return Liste avec :
#'  - score_global : 0-100
#'  - classification : "Excellent" / "Bon" / "Acceptable" / "Problématique"
#'  - criteria_scores : data.frame avec 10 critères
#'  - anomalies : liste des anomalies détectées
calc_quality_score_expert <- function(d,
                                      col_whz = "WHZ_calc",
                                      col_haz = "HAZ_calc",
                                      col_waz = "WAZ_calc",
                                      col_age = "age_mois",
                                      col_sexe = "sexe",
                                      col_team = NULL) {

  # ── Initialiser les scores des 10 critères ──
  crit_scores <- tibble::tibble(
    Critère = character(10),
    Score = numeric(10),
    Note = character(10),
    Sévérité = character(10)
  )

  anomalies <- list()

  # Helper : extraire valeurs non-NA d'une colonne (retourne numeric(0) si absent)
  safe_col <- function(col) {
    if (is.null(col) || !col %in% names(d)) return(numeric(0))
    v <- as.numeric(d[[col]])
    v[!is.na(v) & is.finite(v)]
  }

  z_vals_whz <- safe_col(col_whz)
  z_vals_haz <- safe_col(col_haz)
  z_vals_waz <- safe_col(col_waz)

  # ─ CRITÈRE 1 : Écart-type des Z-scores ─
  sd_whz <- if (length(z_vals_whz) > 1) sd(z_vals_whz) else NA_real_

  if (isTRUE(sd_whz > 0.8 & sd_whz < 1.2)) {
    crit1_score <- 100
    crit1_note <- "Excellent (SD \u2248 1.0)"
    crit1_sev <- "Acceptable"
  } else if (isTRUE(sd_whz > 0.7 & sd_whz < 1.3)) {
    crit1_score <- 80
    crit1_note <- paste0("Bon (SD = ", round(sd_whz, 2), ")")
    crit1_sev <- "Acceptable"
  } else if (!is.na(sd_whz)) {
    crit1_score <- max(40, 100 - 20 * abs(sd_whz - 1.0))
    crit1_note <- paste0("Probl\u00e9matique (SD = ", round(sd_whz, 2), ")")
    crit1_sev <- "Pr\u00e9occupant"
    anomalies[[length(anomalies) + 1]] <- list(
      type = "\u00c9cart-type anormal",
      value = sd_whz,
      interpretation = if (isTRUE(sd_whz < 0.7)) "Peut indiquer des donn\u00e9es trop homog\u00e8nes ou arrondies"
                       else "Peut indiquer trop de variabilit\u00e9 ou d'outliers"
    )
  } else {
    crit1_score <- 50
    crit1_note <- "N/A (donn\u00e9es WHZ insuffisantes)"
    crit1_sev <- "Ind\u00e9termin\u00e9"
  }

  crit_scores[1, ] <- list("1. \u00c9cart-type WHZ", crit1_score, crit1_note, crit1_sev)

  # ─ CRITÈRE 2 : Digit Preference ─
  if (length(z_vals_whz) > 10) {
    z_whl_decimals <- (z_vals_whz * 10) %% 10
    digit_freq <- table(floor(z_whl_decimals))
    digit_entropy <- -sum((digit_freq / length(z_whl_decimals)) * log(digit_freq / length(z_whl_decimals) + 1e-10))
    digit_pct <- min(100, (digit_entropy / log(10)) * 100)

    if (isTRUE(digit_pct > 80)) {
      crit2_score <- 100
      crit2_note <- "Excellent (pas de preference)"
      crit2_sev <- "Acceptable"
    } else if (isTRUE(digit_pct > 60)) {
      crit2_score <- 70
      crit2_note <- paste0("Attention (entropy = ", round(digit_entropy, 2), ")")
      crit2_sev <- "Mod\u00e9r\u00e9"
      anomalies[[length(anomalies) + 1]] <- list(
        type = "Digit preference d\u00e9tect\u00e9",
        value = digit_pct,
        interpretation = "Les d\u00e9cimales ne sont pas uniform\u00e9ment distribu\u00e9es."
      )
    } else {
      crit2_score <- 40
      crit2_note <- "Probl\u00e9matique (forte preference)"
      crit2_sev <- "S\u00e9rieux"
      anomalies[[length(anomalies) + 1]] <- list(
        type = "Digit preference fort",
        value = digit_pct,
        interpretation = "Forte indication d'arrondissage ou d'impr\u00e9cision de mesure."
      )
    }
  } else {
    crit2_score <- 100
    crit2_note <- "N/A (donn\u00e9es insuffisantes)"
    crit2_sev <- "Acceptable"
  }

  crit_scores[2, ] <- list("2. Digit Preference", crit2_score, crit2_note, crit2_sev)

  # ─ CRITÈRE 3 : Age Heaping ─
  if (!is.null(col_age) && col_age %in% names(d)) {
    ages <- d[[col_age]][!is.na(d[[col_age]])]
    round_ages <- c(0, 5, 10, 12, 24, 36, 48, 60)
    n_round <- sum(ages %in% round_ages)
    pct_round <- if (length(ages) > 0) (n_round / length(ages)) * 100 else 0

    if (isTRUE(pct_round < 20)) {
      crit3_score <- 100
      crit3_note <- "Excellent (pas de heaping)"
      crit3_sev <- "Acceptable"
    } else if (isTRUE(pct_round < 35)) {
      crit3_score <- 75
      crit3_note <- paste0("Mod\u00e9r\u00e9 (", round(pct_round, 1), "% heaping)")
      crit3_sev <- "Mod\u00e9r\u00e9"
      anomalies[[length(anomalies) + 1]] <- list(
        type = "Age heaping l\u00e9ger",
        value = pct_round,
        interpretation = "Certains enfants ont des \u00e2ges ronds. Possible approximation."
      )
    } else {
      crit3_score <- 40
      crit3_note <- paste0("S\u00e9rieux (", round(pct_round, 1), "% heaping)")
      crit3_sev <- "S\u00e9rieux"
      anomalies[[length(anomalies) + 1]] <- list(
        type = "Age heaping fort",
        value = pct_round,
        interpretation = paste0(round(pct_round, 1), "% des \u00e2ges sont ronds. Approximation syst\u00e9matique.")
      )
    }
  } else {
    crit3_score <- 100
    crit3_note <- "N/A (pas de colonne \u00e2ge)"
    crit3_sev <- "Acceptable"
  }

  crit_scores[3, ] <- list("3. Age Heaping", crit3_score, crit3_note, crit3_sev)

  # ─ CRITÈRE 4 : Sex Ratio ─
  if (!is.null(col_sexe) && col_sexe %in% names(d)) {
    sexe <- d[[col_sexe]][!is.na(d[[col_sexe]])]
    n_m <- sum(toupper(trimws(sexe)) %in% c("M", "MALE", "MASCULIN", "GARCON", "1", "BOY"))
    n_f <- sum(toupper(trimws(sexe)) %in% c("F", "FEMALE", "FEMININ", "FILLE", "2", "GIRL"))
    n_total_sex <- n_m + n_f

    if (n_total_sex > 0) {
      pct_m <- (n_m / n_total_sex) * 100
      if (isTRUE(pct_m >= 45 & pct_m <= 55)) {
        crit4_score <- 100
        crit4_note <- "Excellent (\u00e9quilibr\u00e9)"
        crit4_sev <- "Acceptable"
      } else if (isTRUE(pct_m >= 40 & pct_m <= 60)) {
        crit4_score <- 80
        crit4_note <- paste0("Bon (M=", round(pct_m, 1), "%)")
        crit4_sev <- "Acceptable"
      } else {
        crit4_score <- 50
        crit4_note <- paste0("Probl\u00e9matique (M=", round(pct_m, 1), "%)")
        crit4_sev <- "Pr\u00e9occupant"
        anomalies[[length(anomalies) + 1]] <- list(
          type = "Sex ratio d\u00e9s\u00e9quilibr\u00e9",
          value = pct_m,
          interpretation = paste0("Ratio gar\u00e7ons = ", round(pct_m, 1), "%. Possible biais de collecte.")
        )
      }
    } else {
      crit4_score <- 100
      crit4_note <- "N/A (sexe non reconnu)"
      crit4_sev <- "Acceptable"
    }
  } else {
    crit4_score <- 100
    crit4_note <- "N/A (pas de colonne sexe)"
    crit4_sev <- "Acceptable"
  }

  crit_scores[4, ] <- list("4. Sex Ratio", crit4_score, crit4_note, crit4_sev)

  # ─ CRITÈRE 5 : Corrélations entre Z-scores ─
  idx_valid <- which(!is.na(d[[col_whz]]) & !is.na(d[[col_haz]]) & !is.na(d[[col_waz]]))
  if (length(idx_valid) > 10) {
    whz_aligned <- d[[col_whz]][idx_valid]
    haz_aligned <- d[[col_haz]][idx_valid]
    waz_aligned <- d[[col_waz]][idx_valid]

    r_whz_haz <- cor(whz_aligned, haz_aligned, use = "complete.obs")
    r_whz_waz <- cor(whz_aligned, waz_aligned, use = "complete.obs")
    r_haz_waz <- cor(haz_aligned, waz_aligned, use = "complete.obs")
    r_mean <- mean(c(r_whz_haz, r_whz_waz, r_haz_waz), na.rm = TRUE)

    if (isTRUE(r_mean > 0.4)) {
      crit5_score <- 100
      crit5_note <- paste0("Excellent (r_mean = ", round(r_mean, 2), ")")
      crit5_sev <- "Acceptable"
    } else if (isTRUE(r_mean > 0.2)) {
      crit5_score <- 75
      crit5_note <- paste0("Acceptable (r_mean = ", round(r_mean, 2), ")")
      crit5_sev <- "Acceptable"
    } else if (!is.na(r_mean)) {
      crit5_score <- 40
      crit5_note <- paste0("Faible coh\u00e9rence (r_mean = ", round(r_mean, 2), ")")
      crit5_sev <- "Pr\u00e9occupant"
      anomalies[[length(anomalies) + 1]] <- list(
        type = "Corr\u00e9lations Z-score faibles",
        value = r_mean,
        interpretation = "WHZ/HAZ/WAZ ne sont pas suffisamment corr\u00e9l\u00e9s."
      )
    } else {
      crit5_score <- 100
      crit5_note <- "N/A (corr\u00e9lation incalculable)"
      crit5_sev <- "Acceptable"
    }
  } else {
    crit5_score <- 100
    crit5_note <- "N/A (donn\u00e9es insuffisantes)"
    crit5_sev <- "Acceptable"
  }

  crit_scores[5, ] <- list("5. Corr\u00e9lations Z-score", crit5_score, crit5_note, crit5_sev)

  # ─ CRITÈRE 6 : Consistance des flags ─
  flag_whz <- if ("flag_WHZ" %in% names(d) && length(idx_valid) > 0)
    d$flag_WHZ[idx_valid] %in% TRUE else logical(0)
  flag_haz <- if ("flag_HAZ" %in% names(d) && length(idx_valid) > 0)
    d$flag_HAZ[idx_valid] %in% TRUE else logical(0)
  flag_waz <- if ("flag_WAZ" %in% names(d) && length(idx_valid) > 0)
    d$flag_WAZ[idx_valid] %in% TRUE else logical(0)

  n_flagged <- sum(flag_whz) + sum(flag_haz) + sum(flag_waz)
  pct_flagged <- if (length(idx_valid) > 0) (n_flagged / (length(idx_valid) * 3)) * 100 else 0

  if (isTRUE(pct_flagged < 1)) {
    crit6_score <- 100
    crit6_note <- "Excellent (<1% flags)"
    crit6_sev <- "Acceptable"
  } else if (isTRUE(pct_flagged < 5)) {
    crit6_score <- 80
    crit6_note <- paste0("Bon (", round(pct_flagged, 2), "% flags)")
    crit6_sev <- "Acceptable"
  } else {
    crit6_score <- max(30, 80 - 10 * pct_flagged)
    crit6_note <- paste0("Probl\u00e9matique (", round(pct_flagged, 2), "% flags)")
    crit6_sev <- "S\u00e9rieux"
    anomalies[[length(anomalies) + 1]] <- list(
      type = "Trop de flags SMART d\u00e9tect\u00e9s",
      value = pct_flagged,
      interpretation = paste0(round(pct_flagged, 1), "% des observations sont flagg\u00e9es.")
    )
  }

  crit_scores[6, ] <- list("6. Consistance flags", crit6_score, crit6_note, crit6_sev)

  # ─ CRITÈRE 7 : Complétude ─
  pct_complete <- mean(c(
    mean(!is.na(d[[col_whz]])),
    mean(!is.na(d[[col_haz]])),
    mean(!is.na(d[[col_waz]]))
  )) * 100

  if (isTRUE(pct_complete > 95)) {
    crit7_score <- 100
    crit7_note <- paste0("Excellent (", round(pct_complete, 1), "%)")
    crit7_sev <- "Acceptable"
  } else if (isTRUE(pct_complete > 90)) {
    crit7_score <- 85
    crit7_note <- paste0("Bon (", round(pct_complete, 1), "%)")
    crit7_sev <- "Acceptable"
  } else if (isTRUE(pct_complete > 80)) {
    crit7_score <- 70
    crit7_note <- paste0("Acceptable (", round(pct_complete, 1), "%)")
    crit7_sev <- "Mod\u00e9r\u00e9"
  } else {
    crit7_score <- 40
    crit7_note <- paste0("Insuffisant (", round(pct_complete, 1), "%)")
    crit7_sev <- "S\u00e9rieux"
  }

  crit_scores[7, ] <- list("7. Compl\u00e9tude", crit7_score, crit7_note, crit7_sev)

  # ─ CRITÈRE 8 : Normalité (Shapiro-Wilk) ─
  if (length(z_vals_whz) > 5 && length(z_vals_whz) <= 5000) {
    sw_test <- tryCatch(shapiro.test(sample(z_vals_whz, min(length(z_vals_whz), 5000))),
                        error = function(e) NULL)
    p_value <- if (!is.null(sw_test)) sw_test$p.value else NA_real_

    if (isTRUE(p_value > 0.05)) {
      crit8_score <- 100
      crit8_note <- paste0("Distribution normale (p=", round(p_value, 3), ")")
      crit8_sev <- "Acceptable"
    } else if (isTRUE(p_value > 0.01)) {
      crit8_score <- 75
      crit8_note <- paste0("L\u00e9g\u00e8rement non-normal (p=", round(p_value, 3), ")")
      crit8_sev <- "Mod\u00e9r\u00e9"
    } else if (!is.na(p_value)) {
      crit8_score <- 50
      crit8_note <- paste0("Significativement non-normal (p=", round(p_value, 3), ")")
      crit8_sev <- "Mod\u00e9r\u00e9"
    } else {
      crit8_score <- 100
      crit8_note <- "N/A (test \u00e9chou\u00e9)"
      crit8_sev <- "Acceptable"
    }
  } else {
    crit8_score <- 100
    crit8_note <- "N/A (n insuffisant pour test)"
    crit8_sev <- "Acceptable"
  }

  crit_scores[8, ] <- list("8. Normalit\u00e9 (Shapiro)", crit8_score, crit8_note, crit8_sev)

  # ─ CRITÈRE 9 : Skewness & Kurtosis ─
  skew_whz <- tryCatch(e1071::skewness(z_vals_whz, na.rm = TRUE), error = function(e) NA_real_)
  kurt_whz <- tryCatch(e1071::kurtosis(z_vals_whz, na.rm = TRUE), error = function(e) NA_real_)

  if (isTRUE(abs(skew_whz) < 0.5) && isTRUE(abs(kurt_whz) < 1)) {
    crit9_score <- 100
    crit9_note <- "Excellent (sym\u00e9trique)"
    crit9_sev <- "Acceptable"
  } else if (isTRUE(abs(skew_whz) < 1) && isTRUE(abs(kurt_whz) < 3)) {
    crit9_score <- 80
    crit9_note <- paste0("Bon (skew=", round(skew_whz, 2), ", kurt=", round(kurt_whz, 2), ")")
    crit9_sev <- "Acceptable"
  } else if (!is.na(skew_whz) && !is.na(kurt_whz)) {
    crit9_score <- 60
    crit9_note <- paste0("Asym\u00e9trique (skew=", round(skew_whz, 2), ", kurt=", round(kurt_whz, 2), ")")
    crit9_sev <- "Mod\u00e9r\u00e9"
  } else {
    crit9_score <- 100
    crit9_note <- "N/A (calcul impossible)"
    crit9_sev <- "Acceptable"
  }

  crit_scores[9, ] <- list("9. Skewness/Kurtosis", crit9_score, crit9_note, crit9_sev)

  # ─ CRITÈRE 10 : Team SD Consistency ─
  if (!is.null(col_team) && col_team %in% names(d)) {
    team_sds <- tryCatch({
      d |>
        dplyr::filter(!is.na(!!rlang::sym(col_whz))) |>
        dplyr::group_by(!!rlang::sym(col_team)) |>
        dplyr::summarise(sd = sd(!!rlang::sym(col_whz), na.rm = TRUE), n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(n > 5)
    }, error = function(e) tibble::tibble())

    if (nrow(team_sds) > 1) {
      sd_mean <- mean(team_sds$sd, na.rm = TRUE)
      sd_coef_var <- if (isTRUE(sd_mean > 0)) sd(team_sds$sd, na.rm = TRUE) / sd_mean else NA_real_

      if (isTRUE(sd_coef_var < 0.2)) {
        crit10_score <- 100
        crit10_note <- "Excellent (SD homog\u00e8ne entre \u00e9quipes)"
        crit10_sev <- "Acceptable"
      } else if (isTRUE(sd_coef_var < 0.35)) {
        crit10_score <- 75
        crit10_note <- paste0("Bon (CV=", round(sd_coef_var, 2), ")")
        crit10_sev <- "Acceptable"
      } else if (!is.na(sd_coef_var)) {
        crit10_score <- 50
        crit10_note <- paste0("H\u00e9t\u00e9rog\u00e8ne (CV=", round(sd_coef_var, 2), ")")
        crit10_sev <- "Pr\u00e9occupant"
        anomalies[[length(anomalies) + 1]] <- list(
          type = "SD h\u00e9t\u00e9rog\u00e8ne entre \u00e9quipes",
          value = sd_coef_var,
          interpretation = "Les \u00e9carts-types varient fortement selon l'\u00e9quipe."
        )
      } else {
        crit10_score <- 100
        crit10_note <- "N/A"
        crit10_sev <- "Acceptable"
      }
    } else {
      crit10_score <- 100
      crit10_note <- "N/A (une seule \u00e9quipe)"
      crit10_sev <- "Acceptable"
    }
  } else {
    crit10_score <- 100
    crit10_note <- "N/A (pas de colonne \u00e9quipe)"
    crit10_sev <- "Acceptable"
  }

  crit_scores[10, ] <- list("10. Team SD Consistency", crit10_score, crit10_note, crit10_sev)

  # ─ Calculer le score global ─
  score_global <- round(mean(crit_scores$Score, na.rm = TRUE), 1)

  classification <- if (isTRUE(score_global >= 90)) "Excellent"
                    else if (isTRUE(score_global >= 75)) "Bon"
                    else if (isTRUE(score_global >= 60)) "Acceptable"
                    else "Probl\u00e9matique"

  color <- switch(classification,
    "Excellent" = "#4caf50",
    "Bon" = "#fbc02d",
    "Acceptable" = "#f57c00",
    "#d32f2f"
  )

  list(
    score_global = score_global,
    classification = classification,
    criteria_scores = crit_scores,
    anomalies = anomalies,
    color = color
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. NARRATIVE GENERATOR — Créer le texte narratif pour chaque indicateur
# ══════════════════════════════════════════════════════════════════════════════

#' Générer un narratif clinique intelligent pour un KPI
#'
#' @param indicator "GAM", "HAZ", ou "WAZ"
#' @param pct_prevalence Prévalence en %
#' @param pct_severe % sévère
#' @param pct_moderate % modéré
#' @param mean_z Moyenne des Z-scores
#' @param sd_z Écart-type des Z-scores
#' @param n_flags Nombre d'observations flaggées
#' @param n_total Total observations
#' @param quality_score Score de qualité 0-100
gen_narrative_kpi <- function(indicator, pct_prevalence, pct_severe, pct_moderate,
                               mean_z, sd_z, n_flags, n_total, quality_score) {

  # Classification OMS
  if (indicator == "GAM") {
    sev_class <- if (pct_prevalence >= 15) "CRITIQUE"
                 else if (pct_prevalence >= 10) "SÉRIEUX"
                 else if (pct_prevalence >= 5) "PAUVRE"
                 else "ACCEPTABLE"
    threshold <- 15
  } else if (indicator == "HAZ") {
    sev_class <- if (pct_prevalence >= 40) "CRITIQUE"
                 else if (pct_prevalence >= 30) "SÉRIEUX"
                 else if (pct_prevalence >= 20) "PAUVRE"
                 else "ACCEPTABLE"
    threshold <- 40
  } else {  # WAZ
    sev_class <- if (pct_prevalence >= 30) "CRITIQUE"
                 else if (pct_prevalence >= 20) "SÉRIEUX"
                 else if (pct_prevalence >= 10) "PAUVRE"
                 else "ACCEPTABLE"
    threshold <- 30
  }

  # Phase 1 : Énoncé principal
  ind_label <- switch(indicator,
    "GAM" = "malnutrition aiguë",
    "HAZ" = "retard de croissance",
    "WAZ" = "insuffisance pondérale"
  )

  narrative <- paste0(
    "**", ind_label, " = ", pct_prevalence, "% (", sev_class, ")**. ",
    if (pct_prevalence >= threshold) {
      paste0(
        "Situation ", tolower(sev_class),
        " dépassant le seuil d'alerte OMS (", threshold, "%). "
      )
    } else {
      paste0(
        "Situation acceptable selon les standards OMS (<", threshold, "%). "
      )
    }
  )

  # Phase 2 : Décomposition (sévère vs modéré)
  narrative <- paste0(narrative,
    "Composition : ", pct_severe, "% sévère + ", pct_moderate, "% modéré. "
  )

  # Phase 3 : Statistiques de distribution
  narrative <- paste0(narrative,
    "Distribution : µ=", round(mean_z, 2), ", σ=", round(sd_z, 2), ". "
  )

  # Phase 4 : Qualité des données
  qual_text <- if (quality_score >= 90) "excellente (très fiable)"
               else if (quality_score >= 75) "bonne (fiable)"
               else if (quality_score >= 60) "acceptable (avec réserve)"
               else "problématique (à interpréter avec prudence)"

  narrative <- paste0(narrative,
    "Qualité des données ", qual_text, " (score=", round(quality_score, 0), "/100). "
  )

  # Phase 5 : Flags
  pct_flags <- if (n_total > 0) round(100 * n_flags / n_total, 1) else 0
  narrative <- paste0(narrative,
    if (pct_flags <= 1) {
      paste0(pct_flags, "% d'observations atypiques — données cohérentes.")
    } else if (pct_flags <= 5) {
      paste0(pct_flags, "% d'observations atypiques — à valider.")
    } else {
      paste0(pct_flags, "% d'observations atypiques — données questionnables.")
    }
  )

  narrative
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. OMS BENCHMARK COMPARATOR — Tableau de comparaison avec seuils OMS
# ══════════════════════════════════════════════════════════════════════════════

#' Créer un tableau de comparaison avec les seuils OMS
#'
#' @param prevalences Named list : GAM, HAZ, WAZ (% values)
#' @return data.frame comparaison
gen_oms_benchmarks <- function(prevalences) {

  benchmarks <- tibble::tribble(
    ~Indicateur, ~Valeur, ~Acceptable, ~Pauvre, ~Sérieux, ~Critique, ~Distance_Seuil,
    "GAM (WHZ<-2)", prevalences$GAM, 5, 9, 14, 15,
    if (prevalences$GAM <= 5) "Far (safe)"
    else if (prevalences$GAM <= 9) paste0(5 - prevalences$GAM, " points (à risque)")
    else if (prevalences$GAM <= 14) paste0(15 - prevalences$GAM, " points (alerte)")
    else paste0("+", prevalences$GAM - 15, " points (CRITIQUE)"),

    "HAZ (Taille<-2)", prevalences$HAZ, 20, 29, 39, 40,
    if (prevalences$HAZ <= 20) "Far (safe)"
    else if (prevalences$HAZ <= 29) paste0(20 - prevalences$HAZ, " points (à risque)")
    else if (prevalences$HAZ <= 39) paste0(40 - prevalences$HAZ, " points (alerte)")
    else paste0("+", prevalences$HAZ - 40, " points (CRITIQUE)"),

    "WAZ (Poids<-2)", prevalences$WAZ, 10, 19, 29, 30,
    if (prevalences$WAZ <= 10) "Far (safe)"
    else if (prevalences$WAZ <= 19) paste0(10 - prevalences$WAZ, " points (à risque)")
    else if (prevalences$WAZ <= 29) paste0(30 - prevalences$WAZ, " points (alerte)")
    else paste0("+", prevalences$WAZ - 30, " points (CRITIQUE)")
  )

  benchmarks
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. RAPPORT EXPERT TEXTE — Auto-générer un rapport Markdown complet
# ══════════════════════════════════════════════════════════════════════════════

#' Générer un rapport expert complet en Markdown
gen_rapport_expert <- function(d, zs_calc_result, quality_result,
                                col_sexe = "sexe", col_age = "age_mois") {

  whz <- d$WHZ_calc[!is.na(d$WHZ_calc)]
  haz <- d$HAZ_calc[!is.na(d$HAZ_calc)]
  waz <- d$WAZ_calc[!is.na(d$WAZ_calc)]

  pct_gam <- round(100 * mean(whz < -2), 1)
  pct_sam <- round(100 * mean(whz < -3), 1)
  pct_haz <- round(100 * mean(haz < -2), 1)
  pct_waz <- round(100 * mean(waz < -2), 1)

  rapport <- paste0(
    "# Rapport Expert Z-scores OMS 2006\n\n",
    "**Date:** ", Sys.Date(), "\n\n",

    "## 📊 Résumé Exécutif\n\n",
    "Population analysée : **", nrow(d), " enfants** (0-60 mois).\n\n",
    "### Prévalences\n",
    "- **GAM (WHZ < -2)** : ", pct_gam, "% (SMA: ", pct_sam, "%)\n",
    "- **Stunting (HAZ < -2)** : ", pct_haz, "%\n",
    "- **Underweight (WAZ < -2)** : ", pct_waz, "%\n\n",

    "### Classification OMS\n",
    "| Indicateur | Prévalence | Classification |\n",
    "|-----------|-----------|----------------|\n",
    "| GAM | ", pct_gam, "% | ",
    if (pct_gam >= 15) "🔴 CRITIQUE" else if (pct_gam >= 10) "🟠 SÉRIEUX" else if (pct_gam >= 5) "🟡 PAUVRE" else "🟢 ACCEPTABLE",
    " |\n",
    "| HAZ | ", pct_haz, "% | ",
    if (pct_haz >= 40) "🔴 CRITIQUE" else if (pct_haz >= 30) "🟠 SÉRIEUX" else if (pct_haz >= 20) "🟡 PAUVRE" else "🟢 ACCEPTABLE",
    " |\n",
    "| WAZ | ", pct_waz, "% | ",
    if (pct_waz >= 30) "🔴 CRITIQUE" else if (pct_waz >= 20) "🟠 SÉRIEUX" else if (pct_waz >= 10) "🟡 PAUVRE" else "🟢 ACCEPTABLE",
    " |\n\n",

    "## 🔬 Qualité des Données\n\n",
    "**Score global : ", quality_result$score_global, "/100 (", quality_result$classification, ")**\n\n",
    "### Critères détaillés\n",
    "```\n"
  )

  # Ajouter les critères
  for (i in 1:nrow(quality_result$criteria_scores)) {
    row <- quality_result$criteria_scores[i, ]
    rapport <- paste0(rapport,
      row$Critère, " : ", row$Score, "/100 (", row$Note, ")\n"
    )
  }

  rapport <- paste0(rapport,
    "```\n\n",
    "## ⚠️ Anomalies Détectées\n\n"
  )

  if (length(quality_result$anomalies) > 0) {
    for (anom in quality_result$anomalies) {
      rapport <- paste0(rapport,
        "- **", anom$type, "** : ", anom$interpretation, "\n"
      )
    }
  } else {
    rapport <- paste0(rapport, "✅ Aucune anomalie majeure détectée.\n\n")
  }

  rapport <- paste0(rapport,
    "\n## 📋 Interprétation Clinique\n\n",
    if (pct_gam >= 15 || pct_haz >= 40 || pct_waz >= 30) {
      paste0(
        "⚠️ **Situation critique** : Les prévalences nutritionnelles dépassent les seuils d'alerte OMS. ",
        "Une intervention nutritionnelle urgente est recommandée.\n\n"
      )
    } else if (pct_gam >= 10 || pct_haz >= 30 || pct_waz >= 20) {
      paste0(
        "🟠 **Situation sérieuse** : Au moins un indicateur dépasse le seuil de 'pauvre'. ",
        "Des mesures de prévention et de traitement nutritionnels sont nécessaires.\n\n"
      )
    } else {
      paste0(
        "🟢 **Situation acceptable** : Les prévalences sont conformes aux standards OMS. ",
        "Maintenir la surveillance nutritionnelle.\n\n"
      )
    },
    "\n## 🎯 Recommandations\n\n",
    "1. Assurer un suivi régulier de l'état nutritionnel\n",
    "2. ", if (pct_sam > 5) "Accroître les programmes de traitement nutritionnel pour la malnutrition sévère\n" else "Maintenir les protocoles de dépistage de la malnutrition aiguë sévère\n",
    "3. ", if (pct_haz > 30) "Implémenter des interventions de prévention du retard de croissance (sensibilisation, micronutriments)\n" else "Continuer les activités de prévention du stunting\n",
    "4. Améliorer la qualité des mesures anthropométriques si anomalies détectées\n",
    "5. Renforcer la surveillance au niveau communautaire\n"
  )

  rapport
}
