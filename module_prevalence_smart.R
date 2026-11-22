# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_prevalence_smart.R — Prévalences SMART pondérées                  ║
# ║  Prévalences GAM/SAM/MAM/Stunting/Underweight                            ║
# ║  Plan de sondage : Clusters + Strates + Poids                            ║
# ║  DEFF · IC 95% design-based · Résultats par strate                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

HAS_SURVEY <- requireNamespace("survey", quietly = TRUE)
if (HAS_SURVEY) library(survey)

# ══════════════════════════════════════════════════════════════════════════════
# 1. CRÉATION DU DESIGN DE SONDAGE
# ══════════════════════════════════════════════════════════════════════════════

create_survey_design <- function(d, col_cluster = NULL, col_strata = NULL,
                                  col_weights = NULL) {
  if (!HAS_SURVEY) {
    warning("Package 'survey' non installé — calculs non pondérés (fallback)")
    return(NULL)
  }

  # Nettoyage : retirer les lignes avec NA dans les colonnes de design
  d_clean <- d
  if (!is.null(col_cluster) && col_cluster %in% names(d))
    d_clean <- d_clean[!is.na(d_clean[[col_cluster]]), ]
  if (!is.null(col_strata) && col_strata %in% names(d))
    d_clean <- d_clean[!is.na(d_clean[[col_strata]]), ]

  # Formules
  cluster_f <- if (!is.null(col_cluster) && col_cluster %in% names(d_clean))
    as.formula(paste0("~ ", col_cluster)) else ~ 1
  strata_f  <- if (!is.null(col_strata) && col_strata %in% names(d_clean))
    as.formula(paste0("~ ", col_strata)) else NULL
  weights_v <- if (!is.null(col_weights) && col_weights %in% names(d_clean))
    d_clean[[col_weights]] else rep(1, nrow(d_clean))

  # Créer le design
  tryCatch({
    if (!is.null(strata_f)) {
      # Strates avec un seul PSU → options pour éviter les erreurs
      options(survey.lonely.psu = "adjust")
      svydesign(ids = cluster_f, strata = strata_f,
                weights = ~ weights_v, data = d_clean, nest = TRUE)
    } else {
      options(survey.lonely.psu = "adjust")
      svydesign(ids = cluster_f, weights = ~ weights_v, data = d_clean)
    }
  }, error = function(e) {
    warning(paste("Erreur design:", e$message, "— fallback SRS"))
    svydesign(ids = ~ 1, weights = ~ weights_v, data = d_clean)
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. CALCUL DE PRÉVALENCE PONDÉRÉE AVEC IC 95% ET DEFF
# ══════════════════════════════════════════════════════════════════════════════

# Prévalence pour un indicateur binaire (0/1)
calc_prevalence_survey <- function(design, col_indicator, label = "Indicateur") {
  if (is.null(design) || !HAS_SURVEY) return(NULL)
  if (!col_indicator %in% names(design$variables)) return(NULL)

  d <- design$variables
  y <- d[[col_indicator]]
  if (!all(na.omit(y) %in% c(0, 1, TRUE, FALSE))) return(NULL)

  # Convertir logical → numeric pour éviter que svymean retourne 2 coefficients
  if (is.logical(y)) {
    design$variables[[col_indicator]] <- as.integer(y)
    y <- design$variables[[col_indicator]]
  }

  # Retirer les NA
  idx_valid <- !is.na(y)
  if (sum(idx_valid) < 5) return(NULL)

  design_sub <- subset(design, idx_valid)

  tryCatch({
    # Prévalence pondérée
    prev <- svymean(as.formula(paste0("~ ", col_indicator)), design_sub, na.rm = TRUE)
    prev_est <- as.numeric(coef(prev))[1]
    prev_se  <- as.numeric(SE(prev))[1]

    # IC 95% (méthode logit pour proportions)
    ci <- tryCatch(
      confint(svyciprop(as.formula(paste0("~ I(", col_indicator, " == 1)")),
                        design_sub, method = "logit")),
      error = function(e) {
        # Fallback : Wald CI
        c(max(0, prev_est - 1.96 * prev_se),
          min(1, prev_est + 1.96 * prev_se))
      }
    )
    ci_low  <- if (is.matrix(ci)) ci[1, 1] else ci[1]
    ci_high <- if (is.matrix(ci)) ci[1, 2] else ci[2]

    # DEFF (Design Effect)
    deff <- tryCatch({
      deff_val <- deff(svymean(as.formula(paste0("~ ", col_indicator)),
                                design_sub, na.rm = TRUE, deff = TRUE))
      as.numeric(deff_val)[1]
    }, error = function(e) NA)

    # N pondéré et non pondéré
    n_unweighted <- sum(idx_valid)
    n_cases      <- sum(y[idx_valid] == 1)
    n_weighted   <- tryCatch(
      as.numeric(svytotal(~ 1, design_sub)),
      error = function(e) n_unweighted
    )

    list(
      label        = label,
      indicator    = col_indicator,
      prevalence   = round(prev_est * 100, 2),
      se           = round(prev_se * 100, 2),
      ci_low       = round(ci_low * 100, 2),
      ci_high      = round(ci_high * 100, 2),
      deff         = round(deff, 2),
      n_unweighted = n_unweighted,
      n_cases      = n_cases,
      n_weighted   = round(n_weighted, 0)
    )
  }, error = function(e) {
    list(label = label, indicator = col_indicator, error = e$message)
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. PRÉVALENCES PAR STRATE (sexe, âge, région, etc.)
# ══════════════════════════════════════════════════════════════════════════════

calc_prevalence_by_group <- function(design, col_indicator, col_group,
                                      label = "Indicateur") {
  if (is.null(design) || !HAS_SURVEY) return(NULL)
  if (!col_indicator %in% names(design$variables)) return(NULL)
  if (!col_group %in% names(design$variables)) return(NULL)

  # Convertir logical → numeric
  if (is.logical(design$variables[[col_indicator]])) {
    design$variables[[col_indicator]] <- as.integer(design$variables[[col_indicator]])
  }

  d <- design$variables
  groups <- unique(na.omit(d[[col_group]]))

  map_dfr(groups, function(grp) {
    tryCatch({
      design_sub <- subset(design, d[[col_group]] == grp & !is.na(d[[col_indicator]]))
      n_sub <- nrow(design_sub$variables)
      if (n_sub < 5) return(NULL)

      prev <- svymean(as.formula(paste0("~ ", col_indicator)),
                      design_sub, na.rm = TRUE)
      prev_est <- as.numeric(coef(prev))[1]
      prev_se  <- as.numeric(SE(prev))[1]

      ci <- tryCatch(
        confint(svyciprop(as.formula(paste0("~ I(", col_indicator, " == 1)")),
                          design_sub, method = "logit")),
        error = function(e) c(max(0, prev_est - 1.96 * prev_se),
                               min(1, prev_est + 1.96 * prev_se))
      )
      ci_low  <- if (is.matrix(ci)) ci[1, 1] else ci[1]
      ci_high <- if (is.matrix(ci)) ci[1, 2] else ci[2]

      deff <- tryCatch({
        as.numeric(deff(svymean(as.formula(paste0("~ ", col_indicator)),
                                 design_sub, na.rm = TRUE, deff = TRUE)))[1]
      }, error = function(e) NA)

      n_cases <- sum(design_sub$variables[[col_indicator]] == 1, na.rm = TRUE)

      tibble(
        Indicateur  = label,
        Strate      = col_group,
        Groupe      = as.character(grp),
        N           = n_sub,
        `N cas`     = n_cases,
        `Prévalence (%)` = round(prev_est * 100, 2),
        `IC bas`    = round(ci_low * 100, 2),
        `IC haut`   = round(ci_high * 100, 2),
        DEFF        = round(deff, 2)
      )
    }, error = function(e) NULL)
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. FALLBACK — Calculs sans package survey (non pondérés mais avec DEFF)
# ══════════════════════════════════════════════════════════════════════════════

# DEFF manuel basé sur les clusters
calc_deff_manual <- function(y, clusters) {
  if (is.null(clusters)) return(1.0)
  y_clean <- y[!is.na(y)]
  cl_clean <- clusters[!is.na(y)]
  if (length(unique(cl_clean)) < 2) return(1.0)

  n <- length(y_clean)
  k <- length(unique(cl_clean))

  # Taille moyenne par cluster
  m_bar <- n / k

  # ICC (intraclass correlation) via ANOVA
  tryCatch({
    mod <- aov(y_clean ~ as.factor(cl_clean))
    s   <- summary(mod)[[1]]
    ms_between <- s[1, "Mean Sq"]
    ms_within  <- s[2, "Mean Sq"]

    # ICC = (MS_between - MS_within) / (MS_between + (m_bar - 1) * MS_within)
    icc <- (ms_between - ms_within) / (ms_between + (m_bar - 1) * ms_within)
    icc <- max(0, icc)  # ICC ne peut pas être négatif en pratique

    # DEFF = 1 + (m_bar - 1) * ICC
    deff <- 1 + (m_bar - 1) * icc
    max(1, deff)
  }, error = function(e) 1.0)
}

# Prévalence non pondérée avec IC Wilson et DEFF approximatif
calc_prevalence_manual <- function(d, col_indicator, col_cluster = NULL,
                                    label = "Indicateur") {
  y <- d[[col_indicator]]
  y_clean <- na.omit(y)
  if (length(y_clean) < 5) return(NULL)
  if (!all(y_clean %in% c(0, 1))) return(NULL)

  n    <- length(y_clean)
  k    <- sum(y_clean == 1)
  p    <- k / n

  # DEFF
  clusters <- if (!is.null(col_cluster) && col_cluster %in% names(d))
    d[[col_cluster]][!is.na(y)] else NULL
  deff <- calc_deff_manual(y_clean, clusters)

  # SE ajusté par DEFF
  se_srs  <- sqrt(p * (1 - p) / n)
  se_deff <- se_srs * sqrt(deff)

  # IC 95% Wilson ajusté par DEFF
  z <- 1.96
  n_eff <- n / deff  # taille effective

  # Wilson CI avec n effectif
  denom <- 1 + z^2 / n_eff
  centre <- (p + z^2 / (2 * n_eff)) / denom
  marge  <- z * sqrt(p * (1 - p) / n_eff + z^2 / (4 * n_eff^2)) / denom
  ci_low  <- max(0, centre - marge)
  ci_high <- min(1, centre + marge)

  list(
    label        = label,
    indicator    = col_indicator,
    prevalence   = round(p * 100, 2),
    se           = round(se_deff * 100, 2),
    ci_low       = round(ci_low * 100, 2),
    ci_high      = round(ci_high * 100, 2),
    deff         = round(deff, 2),
    n_unweighted = n,
    n_cases      = k,
    n_weighted   = n,
    method       = if (!is.null(clusters)) "Wilson + DEFF (cluster)" else "Wilson (SRS)"
  )
}

# Prévalence par groupe (fallback)
calc_prevalence_by_group_manual <- function(d, col_indicator, col_group,
                                             col_cluster = NULL,
                                             label = "Indicateur") {
  groups <- unique(na.omit(d[[col_group]]))

  map_dfr(groups, function(grp) {
    d_sub <- d[d[[col_group]] == grp & !is.na(d[[col_group]]), ]
    res <- calc_prevalence_manual(d_sub, col_indicator, col_cluster, label)
    if (is.null(res)) return(NULL)
    tibble(
      Indicateur       = label,
      Strate           = col_group,
      Groupe           = as.character(grp),
      N                = res$n_unweighted,
      `N cas`          = res$n_cases,
      `Prévalence (%)` = res$prevalence,
      `IC bas`         = res$ci_low,
      `IC haut`        = res$ci_high,
      DEFF             = res$deff
    )
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. FONCTION MAÎTRESSE — Calcul complet SMART
# ══════════════════════════════════════════════════════════════════════════════

run_smart_prevalences <- function(d,
                                   indicators,      # named list: name → col_name
                                   col_cluster = NULL,
                                   col_strata = NULL,
                                   col_weights = NULL,
                                   stratifications = NULL  # named list: label → col_name
                                   ) {
  results <- list()

  # ── Méthode survey si disponible ──
  if (HAS_SURVEY) {
    design <- create_survey_design(d, col_cluster, col_strata, col_weights)
    method_label <- if (!is.null(design)) "survey::svydesign (design-based)" else "Fallback manuel"

    # Prévalences globales
    global <- map_dfr(names(indicators), function(nm) {
      col <- indicators[[nm]]
      if (!col %in% names(d)) return(NULL)
      if (!is.null(design)) {
        res <- calc_prevalence_survey(design, col, nm)
      } else {
        res <- calc_prevalence_manual(d, col, col_cluster, nm)
      }
      if (is.null(res) || !is.null(res$error)) return(NULL)
      tibble(
        Indicateur       = res$label,
        N                = res$n_unweighted,
        `N cas`          = res$n_cases,
        `Prévalence (%)` = res$prevalence,
        `ES (%)`         = res$se,
        `IC bas`         = res$ci_low,
        `IC haut`        = res$ci_high,
        DEFF             = res$deff
      )
    })

    results$global <- global
    results$method <- method_label

    # Prévalences par strate
    if (!is.null(stratifications) && length(stratifications) > 0) {
      strat_results <- map_dfr(names(stratifications), function(strat_label) {
        col_grp <- stratifications[[strat_label]]
        if (!col_grp %in% names(d)) return(NULL)
        map_dfr(names(indicators), function(nm) {
          col_ind <- indicators[[nm]]
          if (!col_ind %in% names(d)) return(NULL)
          if (!is.null(design)) {
            calc_prevalence_by_group(design, col_ind, col_grp, nm)
          } else {
            calc_prevalence_by_group_manual(d, col_ind, col_grp, col_cluster, nm)
          }
        })
      })
      results$strat <- strat_results
    }

  # ── Fallback sans survey ──
  } else {
    results$method <- "Fallback : Wilson CI + DEFF approximatif (installer 'survey' pour résultats exacts)"

    global <- map_dfr(names(indicators), function(nm) {
      col <- indicators[[nm]]
      if (!col %in% names(d)) return(NULL)
      res <- calc_prevalence_manual(d, col, col_cluster, nm)
      if (is.null(res)) return(NULL)
      tibble(
        Indicateur       = res$label,
        N                = res$n_unweighted,
        `N cas`          = res$n_cases,
        `Prévalence (%)` = res$prevalence,
        `ES (%)`         = res$se,
        `IC bas`         = res$ci_low,
        `IC haut`        = res$ci_high,
        DEFF             = res$deff,
        Méthode          = res$method
      )
    })
    results$global <- global

    if (!is.null(stratifications) && length(stratifications) > 0) {
      strat_results <- map_dfr(names(stratifications), function(strat_label) {
        col_grp <- stratifications[[strat_label]]
        if (!col_grp %in% names(d)) return(NULL)
        map_dfr(names(indicators), function(nm) {
          col_ind <- indicators[[nm]]
          if (!col_ind %in% names(d)) return(NULL)
          calc_prevalence_by_group_manual(d, col_ind, col_grp, col_cluster, nm)
        })
      })
      results$strat <- strat_results
    }
  }

  # ── Informations de design ──
  results$design_info <- list(
    n_total   = nrow(d),
    n_clusters = if (!is.null(col_cluster) && col_cluster %in% names(d))
      length(unique(na.omit(d[[col_cluster]]))) else NA,
    n_strata  = if (!is.null(col_strata) && col_strata %in% names(d))
      length(unique(na.omit(d[[col_strata]]))) else NA,
    weighted  = !is.null(col_weights) && col_weights %in% names(d),
    has_survey = HAS_SURVEY
  )

  results
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. CLASSIFICATION OMS DES PRÉVALENCES
# ══════════════════════════════════════════════════════════════════════════════

classify_prevalence_oms <- function(indicator, prevalence) {
  indicator <- tolower(indicator)

  if (grepl("gam|émaciation globale|wasting", indicator)) {
    # GAM : <5% acceptable, 5-9.9% pauvre, 10-14.9% sérieux, ≥15% critique
    if (prevalence < 5) list(label = "Acceptable", color = "#27ae60", level = 1)
    else if (prevalence < 10) list(label = "Pauvre", color = "#f39c12", level = 2)
    else if (prevalence < 15) list(label = "Sérieux", color = "#e67e22", level = 3)
    else list(label = "Critique", color = "#e74c3c", level = 4)

  } else if (grepl("sam|émaciation sévère|severe", indicator)) {
    # SAM : <1% acceptable, 1-1.9% pauvre, 2-4.9% sérieux, ≥5% critique
    if (prevalence < 1) list(label = "Acceptable", color = "#27ae60", level = 1)
    else if (prevalence < 2) list(label = "Pauvre", color = "#f39c12", level = 2)
    else if (prevalence < 5) list(label = "Sérieux", color = "#e67e22", level = 3)
    else list(label = "Critique", color = "#e74c3c", level = 4)

  } else if (grepl("stunt|retard|chronic", indicator)) {
    # Stunting : <20% acceptable, 20-29.9% pauvre, 30-39.9% sérieux, ≥40% critique
    if (prevalence < 20) list(label = "Acceptable", color = "#27ae60", level = 1)
    else if (prevalence < 30) list(label = "Pauvre", color = "#f39c12", level = 2)
    else if (prevalence < 40) list(label = "Sérieux", color = "#e67e22", level = 3)
    else list(label = "Critique", color = "#e74c3c", level = 4)

  } else if (grepl("underweight|insuffisance|waz", indicator)) {
    # Underweight : <10% acceptable, 10-19.9% pauvre, 20-29.9% sérieux, ≥30% critique
    if (prevalence < 10) list(label = "Acceptable", color = "#27ae60", level = 1)
    else if (prevalence < 20) list(label = "Pauvre", color = "#f39c12", level = 2)
    else if (prevalence < 30) list(label = "Sérieux", color = "#e67e22", level = 3)
    else list(label = "Critique", color = "#e74c3c", level = 4)

  } else {
    list(label = "—", color = "#888", level = 0)
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# 7. RENDU HTML — Tableau SMART standard
# ══════════════════════════════════════════════════════════════════════════════

render_smart_table_html <- function(global_df) {
  if (is.null(global_df) || nrow(global_df) == 0) {
    return(tags$p(style = "color:#888;", "Aucun résultat disponible"))
  }

  rows_html <- paste(apply(global_df, 1, function(row) {
    prev <- as.numeric(row[["Prévalence (%)"]])
    cls  <- classify_prevalence_oms(row[["Indicateur"]], prev)

    paste0(
      "<tr>",
      "<td style='padding:8px 12px;font-weight:700;'>", row[["Indicateur"]], "</td>",
      "<td style='padding:8px 12px;text-align:center;'>", row[["N"]], "</td>",
      "<td style='padding:8px 12px;text-align:center;'>", row[["N cas"]], "</td>",
      "<td style='padding:8px 12px;text-align:center;font-size:16px;font-weight:800;color:",
      cls$color, ";'>", prev, "%</td>",
      "<td style='padding:8px 12px;text-align:center;font-size:12px;color:#555;'>",
      "[", row[["IC bas"]], " — ", row[["IC haut"]], "]</td>",
      "<td style='padding:8px 12px;text-align:center;'>",
      if (!is.na(row[["DEFF"]])) row[["DEFF"]] else "—", "</td>",
      "<td style='padding:8px 12px;text-align:center;'>",
      "<span style='background:", cls$color, ";color:white;padding:2px 8px;",
      "border-radius:4px;font-size:11px;font-weight:600;'>", cls$label, "</span></td>",
      "</tr>"
    )
  }), collapse = "")

  HTML(paste0(
    "<table style='width:100%;border-collapse:collapse;font-size:13px;border:1px solid #ddd;'>",
    "<thead><tr style='background:linear-gradient(135deg,#00426A,#008DC9);color:white;'>",
    "<th style='padding:10px 12px;text-align:left;'>Indicateur</th>",
    "<th style='padding:10px 12px;text-align:center;'>N</th>",
    "<th style='padding:10px 12px;text-align:center;'>Cas</th>",
    "<th style='padding:10px 12px;text-align:center;'>Prévalence</th>",
    "<th style='padding:10px 12px;text-align:center;'>IC 95%</th>",
    "<th style='padding:10px 12px;text-align:center;'>DEFF</th>",
    "<th style='padding:10px 12px;text-align:center;'>Classification OMS</th>",
    "</tr></thead><tbody>",
    rows_html,
    "</tbody></table>"
  ))
}

# ══════════════════════════════════════════════════════════════════════════════
# 8. GRAPHIQUES — Forest plot des prévalences
# ══════════════════════════════════════════════════════════════════════════════

plot_prevalence_forest <- function(df_prev, title = "Prévalences SMART avec IC 95%") {
  if (is.null(df_prev) || nrow(df_prev) == 0) return(plotly_empty())

  # Ajouter la classification
  df_plot <- df_prev |>
    mutate(
      label_full = if ("Groupe" %in% names(df_prev))
        paste0(Indicateur, " — ", Groupe) else Indicateur,
      color = sapply(`Prévalence (%)`, function(p) {
        if (p < 5) "#27ae60"
        else if (p < 10) "#f39c12"
        else if (p < 15) "#e67e22"
        else "#e74c3c"
      })
    )

  p <- ggplot(df_plot, aes(
    x = `Prévalence (%)`,
    y = reorder(label_full, `Prévalence (%)`),
    text = paste0(label_full, "\nPrévalence: ", `Prévalence (%)`, "%",
                  "\nIC 95%: [", `IC bas`, " — ", `IC haut`, "]",
                  if ("DEFF" %in% names(df_plot)) paste0("\nDEFF: ", DEFF) else "")
  )) +
    geom_errorbarh(aes(xmin = `IC bas`, xmax = `IC haut`),
                   height = 0.3, linewidth = 0.8, color = "#555") +
    geom_point(aes(color = `Prévalence (%)`), size = 4) +
    # Seuils OMS GAM
    geom_vline(xintercept = 5, linetype = "dotted", color = "#f39c12", linewidth = 0.5) +
    geom_vline(xintercept = 10, linetype = "dotted", color = "#e67e22", linewidth = 0.5) +
    geom_vline(xintercept = 15, linetype = "dashed", color = "#e74c3c", linewidth = 0.7) +
    scale_color_gradientn(
      colors = c("#27ae60", "#f39c12", "#e67e22", "#e74c3c"),
      values = scales::rescale(c(0, 5, 10, 15)),
      limits = c(0, max(df_plot$`IC haut`, 20)),
      name = "Prévalence (%)"
    ) +
    annotate("rect", xmin = 0, xmax = 5, ymin = -Inf, ymax = Inf,
             fill = "#27ae60", alpha = 0.04) +
    annotate("rect", xmin = 5, xmax = 10, ymin = -Inf, ymax = Inf,
             fill = "#f39c12", alpha = 0.04) +
    annotate("rect", xmin = 10, xmax = 15, ymin = -Inf, ymax = Inf,
             fill = "#e67e22", alpha = 0.04) +
    annotate("rect", xmin = 15, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "#e74c3c", alpha = 0.04) +
    labs(x = "Prévalence (%)", y = "", title = title,
         caption = "Seuils OMS : 5% (pauvre) | 10% (sérieux) | 15% (critique)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
  ggplotly(p, tooltip = "text")
}

# ══════════════════════════════════════════════════════════════════════════════
# 9. RÉSUMÉ DESIGN DE SONDAGE (HTML)
# ══════════════════════════════════════════════════════════════════════════════

render_design_info_html <- function(design_info, method_label) {
  tags$div(
    style = "background:#f0f8ff;border:1px solid #b8daff;border-radius:8px;padding:14px;margin-bottom:12px;",
    tags$div(style = "font-size:11px;font-weight:700;color:#00426A;text-transform:uppercase;letter-spacing:.8px;margin-bottom:8px;",
             "DESIGN DU PLAN DE SONDAGE"),
    tags$div(style = "display:flex;gap:16px;flex-wrap:wrap;",
      tags$div(style = "flex:1;min-width:120px;text-align:center;",
        tags$div(style = "font-size:24px;font-weight:800;color:#00426A;",
                 design_info$n_total),
        tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;",
                 "Observations")),
      tags$div(style = "flex:1;min-width:120px;text-align:center;",
        tags$div(style = "font-size:24px;font-weight:800;color:#008DC9;",
                 if (!is.na(design_info$n_clusters)) design_info$n_clusters else "—"),
        tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;",
                 "Clusters (PSU)")),
      tags$div(style = "flex:1;min-width:120px;text-align:center;",
        tags$div(style = "font-size:24px;font-weight:800;color:#2980B9;",
                 if (!is.na(design_info$n_strata)) design_info$n_strata else "—"),
        tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;",
                 "Strates")),
      tags$div(style = "flex:1;min-width:120px;text-align:center;",
        tags$div(style = "font-size:18px;font-weight:700;",
                 if (design_info$weighted) "✅ Oui" else "⬜ Non"),
        tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;",
                 "Poids d'échantillonnage"))
    ),
    tags$div(style = "margin-top:8px;font-size:11px;color:#555;border-top:1px solid #ddd;padding-top:6px;",
      tags$b("Méthode : "), method_label,
      if (!design_info$has_survey)
        tags$span(style = "color:#e74c3c;font-weight:600;",
                  " ⚠ Installer le package 'survey' pour des résultats design-based exacts")
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 10. MOYENNES DESIGN-BASED — Indicateurs continus SA (FCS, rCSI, HDDS, HHS)
# ══════════════════════════════════════════════════════════════════════════════

#' Calcule les moyennes pondérées avec IC 95% design-based pour des variables continues.
#' @param design  svydesign object ou NULL
#' @param cols    named character vector : nom_affichage → col_data
#' @param group_col  colonne de stratification (NULL = global)
#' @return tibble avec Indicateur, Groupe, N, Moy, IC_lo, IC_hi, SE, DEFF
calc_means_survey <- function(design, cols, group_col = NULL) {
  if (!HAS_SURVEY || is.null(design)) return(NULL)
  valid_cols <- cols[cols %in% names(design$variables)]
  if (length(valid_cols) == 0) return(NULL)

  compute_one <- function(des, ind_label, col_name) {
    tryCatch({
      sub <- subset(des, !is.na(des$variables[[col_name]]))
      if (nrow(sub$variables) < 5) return(NULL)

      m   <- svymean(as.formula(paste0("~", col_name)), sub, na.rm = TRUE, deff = TRUE)
      ci  <- confint(m, level = 0.95)
      dv  <- tryCatch(as.numeric(deff(m))[1], error = function(e) NA_real_)
      n   <- nrow(sub$variables)

      tibble(
        Indicateur = ind_label,
        N          = n,
        Moyenne    = round(as.numeric(coef(m)), 3),
        IC_lo      = round(ci[1], 3),
        IC_hi      = round(ci[2], 3),
        SE         = round(as.numeric(SE(m)), 3),
        DEFF       = round(dv, 2),
        n_eff      = round(n / max(dv, 1), 0)
      )
    }, error = function(e) NULL)
  }

  if (is.null(group_col) || !group_col %in% names(design$variables)) {
    # Global
    bind_rows(lapply(names(valid_cols), function(nm)
      compute_one(design, nm, valid_cols[nm])))
  } else {
    # Par groupe
    groups <- unique(na.omit(design$variables[[group_col]]))
    bind_rows(lapply(groups, function(grp) {
      des_sub <- subset(design, design$variables[[group_col]] == grp)
      res <- bind_rows(lapply(names(valid_cols), function(nm)
        compute_one(des_sub, nm, valid_cols[nm])))
      if (!is.null(res) && nrow(res) > 0) mutate(res, Groupe = as.character(grp), .before = 1)
      else NULL
    }))
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# 11. COMPARAISON DE DESIGNS — IC et DEFF selon 3 approches
# ══════════════════════════════════════════════════════════════════════════════

#' Compare les IC 95% et DEFF entre 3 designs pour un même indicateur binaire.
#' Designs : (1) Stratifié+Grappes (2) Grappes seules (3) SRS naïf
#' @param d          data.frame des observations
#' @param col_inds   named character vector : libellé → col_name (binaire 0/1)
#' @param col_cluster character ou NULL
#' @param col_strata  character ou NULL
#' @param col_weights character ou NULL
#' @return tibble long avec colonnes Design, Indicateur, Prev, CI_lo, CI_hi, SE, Largeur_IC
calc_design_comparison <- function(d, col_inds, col_cluster = NULL,
                                    col_strata = NULL, col_weights = NULL) {
  if (!HAS_SURVEY) return(NULL)
  valid <- col_inds[col_inds %in% names(d)]
  if (length(valid) == 0) return(NULL)

  options(survey.lonely.psu = "adjust")

  # Créer les 3 designs
  designs_list <- list()

  # Design 1 : Stratifié + Grappes (le plus complet)
  if (!is.null(col_cluster) && col_cluster %in% names(d)) {
    ids_f <- as.formula(paste0("~", col_cluster))
    if (!is.null(col_strata) && col_strata %in% names(d)) {
      designs_list[["Stratifié + Grappes"]] <- tryCatch(
        svydesign(ids = ids_f, strata = as.formula(paste0("~", col_strata)),
                  data = d, nest = TRUE),
        error = function(e) NULL
      )
    }
    # Design 2 : Grappes seules (sans strate)
    designs_list[["Grappes seules"]] <- tryCatch(
      svydesign(ids = ids_f, data = d, nest = TRUE),
      error = function(e) NULL
    )
  }
  # Design 3 : SRS naïf (aucun ajustement)
  designs_list[["SRS (naïf)"]] <- svydesign(ids = ~1, data = d)

  bind_rows(lapply(names(designs_list), function(des_nm) {
    des <- designs_list[[des_nm]]
    if (is.null(des)) return(NULL)

    bind_rows(lapply(names(valid), function(nm) {
      col <- valid[nm]
      tryCatch({
        sub <- subset(des, !is.na(des$variables[[col]]))
        if (nrow(sub$variables) < 5) return(NULL)

        m  <- svymean(as.formula(paste0("~", col)), sub, na.rm = TRUE, deff = TRUE)
        ci <- confint(m, level = 0.95)
        dv <- tryCatch(as.numeric(deff(m))[1], error = function(e) NA_real_)
        p  <- as.numeric(coef(m))
        se <- as.numeric(SE(m))
        n  <- nrow(sub$variables)

        tibble(
          Design     = des_nm,
          Indicateur = nm,
          N          = n,
          Prev_pct   = round(p * 100, 2),
          CI_lo      = round(ci[1] * 100, 2),
          CI_hi      = round(ci[2] * 100, 2),
          Largeur_IC = round((ci[2] - ci[1]) * 100, 2),
          SE_pct     = round(se * 100, 3),
          DEFF       = round(dv, 3),
          n_eff      = round(n / max(dv, 1), 0)
        )
      }, error = function(e) NULL)
    }))
  }))
}

#' Forest plot de comparaison des designs (1 indicateur, 3 designs, barres CI)
plot_design_comparison <- function(df_cmp, indicator_filter = NULL) {
  if (is.null(df_cmp) || nrow(df_cmp) == 0)
    return(plotly_empty() |> layout(title = list(text = "Aucune donnée", x = .5)))

  df <- if (!is.null(indicator_filter))
    df_cmp |> filter(Indicateur %in% indicator_filter) else df_cmp

  # Couleurs par design
  col_des <- c(
    "Stratifié + Grappes" = "#00426A",
    "Grappes seules"       = "#008DC9",
    "SRS (naïf)"           = "#95A5A6"
  )

  df$label_y <- paste0(df$Indicateur, " — ", df$Design)
  df$hover   <- paste0(
    "<b>", df$Indicateur, " — ", df$Design, "</b><br>",
    "Prévalence : ", df$Prev_pct, "%<br>",
    "IC 95% : [", df$CI_lo, "% – ", df$CI_hi, "%]<br>",
    "Largeur IC : ", df$Largeur_IC, " pts<br>",
    "DEFF : ", df$DEFF, "<br>",
    "N effectif : ", df$n_eff
  )

  des_order  <- c("SRS (naïf)", "Grappes seules", "Stratifié + Grappes")
  ind_order  <- unique(df$Indicateur)

  # Regrouper verticalement : indicateurs séparés, designs colorés
  df <- df |>
    mutate(
      y_pos = as.integer(factor(Indicateur, levels = ind_order)) * 4 +
              as.integer(factor(Design, levels = des_order)) - 2
    )

  fig <- plot_ly()
  for (des_nm in names(col_des)) {
    d_des <- df |> filter(Design == des_nm)
    if (nrow(d_des) == 0) next
    fig <- fig |>
      add_segments(
        data = d_des,
        x = ~CI_lo, xend = ~CI_hi,
        y = ~y_pos, yend = ~y_pos,
        line = list(color = col_des[des_nm], width = 3),
        name = des_nm, legendgroup = des_nm, showlegend = TRUE,
        hoverinfo = "none"
      ) |>
      add_trace(
        data = d_des, x = ~Prev_pct, y = ~y_pos,
        type = "scatter", mode = "markers",
        marker = list(color = col_des[des_nm], size = 10,
                      symbol = "diamond", line = list(color = "white", width = 1)),
        text = ~hover, hoverinfo = "text",
        name = des_nm, legendgroup = des_nm, showlegend = FALSE
      )
  }

  # Annotations indicateurs
  annots <- lapply(seq_along(ind_order), function(i) {
    list(x = -1, y = i * 4, text = paste0("<b>", ind_order[i], "</b>"),
         xref = "x", yref = "y", showarrow = FALSE,
         xanchor = "right", font = list(size = 11, color = "#00426A"))
  })

  fig |> layout(
    title = list(
      text = "<b>Comparaison des IC 95% selon le design de sondage</b><br><sup>Diamant = estimation ponctuelle | Segment = IC 95%</sup>",
      font = list(size = 13, color = "#00426A"), x = 0
    ),
    xaxis = list(title = "Prévalence (%)", gridcolor = "#f0f0f0"),
    yaxis = list(showticklabels = FALSE, gridcolor = "#f0f0f0"),
    legend = list(orientation = "h", y = -0.15, x = 0,
                  title = list(text = "Design de sondage")),
    annotations = annots,
    plot_bgcolor = "white", paper_bgcolor = "white",
    margin = list(l = 140)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 12. ICC (INTRACLASS CORRELATION COEFFICIENT) PAR INDICATEUR
# ══════════════════════════════════════════════════════════════════════════════

#' Calcule l'ICC et le DEFF théorique par indicateur à partir des données brutes.
#' Formule : ICC = (MS_between - MS_within) / (MS_between + (m̄ - 1) × MS_within)
#' DEFF théorique = 1 + (m̄ - 1) × ICC
#' @param d          data.frame
#' @param col_inds   named character vector : libellé → col_name
#' @param col_cluster character
#' @return tibble avec Indicateur, ICC, DEFF_theorique, m_bar, n_clusters, interpr.
calc_icc_table <- function(d, col_inds, col_cluster) {
  if (is.null(col_cluster) || !col_cluster %in% names(d)) return(NULL)
  valid <- col_inds[col_inds %in% names(d)]
  if (length(valid) == 0) return(NULL)

  clusters <- d[[col_cluster]]
  k <- length(unique(na.omit(clusters)))
  if (k < 2) return(NULL)

  bind_rows(lapply(names(valid), function(nm) {
    col <- valid[nm]
    y   <- as.numeric(d[[col]])
    idx <- !is.na(y) & !is.na(clusters)
    y_c <- y[idx]; cl_c <- clusters[idx]
    n_tot <- length(y_c)
    if (n_tot < k * 2) return(NULL)

    m_bar <- n_tot / k

    icc_val <- tryCatch({
      mod <- aov(y_c ~ as.factor(cl_c))
      s   <- summary(mod)[[1]]
      msb <- s["as.factor(cl_c)", "Mean Sq"]
      msw <- s["Residuals", "Mean Sq"]
      icc <- (msb - msw) / (msb + (m_bar - 1) * msw)
      max(-1 / (k - 1), icc)  # borner à -1/(k-1) minimum théorique
    }, error = function(e) NA_real_)

    deff_th <- if (!is.na(icc_val)) round(1 + (m_bar - 1) * icc_val, 3) else NA_real_
    icc_r   <- round(icc_val, 4)

    interpr <- if (is.na(icc_r)) "N/A" else
      if (icc_r < 0.05) "Faible (< 0.05)" else
      if (icc_r < 0.15) "Modéré (0.05–0.15)" else
      if (icc_r < 0.30) "Élevé (0.15–0.30)" else "Très élevé (≥ 0.30)"

    tibble(
      Indicateur      = nm,
      ICC             = icc_r,
      DEFF_theorique  = deff_th,
      m_bar           = round(m_bar, 1),
      n_clusters      = k,
      N_total         = n_tot,
      Interprétation  = interpr
    )
  }))
}

# ══════════════════════════════════════════════════════════════════════════════
# 13. DIAGNOSTICS DU PLAN DE SONDAGE
# ══════════════════════════════════════════════════════════════════════════════

#' Résumé des tailles de PSU (grappes) et détection des anomalies
calc_design_diagnostics <- function(d, col_cluster, col_strata = NULL) {
  if (is.null(col_cluster) || !col_cluster %in% names(d)) return(NULL)

  cluster_sizes <- d |>
    group_by(Cluster = .data[[col_cluster]]) |>
    summarise(N = n(), .groups = "drop")

  if (!is.null(col_strata) && col_strata %in% names(d)) {
    cluster_sizes <- d |>
      group_by(Strate = .data[[col_strata]], Cluster = .data[[col_cluster]]) |>
      summarise(N = n(), .groups = "drop")
  }

  # Anomalies
  q1 <- quantile(cluster_sizes$N, 0.25)
  q3 <- quantile(cluster_sizes$N, 0.75)
  iqr <- q3 - q1
  outliers <- cluster_sizes[cluster_sizes$N < q1 - 1.5 * iqr |
                             cluster_sizes$N > q3 + 1.5 * iqr, ]

  # Strates avec un seul cluster (lonely PSU)
  lonely <- if (!is.null(col_strata) && col_strata %in% names(d)) {
    d |>
      group_by(.data[[col_strata]]) |>
      summarise(n_clusters = n_distinct(.data[[col_cluster]]), .groups = "drop") |>
      filter(n_clusters == 1) |>
      pull(.data[[col_strata]])
  } else character(0)

  list(
    cluster_sizes = cluster_sizes,
    n_clusters    = nrow(cluster_sizes),
    min_size      = min(cluster_sizes$N),
    max_size      = max(cluster_sizes$N),
    mean_size     = round(mean(cluster_sizes$N), 1),
    med_size      = median(cluster_sizes$N),
    cv_size       = round(sd(cluster_sizes$N) / mean(cluster_sizes$N) * 100, 1),
    outlier_clusters = nrow(outliers),
    lonely_strata    = lonely
  )
}

#' Graphique de distribution des tailles de PSU
plot_cluster_sizes <- function(diag_info) {
  if (is.null(diag_info)) return(plotly_empty())

  df <- diag_info$cluster_sizes
  q1 <- quantile(df$N, 0.25)
  q3 <- quantile(df$N, 0.75)

  fig <- plot_ly(df, x = ~N, type = "histogram", nbinsx = 20,
                 marker = list(color = "#008DC9", line = list(color = "white", width = 1)),
                 hovertemplate = "Taille : %{x}<br>Fréquence : %{y}<extra></extra>",
                 name = "PSU") |>
    add_vline(x = diag_info$mean_size, line_color = "#E67E22",
              line_dash = "dash", line_width = 2,
              annotation_text = paste0("Moy = ", diag_info$mean_size),
              annotation_font = list(color = "#E67E22", size = 11)) |>
    add_vline(x = diag_info$med_size, line_color = "#27AE60",
              line_dash = "dot", line_width = 2,
              annotation_text = paste0("Méd = ", diag_info$med_size),
              annotation_font = list(color = "#27AE60", size = 11)) |>
    layout(
      title = list(
        text = paste0(
          "<b>Distribution des tailles de grappe (PSU)</b><br>",
          "<sup>", diag_info$n_clusters, " grappes | ",
          "Min=", diag_info$min_size, " | Max=", diag_info$max_size,
          " | CV=", diag_info$cv_size, "%</sup>"
        ),
        font = list(size = 13, color = "#00426A"), x = 0
      ),
      xaxis = list(title = "N observations par grappe", gridcolor = "#f0f0f0"),
      yaxis = list(title = "Nombre de grappes", gridcolor = "#f0f0f0"),
      plot_bgcolor = "white", paper_bgcolor = "white"
    )
  fig
}

#' HTML des alertes du plan de sondage (lonely PSU, outliers, etc.)
render_design_alerts <- function(diag_info) {
  if (is.null(diag_info)) return(NULL)

  alerts <- list()

  # PSU outliers
  if (diag_info$outlier_clusters > 0) {
    alerts[[length(alerts) + 1]] <- tags$div(
      style = "background:#fff3e0;border-left:3px solid #E67E22;padding:8px 10px;border-radius:0 4px 4px 0;margin-bottom:6px;font-size:11px;",
      icon("exclamation-triangle", style = "color:#E67E22;"), " ",
      tags$b(paste0(diag_info$outlier_clusters, " grappe(s) avec taille atypique")),
      " (hors Q1-1.5×IQR / Q3+1.5×IQR). Vérifier la collecte de terrain."
    )
  }

  # Lonely PSU
  if (length(diag_info$lonely_strata) > 0) {
    alerts[[length(alerts) + 1]] <- tags$div(
      style = "background:#ffebee;border-left:3px solid #C0392B;padding:8px 10px;border-radius:0 4px 4px 0;margin-bottom:6px;font-size:11px;",
      icon("exclamation-circle", style = "color:#C0392B;"), " ",
      tags$b(paste0(length(diag_info$lonely_strata), " strate(s) avec 1 seule grappe")),
      " (PSU solitaire) : ",
      paste(diag_info$lonely_strata, collapse = ", "),
      ". L'option ", tags$code("survey.lonely.psu = 'adjust'"), " est appliquée automatiquement."
    )
  }

  # CV élevé des tailles
  if (!is.null(diag_info$cv_size) && diag_info$cv_size > 30) {
    alerts[[length(alerts) + 1]] <- tags$div(
      style = "background:#fff9c4;border-left:3px solid #F1C40F;padding:8px 10px;border-radius:0 4px 4px 0;margin-bottom:6px;font-size:11px;",
      icon("info-circle", style = "color:#F1C40F;"), " ",
      tags$b(paste0("Forte variabilité des tailles de grappe (CV = ", diag_info$cv_size, "%)")),
      ". Considérer des poids de sondage (probabilités d'inclusion) si disponibles."
    )
  }

  if (length(alerts) == 0) {
    alerts[[1]] <- tags$div(
      style = "background:#e8f5e9;border-left:3px solid #27AE60;padding:8px 10px;border-radius:0 4px 4px 0;font-size:11px;",
      icon("check-circle", style = "color:#27AE60;"), " ",
      tags$b("Plan de sondage validé"), " — Aucune anomalie détectée."
    )
  }

  tagList(alerts)
}

# ══════════════════════════════════════════════════════════════════════════════
# 14. EXPORT FORMAT SMART/ENA
# ══════════════════════════════════════════════════════════════════════════════

#' Formate les résultats pour export CSV compatible ENA/SMART
format_smart_export <- function(global_df, strat_df = NULL,
                                 design_info = NULL, method_label = "") {
  if (is.null(global_df) || nrow(global_df) == 0) return(NULL)

  # En-tête métadonnées
  meta_rows <- tibble(
    `#METADATA` = c(
      paste0("Export NutriAnalyse Pro v2 — ", Sys.time()),
      paste0("Méthode : ", method_label),
      paste0("N total : ", if (!is.null(design_info)) design_info$n_total else "?"),
      paste0("N grappes : ", if (!is.null(design_info)) design_info$n_clusters else "?"),
      paste0("N strates : ", if (!is.null(design_info)) design_info$n_strata else "?")
    )
  )

  # Table principale
  main_cols <- c("Indicateur", "N", "N cas", "Prévalence (%)", "ES (%)",
                 "IC bas", "IC haut", "DEFF")
  main_out <- global_df |>
    select(any_of(main_cols)) |>
    mutate(Strate = "Global", Groupe = "Tous", .before = 1)

  # Ajouter par strate si disponible
  if (!is.null(strat_df) && nrow(strat_df) > 0) {
    strat_out <- strat_df |>
      select(any_of(c("Indicateur", "Strate", "Groupe", "N",
                      "N cas", "Prévalence (%)", "IC bas", "IC haut", "DEFF")))
    main_out <- bind_rows(main_out, strat_out)
  }

  # Ajouter colonnes de conformité OMS
  main_out <- main_out |>
    rowwise() |>
    mutate(
      Classification_OMS = {
        cls <- classify_prevalence_oms(Indicateur, `Prévalence (%)`)
        cls$label
      },
      n_eff = if ("DEFF" %in% names(main_out) && !is.na(DEFF) && DEFF > 0)
                round(N / DEFF, 0) else N
    ) |>
    ungroup()

  main_out
}

cat("module_prevalence_smart.R chargé — Prévalences SMART pondérées · DEFF · ICC · Diagnostics\n")
