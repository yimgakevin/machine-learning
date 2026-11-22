# Extrait de app_v2_final.R — dashboard accueil — rapport automatique Word/HTML
  # ── Rapport automatique — helper : agréger par groupe pour graphiques ────────
  agg_cat_by_grp <- function(d, score_col, grp_col) {
    if (is.null(grp_col) || !(grp_col %in% names(d))) return(NULL)
    if (!(score_col %in% names(d))) return(NULL)
    d |>
      filter(!is.na(.data[[score_col]]), !is.na(.data[[grp_col]])) |>
      count(grp = as.character(.data[[grp_col]]),
            !!score_col := .data[[score_col]]) |>
      group_by(grp) |>
      mutate(pct = n / sum(n) * 100) |>
      ungroup() |>
      rename_with(~ score_col, all_of(score_col))
  }

  temporal_agg_rpt <- function(d, sv_col, score_col) {
    if (is.null(sv_col) || !(sv_col %in% names(d))) return(NULL)
    if (!(score_col %in% names(d))) return(NULL)
    rounds_ord <- sort_rounds(unique(na.omit(as.character(d[[sv_col]]))))
    d |>
      filter(!is.na(.data[[score_col]]), !is.na(.data[[sv_col]])) |>
      mutate(.round_chr = factor(as.character(.data[[sv_col]]), levels = rounds_ord)) |>
      group_by(.round_chr) |>
      summarise(
        mean_val = mean(.data[[score_col]], na.rm = TRUE),
        sd_val   = sd(.data[[score_col]],   na.rm = TRUE),
        n        = sum(!is.na(.data[[score_col]])),
        .groups  = "drop"
      ) |>
      mutate(
        ci_lo = mean_val - 1.96 * sd_val / sqrt(pmax(n, 1)),
        ci_hi = mean_val + 1.96 * sd_val / sqrt(pmax(n, 1))
      )
  }

  # ── Rapport automatique — reactive données ────────────────────────────────────
  rapport_data <- reactive({
    d <- df_f(); m <- map_cols()
    if (is.null(d) || nrow(d) == 0) return(NULL)

    geo       <- selected_geo()
    pays      <- if (!is.null(geo) && nzchar(geo$country_code)) geo$country_name else ""
    grp_col   <- m$region
    grp_label <- if (!is.null(grp_col) && grp_col %in% names(d)) grp_col else "Groupe"
    sv_col    <- m$survey

    # Flags disponibilité
    has_fcs  <- ".fcs"  %in% names(d) && !all(is.na(d$.fcs))
    has_rcsi <- ".rcsi" %in% names(d) && !all(is.na(d$.rcsi))
    has_hdds <- ".hdds" %in% names(d) && !all(is.na(d$.hdds))
    has_hhs  <- ".hhs"  %in% names(d) && !all(is.na(d$.hhs))
    has_temporal <- !is.null(sv_col) && sv_col %in% names(d) &&
      (has_fcs || has_rcsi) &&
      length(unique(na.omit(as.character(d[[sv_col]])))) >= 2

    # ── Statistiques FCS ──────────────────────────────────────────────────────
    fcs_mean <- fcs_sd <- fcs_median <- fcs_min <- fcs_max <- NA
    pct_fcs_poor <- pct_fcs_limit <- pct_fcs_acceptable <- 0
    if (has_fcs) {
      fcs_v  <- d$.fcs[!is.na(d$.fcs)]
      fcs_mean   <- mean(fcs_v);   fcs_sd     <- sd(fcs_v)
      fcs_median <- median(fcs_v); fcs_min    <- min(fcs_v); fcs_max <- max(fcs_v)
      if ("fcs_cat" %in% names(d)) {
        fc  <- table(d$fcs_cat); tot <- sum(fc)
        spct <- function(k) { v <- as.numeric(fc[k]); if (is.na(v)) 0 else v / tot * 100 }
        pct_fcs_poor       <- spct("Pauvre (<21)")
        pct_fcs_limit      <- spct("Limite (21-35)")
        pct_fcs_acceptable <- spct("Acceptable (>35)")
      }
    }

    # ── Statistiques rCSI ─────────────────────────────────────────────────────
    rcsi_mean <- rcsi_sd <- rcsi_median <- rcsi_min <- rcsi_max <- NA
    pct_rcsi_low <- pct_rcsi_mid <- pct_rcsi_high <- 0
    if (has_rcsi) {
      rv <- d$.rcsi[!is.na(d$.rcsi)]
      rcsi_mean   <- mean(rv);   rcsi_sd     <- sd(rv)
      rcsi_median <- median(rv); rcsi_min    <- min(rv); rcsi_max <- max(rv)
      if ("rcsi_cat" %in% names(d)) {
        rc  <- table(d$rcsi_cat); tot <- sum(rc)
        spct <- function(k) { v <- as.numeric(rc[k]); if (is.na(v)) 0 else v / tot * 100 }
        pct_rcsi_low  <- spct("Pas/Peu de coping")
        pct_rcsi_mid  <- spct("Coping moyen")
        pct_rcsi_high <- spct("Coping \u00e9lev\u00e9 (\u226519)")
      }
    }

    # ── Statistiques HDDS ────────────────────────────────────────────────────
    hdds_mean <- hdds_sd <- hdds_median <- NA
    pct_hdds_low <- pct_hdds_mid <- pct_hdds_high <- 0
    if (has_hdds) {
      hv <- d$.hdds[!is.na(d$.hdds)]
      hdds_mean <- mean(hv); hdds_sd <- sd(hv); hdds_median <- median(hv)
      if ("hdds_cat" %in% names(d)) {
        hc  <- table(d$hdds_cat); tot <- sum(hc)
        spct <- function(k) { v <- as.numeric(hc[k]); if (is.na(v)) 0 else v / tot * 100 }
        pct_hdds_low  <- spct("Faible diversit\u00e9 (\u22643)")
        pct_hdds_mid  <- spct("Diversit\u00e9 moyenne (4-5)")
        pct_hdds_high <- spct("Bonne diversit\u00e9 (\u22656)")
      }
    }

    # ── Statistiques HHS ─────────────────────────────────────────────────────
    hhs_mean <- hhs_sd <- NA
    if (has_hhs) {
      hh <- d$.hhs[!is.na(d$.hhs)]
      hhs_mean <- mean(hh); hhs_sd <- sd(hh)
    }

    # ── Double vulnérabilité ──────────────────────────────────────────────────
    n_valid <- sum(!is.na(d$rcsi_cat) & !is.na(d$fcs_cat))
    n_dv    <- sum(
      !is.na(d$rcsi_cat) & !is.na(d$fcs_cat) &
        d$rcsi_cat == "Coping \u00e9lev\u00e9 (\u226519)" &
        d$fcs_cat  == "Pauvre (<21)", na.rm = TRUE
    )
    pct_dv <- if (n_valid > 0) round(n_dv / n_valid * 100, 1) else 0

    # ── Données pour graphiques ───────────────────────────────────────────────
    fcs_cat_by_grp  <- if (has_fcs  && "fcs_cat"  %in% names(d))
      agg_cat_by_grp(d, "fcs_cat",  grp_col) else NULL
    rcsi_cat_by_grp <- if (has_rcsi && "rcsi_cat" %in% names(d))
      agg_cat_by_grp(d, "rcsi_cat", grp_col) else NULL
    hdds_cat_by_grp <- if (has_hdds && "hdds_cat" %in% names(d))
      agg_cat_by_grp(d, "hdds_cat", grp_col) else NULL

    # Scatter (échantillon max 2000 pts)
    scatter_data <- if (has_fcs && has_rcsi) {
      sc_cols <- intersect(c(".fcs", ".rcsi", "rcsi_cat", "fcs_cat"), names(d))
      sc <- d[complete.cases(d[sc_cols]), sc_cols]
      if (nrow(sc) > 2000) sc[sample(nrow(sc), 2000), ] else sc
    } else NULL

    # Données temporelles
    temporal_fcs  <- if (has_temporal && has_fcs)
      temporal_agg_rpt(d, sv_col, ".fcs") else NULL
    temporal_rcsi <- if (has_temporal && has_rcsi)
      temporal_agg_rpt(d, sv_col, ".rcsi") else NULL
    rounds <- if (has_temporal)
      sort_rounds(unique(na.omit(as.character(d[[sv_col]])))) else character(0)

    # Tableau croisé (version display sans colonnes cachées)
    crosstab_disp <- if ("rcsi_cat" %in% names(d) && "fcs_cat" %in% names(d)) {
      ct <- build_crosstab(d, grp_col)
      if (!is.null(ct)) {
        ct |>
          transmute(
            Groupe          = .grp,
            `Cat. rCSI`     = as.character(rcsi_cat),
            `FCS Pauvre (<21)`     = paste0(n_P, " (", pct_P, "%)"),
            `FCS Limite (21-35)`   = paste0(n_L, " (", pct_L, "%)"),
            `FCS Acceptable (>35)` = paste0(n_A, " (", pct_A, "%)"),
            Total                  = paste0(Total_n, " (", pct_T, "%)")
          )
      } else NULL
    } else NULL

    # Résultats Chi²/Fisher
    chi2_stat <- chi2_df <- chi2_pval <- cramer_v <- fisher_pval <- min_exp <- NULL
    use_fisher <- FALSE; concordant <- TRUE
    if ("rcsi_cat" %in% names(d) && "fcs_cat" %in% names(d)) {
      d_v    <- d[!is.na(d$rcsi_cat) & !is.na(d$fcs_cat), ]
      ct_mat <- table(d_v$rcsi_cat, d_v$fcs_cat)
      chi2r  <- tryCatch(chisq.test(ct_mat, correct = FALSE), error = function(e) NULL)
      if (!is.null(chi2r)) {
        chi2_stat  <- as.numeric(chi2r$statistic)
        chi2_df    <- as.integer(chi2r$parameter)
        chi2_pval  <- chi2r$p.value
        n_ct       <- sum(ct_mat)
        cramer_v   <- sqrt(chi2_stat / (n_ct * (min(nrow(ct_mat), ncol(ct_mat)) - 1)))
        min_exp    <- round(min(chi2r$expected), 2)
        if (min_exp < 5) {
          fr <- tryCatch(
            fisher.test(ct_mat, simulate.p.value = TRUE, B = 10000),
            error = function(e) NULL
          )
          if (!is.null(fr)) {
            use_fisher  <- TRUE
            fisher_pval <- fr$p.value
            concordant  <- (chi2_pval < 0.05) == (fisher_pval < 0.05)
          }
        }
      }
    }

    list(
      pays = pays, n_total = nrow(d), date_rapport = format(Sys.Date(), "%d/%m/%Y"),
      grp_label = grp_label,
      has_fcs = has_fcs, has_rcsi = has_rcsi, has_hdds = has_hdds,
      has_hhs = has_hhs, has_temporal = has_temporal,
      fcs_mean = fcs_mean, fcs_sd = fcs_sd, fcs_median = fcs_median,
      fcs_min = fcs_min, fcs_max = fcs_max,
      pct_fcs_poor = pct_fcs_poor, pct_fcs_limit = pct_fcs_limit,
      pct_fcs_acceptable = pct_fcs_acceptable,
      rcsi_mean = rcsi_mean, rcsi_sd = rcsi_sd, rcsi_median = rcsi_median,
      rcsi_min = rcsi_min, rcsi_max = rcsi_max,
      pct_rcsi_low = pct_rcsi_low, pct_rcsi_mid = pct_rcsi_mid,
      pct_rcsi_high = pct_rcsi_high,
      hdds_mean = hdds_mean, hdds_sd = hdds_sd, hdds_median = hdds_median,
      pct_hdds_low = pct_hdds_low, pct_hdds_mid = pct_hdds_mid,
      pct_hdds_high = pct_hdds_high,
      hhs_mean = hhs_mean, hhs_sd = hhs_sd,
      n_dv = n_dv, pct_dv = pct_dv,
      fcs_cat_by_grp = fcs_cat_by_grp, rcsi_cat_by_grp = rcsi_cat_by_grp,
      hdds_cat_by_grp = hdds_cat_by_grp, scatter_data = scatter_data,
      temporal_fcs = temporal_fcs, temporal_rcsi = temporal_rcsi, rounds = rounds,
      crosstab_disp = crosstab_disp,
      chi2_stat = chi2_stat, chi2_df = chi2_df, chi2_pval = chi2_pval,
      cramer_v = cramer_v, fisher_pval = fisher_pval, min_exp = min_exp,
      use_fisher = use_fisher, concordant = concordant
    )
  })

  # ── Rapport automatique — UI barre ────────────────────────────────────────────
  output$accueil_rapport_bar <- renderUI({
    d <- df_f()
    if (is.null(d) || nrow(d) == 0) return(NULL)

    tags$div(
      style = paste0(
        "background:linear-gradient(135deg,#0B3954 0%,#0077B6 100%);",
        "border-radius:12px; padding:14px 22px; margin-bottom:18px;",
        "display:flex; align-items:center; justify-content:space-between;",
        "flex-wrap:wrap; gap:12px;"
      ),
      # Texte
      tags$div(
        tags$div(
          style = "font-size:14px; font-weight:700; color:white; margin-bottom:3px;",
          icon("file-alt"), " G\u00e9n\u00e9rer le rapport automatique"
        ),
        tags$div(
          style = "font-size:11.5px; color:rgba(255,255,255,0.80);",
          "Synth\u00e8se compl\u00e8te : KPI, graphiques, tableau crois\u00e9, tests statistiques. ",
          tags$b(style = "color:white;", paste0(nrow(d), " m\u00e9nages")), " inclus."
        )
      ),
      # Boutons
      tags$div(
        style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
        downloadButton(
          "dl_rapport_word",
          label = tagList(icon("file-word"),   " Word (.docx)"),
          style = paste0(
            "background:white; color:#0B3954; border:none; font-weight:700;",
            "font-size:12px; padding:7px 14px; border-radius:8px;"
          )
        ),
        downloadButton(
          "dl_rapport_html",
          label = tagList(icon("globe"), " HTML"),
          style = paste0(
            "background:rgba(255,255,255,0.2); color:white; border:1px solid rgba(255,255,255,0.5);",
            "font-weight:600; font-size:12px; padding:7px 14px; border-radius:8px;"
          )
        ),
        tags$span(
          style = "font-size:10px; color:rgba(255,255,255,0.65); max-width:160px; line-height:1.3;",
          icon("clock"), " G\u00e9n\u00e9ration 15-30 sec."
        )
      )
    )
  })

  # ── Helper : construire + sauvegarder le RDS, retourner chemin template ───────
  .build_rapport_rds <- function() {
    rpt  <- rapport_data()
    if (is.null(rpt)) stop("Données insuffisantes pour générer le rapport.")
    rds  <- tempfile(fileext = ".rds")
    saveRDS(rpt, rds)
    tmpl <- file.path(getwd(), "rapport_template_nutri.Rmd")
    if (!file.exists(tmpl)) stop(paste0("Template introuvable : ", tmpl))
    list(rds = rds, tmpl = tmpl,
         titre = paste0("Rapport s\u00e9curit\u00e9 alimentaire",
                        if (nzchar(rpt$pays)) paste0(" \u2014 ", rpt$pays) else ""),
         date_rapport = rpt$date_rapport)
  }

  # ── Download Word ─────────────────────────────────────────────────────────────
  output$dl_rapport_word <- downloadHandler(
    filename = function() {
      rpt <- rapport_data()
      pays <- if (!is.null(rpt) && nzchar(rpt$pays))
        gsub("[^A-Za-z0-9]", "_", rpt$pays) else "rapport"
      paste0("rapport_SA_", pays, "_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      info <- .build_rapport_rds()
      rmarkdown::render(
        input         = info$tmpl,
        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 2),
        output_file   = file,
        params        = list(data_file    = info$rds,
                             titre        = info$titre,
                             date_rapport = info$date_rapport),
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
    }
  )

  # ── Download HTML ─────────────────────────────────────────────────────────────
  output$dl_rapport_html <- downloadHandler(
    filename = function() {
      rpt <- rapport_data()
      pays <- if (!is.null(rpt) && nzchar(rpt$pays))
        gsub("[^A-Za-z0-9]", "_", rpt$pays) else "rapport"
      paste0("rapport_SA_", pays, "_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      info <- .build_rapport_rds()
      rmarkdown::render(
        input         = info$tmpl,
        output_format = rmarkdown::html_document(
          toc = TRUE, toc_float = TRUE, toc_depth = 2,
          self_contained = TRUE, theme = "flatly"
        ),
        output_file   = file,
        params        = list(data_file    = info$rds,
                             titre        = info$titre,
                             date_rapport = info$date_rapport),
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
    }
  )

