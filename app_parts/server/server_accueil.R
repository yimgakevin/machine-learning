# Extrait de app_v2_final.R — KPI et cartes du tableau de bord d'accueil
  # ── ACCUEIL ─────────────────────────────────────────────────────────────────
  output$accueil_badge <- renderUI({
    df <- mapped_data()
    if (is.null(df)) {
      tags$div(style="text-align:right;",
        tags$div(style="font-size:36px;opacity:.5;","⬆"),
        tags$p(style="font-size:13px;opacity:.8;","Importez un fichier pour commencer"))
    } else {
      tags$div(style="text-align:right;",
        tags$span(class="badge-ok",style="font-size:14px;padding:6px 14px;",
                  paste0("✓ ",nrow(df)," obs. · ",ncol(df)," variables")),
        br(), br(),
        if(has_oms()) tags$span(class="badge-ok",style="font-size:13px;padding:5px 12px;",
                                "✓ Indicateurs OMS calculés")
        else tags$span(class="badge-warn",style="font-size:13px;padding:5px 12px;",
                       "⚠ Mapper les colonnes OMS")
      )
    }
  })

  output$accueil_alertes <- renderUI({
    if (!has_oms()) return(NULL)
    d <- df_f()
    inds <- list(
      list(v="GAM", lbl="GAM", seuil=15),
      list(v="SAM", lbl="SAM", seuil=2),
      list(v="Stunting", lbl="Stunting", seuil=30)
    )
    alertes <- lapply(inds, function(ind) {
      if (!ind$v %in% names(d)) return(NULL)
      p <- round(mean(d[[ind$v]], na.rm=TRUE)*100, 1)
      if (is.na(p)) return(NULL)
      sev <- alerte_label(p)
      cls <- if(p < 5)"alerte-ok" else if(p < 10)"alerte-alerte" else if(p < 15)"alerte-serieux" else "alerte-crit"
      tags$div(class=paste("alerte-box", cls),
        tags$strong(paste0(sev, " — ", ind$lbl, " : ", p, "%")),
        " | Seuil OMS critique : ", ind$seuil, "% | Situation actuelle : ",
        if(p >= ind$seuil) "⚠️ Au-dessus du seuil critique" else "✓ En dessous du seuil critique"
      )
    })
    fluidRow(column(12, tagList(alertes)))
  })

  # ── Helper : créer une KPI card premium (accueil) ──
  make_nutri_kpi <- function(value, label, subtitle = "", icon_name = "chart-bar",
                            icon_bg = "#0077B6", severity = "acceptable",
                            progress_pct = NULL) {
    sev_colors <- list(
      acceptable = list(text = "#166534", badge_cls = "kpi-badge-acceptable"),
      alerte     = list(text = "#854d0e", badge_cls = "kpi-badge-alerte"),
      serieux    = list(text = "#9a3412", badge_cls = "kpi-badge-serieux"),
      critique   = list(text = "#991b1b", badge_cls = "kpi-badge-critique"),
      urgence    = list(text = "#7f1d1d", badge_cls = "kpi-badge-urgence")
    )
    sc <- sev_colors[[severity]] %||% sev_colors$acceptable
    bar_colors <- list(acceptable="#2E7D52", alerte="#D4A843", serieux="#D98B14",
                       critique="#BF3636", urgence="#5C1A1A")

    progress_html <- if (!is.null(progress_pct)) {
      pct <- min(100, max(0, progress_pct))
      bc <- bar_colors[[severity]] %||% "#0077B6"
      tags$div(class = "kpi-progress",
        tags$div(class = "kpi-progress-fill",
                 style = sprintf("width:%.0f%%;background:%s;", pct, bc)))
    }

    tags$div(class = paste("nutri-kpi-card", paste0("sev-", severity)),
      tags$span(class = "kpi-bg-icon", icon(icon_name)),
      tags$div(class = "kpi-header",
        tags$div(class = "kpi-icon", style = sprintf("background:%s;", icon_bg),
                 icon(icon_name)),
        tags$div(class = "kpi-label", label)
      ),
      tags$div(class = "kpi-value", style = sprintf("color:%s;", sc$text), value),
      if (nzchar(subtitle)) tags$p(class = "kpi-sub", subtitle),
      tags$span(class = paste("kpi-badge", sc$badge_cls),
                toupper(severity)),
      progress_html
    )
  }

  # ── Helper : palette des sévérités ──
  sev_icon_bg <- list(acceptable="#2E7D52", alerte="#D4A843",
                      serieux="#D98B14", critique="#BF3636", urgence="#5C1A1A")

  # ═══ Score barre horizontale ═══
  output$accueil_score_bar <- renderUI({
    if (!has_oms()) return(NULL)
    d <- filter_context()$data
    score <- measure_global_score(d)
    if (is.null(score)) return(NULL)
    score_global <- score$score
    sev_global <- score$severity
    sev_label <- toupper(sev_global)
    sev_col <- sev_icon_bg[[sev_global]]
    marker_pct <- 100 - score_global  # inversé : 100=bon (gauche), 0=mauvais (droite)

    tags$div(class = "nutri-score-bar-container",
      fluidRow(
        column(3,
          tags$div(style="text-align:center;",
            tags$div(style=sprintf("font-size:42px;font-weight:800;color:%s;line-height:1;", sev_col),
                     score_global),
            tags$div(style="font-size:12px;font-weight:600;color:#64748b;margin-top:4px;",
                     "SCORE GLOBAL"),
            tags$span(class=paste0("kpi-badge kpi-badge-",sev_global),
                      style="margin-top:6px;", sev_label)
          )
        ),
        column(9,
          tags$div(style="padding-top:12px;",
            tags$div(class = "nutri-score-bar",
              tags$div(class = "nutri-score-marker",
                       style = sprintf("left:%.0f%%;", marker_pct))
            ),
            tags$div(class = "nutri-score-labels",
              tags$span("Urgence"), tags$span("Critique"),
              tags$span("Sérieux"), tags$span("Alerte"),
              tags$span("Acceptable")
            )
          )
        )
      )
    )
  })

  # ═══ KPI Cards Premium ═══
  output$accueil_kpi_premium <- renderUI({
    df <- mapped_data()
    if (is.null(df)) {
      return(tags$div(class="nutri-empty-state",
        tags$div(class="empty-icon", icon("cloud-upload-alt")),
        tags$h3("Importez un jeu de données pour commencer"),
        tags$p("Formats : CSV \u00b7 Excel (.xlsx) \u00b7 SPSS (.sav) \u00b7 Stata (.dta) \u00b7 Parquet"),
        tags$p("Cliquez sur ", tags$strong("Importer un fichier"), " dans la barre latérale")
      ))
    }
    d <- filter_context()$data
    cards <- list()

    # Card 1 : Observations
    cards[[1]] <- make_nutri_kpi(
      value = format(nrow(d), big.mark = " "),
      label = "Observations",
      subtitle = sprintf("%d variables \u00b7 %d numériques \u00b7 %d catégorielles",
                         length(vars_num()) + length(vars_cat()), length(vars_num()), length(vars_cat())),
      icon_name = "database", icon_bg = "#0077B6", severity = "acceptable"
    )

    for (card in measure_card_specs(d, c("GAM", "SAM", "Stunting", "Underweight", ".whz", ".haz", "MUAC_GAM"))) {
      ms <- card$measure
      spec <- card$spec
      subtitle <- if (identical(card$id, "Stunting")) {
        sprintf("Seuil OMS sérieux : 40%%")
      } else if (identical(card$id, "Underweight")) {
        sprintf("Seuil OMS sérieux : 30%%")
      } else if (identical(card$id, "MUAC_GAM")) {
        "Périmètre brachial < 125mm"
      } else if (identical(ms$kind, "mean")) {
        sprintf("± %.2f SD · Réf. OMS : 0 ± 1", ms$sd)
      } else {
        measure_ci_text(ms)
      }
      value <- if (identical(ms$kind, "mean")) ms$mean else paste0(ms$pct, "%")
      progress <- if (identical(ms$kind, "binary") && !is.null(spec$max_pct)) {
        min(100, ms$pct / spec$max_pct * 100)
      } else NULL
      cards[[length(cards) + 1]] <- make_nutri_kpi(
        value = value, label = spec$label, subtitle = subtitle,
        icon_name = spec$icon, icon_bg = sev_icon_bg[[ms$severity]],
        severity = ms$severity, progress_pct = progress
      )
    }

    tags$div(class = "nutri-kpi-grid", tagList(cards))
  })

  # ═══ Compteur d'observations filtré ═══
  output$dash_filter_counter <- renderUI({
    ctx <- filter_context()
    tags$span(style = "font-size:11px;color:#64748b;",
      format(ctx$n, big.mark = " "), " / ",
      format(ctx$n_total, big.mark = " "), " obs."
    )
  })
  outputOptions(output, "dash_filter_counter", suspendWhenHidden = FALSE)

  # ═══ KPI Qualité des données + Démographie ═══
  output$accueil_kpi_qualite <- renderUI({
    df <- mapped_data()
    if (is.null(df)) return(NULL)
    d <- filter_context()$data; m <- map_cols()
    cards <- list()

    # Complétude globale
    total_cells <- nrow(d) * ncol(d)
    na_cells <- sum(is.na(d))
    completeness <- round((1 - na_cells / total_cells) * 100, 1)
    sev_comp <- if(completeness >= 95) "acceptable" else if(completeness >= 85) "alerte" else if(completeness >= 70) "serieux" else "critique"
    # Colonnes les plus incomplètes
    na_by_col <- sort(colMeans(is.na(d)) * 100, decreasing = TRUE)
    worst_cols <- head(na_by_col[na_by_col > 0], 3)
    worst_text <- if(length(worst_cols) > 0) {
      paste(sprintf("%s (%.0f%%)", names(worst_cols), worst_cols), collapse = " \u00b7 ")
    } else "Aucune colonne avec valeurs manquantes"

    cards[[1]] <- make_nutri_kpi(
      value = paste0(completeness, "%"),
      label = "Complétude des données",
      subtitle = paste0("\u26a0 Plus incomplets : ", worst_text),
      icon_name = "check-double", icon_bg = sev_icon_bg[[sev_comp]],
      severity = sev_comp, progress_pct = completeness
    )

    # Nombre de doublons
    n_dup <- sum(duplicated(d))
    sev_dup <- if(n_dup == 0) "acceptable" else if(n_dup / nrow(d) < 0.02) "alerte" else "serieux"
    cards[[length(cards)+1]] <- make_nutri_kpi(
      value = format(n_dup, big.mark = " "),
      label = "Lignes dupliquées",
      subtitle = sprintf("%.1f%% du total \u00b7 %s lignes uniques",
                         n_dup / nrow(d) * 100, format(nrow(d) - n_dup, big.mark = " ")),
      icon_name = "clone", icon_bg = sev_icon_bg[[sev_dup]],
      severity = sev_dup
    )

    # Ratio filles/garçons (démographie)
    if (!is.null(m$sexe) && m$sexe %in% names(d)) {
      sex_tbl <- table(d[[m$sexe]], useNA = "no")
      if (length(sex_tbl) >= 2) {
        sex_vals <- sort(sex_tbl, decreasing = TRUE)
        ratio_txt <- paste(sprintf("%s: %s (%.0f%%)",
                                   names(sex_vals), format(as.integer(sex_vals), big.mark=" "),
                                   as.integer(sex_vals)/sum(sex_vals)*100), collapse = " \u00b7 ")
        # Vérifier l'équilibre (50/50 idéal)
        max_pct <- max(as.integer(sex_vals)) / sum(sex_vals) * 100
        sev_sex <- if(max_pct <= 55) "acceptable" else if(max_pct <= 60) "alerte" else "serieux"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = sprintf("%.0f/%.0f", as.integer(sex_vals[1])/sum(sex_vals)*100,
                          as.integer(sex_vals[2])/sum(sex_vals)*100),
          label = "Ratio par sexe",
          subtitle = ratio_txt,
          icon_name = "venus-mars", icon_bg = sev_icon_bg[[sev_sex]],
          severity = sev_sex
        )
      }
    }

    # Âge moyen
    if (".age" %in% names(d)) {
      age_m <- round(mean(d$.age, na.rm = TRUE), 1)
      age_sd <- round(sd(d$.age, na.rm = TRUE), 1)
      age_range <- range(d$.age, na.rm = TRUE)
      cards[[length(cards)+1]] <- make_nutri_kpi(
        value = paste0(age_m, " mois"),
        label = "\u00c2ge moyen",
        subtitle = sprintf("\u00b1 %.1f SD \u00b7 Min: %.0f \u00b7 Max: %.0f mois",
                           age_sd, age_range[1], age_range[2]),
        icon_name = "baby", icon_bg = "#0077B6", severity = "acceptable"
      )
    }

    # Nombre de régions/clusters
    if (!is.null(m$region) && m$region %in% names(d)) {
      n_regions <- length(unique(na.omit(d[[m$region]])))
      avg_per_region <- round(nrow(d) / n_regions)
      sev_n <- if(avg_per_region >= 25) "acceptable" else if(avg_per_region >= 15) "alerte" else "serieux"
      cards[[length(cards)+1]] <- make_nutri_kpi(
        value = n_regions,
        label = "Zones / Régions",
        subtitle = sprintf("~%s obs/zone \u00b7 colonne : %s",
                           format(avg_per_region, big.mark = " "), m$region),
        icon_name = "map-marked-alt", icon_bg = sev_icon_bg[[sev_n]],
        severity = sev_n
      )
    }

    # MUAC moyen
    if (".muac" %in% names(d)) {
      muac_m <- round(mean(d$.muac, na.rm=TRUE), 1)
      muac_sd <- round(sd(d$.muac, na.rm=TRUE), 1)
      sev_muac <- if(muac_m >= 135) "acceptable" else if(muac_m >= 125) "alerte" else if(muac_m >= 115) "serieux" else "critique"
      cards[[length(cards)+1]] <- make_nutri_kpi(
        value = paste0(muac_m, " mm"),
        label = "MUAC moyen (Périmètre brachial)",
        subtitle = sprintf("\u00b1 %.1f mm \u00b7 < 125mm = MAG \u00b7 < 115mm = MAS", muac_sd),
        icon_name = "ruler", icon_bg = sev_icon_bg[[sev_muac]],
        severity = sev_muac
      )
    }

    if (length(cards) == 0) return(NULL)
    tagList(
      tags$div(class = "nutri-section-title", icon("clipboard-check"),
               " Qualité des données & Démographie"),
      tags$div(class = "nutri-kpi-grid", tagList(cards))
    )
  })

  # ═══ KPI Sécurité alimentaire (auto-détecté) ═══
  output$accueil_kpi_secu <- renderUI({
    d <- filter_context()$data
    if (is.null(d)) return(NULL)
    cards <- list()
    cols_lower <- tolower(names(d))

    # Helper : préfère la colonne interne calculée (ex: .fcs) sinon cherche dans les noms
    pick_sa_col <- function(d, internal_col, patterns, numeric_only = TRUE) {
      if (internal_col %in% names(d) && is.numeric(d[[internal_col]]) && !all(is.na(d[[internal_col]])))
        return(internal_col)
      candidates <- names(d)[grepl(patterns, tolower(names(d)))]
      # Exclure les colonnes internes (commençant par ".") et les catégories (_cat)
      candidates <- candidates[!grepl("^\\.|_cat$", candidates)]
      if (numeric_only) candidates <- candidates[vapply(candidates, function(v) {
        tryCatch(isTRUE(is.numeric(d[[v]])), error = function(e) FALSE)
      }, logical(1))]
      if (length(candidates) > 0) return(candidates[1])
      NULL
    }

    # FCS (Food Consumption Score)
    fcs_col <- pick_sa_col(d, ".fcs", "^fcs$|^fcs_score$|food.?consumption.?score|^score.*fcs|^sca$")
    if (!is.null(fcs_col)) {
      fcs_vals <- na.omit(d[[fcs_col]])
      if (length(fcs_vals) > 0 && is.numeric(fcs_vals)) {
        m_fcs <- round(mean(fcs_vals), 1)
        poor <- round(mean(fcs_vals <= 21) * 100, 1)
        border <- round(mean(fcs_vals > 21 & fcs_vals <= 35) * 100, 1)
        acceptable <- round(mean(fcs_vals > 35) * 100, 1)
        sev <- if(poor >= 40) "critique" else if(poor >= 20) "serieux" else if(poor >= 10) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = m_fcs, label = "FCS moyen (Consommation Alimentaire)",
          subtitle = sprintf("Pauvre \u226421: %.1f%% \u00b7 Limite 21-35: %.1f%% \u00b7 Acceptable >35: %.1f%%",
                             poor, border, acceptable),
          icon_name = "utensils", icon_bg = sev_icon_bg[[sev]],
          severity = sev, progress_pct = min(100, acceptable)
        )
      }
    }

    # rCSI (reduced Coping Strategy Index) — utilise la colonne calculée .rcsi en priorité
    rcsi_col <- pick_sa_col(d, ".rcsi", "^rcsi$|^rcsi_score$|^r_csi$|^rcsiscore$")
    if (!is.null(rcsi_col)) {
      rcsi_vals <- na.omit(d[[rcsi_col]])
      if (length(rcsi_vals) > 0 && is.numeric(rcsi_vals)) {
        m_rcsi <- round(mean(rcsi_vals), 1)
        high <- round(mean(rcsi_vals >= 19) * 100, 1)
        sev <- if(high >= 50) "critique" else if(high >= 30) "serieux" else if(high >= 15) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = m_rcsi, label = "rCSI moyen (Strat\u00e9gies d'Adaptation)",
          subtitle = sprintf("Niveau \u00e9lev\u00e9 (\u226519) : %.1f%% \u00b7 Plus le score est haut, plus la situation est grave", high),
          icon_name = "hand-holding-heart", icon_bg = sev_icon_bg[[sev]],
          severity = sev
        )
      }
    }

    # HDDS (Household Dietary Diversity Score) — utilise .hdds en priorité
    hdds_col <- pick_sa_col(d, ".hdds", "^hdds$|^hdds_score$|dietary.?diversity.?score|^dds$|^sdam$")
    if (!is.null(hdds_col)) {
      hdds_vals <- na.omit(d[[hdds_col]])
      if (length(hdds_vals) > 0 && is.numeric(hdds_vals)) {
        m_hdds <- round(mean(hdds_vals), 1)
        low <- round(mean(hdds_vals <= 3) * 100, 1)
        sev <- if(low >= 50) "critique" else if(low >= 30) "serieux" else if(low >= 15) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = m_hdds, label = "HDDS moyen (Diversit\u00e9 Alimentaire)",
          subtitle = sprintf("Faible diversit\u00e9 (\u22643 groupes) : %.1f%%", low),
          icon_name = "carrot", icon_bg = sev_icon_bg[[sev]],
          severity = sev
        )
      }
    }

    # HHS (Household Hunger Scale) — utilise .hhs en priorité
    hhs_col <- pick_sa_col(d, ".hhs", "^hhs$|^hhs_score$|^hhs_total$|household.?hunger.?scale")
    if (!is.null(hhs_col)) {
      hhs_vals <- na.omit(d[[hhs_col]])
      if (length(hhs_vals) > 0 && is.numeric(hhs_vals)) {
        m_hhs <- round(mean(hhs_vals), 1)
        severe <- round(mean(hhs_vals >= 4) * 100, 1)
        sev <- if(severe >= 20) "critique" else if(severe >= 10) "serieux" else if(severe >= 5) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = m_hhs, label = "HHS moyen (\u00c9chelle de la Faim)",
          subtitle = sprintf("Faim s\u00e9v\u00e8re (\u22654) : %.1f%% \u00b7 Seuils : 0-1 Peu/Pas \u00b7 2-3 Mod\u00e9r\u00e9e \u00b7 4-6 S\u00e9v\u00e8re", severe),
          icon_name = "frown", icon_bg = sev_icon_bg[[sev]],
          severity = sev
        )
      }
    }

    # ── MUAC (Périmètre Brachial) — carte KPI dashboard ──
    muac_col <- intersect(c(".muac", "muac", "MUAC", "pb", "PB", "muac_mm"), names(d))
    if (length(muac_col) > 0) {
      muac_vals <- na.omit(as.numeric(d[[muac_col[1]]]))
      if (length(muac_vals) >= 5) {
        pct_mag <- round(mean(muac_vals < 125) * 100, 1)
        pct_mas <- round(mean(muac_vals < 115) * 100, 1)
        muac_mean <- round(mean(muac_vals), 1)
        sev_muac <- if (pct_mag >= 15) "critique" else if (pct_mag >= 10) "serieux" else if (pct_mag >= 5) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = paste0(pct_mag, "%"), label = "MAG — MUAC (P\u00e9rim\u00e8tre Brachial)",
          subtitle = sprintf("MAG (< 125mm): %.1f%% \u00b7 MAS (< 115mm): %.1f%% \u00b7 Moy: %.1f mm",
                             pct_mag, pct_mas, muac_mean),
          icon_name = "ruler", icon_bg = sev_icon_bg[[sev_muac]],
          severity = sev_muac,
          progress_pct = min(100, pct_mag * 3)
        )
      }
    }

    # ── Estimation Phase IPC (si FCS + rCSI disponibles) ──
    fcs_s_ipc <- if (!is.null(fcs_col) && fcs_col %in% names(d)) d[[fcs_col]] else NULL
    rcsi_s_ipc <- if (!is.null(rcsi_col) && rcsi_col %in% names(d)) d[[rcsi_col]] else NULL
    hhs_s_ipc  <- if (!is.null(hhs_col) && hhs_col %in% names(d)) d[[hhs_col]] else NULL
    muac_s_ipc <- if (length(muac_col) > 0) as.numeric(d[[muac_col[1]]]) else NULL

    if (!is.null(fcs_s_ipc) || !is.null(rcsi_s_ipc)) {
      phases_ipc <- tryCatch(
        calc_ipc_phase(fcs_s_ipc, rcsi_s_ipc, hhs_s_ipc, muac_s_ipc),
        error = function(e) NULL
      )
      if (!is.null(phases_ipc)) {
        pct_phase3plus <- round(mean(phases_ipc >= 3) * 100, 1)
        pct_phase4plus <- round(mean(phases_ipc >= 4) * 100, 1)
        sev_ipc <- if (pct_phase3plus >= 20) "critique" else if (pct_phase3plus >= 10) "serieux" else if (pct_phase3plus >= 5) "alerte" else "acceptable"
        cards[[length(cards)+1]] <- make_nutri_kpi(
          value = paste0(pct_phase3plus, "%"),
          label = "Phase IPC 3+ (Crise \u00e0 Urgence)",
          subtitle = sprintf("Phase 4+: %.1f%% \u00b7 Estimation approximative FCS+rCSI+HHS%s",
                             pct_phase4plus,
                             if (!is.null(muac_s_ipc)) "+MUAC" else ""),
          icon_name = "globe",
          icon_bg = sev_icon_bg[[sev_ipc]],
          severity = sev_ipc
        )
      }
    }

    if (length(cards) == 0) return(NULL)
    tagList(
      tags$div(class = "nutri-section-title", icon("utensils"),
               " Sécurité alimentaire · MUAC · Phase IPC"),
      tags$div(class = "nutri-kpi-grid", tagList(cards))
    )
  })

  output$accueil_prev_chart <- renderPlotly({
    req(has_oms()); d <- filter_context()$data; m <- map_cols()
    grp_col <- if (!is.null(m$region) && m$region %in% names(d)) m$region else vars_cat()[1]
    req(!is.null(grp_col))
    inds <- intersect(c("GAM","SAM","Stunting","Underweight"), names(d))
    req(length(inds) > 0)
    res <- d |> group_by(.data[[grp_col]]) |>
      group_modify(~map_dfr(inds, function(v)
        calc_prev_wilson(.x, v, v))) |> ungroup()
    n_grp <- length(unique(res[[grp_col]]))
    res$..region_key <- as.character(res[[grp_col]])
    res[[grp_col]] <- truncate_label(res[[grp_col]], 20)
    use_horiz <- n_grp > 10
    p <- if (use_horiz) {
      ggplot(res, aes(y=.data[[grp_col]], x=`Prév. (%)`,
                       fill=Indicateur, key=.data[["..region_key"]],
                       text=paste0(.data[[grp_col]]," — ",Indicateur,
                                   "\n",`Prév. (%)`,"% [",`IC95 bas (%)`,"-",`IC95 haut (%)`,"%]"))) +
        geom_col(position="dodge", width=0.72, alpha=0.88) +
        geom_errorbarh(aes(xmin=`IC95 bas (%)`, xmax=`IC95 haut (%)`),
                       position=position_dodge(0.72), height=0.22, color="gray30", linewidth=0.7) +
        geom_vline(xintercept=15, linetype="dashed", color=C$rouge, linewidth=0.7) +
        scale_fill_manual(values=c(GAM=C$rouge,SAM=C$rouge_f,
                                    Stunting=C$bleu,Underweight=C$violet,
                                    MAM=C$orange)) +
        labs(y="", x="Prévalence (%)") + theme_nutri()
    } else {
      ggplot(res, aes(x=.data[[grp_col]], y=`Prév. (%)`,
                       fill=Indicateur, key=.data[["..region_key"]],
                       text=paste0(.data[[grp_col]]," — ",Indicateur,
                                   "\n",`Prév. (%)`,"% [",`IC95 bas (%)`,"-",`IC95 haut (%)`,"%]"))) +
        geom_col(position="dodge", width=0.72, alpha=0.88) +
        geom_errorbar(aes(ymin=`IC95 bas (%)`, ymax=`IC95 haut (%)`),
                      position=position_dodge(0.72), width=0.22, color="gray30", linewidth=0.7) +
        geom_hline(yintercept=15, linetype="dashed", color=C$rouge, linewidth=0.7) +
        scale_fill_manual(values=c(GAM=C$rouge,SAM=C$rouge_f,
                                    Stunting=C$bleu,Underweight=C$violet,
                                    MAM=C$orange)) +
        labs(x="", y="Prévalence (%)") + theme_nutri() + smart_axis_x(n_grp)
    }
    ggplotly(p, tooltip="text", source="accueil_prev") |>
      event_register("plotly_click") |>
      layout(legend=list(orientation="h", y=-0.25), hovermode="closest") |> nutri_config()
  })

  observeEvent(event_data("plotly_click", source = "accueil_prev"), {
    click <- event_data("plotly_click", source = "accueil_prev")
    key <- click$key
    req(is.character(key), length(key) == 1, nzchar(key))
    if (!is.null(input$f_region)) {
      updateSelectInput(session, "f_region", selected = key)
      showNotification(paste0("Filtre région : ", key), type = "message", duration = 2)
    }
  })

  output$accueil_gauge <- renderPlotly({
    req(has_oms()); d <- filter_context()$data
    gam_p <- round(mean(d$GAM, na.rm=TRUE)*100, 1)
    plot_ly(type="indicator", mode="gauge+number+delta",
            value=gam_p,
            title=list(text="GAM (%)", font=list(size=15, color=C$sidebar)),
            delta=list(reference=15, increasing=list(color=C$rouge),
                       decreasing=list(color=C$vert)),
            gauge=list(
              axis=list(range=list(0,50), tickwidth=1, tickfont=list(size=10)),
              bar=list(color=couleur_prevalence(gam_p)),
              steps=list(
                list(range=c(0,5),  color="#e8f5e9"),
                list(range=c(5,10), color="#fffde7"),
                list(range=c(10,15),color="#fff3e0"),
                list(range=c(15,30),color="#ffebee"),
                list(range=c(30,50),color="#ffcdd2")),
              threshold=list(
                line=list(color=C$rouge,width=3),
                thickness=0.75, value=15)
            )) |>
      layout(margin=list(l=20,r=20,b=10,t=40),
             paper_bgcolor="rgba(0,0,0,0)")
  })

  output$accueil_seuil_badge <- renderUI({
    req(has_oms()); d <- filter_context()$data
    p <- round(mean(d$GAM, na.rm=TRUE)*100, 1)
    cls <- if(p<5)"alerte-ok" else if(p<10)"alerte-alerte" else if(p<15)"alerte-serieux" else "alerte-crit"
    tags$div(class=paste("alerte-box",cls), style="margin:8px;",
             tags$strong(alerte_label(p)), " — GAM : ", p, "%")
  })

  output$accueil_zscore <- renderPlotly({
    d <- filter_context()$data; m <- map_cols()
    z_cols <- c(m$whz, m$haz, m$waz)
    z_cols <- z_cols[!sapply(z_cols,is.null) & z_cols %in% names(d)]
    req(length(z_cols) > 0)
    x_ref <- seq(-4.5, 4.5, length.out=300)
    ref_df <- tibble(x=x_ref, y=dnorm(x_ref), Courbe="Référence OMS N(0,1)")
    df_long <- d |> select(all_of(z_cols)) |>
      pivot_longer(everything(), names_to="Score", values_to="Val")
    noms <- setNames(c("WHZ","HAZ","WAZ","MUAC")[1:length(z_cols)], z_cols)
    df_long$Score <- noms[df_long$Score]
    p <- ggplot(df_long, aes(x=Val, fill=Score, color=Score)) +
      geom_density(alpha=0.3, linewidth=0.8) +
      geom_line(data=ref_df, aes(x=x,y=y), color="black",
                linewidth=1, linetype="dashed", inherit.aes=FALSE) +
      geom_vline(xintercept=c(-2,-3), linetype="dashed",
                 color=c(C$rouge,C$rouge_f), linewidth=0.7) +
      scale_fill_manual(values=gpal(length(unique(df_long$Score)),"cat")) +
      scale_color_manual(values=gpal(length(unique(df_long$Score)),"cat")) +
      labs(x="Score Z", y="Densité",
           caption="Tirets rouges = seuils OMS -2 et -3 DS | Tiret noir = référence OMS N(0,1)") +
      theme_minimal(base_size=11)
    ggplotly(p) |> layout(hovermode="x unified",
                          legend=list(orientation="h", y=-0.2)) |> nutri_config()
  })

