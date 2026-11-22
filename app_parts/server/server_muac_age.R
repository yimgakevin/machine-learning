# Extrait de app_v2_final.R — MUAC et z-scores par tranche d'âge
  # ══════════════════════════════════════════════════════════════════
  # MODULE MUAC — Périmètre Brachial (onglet Z-scores)
  # ══════════════════════════════════════════════════════════════════

  # Helper : extraire le vecteur MUAC (colonne .muac ou auto-détection)
  .muac_vec_r <- reactive({
    d_cur <- df_f(); req(d_cur)
    mc <- intersect(c(".muac", "muac", "MUAC", "pb", "PB", "mid_upper_arm_circ",
                      "muac_mm", "pbm"), names(d_cur))
    if (length(mc) == 0) return(NULL)
    as.numeric(d_cur[[mc[1]]])
  })

  output$zs_muac_group_ui <- renderUI({
    d_cur <- df_f(); req(d_cur)
    choices_grp <- c("Global" = "", "Région" = "region",
                     "Département" = "departement",
                     "Round" = "survey_round", "Sexe" = "sexe",
                     "Cluster" = "cluster")
    valid_choices <- choices_grp[c(TRUE, sapply(choices_grp[-1], function(v) v %in% names(d_cur)))]
    selectInput("zs_muac_group_var", "Grouper par :",
                choices = valid_choices, selected = "", width = "100%")
  })

  output$zs_muac_kpi_row <- renderUI({
    muac_v <- .muac_vec_r(); req(muac_v)
    muac_v <- muac_v[!is.na(muac_v)]
    req(length(muac_v) > 0)
    n_tot    <- length(muac_v)
    n_mag    <- sum(muac_v < 125)
    n_mas    <- sum(muac_v < 115)
    n_mam    <- sum(muac_v >= 115 & muac_v < 125)
    pct_mag  <- round(n_mag / n_tot * 100, 1)
    pct_mas  <- round(n_mas / n_tot * 100, 1)
    pct_mam  <- round(n_mam / n_tot * 100, 1)
    ci_res   <- tryCatch(binom::binom.wilson(n_mag, n_tot), error = function(e) NULL)
    ci_lo    <- if (!is.null(ci_res)) round(ci_res$lower * 100, 1) else "?"
    ci_hi    <- if (!is.null(ci_res)) round(ci_res$upper * 100, 1) else "?"

    sev_col  <- if (pct_mag >= 15) "#C0392B" else if (pct_mag >= 10) "#E67E22" else if (pct_mag >= 5) "#F1C40F" else "#27AE60"
    sev_lbl  <- if (pct_mag >= 15) "Critique" else if (pct_mag >= 10) "Sérieux" else if (pct_mag >= 5) "Alerte" else "Acceptable"

    make_muac_box <- function(val, lbl, sub_lbl, color) {
      tags$div(style=paste0("background:white;border-radius:10px;padding:14px 16px;",
                             "border-left:4px solid ", color, ";text-align:center;flex:1;"),
        tags$div(style=paste0("font-size:26px;font-weight:800;color:", color, ";"), val),
        tags$div(style="font-size:12px;font-weight:600;color:#333;", lbl),
        tags$div(style="font-size:10px;color:#888;margin-top:2px;", sub_lbl)
      )
    }
    tags$div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
      make_muac_box(paste0(pct_mag, "%"), "MAG (MUAC < 125mm)",
        paste0("IC 95% : [", ci_lo, "–", ci_hi, "%] | n=", n_mag, "/", n_tot), sev_col),
      make_muac_box(paste0(pct_mas, "%"), "MAS (MUAC < 115mm)",
        paste0("Malnutrition aiguë sévère | n=", n_mas), "#C0392B"),
      make_muac_box(paste0(pct_mam, "%"), "MAM (115–124mm)",
        paste0("Malnutrition aiguë modérée | n=", n_mam), "#E67E22"),
      make_muac_box(paste0(round(mean(muac_v), 1), "mm"), "MUAC moyen",
        paste0("Médiane : ", median(muac_v), "mm | SD : ", round(sd(muac_v), 1), "mm"), "#0077B6"),
      tags$div(style=paste0("background:white;border-radius:10px;padding:14px 16px;",
                             "border-left:4px solid ", sev_col, ";text-align:center;flex:1;"),
        tags$div(style=paste0("font-size:14px;font-weight:800;color:", sev_col, ";"), sev_lbl),
        tags$div(style="font-size:11px;font-weight:600;color:#333;", "Niveau MAG"),
        tags$div(style="font-size:10px;color:#888;",
                 if (pct_mag >= 15) "≥15% — Urgence nutritionnelle" else
                 if (pct_mag >= 10) "10–14% — Situation sérieuse" else
                 if (pct_mag >= 5)  "5–9% — Seuil d'alerte" else "< 5% — Acceptable")
      )
    )
  })

  output$zs_muac_dist <- renderPlotly({
    muac_v <- .muac_vec_r(); req(muac_v)
    d_cur  <- df_f(); req(d_cur)
    grp_var <- if (!is.null(input$zs_muac_by_sex) && isTRUE(input$zs_muac_by_sex)) {
      sex_col <- intersect(c("sexe", "sex", "genre"), names(d_cur))
      if (length(sex_col) > 0) d_cur[[sex_col[1]]] else NULL
    } else NULL
    tryCatch(
      plot_muac_distribution(muac_v, group_var = grp_var),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$zs_muac_donut <- renderPlotly({
    muac_v <- .muac_vec_r(); req(muac_v)
    muac_v_clean <- muac_v[!is.na(muac_v)]
    req(length(muac_v_clean) > 0)
    counts <- c(
      sum(muac_v_clean < 115),
      sum(muac_v_clean >= 115 & muac_v_clean < 125),
      sum(muac_v_clean >= 125 & muac_v_clean < 135),
      sum(muac_v_clean >= 135)
    )
    labels <- MUAC_CATEGORIES$cat
    pcts   <- round(counts / sum(counts) * 100, 1)
    plot_ly(labels = labels, values = counts, type = "pie",
            hole = 0.5, textinfo = "percent",
            marker = list(colors = MUAC_CATEGORIES$couleur,
                          line = list(color = "white", width = 1.5)),
            hovertemplate = "<b>%{label}</b><br>%{value} enfants (%{percent})<extra></extra>") |>
      layout(
        title = list(text = "<b>Classification MUAC</b>",
                     font = list(size = 13, color = "#0B3954"), x = 0.5),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2, font = list(size = 10)),
        paper_bgcolor = "white"
      )
  })

  output$zs_muac_by_group <- renderPlotly({
    muac_v <- .muac_vec_r(); req(muac_v)
    d_cur  <- df_f(); req(d_cur)
    grp_var_name <- if (!is.null(input$zs_muac_group_var) && input$zs_muac_group_var != "")
      input$zs_muac_group_var else NULL
    req(grp_var_name, grp_var_name %in% names(d_cur))
    grp_vec <- d_cur[[grp_var_name]]
    tryCatch(
      plot_muac_by_group(muac_v, grp_vec, grp_var_name),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$zs_muac_vs_whz_panel <- renderUI({
    res <- zs_data()
    if (is.null(res) || !"WHZ_calc" %in% names(res))
      return(tags$div(style="background:#f5f5f5;border-radius:8px;padding:12px;font-size:11px;color:#888;",
        icon("info-circle"), " Calculez les Z-scores pour afficher la comparaison MUAC vs WHZ."))
    tagList(
      tags$div(class="section-header", "Comparaison MUAC vs WHZ (méthodes de mesure MAG)"),
      plotlyOutput("zs_muac_vs_whz_plot", height="350px")
    )
  })

  output$zs_muac_vs_whz_plot <- renderPlotly({
    muac_v <- .muac_vec_r(); req(muac_v)
    res    <- zs_data(); req(res, "WHZ_calc" %in% names(res))
    d_cur  <- df_f(); req(d_cur)
    sex_col <- intersect(c("sexe", "sex", "genre"), names(d_cur))
    sex_vec <- if (length(sex_col) > 0) d_cur[[sex_col[1]]] else NULL
    tryCatch(
      plot_muac_vs_whz(muac_v, res$WHZ_calc, sex_vec),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$zs_muac_table <- renderDT({
    muac_v <- .muac_vec_r(); req(muac_v)
    d_cur  <- df_f(); req(d_cur)
    grp_var_name <- if (!is.null(input$zs_muac_group_var) && input$zs_muac_group_var != "")
      input$zs_muac_group_var else NULL
    grp_vec <- if (!is.null(grp_var_name) && grp_var_name %in% names(d_cur)) d_cur[[grp_var_name]] else NULL
    summ <- tryCatch(
      calc_muac_summary(muac_v, grp_vec),
      error = function(e) NULL
    )
    req(summ)
    datatable(
      summ |> rename(Groupe = groupe, N_total = N,
                     `Moy (mm)` = Moy_MUAC, `Méd (mm)` = Med_MUAC, `SD` = SD_MUAC,
                     `n MAS` = N_MAS, `n MAM` = N_MAM, `n MAG` = N_MAG,
                     `% MAS` = Pct_MAS, `% MAM` = Pct_MAM, `% MAG` = Pct_MAG,
                     `IC lo MAG` = MAG_CI_lo, `IC hi MAG` = MAG_CI_hi),
      rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15),
      class = "compact stripe hover"
    ) |>
      formatStyle("% MAG",
        background = styleColorBar(c(0, 50), "#E67E22"),
        backgroundSize = "100% 75%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
      )
  })

  # ══════════════════════════════════════════════════════════════════
  # MODULE Z-SCORES PAR TRANCHE D'ÂGE
  # ══════════════════════════════════════════════════════════════════

  output$zs_age_boxplot <- renderPlotly({
    res <- zs_data(); req(res)
    ind <- if (!is.null(input$zs_age_indicator)) input$zs_age_indicator else "WHZ_calc"
    req(ind %in% names(res))
    tryCatch(
      plot_zscore_by_age_group(res, ind),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$zs_age_table <- renderDT({
    res <- zs_data(); req(res)
    tbl <- tryCatch(calc_zscore_by_age_group(res), error = function(e) NULL)
    req(tbl)
    datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"),
              class = "compact stripe")
  })

