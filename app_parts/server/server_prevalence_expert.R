# Extrait de app_v2_final.R — prévalences — comparaison designs, ICC, diagnostics
  # ══════════════════════════════════════════════════════════════════
  # PRÉVALENCES — NOUVEAUX OUTPUTS (Moyennes SA · Comparaison designs · ICC · Diagnostics)
  # ══════════════════════════════════════════════════════════════════

  # ── Réactive partagée : design actif ──
  .sp_design_r <- reactive({
    res <- sp_results(); req(!is.null(res), is.null(res$error))
    d <- df_f(); req(d)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    col_strata  <- nullify(input$sp_col_strata)
    col_weights <- nullify(input$sp_col_weights)
    if (!HAS_SURVEY) return(NULL)
    options(survey.lonely.psu = "adjust")
    tryCatch(
      create_survey_design(d, col_cluster, col_strata, col_weights),
      error = function(e) NULL
    )
  })

  # ── UI : sélecteurs colonnes SA continues ──
  output$sp_means_sa_cols_ui <- renderUI({
    d <- df_f(); req(d)
    cont_cols <- names(d)[sapply(names(d), function(v) is.numeric(d[[v]]))]
    # Auto-détecter colonnes SA
    guess_sa <- function(pats) {
      m <- unname(sapply(pats, function(p) {
        found <- cont_cols[grepl(p, tolower(cont_cols))]
        if (length(found)) found[1] else NA_character_
      }))
      na.omit(m)[1]
    }
    defaults <- c(
      guess_sa(c("^fcs$","^fcs_score$","food.cons","^score_fcs")),
      guess_sa(c("^rcsi$","^rcsi_score$","^r_csi$")),
      guess_sa(c("^hdds$","^hdds_score$","dietary.div")),
      guess_sa(c("^hhs$","^hhs_score$","household.hunger"))
    )
    checkboxGroupInput("sp_means_sa_cols", "Indicateurs continus :",
      choices  = setNames(cont_cols, cont_cols),
      selected = na.omit(defaults),
      width    = "100%")
  })

  output$sp_means_group_ui <- renderUI({
    d <- df_f(); req(d)
    cat_cols <- names(d)[sapply(names(d), function(v) is.character(d[[v]]) || is.factor(d[[v]]))]
    choices <- c("Global" = "", setNames(cat_cols, cat_cols))
    selectInput("sp_means_group_var", "Grouper par :",
                choices = choices, selected = "", width = "100%")
  })

  output$sp_means_forest <- renderPlotly({
    des <- .sp_design_r(); req(des)
    req(input$sp_means_sa_cols, length(input$sp_means_sa_cols) > 0)
    d <- df_f(); req(d)

    cols <- setNames(input$sp_means_sa_cols, input$sp_means_sa_cols)
    grp  <- if (!is.null(input$sp_means_group_var) && input$sp_means_group_var != "")
              input$sp_means_group_var else NULL

    tbl <- tryCatch(calc_means_survey(des, cols, grp), error = function(e) NULL)
    req(tbl, nrow(tbl) > 0)

    has_group <- "Groupe" %in% names(tbl)
    tbl$y_lbl <- if (has_group) paste0(tbl$Indicateur, " — ", tbl$Groupe) else tbl$Indicateur
    tbl$hover <- paste0(
      "<b>", tbl$y_lbl, "</b><br>",
      "Moy : ", tbl$Moyenne, "<br>",
      "IC 95% : [", tbl$IC_lo, " – ", tbl$IC_hi, "]<br>",
      "SE : ", tbl$SE, "<br>",
      "DEFF : ", tbl$DEFF, "<br>",
      "N : ", tbl$N, " | N eff : ", tbl$n_eff
    )

    cols_ind <- scales::hue_pal()(length(unique(tbl$Indicateur)))
    col_map  <- setNames(cols_ind, unique(tbl$Indicateur))

    fig <- plot_ly()
    for (ind in unique(tbl$Indicateur)) {
      d_ind <- tbl |> filter(Indicateur == ind)
      fig <- fig |>
        add_segments(
          data = d_ind, x = ~IC_lo, xend = ~IC_hi,
          y = ~y_lbl, yend = ~y_lbl,
          line = list(color = col_map[ind], width = 3.5),
          name = ind, legendgroup = ind, showlegend = TRUE, hoverinfo = "none"
        ) |>
        add_trace(
          data = d_ind, x = ~Moyenne, y = ~y_lbl,
          type = "scatter", mode = "markers",
          marker = list(color = col_map[ind], size = 10, symbol = "circle",
                        line = list(color = "white", width = 1.5)),
          text = ~hover, hoverinfo = "text",
          name = ind, legendgroup = ind, showlegend = FALSE
        )
    }
    fig |> layout(
      title = list(text = "<b>Moyennes avec IC 95% design-based</b>",
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "Valeur moyenne (échelle de l'indicateur)", gridcolor = "#f0f0f0"),
      yaxis = list(title = "", tickfont = list(size = 10)),
      legend = list(orientation = "h", y = -0.2),
      plot_bgcolor = "white", paper_bgcolor = "white"
    )
  })

  output$sp_means_table <- renderDT({
    des <- .sp_design_r(); req(des)
    req(input$sp_means_sa_cols, length(input$sp_means_sa_cols) > 0)
    cols <- setNames(input$sp_means_sa_cols, input$sp_means_sa_cols)
    grp  <- if (!is.null(input$sp_means_group_var) && input$sp_means_group_var != "")
              input$sp_means_group_var else NULL
    tbl <- tryCatch(calc_means_survey(des, cols, grp), error = function(e) NULL)
    req(tbl, nrow(tbl) > 0)
    datatable(
      tbl |> rename(`IC bas` = IC_lo, `IC haut` = IC_hi,
                     `N effectif` = n_eff),
      rownames = FALSE,
      options  = list(dom = "ftip", pageLength = 20, scrollX = TRUE),
      class    = "compact stripe hover"
    ) |>
      formatStyle("DEFF",
        color = styleInterval(c(1.5, 2.5), c("#27AE60", "#E67E22", "#C0392B")),
        fontWeight = "bold"
      )
  })

  # ── Comparaison designs ──
  .sp_cmp_r <- reactive({
    res <- sp_results(); req(!is.null(res), is.null(res$error))
    d <- df_f(); req(d)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    col_strata  <- nullify(input$sp_col_strata)
    col_weights <- nullify(input$sp_col_weights)
    inds_sel    <- if (!is.null(input$sp_cmp_inds)) input$sp_cmp_inds else c("GAM","MAG_muac")

    # Indicateurs disponibles dans les données
    inds_avail <- intersect(inds_sel, names(d))
    req(length(inds_avail) > 0)
    col_inds <- setNames(inds_avail, inds_avail)

    tryCatch(
      calc_design_comparison(d, col_inds, col_cluster, col_strata, col_weights),
      error = function(e) NULL
    )
  })

  output$sp_cmp_forest <- renderPlotly({
    df_cmp <- .sp_cmp_r(); req(df_cmp, nrow(df_cmp) > 0)
    tryCatch(
      plot_design_comparison(df_cmp),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x = .5))
    )
  })

  output$sp_cmp_table <- renderDT({
    df_cmp <- .sp_cmp_r(); req(df_cmp, nrow(df_cmp) > 0)
    datatable(
      df_cmp |>
        rename(`Prévalence (%)` = Prev_pct, `IC bas` = CI_lo, `IC haut` = CI_hi,
               `Largeur IC (pts)` = Largeur_IC, `SE (%)` = SE_pct, `N effectif` = n_eff) |>
        arrange(Indicateur, Design),
      rownames = FALSE,
      options  = list(dom = "ftip", pageLength = 25, scrollX = TRUE),
      class    = "compact stripe hover"
    ) |>
      formatStyle("Design",
        target = "row",
        backgroundColor = styleEqual(
          c("Stratifié + Grappes", "Grappes seules", "SRS (naïf)"),
          c("#e8f4fd", "#f0fff4", "#f9f9f9")
        )
      ) |>
      formatStyle("DEFF",
        color = styleInterval(c(1.5, 2.5), c("#27AE60", "#E67E22", "#C0392B")),
        fontWeight = "bold"
      )
  })

  # ── ICC et DEFF ──
  output$sp_icc_table <- renderDT({
    res <- sp_results(); req(!is.null(res), is.null(res$error))
    d <- df_f(); req(d)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    req(col_cluster)

    inds_all <- c(GAM = "GAM", SAM = "SAM",
                  `MAG MUAC` = "MAG_muac", `MAS MUAC` = "MAS_muac",
                  Stunting = "Stunting", Underweight = "Underweight")
    inds_avail <- inds_all[inds_all %in% names(d)]
    req(length(inds_avail) > 0)

    tbl <- tryCatch(
      calc_icc_table(d, inds_avail, col_cluster),
      error = function(e) NULL
    )
    req(tbl, nrow(tbl) > 0)

    datatable(
      tbl |> rename(
        `DEFF théorique` = DEFF_theorique, `m̄ (taille moy. grappe)` = m_bar,
        `K (grappes)` = n_clusters, `N total` = N_total
      ),
      rownames = FALSE,
      options  = list(dom = "t", pageLength = 15),
      class    = "compact stripe"
    ) |>
      formatStyle("ICC",
        background = styleColorBar(c(0, 0.4), "#E67E22"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      formatStyle("DEFF théorique",
        color = styleInterval(c(1.5, 2.5), c("#27AE60", "#E67E22", "#C0392B")),
        fontWeight = "bold"
      )
  })

  output$sp_deff_bar <- renderPlotly({
    res <- sp_results(); req(!is.null(res), !is.null(res$global))
    df_g <- res$global
    req("DEFF" %in% names(df_g))
    df_g <- df_g |> filter(!is.na(DEFF))
    req(nrow(df_g) > 0)

    df_g$couleur <- case_when(
      df_g$DEFF < 1.5  ~ "#27AE60",
      df_g$DEFF < 2.5  ~ "#E67E22",
      TRUE             ~ "#C0392B"
    )
    df_g$hover <- paste0(
      "<b>", df_g$Indicateur, "</b><br>",
      "DEFF = ", df_g$DEFF, "<br>",
      "N effectif = ", round(df_g$N / df_g$DEFF, 0), " / ", df_g$N
    )

    plot_ly(df_g, x = ~DEFF, y = ~reorder(Indicateur, DEFF),
            type = "bar", orientation = "h",
            marker = list(color = ~couleur, line = list(color = "white", width = 1)),
            text = ~paste0("DEFF = ", DEFF), textposition = "outside",
            hoverinfo = "text", hovertext = ~hover) |>
      add_vline(x = 1, line_color = "#888", line_dash = "dash",
                line_width = 1, annotation_text = "DEFF = 1 (SRS)", 
                annotation_font = list(size = 10, color = "#888")) |>
      layout(
        title = list(text = "<b>Effets de sondage (DEFF) par indicateur</b>",
                     font = list(size = 13, color = "#0B3954"), x = 0),
        xaxis = list(title = "DEFF", gridcolor = "#f0f0f0"),
        yaxis = list(title = ""),
        showlegend = FALSE,
        plot_bgcolor = "white", paper_bgcolor = "white"
      )
  })

  # ── Diagnostics plan de sondage ──
  .sp_diag_r <- reactive({
    d <- df_f(); req(d)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    col_strata  <- nullify(input$sp_col_strata)
    req(col_cluster, col_cluster %in% names(d))
    tryCatch(
      calc_design_diagnostics(d, col_cluster, col_strata),
      error = function(e) NULL
    )
  })

  output$sp_diag_alerts <- renderUI({
    diag <- .sp_diag_r()
    if (is.null(diag)) {
      return(tags$div(
        style = "background:#f5f5f5;border-radius:8px;padding:12px;font-size:11px;color:#888;",
        icon("info-circle"), " Sélectionnez une variable Cluster pour afficher les diagnostics."
      ))
    }
    tagList(
      tags$div(
        style = "display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
        lapply(list(
          list("Grappes (PSU)", diag$n_clusters, "#0B3954"),
          list("Taille min", diag$min_size, "#27AE60"),
          list("Taille max", diag$max_size, "#E67E22"),
          list("Taille moyenne", diag$mean_size, "#0077B6"),
          list("CV taille (%)", paste0(diag$cv_size, "%"),
               if (diag$cv_size > 30) "#C0392B" else "#27AE60"),
          list("PSU outliers", diag$outlier_clusters,
               if (diag$outlier_clusters > 0) "#E67E22" else "#27AE60")
        ), function(box) {
          tags$div(
            style = paste0("background:white;border-radius:8px;padding:10px 14px;",
                           "border-left:3px solid ", box[[3]], ";text-align:center;flex:1;min-width:100px;"),
            tags$div(style = paste0("font-size:20px;font-weight:800;color:", box[[3]], ";"), box[[2]]),
            tags$div(style = "font-size:10px;color:#888;text-transform:uppercase;", box[[1]])
          )
        })
      ),
      render_design_alerts(diag)
    )
  })

  output$sp_cluster_sizes_hist <- renderPlotly({
    diag <- .sp_diag_r(); req(diag)
    tryCatch(
      plot_cluster_sizes(diag),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x = .5))
    )
  })

  output$sp_cluster_sizes_table <- renderDT({
    diag <- .sp_diag_r(); req(diag)
    tbl  <- diag$cluster_sizes
    datatable(
      tbl,
      rownames = FALSE,
      options  = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class    = "compact stripe"
    ) |>
      formatStyle("N",
        background = styleColorBar(range(tbl$N), "#0077B6"),
        backgroundSize = "100% 75%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

