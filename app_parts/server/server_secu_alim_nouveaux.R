# Extrait de app_v2_final.R — sécurité alimentaire — composition, IPC, rounds
  # ══════════════════════════════════════════════════════════════════
  # MODULE SA — NOUVEAUX OUTPUTS (FCS Composition · rCSI Détail · IPC · Rounds)
  # ══════════════════════════════════════════════════════════════════

  # ── FCS Composition ──
  output$sa_fcs_composition_bar <- renderPlotly({
    req(sa_res$fcs)
    tryCatch(
      plot_fcs_composition_bar(sa_res$fcs),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_radar <- renderPlotly({
    tryCatch(
      plot_sa_radar(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs),
      error = function(e) plotly_empty() |> layout(title = list(text = "Radar — calculez 2+ indicateurs", x=.5))
    )
  })

  output$sa_fcs_composition_table <- renderDT({
    req(sa_res$fcs)
    comp <- calc_fcs_composition(sa_res$fcs)
    req(comp)
    datatable(
      comp |> rename(
        `Groupe alimentaire` = Groupe, `Poids PAM` = Poids_PAM,
        `Consommation moy (j/sem)` = Consommation_moy,
        `Contribution moy (pts)` = Contribution_moy,
        `Contribution (%)` = Contribution_pct
      ),
      rownames = FALSE, options = list(dom = "t", pageLength = 12),
      class = "compact stripe"
    ) |>
      formatStyle(
        "Contribution (%)",
        background = styleColorBar(c(0, 100), "#0077B6"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ── rCSI Composantes ──
  output$sa_rcsi_components_plot <- renderPlotly({
    req(sa_res$rcsi)
    d_cur <- df_f()
    req(d_cur)
    cols_rcsi <- list(
      col_moins_prefere    = if (!is.null(input$sa_rcsi_prefere))    input$sa_rcsi_prefere    else NULL,
      col_emprunter        = if (!is.null(input$sa_rcsi_emprunter))   input$sa_rcsi_emprunter   else NULL,
      col_reduire_repas    = if (!is.null(input$sa_rcsi_repas))       input$sa_rcsi_repas       else NULL,
      col_reduire_portions = if (!is.null(input$sa_rcsi_portions))    input$sa_rcsi_portions    else NULL,
      col_reduire_adultes  = if (!is.null(input$sa_rcsi_adultes))     input$sa_rcsi_adultes     else NULL
    )
    tryCatch(
      do.call(plot_rcsi_components, c(list(d = d_cur), cols_rcsi)),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_rcsi_components_table <- renderDT({
    req(sa_res$rcsi)
    d_cur <- df_f(); req(d_cur)
    cols_rcsi <- list(
      col_moins_prefere    = if (!is.null(input$sa_rcsi_prefere))    input$sa_rcsi_prefere    else NULL,
      col_emprunter        = if (!is.null(input$sa_rcsi_emprunter))   input$sa_rcsi_emprunter   else NULL,
      col_reduire_repas    = if (!is.null(input$sa_rcsi_repas))       input$sa_rcsi_repas       else NULL,
      col_reduire_portions = if (!is.null(input$sa_rcsi_portions))    input$sa_rcsi_portions    else NULL,
      col_reduire_adultes  = if (!is.null(input$sa_rcsi_adultes))     input$sa_rcsi_adultes     else NULL
    )
    comp <- tryCatch(
      do.call(calc_rcsi_components, c(list(d = d_cur), cols_rcsi)),
      error = function(e) NULL
    )
    req(comp)
    datatable(
      comp |> rename(
        `Stratégie d'adaptation` = Stratégie, `Poids sévérité` = Poids,
        `Moy jours/sem` = Moy_jours, `% ménages recourant` = Pct_recours,
        `Contribution pondérée` = Contribution_moy
      ),
      rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
    ) |>
      formatStyle(
        "% ménages recourant",
        background = styleColorBar(c(0, 100), "#E67E22"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
      )
  })

  # ── Phases IPC/CH ──
  .ipc_phases_r <- reactive({
    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL
    d_cur  <- df_f()
    muac_s <- if (!is.null(d_cur) && ".muac" %in% names(d_cur)) d_cur$.muac else NULL
    if (is.null(fcs_s) && is.null(rcsi_s) && is.null(hhs_s)) return(NULL)
    calc_ipc_phase(fcs_s, rcsi_s, hhs_s, muac_s)
  })

  output$sa_ipc_kpi_cards <- renderUI({
    phases <- .ipc_phases_r(); req(phases)
    summ   <- calc_ipc_summary(phases); req(summ)
    phase3plus <- sum(summ$N[summ$Phase >= 3])
    pct3plus   <- round(phase3plus / sum(summ$N) * 100, 1)
    phase4plus <- sum(summ$N[summ$Phase >= 4])
    pct4plus   <- round(phase4plus / sum(summ$N) * 100, 1)

    col_main <- if (pct3plus >= 20) "#C80000" else if (pct3plus >= 10) "#E67800" else "#59A608"

    tags$div(
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;margin-bottom:10px;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Phase 3+ (Crise+)"),
        tags$div(style=paste0("font-size:32px;font-weight:800;color:", col_main, ";"), paste0(pct3plus, "%")),
        tags$div(style="font-size:11px;color:#555;", paste0(phase3plus, " / ", sum(summ$N), " ménages"))
      ),
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;margin-bottom:10px;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Phase 4-5 (Urgence+)"),
        tags$div(style="font-size:28px;font-weight:800;color:#C80000;", paste0(pct4plus, "%")),
        tags$div(style="font-size:11px;color:#555;", paste0(phase4plus, " ménages"))
      ),
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Répartition phases"),
        lapply(1:5, function(ph) {
          r <- summ[summ$Phase == ph, ]
          if (nrow(r) == 0 || r$N == 0) return(NULL)
          tags$div(style="display:flex;align-items:center;gap:6px;margin:3px 0;",
            tags$div(style=paste0("width:12px;height:12px;border-radius:2px;background:", r$Couleur, ";flex-shrink:0;")),
            tags$span(style="font-size:10px;color:#555;flex:1;", paste0("Ph", ph)),
            tags$span(style="font-size:11px;font-weight:700;", paste0(r$Pct, "%"))
          )
        })
      )
    )
  })

  output$sa_ipc_phases_plot <- renderPlotly({
    phases <- .ipc_phases_r(); req(phases)
    tryCatch(
      plot_ipc_phases(phases),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_ipc_by_group_plot <- renderPlotly({
    phases <- .ipc_phases_r(); req(phases)
    d_cur <- df_f(); req(d_cur)
    grp_var <- if (!is.null(input$sa_ipc_group_var) && input$sa_ipc_group_var != "")
      input$sa_ipc_group_var else NULL
    grp_vec <- if (!is.null(grp_var) && grp_var %in% names(d_cur)) d_cur[[grp_var]] else NULL
    tryCatch(
      plot_ipc_phases(phases, group_var = grp_vec),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_ipc_table <- renderDT({
    phases <- .ipc_phases_r(); req(phases)
    summ <- calc_ipc_summary(phases); req(summ)
    datatable(
      summ |> select(-Couleur) |> rename(`Phase IPC` = Phase, Libellé = Label,
                                          `N ménages` = N, `%` = Pct),
      rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
    ) |>
      formatStyle("Phase IPC",
        target = "row",
        backgroundColor = styleEqual(1:5, c("#2E7D52","#D4A843","#D98B14","#BF3636","#5C1A1A")),
        color = styleEqual(1:5, c("white","black","white","white","white"))
      )
  })

  # ── Comparaison inter-rounds ──
  output$sa_rounds_plot <- renderPlotly({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    req(length(round_col) > 0)
    round_col <- round_col[1]
    if (length(unique(d_cur[[round_col]])) < 2)
      return(plotly_empty() |> layout(title = list(
        text = "Au moins 2 rounds requis pour la comparaison", x = .5)))

    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hdds_s <- if (!is.null(sa_res$hdds)) sa_res$hdds$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL

    if (is.null(fcs_s) && is.null(rcsi_s) && is.null(hdds_s) && is.null(hhs_s))
      return(plotly_empty() |> layout(title = list(text = "Calculez les indicateurs SA d'abord", x = .5)))

    tryCatch(
      plot_sa_rounds(d_cur, round_col, fcs_s, rcsi_s, hdds_s, hhs_s),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_rounds_table <- renderDT({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    req(length(round_col) > 0)
    round_col <- round_col[1]
    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hdds_s <- if (!is.null(sa_res$hdds)) sa_res$hdds$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL

    tbl <- tryCatch(
      calc_sa_by_round(d_cur, round_col, fcs_s, rcsi_s, hdds_s, hhs_s),
      error = function(e) NULL
    )
    req(tbl)
    datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"),
              class = "compact stripe")
  })

