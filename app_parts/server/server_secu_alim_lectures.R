# Extrait de app_v2_final.R — sécurité alimentaire — lectures expertes et exports
  # ══════════════════════════════════════════════════════════════════
  # MODULE SA — COUCHE EXPERTE (lectures · OMS · ménages · export)
  # ══════════════════════════════════════════════════════════════════

  # ── Lectures expertes par indicateur ──
  output$sa_fcs_reading <- renderUI({
    txt <- sa_reading_fcs(sa_res$fcs); if (is.null(txt)) return(NULL)
    pk_info(txt, "utensils", "#0077B6", "#e8f6fc")
  })
  output$sa_rcsi_reading <- renderUI({
    txt <- sa_reading_rcsi(sa_res$rcsi); if (is.null(txt)) return(NULL)
    pk_info(txt, "triangle-exclamation", "#E67E22", "#fff3e0")
  })
  output$sa_hdds_reading <- renderUI({
    txt <- sa_reading_hdds(sa_res$hdds); if (is.null(txt)) return(NULL)
    pk_info(txt, "seedling", "#27AE60", "#eafaf1")
  })
  output$sa_hhs_reading <- renderUI({
    txt <- sa_reading_hhs(sa_res$hhs); if (is.null(txt)) return(NULL)
    pk_info(txt, "heartbeat", "#C0392B", "#fdf2f2")
  })

  # ── Lecture composition FCS ──
  output$sa_fcs_comp_reading <- renderUI({
    r <- sa_res$fcs; if (is.null(r)) return(NULL)
    comp <- calc_fcs_composition(r); if (is.null(comp) || nrow(comp) == 0) return(NULL)
    top <- comp[1, ]
    txt <- sprintf(paste0(
      "Lecture experte : le groupe %s contribue le plus au score moyen (%.1f pts, %.1f %% du FCS), ",
      "suivi de %s. Les c\u00e9r\u00e9ales et l\u00e9gumineuses p\u00e8sent structurellement lourd (poids PAM \u00d72 et \u00d73) : ",
      "un score acceptable peut masquer une alimentation peu diversifi\u00e9e si ces seuls groupes sont consomm\u00e9s."),
      top$Groupe, top$Contribution_moy, top$Contribution_pct,
      if (nrow(comp) > 1) comp$Groupe[2] else "\u2014")
    pk_info(txt, "chart-bar", "#0077B6", "#e8f6fc")
  })

  # ── Lecture rCSI détail ──
  output$sa_rcsi_comp_reading <- renderUI({
    if (is.null(sa_res$rcsi)) return(NULL)
    d_cur <- df_f(); req(d_cur)
    comp <- tryCatch(calc_rcsi_components(d_cur,
      col_moins_prefere    = if (is.null(input$sa_rcsi_prefere) || input$sa_rcsi_prefere == "") NULL else input$sa_rcsi_prefere,
      col_emprunter        = if (is.null(input$sa_rcsi_emprunter) || input$sa_rcsi_emprunter == "") NULL else input$sa_rcsi_emprunter,
      col_reduire_repas    = if (is.null(input$sa_rcsi_repas) || input$sa_rcsi_repas == "") NULL else input$sa_rcsi_repas,
      col_reduire_portions = if (is.null(input$sa_rcsi_portions) || input$sa_rcsi_portions == "") NULL else input$sa_rcsi_portions,
      col_reduire_adultes  = if (is.null(input$sa_rcsi_adultes) || input$sa_rcsi_adultes == "") NULL else input$sa_rcsi_adultes),
      error = function(e) NULL)
    if (is.null(comp)) return(NULL)
    top <- comp[1, ]
    txt <- sprintf(paste0(
      "Lecture experte : la strat\u00e9gie la plus fr\u00e9quente est \u00ab %s \u00bb (%.1f %% des m\u00e9nages, ",
      "%.2f j/sem en moyenne, contribution %.2f pts). ",
      "Les strat\u00e9gies de poids 3 (restriction des adultes) signalent les situations les plus s\u00e9v\u00e8res : ",
      "les suivre s\u00e9par\u00e9ment permet de distinguer stress \u00e9conomique et ins\u00e9curit\u00e9 alimentaire install\u00e9e."),
      top$Stratégie, top$Pct_recours, top$Moy_jours, top$Contribution_moy)
    pk_info(txt, "exclamation-circle", "#E67E22", "#fff3e0")
  })

  # ── Lecture stratification (χ²) ──
  output$sa_strat_reading <- renderUI({
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    if (is.null(strat_var)) return(NULL)
    d_cur <- df_f(); req(d_cur, strat_var %in% names(d_cur))
    class_vec <- if (!is.null(sa_res$fcs)) sa_res$fcs$class
                 else if (!is.null(sa_res$rcsi)) sa_res$rcsi$class
                 else NULL
    if (is.null(class_vec)) return(NULL)
    test <- calc_strat_test(class_vec, d_cur[[strat_var]])
    txt <- sa_reading_strat(test, strat_var)
    pk_info(txt, "chart-simple", "#0B3954", "#eef2f5")
  })

  # ── Lecture IPC ──
  output$sa_ipc_reading <- renderUI({
    phases <- .ipc_phases_r(); if (is.null(phases)) return(NULL)
    txt <- sa_reading_ipc(calc_ipc_summary(phases)); if (is.null(txt)) return(NULL)
    pk_info(txt, "globe", "#D4A843", "#fff8e1")
  })

  # ── Lecture rounds (tendance + test) ──
  output$sa_rounds_reading <- renderUI({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    if (length(round_col) == 0 || is.null(sa_res$fcs)) return(NULL)
    trend <- tryCatch(
      calc_rounds_trend(d_cur[[round_col[1]]], sa_res$fcs$class, "Pauvre",
                        "FCS pauvre"),
      error = function(e) NULL)
    txt <- sa_reading_rounds(trend); if (is.null(txt)) return(NULL)
    pk_info(txt, "calendar-alt", "#27AE60", "#eafaf1")
  })

  # ── Indicateurs OMS : LCSI · FIES · MDD-W ──
  sa_oms_any <- function() {
    !is.null(sa_res$lcsi) || !is.null(sa_res$fies) || !is.null(sa_res$mddw)
  }
  output$sa_lcsi_plot <- renderPlotly({
    if (is.null(sa_res$lcsi))
      return(plotly_empty() |> layout(title = list(
        text = "LCSI non calculé — mappez une classe ou les stratégies", x = .5)))
    tryCatch(plot_lcsi_plotly(sa_res$lcsi),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_lcsi_table <- renderDT({
    if (is.null(sa_res$lcsi) || is.null(sa_res$lcsi$summary))
      return(datatable(data.frame(Message = "LCSI non calculé"), rownames = FALSE, options = list(dom = "t")))
    datatable(sa_add_ci(sa_res$lcsi$summary) |> select(-Couleur) |>
                rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })
  output$sa_fies_plot <- renderPlotly({
    if (is.null(sa_res$fies))
      return(plotly_empty() |> layout(title = list(
        text = "FIES non calculé — sélectionnez une colonne de score (0-8)", x = .5)))
    tryCatch(plot_fies_plotly(sa_res$fies),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_fies_table <- renderDT({
    if (is.null(sa_res$fies) || is.null(sa_res$fies$summary))
      return(datatable(data.frame(Message = "FIES non calculé"), rownames = FALSE, options = list(dom = "t")))
    s <- sa_add_ci(sa_res$fies$summary) |> select(-Couleur)
    datatable(s, rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })
  output$sa_mddw_plot <- renderPlotly({
    if (is.null(sa_res$mddw))
      return(plotly_empty() |> layout(title = list(
        text = "MDD-W non calculé — sélectionnez la colonne binaire", x = .5)))
    tryCatch(plot_mddw_plotly(sa_res$mddw),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_oms_reading <- renderUI({
    if (!sa_oms_any()) return(NULL)
    tagList(
      if (!is.null(sa_res$lcsi)) pk_info(sa_reading_lcsi(sa_res$lcsi),
        "briefcase", "#0077B6", "#e8f6fc"),
      if (!is.null(sa_res$fies)) pk_info(sa_reading_fies(sa_res$fies),
        "face-frown", "#E67E22", "#fff3e0"),
      if (!is.null(sa_res$mddw)) pk_info(sa_reading_mddw(sa_res$mddw),
        "bowl-food", "#27AE60", "#eafaf1")
    )
  })

  # ── Table ménages ──
  sa_household_r <- reactive({
    d_cur <- df_f(); req(d_cur)
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs) ||
               !is.null(sa_res$lcsi) || !is.null(sa_res$fies) || !is.null(sa_res$mddw)
    if (!any_res) return(NULL)
    build_sa_household_table(d_cur,
      fcs_res = sa_res$fcs, rcsi_res = sa_res$rcsi,
      hdds_res = sa_res$hdds, hhs_res = sa_res$hhs,
      lcsi_res = sa_res$lcsi, fies_res = sa_res$fies,
      mddw_res = sa_res$mddw, sev_res = sa_severity_r(),
      ipc_phases = .ipc_phases_r())
  })
  output$sa_household_table <- renderDT({
    tbl <- sa_household_r(); req(tbl)
    datatable(tbl, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
              class = "compact stripe hover", filter = "top")
  })

  # ── Exports ──
  sa_frames_r <- reactive({
    sa_export_frames(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs,
                     sa_res$lcsi, sa_res$fies, sa_res$mddw,
                     sa_severity_r(), calc_ipc_summary(.ipc_phases_r()),
                     sa_household_r(), sa_verdict_r())
  })
  output$sa_dl_xlsx <- downloadHandler(
    filename = function() paste0("securite_alimentaire_", Sys.Date(), ".xlsx"),
    content = function(file) {
      frames <- sa_frames_r()
      if (length(frames) == 0) { writeLines("Aucun résultat", file); return() }
      wb <- openxlsx::createWorkbook()
      for (nm in names(frames)) {
        openxlsx::addWorksheet(wb, substr(nm, 1, 31))
        openxlsx::writeData(wb, substr(nm, 1, 31), frames[[nm]])
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })
  sa_write_csv <- function(file) {
    hh <- sa_household_r()
    if (is.null(hh)) { writeLines("Aucun résultat", file); return(invisible()) }
    utils::write.csv2(hh, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
  output$sa_dl_csv_hh <- downloadHandler(
    filename = function() paste0("menages_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) sa_write_csv(file))
  output$sa_dl_csv_hh2 <- downloadHandler(
    filename = function() paste0("menages_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) sa_write_csv(file))
  output$sa_dl_csv_summary <- downloadHandler(
    filename = function() paste0("synthese_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) {
      frames <- sa_frames_r()
      frames$Menages <- NULL
      if (length(frames) == 0) { writeLines("Aucun résultat", file); return() }
      synth <- bind_rows(lapply(names(frames), function(nm) {
        s <- frames[[nm]]
        if (is.null(s) || nrow(s) == 0) return(NULL)
        s$Section <- nm
        s
      }))
      utils::write.csv2(synth, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output$sa_export_info <- renderUI({
    frames <- sa_frames_r()
    tags$div(style = "font-size:11px;color:#666;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
      icon("circle-info"), " Contenu disponible : ",
      tags$b(paste(length(frames), "feuille(s)")), " — ",
      paste(setdiff(names(frames), "Menages"), collapse = ", "),
      if ("Menages" %in% names(frames))
        paste0(" + table ménages (", nrow(sa_household_r()), " lignes)")
    )
  })

