# Extrait de app_v2_final.R — modules SENS — mortalité, IYCF, anémie, paludisme, WASH, ménage, handicap
  # ══════════════════════════════════════════════════════════════════════════════
  # SERVEUR — MODULES SENS (Mortalité, IYCF, Anémie, Paludisme, WASH, Ménage, WG)
  # ══════════════════════════════════════════════════════════════════════════════

  # Helper pour créer un selectInput dynamique basé sur auto-détection
  sens_select <- function(id, label, detect_key, d, allow_empty = TRUE) {
    all_cols <- names(d)
    det <- auto_detect(d)
    sel <- det[[detect_key]]
    choices <- if (allow_empty) c("—" = "", all_cols) else all_cols
    selectInput(id, label, choices = choices, selected = sel)
  }

  # ── MORTALITÉ — module namespacé (mod_mortalite.R) ─────────────────────────
  mod_mortalite_server("mortalite", df_f = df_f)

  # ── IYCF ───────────────────────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    iycf_det <- iycf_detect_columns(d, list(), use_design = TRUE)
    iycf_pick <- function(key, aux = FALSE) {
      v <- if (aux) iycf_det$aux[[key]] else iycf_det$columns[[key]]
      if (is.null(v) || !nzchar(v)) "" else v
    }
    iycf_select <- function(id, label, selected) {
      selectInput(id, label, choices = c("—" = "", names(d)), selected = selected)
    }
    output$iycf_age_ui <- renderUI(iycf_select("iycf_age", "Âge (mois)", iycf_pick("age", TRUE)))
    output$iycf_cluster_ui <- renderUI({
      cand <- intersect(c("cluster", "cluster_id", "grappe", "psu", "primary_sampling_unit"), names(d))
      iycf_select("iycf_cluster", "Cluster / grappe", if (length(cand) > 0) cand[1] else "")
    })
    output$iycf_strata_ui <- renderUI({
      cand <- intersect(c("strate", "strata", "stratum", "sampling_strata"), names(d))
      iycf_select("iycf_strata", "Strates", if (length(cand) > 0) cand[1] else "")
    })
    output$iycf_weights_ui <- renderUI({
      cand <- intersect(c("poids_echantillon", "sampling_weight", "survey_weight",
                          "design_weight", "poids_pondere", "weights"), names(d))
      iycf_select("iycf_weights", "Poids de sondage", if (length(cand) > 0) cand[1] else "")
    })
    output$iycf_bfstatus_ui <- renderUI({
      iycf_select("iycf_bfstatus", "Statut d'allaitement (dérivation)", iycf_pick("bf_status", TRUE))
    })
    output$iycf_ebf_ui    <- renderUI(iycf_select("iycf_ebf", "Allaitement exclusif", iycf_pick("ebf")))
    output$iycf_cbf_ui    <- renderUI(iycf_select("iycf_cbf", "Allaitement continu", iycf_pick("cbf")))
    output$iycf_eibf_ui   <- renderUI(iycf_select("iycf_eibf", "Mise au sein précoce", iycf_pick("eibf")))
    output$iycf_isf_ui    <- renderUI(iycf_select("iycf_isf", "Intro aliments solides", iycf_pick("isf")))
    output$iycf_mdd_ui    <- renderUI(iycf_select("iycf_mdd", "Diversité min (MDD)", iycf_pick("mdd")))
    output$iycf_mmf_ui    <- renderUI(iycf_select("iycf_mmf", "Fréquence min repas (MMF)", iycf_pick("mmf")))
    output$iycf_mad_ui    <- renderUI(iycf_select("iycf_mad", "Régime acceptable (MAD)", iycf_pick("mad")))
    output$iycf_iron_ui   <- renderUI(iycf_select("iycf_iron", "Aliments riches en fer", iycf_pick("iron")))
    output$iycf_bottle_ui <- renderUI(iycf_select("iycf_bottle", "Biberon", iycf_pick("bottle")))
    output$iycf_sweet_ui  <- renderUI(iycf_select("iycf_sweet", "Boissons sucrées", iycf_pick("sweet")))
    output$iycf_upt_ui    <- renderUI(iycf_select("iycf_upt", "Ultra-transformés", iycf_pick("upt")))
    output$iycf_zvf_ui    <- renderUI(iycf_select("iycf_zvf", "Zéro légumes/fruits", iycf_pick("zvf")))
    output$iycf_vita_ui   <- renderUI(iycf_select("iycf_vita", "Vitamine A (6-59 mois)", iycf_pick("vita")))
    output$iycf_anc_ui    <- renderUI(iycf_select("iycf_anc", "ANC 4+ (visites CPN)", iycf_pick("anc4")))
    output$iycf_stratvar_ui <- renderUI({
      cats <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]
      cats <- setdiff(cats, c("household_id", "child_id"))
      sel <- intersect(c("region", "province", "department", "survey_round", "child_sex"), cats)
      selectInput("iycf_stratvar", "Strate", choices = cats,
                  selected = if (length(sel) > 0) sel[1] else cats[1])
    })
    output$iycf_equity_var_ui <- renderUI({
      cats <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]
      cats <- setdiff(cats, c("household_id", "child_id", "id_menage"))
      sel <- intersect(c("region", "province", "departement", "dept", "sexe",
                         "child_sex", "milieu", "strate"), cats)
      selectInput("iycf_equity_var", "Variable d'équité", choices = cats,
                  selected = if (length(sel) > 0) sel[1] else cats[1])
    })
  }) |> bindEvent(df_f())

  iycf_res <- reactiveVal(NULL)

  observeEvent(input$run_iycf, {
    d <- df_f(); req(d)
    withProgress(message = "Calcul IYCF...", {
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      result <- calc_iycf_indicators(d, col_age = g("iycf_age"),
        col_ebf = g("iycf_ebf"), col_cbf = g("iycf_cbf"), col_eibf = g("iycf_eibf"),
        col_isf = g("iycf_isf"), col_mdd = g("iycf_mdd"), col_mmf = g("iycf_mmf"),
        col_mad = g("iycf_mad"), col_iron = g("iycf_iron"), col_bottle = g("iycf_bottle"),
        col_sweet = g("iycf_sweet"), col_upt = g("iycf_upt"), col_zvf = g("iycf_zvf"),
        col_bfstatus = g("iycf_bfstatus"), col_vita = g("iycf_vita"),
        col_anc = g("iycf_anc"),
        col_cluster = g("iycf_cluster"), col_strata = g("iycf_strata"),
        col_weights = g("iycf_weights"), use_design = isTRUE(input$iycf_use_design))
      if (!is.null(result$error)) {
        showNotification(paste("IYCF :", result$error), type = "error", duration = 8)
        return(invisible(NULL))
      }
      iycf_res(result)
    })
    showNotification(paste0("IYCF : ", iycf_res()$n_indicators, " indicateurs calculés ✓"), type = "message")
  })

  output$iycf_kpi <- renderUI({ req(iycf_res()); HTML(render_iycf_kpi(iycf_res())) })

  iycf_gap_r <- reactive({ req(iycf_res()); calc_iycf_gap(iycf_res()) })
  iycf_verdict_r <- reactive({ req(iycf_res()); calc_iycf_verdict(iycf_res()) })

  output$iycf_summary_table <- DT::renderDT({
    g <- iycf_gap_r(); req(nrow(g) > 0)
    tbl <- data.frame(
      Indicateur = g$Indicateur,
      N = g$N, Positifs = g$Positifs,
      `Prévalence (%)` = g$Prevalence,
      `IC 95% inf` = g$IC_inf, `IC 95% sup` = g$IC_sup,
      `Cible OMS (%)` = g$Cible_OMS, `Écart (pts)` = g$Gap,
      Statut = g$Statut, check.names = FALSE)
    DT::datatable(tbl, rownames = FALSE,
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE),
                  class = "compact stripe hover")
  })

  output$iycf_verdict <- renderUI({
    v <- iycf_verdict_r(); req(v)
    tagList(
      tags$div(style = paste0("background:", v$bg, ";border-left:5px solid ", v$color,
        ";border-radius:10px;padding:14px 18px;margin-bottom:12px;"),
        tags$div(style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
          tags$span(style = paste0("background:", v$color,
            ";color:white;font-size:11px;font-weight:700;padding:3px 10px;",
            "border-radius:20px;letter-spacing:.5px;text-transform:uppercase;"), v$badge),
          tags$span(style = "font-size:13px;color:#333;", tags$b(v$headline))
        ),
        if (length(v$bullets) > 0)
          tags$ul(style = "margin:8px 0 0 0;padding-left:18px;font-size:12px;color:#444;line-height:1.7;",
            lapply(v$bullets, function(b) tags$li(b)))
      )
    )
  })

  output$iycf_gauge <- renderPlotly({ v <- iycf_verdict_r(); req(v); plot_iycf_gauge(v) })
  output$iycf_reading_synth <- renderUI({ HTML(iycf_reading_synthesis(iycf_verdict_r()) %||% "") })

  output$iycf_dashboard_plot <- renderPlot({ req(iycf_res()); plot_iycf_dashboard(iycf_res()) })
  output$iycf_reading_dash <- renderUI({ HTML(iycf_reading_dashboard(iycf_res()) %||% "") })

  output$iycf_bf_cascade_plot <- renderPlot({ req(iycf_res()); plot_breastfeeding_cascade(iycf_res()) })
  output$iycf_reading_bf <- renderUI({ HTML(iycf_reading_bf(iycf_res()) %||% "") })

  output$iycf_diet_plot <- renderPlotly({
    g <- iycf_gap_r(); req(nrow(g) > 0)
    gg <- g[g$key %in% c("isf", "mdd", "mmf", "mad", "iron", "vita"), , drop = FALSE]
    plot_iycf_gap(gg)
  })
  output$iycf_reading_diet <- renderUI({ HTML(iycf_reading_diet(iycf_res()) %||% "") })

  output$iycf_risk_plot <- renderPlotly({ g <- iycf_gap_r(); req(nrow(g) > 0); plot_iycf_risk(g) })
  output$iycf_reading_risks <- renderUI({ HTML(iycf_reading_risks(iycf_res()) %||% "") })

  output$iycf_gap_plot <- renderPlotly({ plot_iycf_gap(iycf_gap_r()) })
  output$iycf_gap_table <- DT::renderDT({
    g <- iycf_gap_r(); req(nrow(g) > 0)
    tbl <- data.frame(
      Indicateur = g$Indicateur,
      `Prévalence (%)` = g$Prevalence,
      `IC inf` = g$IC_inf, `IC sup` = g$IC_sup,
      `Cible (%)` = g$Cible_OMS, `Écart (pts)` = g$Gap,
      Statut = g$Statut, check.names = FALSE)
    DT::datatable(tbl, rownames = FALSE,
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_reading_gaps <- renderUI({ HTML(iycf_reading_gaps(iycf_gap_r()) %||% "") })

  output$iycf_focus_ui <- renderUI({
    r <- iycf_res()
    if (is.null(r) || nrow(r$summary) == 0)
      return(tags$div(style = "font-size:11px;color:#777;",
                      icon("info-circle"), " Lancer le calcul pour choisir l'indicateur de suivi."))
    keys <- r$summary$key
    labs <- vapply(keys, function(k) IYCF_INDICATORS[[k]]$label, character(1))
    sel <- if ("eibf" %in% keys) "eibf" else keys[1]
    selectInput("iycf_focus", "Indicateur de suivi",
                choices = stats::setNames(keys, labs), selected = sel)
  })

  iycf_strat_r <- reactive({
    r <- iycf_res(); d <- df_f()
    if (is.null(r) || is.null(input$iycf_focus) || is.null(input$iycf_stratvar)) return(NULL)
    calc_iycf_strat(r, input$iycf_focus, input$iycf_stratvar, d)
  })
  output$iycf_strat_plot <- renderPlotly({
    s <- iycf_strat_r()
    if (is.null(s))
      return(plotly::plotly_empty() |> plotly::layout(title = list(text = "Test non calculable", x = 0.5)))
    plot_iycf_strat(s)
  })
  output$iycf_strat_table <- DT::renderDT({
    s <- iycf_strat_r(); req(s)
    tbl <- as.data.frame(s$freq)
    names(tbl) <- c("Strate", "N", "Atteints", "Prévalence (%)")
    DT::datatable(tbl, rownames = FALSE, options = list(dom = "ftip", pageLength = 15))
  })
  output$iycf_strat_test_out <- renderUI({
    s <- iycf_strat_r()
    if (is.null(s))
      return(tags$div(style = "font-size:12px;color:#777;", "Association non calculable pour cet indicateur."))
    tags$div(style = "font-size:12px;color:#333;background:#f8f9fa;border-radius:6px;padding:8px 12px;",
      tags$b("Test d'association — "), s$test$note, " ; ", fmt_p(s$test$p),
      " ; V de Cramér = ", ifelse(is.na(s$test$V), "—", round(s$test$V, 3)),
      " ; n = ", s$test$n)
  })
  output$iycf_reading_strat <- renderUI({ HTML(iycf_reading_strat(iycf_strat_r()) %||% "") })

  iycf_round_var <- reactive({
    d <- df_f(); req(d)
    cand <- c(auto_detect(d)[["survey"]],
              intersect(c("survey_round", "round", "vague", "wave", "vague_enquete",
                          "survey", "round_enquete"), names(d)))
    cand <- unique(cand[!is.na(cand) & nzchar(cand) & cand %in% names(d)])
    cand <- cand[vapply(cand, function(v) {
      length(unique(stats::na.omit(d[[v]]))) >= 2
    }, logical(1))]
    if (length(cand) > 0) cand[1] else NULL
  })
  iycf_rounds_r <- reactive({
    r <- iycf_res(); rv <- iycf_round_var(); d <- df_f()
    if (is.null(r) || is.null(input$iycf_focus) || is.null(rv)) return(NULL)
    calc_iycf_rounds(r, input$iycf_focus, rv, d)
  })
  output$iycf_rounds_plot <- renderPlotly({
    r <- iycf_rounds_r()
    if (is.null(r))
      return(plotly::plotly_empty() |> plotly::layout(title = list(text = "Tendance non calculable", x = 0.5)))
    plot_iycf_rounds(r)
  })
  output$iycf_rounds_table <- DT::renderDT({
    r <- iycf_rounds_r()
    if (is.null(r)) return(DT::datatable(data.frame(Message = "Tendance non calculable")))
    tbl <- as.data.frame(r$summary)
    names(tbl) <- c("Round", "N", "Atteints", "Prévalence (%)", "IC inf", "IC sup")
    DT::datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 15))
  })
  output$iycf_reading_rounds <- renderUI({ HTML(iycf_reading_rounds(iycf_rounds_r()) %||% "") })

  invisible(lapply(c("iycf_verdict", "iycf_kpi", "iycf_gauge", "iycf_summary_table",
                    "iycf_quality_summary", "iycf_quality_controls", "iycf_quality_indicators",
                    "iycf_mapping_table", "iycf_age_plot", "iycf_age_table",
                    "iycf_equity_plot", "iycf_equity_table", "iycf_export_info",
                    "iycf_reading_synth", "iycf_reading_quality", "iycf_reading_age",
                    "iycf_reading_equity"),
                  function(id) outputOptions(output, id, suspendWhenHidden = FALSE)))

  output$iycf_quality_summary <- renderUI({
    r <- iycf_res(); q <- r$quality
    if (is.null(q))
      return(tags$div(style = "font-size:12px;color:#777;", "Contrôle qualité non disponible."))
    tags$div(style = paste0("background:#f8f9fa;border-left:5px solid ",
                            if (q$score >= 85) "#27AE60" else if (q$score >= 70) "#E67E22" else "#C80000",
                            ";border-radius:8px;padding:10px 14px;font-size:12px;color:#333;"),
      tags$b(sprintf("Qualité des données : %s/100 — %s", q$score, q$label)), tags$br(),
      if (length(q$issues) > 0) paste0("Points de vigilance : ", paste(q$issues, collapse = " ; "))
      else "Aucun problème majeur détecté.")
  })
  output$iycf_quality_controls <- DT::renderDT({
    r <- iycf_res(); q <- r$quality; req(q, q$child)
    DT::datatable(q$child, rownames = FALSE, options = list(dom = "t", pageLength = 15))
  })
  output$iycf_quality_indicators <- DT::renderDT({
    r <- iycf_res(); q <- r$quality; req(q, q$indicators)
    DT::datatable(q$indicators, rownames = FALSE,
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_mapping_table <- DT::renderDT({
    r <- iycf_res(); req(r)
    tbl <- iycf_mapping_table(r)
    if (nrow(tbl) == 0) return(DT::datatable(data.frame(Message = "Aucun mapping disponible")))
    DT::datatable(tbl, rownames = FALSE, filter = "top",
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_reading_quality <- renderUI({ HTML(iycf_reading_quality(iycf_res()) %||% "") })

  output$iycf_age_plot <- renderPlot({
    r <- iycf_res(); req(r)
    plot_iycf_age(r, input$iycf_focus)
  })
  output$iycf_age_table <- DT::renderDT({
    r <- iycf_res(); ap <- r$age_profile
    if (is.null(ap) || nrow(ap) == 0) return(DT::datatable(data.frame(Message = "Profil par âge non disponible")))
    if (!is.null(input$iycf_focus) && input$iycf_focus %in% ap$key)
      ap <- ap[ap$key == input$iycf_focus, , drop = FALSE]
    tbl <- ap[, intersect(c("Indicateur", "Groupe_age", "N", "Positifs", "Prevalence",
                            "IC_inf", "IC_sup", "N_pondere", "Methode"), names(ap)), drop = FALSE]
    DT::datatable(tbl, rownames = FALSE, options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_reading_age <- renderUI({ HTML(iycf_reading_age(iycf_res()) %||% "") })

  iycf_equity_r <- reactive({
    r <- iycf_res(); d <- df_f(); gv <- input$iycf_equity_var
    if (is.null(r) || is.null(gv) || !gv %in% names(d)) return(data.frame())
    calc_iycf_equity(r, d, gv)
  })
  output$iycf_equity_plot <- renderPlotly({
    e <- iycf_equity_r()
    if (is.null(e) || nrow(e) == 0)
      return(plotly::plotly_empty() |> plotly::layout(title = list(text = "Équité non calculable", x = 0.5)))
    plot_iycf_equity(e, input$iycf_focus)
  })
  output$iycf_equity_table <- DT::renderDT({
    e <- iycf_equity_r()
    if (is.null(e) || nrow(e) == 0) return(DT::datatable(data.frame(Message = "Équité non calculable")))
    tbl <- e[, intersect(c("Indicateur", "group", "n", "k", "pct", "ci_lo", "ci_hi",
                           "Prevalence_globale", "Ecart_pts", "p_value", "Test",
                           "Methode_test"), names(e)), drop = FALSE]
    DT::datatable(tbl, rownames = FALSE, filter = "top",
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE))
  })
  output$iycf_reading_equity <- renderUI({ HTML(iycf_reading_equity(iycf_equity_r()) %||% "") })

  iycf_child_r <- reactive({ req(iycf_res()); build_iycf_child_table(df_f(), iycf_res()) })
  output$iycf_child_table <- DT::renderDT({
    tbl <- iycf_child_r(); req(tbl)
    DT::datatable(tbl, rownames = FALSE, filter = "top",
                  options = list(dom = "ftip", pageLength = 15, scrollX = TRUE),
                  class = "compact stripe hover")
  })

  iycf_frames_r <- reactive({
    if (is.null(iycf_res())) return(list())
    iycf_export_frames(iycf_res(), iycf_verdict_r(), iycf_strat_r(),
                       iycf_rounds_r(), iycf_child_r(), iycf_equity_r())
  })
  output$iycf_dl_xlsx <- downloadHandler(
    filename = function() paste0("iycf_anje_", Sys.Date(), ".xlsx"),
    content = function(file) {
      frames <- iycf_frames_r()
      if (length(frames) == 0) { writeLines("Aucun résultat IYCF", file); return() }
      wb <- openxlsx::createWorkbook()
      for (nm in names(frames)) {
        openxlsx::addWorksheet(wb, substr(nm, 1, 31))
        openxlsx::writeData(wb, substr(nm, 1, 31), frames[[nm]])
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })
  output$iycf_dl_csv_summary <- downloadHandler(
    filename = function() paste0("synthese_iycf_", Sys.Date(), ".csv"),
    content = function(file) {
      frames <- iycf_frames_r()
      frames$Enfants <- NULL
      if (length(frames) == 0) { writeLines("Aucun résultat IYCF", file); return() }
      synth <- dplyr::bind_rows(lapply(names(frames), function(nm) {
        x <- frames[[nm]]
        if (is.null(x) || nrow(x) == 0) return(NULL)
        x$Section <- nm
        x
      }))
      utils::write.csv2(synth, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output$iycf_dl_csv_child <- downloadHandler(
    filename = function() paste0("enfants_iycf_", Sys.Date(), ".csv"),
    content = function(file) {
      tbl <- iycf_child_r()
      if (is.null(tbl)) { writeLines("Aucun résultat IYCF", file); return() }
      utils::write.csv2(tbl, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output$iycf_export_info <- renderUI({
    frames <- iycf_frames_r()
    tags$div(style = "font-size:11px;color:#666;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
      icon("circle-info"), " Contenu disponible : ",
      tags$b(paste(length(frames), "feuille(s)")), " — ",
      paste(setdiff(names(frames), "Enfants"), collapse = ", "),
      if ("Enfants" %in% names(frames))
        paste0(" + table enfants (", nrow(iycf_child_r()), " lignes)")
    )
  })

  # ── ANÉMIE ─────────────────────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$anem_hb_ui    <- renderUI(sens_select("anem_hb", "Hémoglobine (g/dL)", "hemoglobine", d))
    output$anem_alt_ui   <- renderUI(sens_select("anem_alt", "Altitude (m)", "altitude", d))
    output$anem_smoke_ui <- renderUI(sens_select("anem_smoke", "Fumeur", "fumeur", d))
    output$anem_preg_ui  <- renderUI(sens_select("anem_preg", "Enceinte", "enceinte", d))
  }) |> bindEvent(df_f())

  anem_res <- reactiveVal(NULL)

  observeEvent(input$run_anem, {
    d <- df_f(); req(d)
    g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
    result <- calc_anemia_prevalence(d, col_hb = g("anem_hb"), col_altitude = g("anem_alt"),
      col_smoker = g("anem_smoke"), col_pregnant = g("anem_preg"), population = input$anem_pop)
    anem_res(result)
    showNotification(paste0("Anémie totale: ", result$total_anemia, "% ✓"), type = "message")
  })

  output$anem_kpi <- renderUI({ req(anem_res()); HTML(render_anemia_kpi(anem_res())) })
  output$anem_summary_table <- DT::renderDT({
    req(anem_res()); DT::datatable(anem_res()$summary, options = list(dom = "t"))
  })
  output$anem_hb_dist <- renderPlot({ req(anem_res()); plot_hb_distribution(anem_res()) })
  output$anem_donut <- plotly::renderPlotly({ req(anem_res()); plot_anemia_donut(anem_res()) })

  # ── PALUDISME — module namespacé (mod_paludisme.R) ─────────────────────────
  mod_paludisme_server("paludisme", df_f = df_f)

  # ── WASH — module namespacé (mod_wash.R) ───────────────────────────────────
  mod_wash_server("wash", df_f = df_f)

  # ── MÉNAGE & PROTECTION ───────────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$hh_size_ui       <- renderUI(sens_select("hh_size", "Taille ménage", "taille_menage", d))
    output$hh_u5_ui         <- renderUI(sens_select("hh_u5", "Enfants < 5", "taille_menage_0_5", d))
    output$hh_5_17_ui       <- renderUI(sens_select("hh_5_17", "Enfants 5-17", "taille_menage_5_17", d))
    output$hh_adults_ui     <- renderUI(sens_select("hh_adults", "Adultes", "taille_menage_adultes", d))
    output$hh_head_sex_ui   <- renderUI(sens_select("hh_head_sex", "Sexe chef ménage", "chef_menage_sexe", d))
    output$hh_head_age_ui   <- renderUI(sens_select("hh_head_age", "Âge chef ménage", "chef_menage_age", d))
    output$hh_education_ui  <- renderUI(sens_select("hh_education", "Éducation chef", "education_cm", d))
    output$hh_income_ui     <- renderUI(sens_select("hh_income", "Revenus", "revenus", d))
    output$hh_income_src_ui <- renderUI(sens_select("hh_income_src", "Source revenus", "source_revenus", d))
    output$hh_expenditure_ui<- renderUI(sens_select("hh_expenditure", "Dépenses totales", "depenses", d))
    output$hh_food_exp_ui   <- renderUI(sens_select("hh_food_exp", "Dépenses alim.", "depenses_alim", d))
    output$hh_livelihood_ui <- renderUI(sens_select("hh_livelihood", "Moyens subsistance", "moyens_subsistance", d))
    output$hh_shelter_ui    <- renderUI(sens_select("hh_shelter", "Type abri", "type_abri", d))
    output$hh_tenure_ui     <- renderUI(sens_select("hh_tenure", "Statut propriété", "statut_propriete", d))
    output$hh_registration_ui  <- renderUI(sens_select("hh_registration", "Enregistrement", "enregistrement", d))
    output$hh_documentation_ui <- renderUI(sens_select("hh_documentation", "Documentation", "documentation", d))
    output$hh_incident_ui      <- renderUI(sens_select("hh_incident", "Incident protection", "incident_protection", d))
    output$hh_child_labor_ui   <- renderUI(sens_select("hh_child_labor", "Travail enfant", "travail_enfant", d))
    output$hh_marriage_ui      <- renderUI(sens_select("hh_marriage", "Mariage précoce", "mariage_precoce", d))
  }) |> bindEvent(df_f())

  hh_profile_res <- reactiveVal(NULL); hh_econ_res <- reactiveVal(NULL)
  hh_shelter_res <- reactiveVal(NULL); hh_prot_res <- reactiveVal(NULL)

  observeEvent(input$run_menage, {
    d <- df_f(); req(d)
    g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
    withProgress(message = "Analyse ménage...", {
      hh_profile_res(calc_household_profile(d, g("hh_size"), g("hh_u5"), g("hh_5_17"), g("hh_adults"),
        g("hh_head_sex"), g("hh_head_age"), g("hh_education")))
      hh_econ_res(calc_household_economy(d, g("hh_income"), g("hh_income_src"),
        g("hh_expenditure"), g("hh_food_exp"), g("hh_livelihood")))
      hh_shelter_res(calc_shelter(d, g("hh_shelter"), g("hh_tenure")))
      hh_prot_res(calc_protection(d, g("hh_registration"), g("hh_documentation"),
        g("hh_incident"), g("hh_child_labor"), g("hh_marriage")))
    })
    showNotification("Analyse ménage terminée ✓", type = "message")
  })

  output$hh_kpi <- renderUI({ req(hh_profile_res()); HTML(render_household_kpi(hh_profile_res(), hh_econ_res())) })
  output$hh_composition_table <- DT::renderDT({
    req(hh_profile_res()); DT::datatable(hh_profile_res()$composition, options = list(dom = "t"))
  })
  output$hh_income_plot <- renderPlot({ req(hh_econ_res()); plot_income_expenditure(hh_econ_res()) })
  output$hh_livelihood_table <- DT::renderDT({
    req(hh_econ_res()); r <- hh_econ_res()$results
    if (!is.null(r$livelihood)) DT::datatable(r$livelihood, options = list(pageLength = 10))
  })
  output$hh_shelter_table <- DT::renderDT({
    req(hh_shelter_res()); r <- hh_shelter_res()$results
    if (!is.null(r$shelter_types)) DT::datatable(r$shelter_types, options = list(dom = "t"))
  })
  output$hh_tenure_table <- DT::renderDT({
    req(hh_shelter_res()); r <- hh_shelter_res()$results
    if (!is.null(r$tenure_status)) DT::datatable(r$tenure_status, options = list(dom = "t"))
  })
  output$hh_protection_plot <- renderPlot({ req(hh_prot_res()); plot_protection_indicators(hh_prot_res()) })
  output$hh_protection_table <- DT::renderDT({
    req(hh_prot_res()); DT::datatable(hh_prot_res()$summary, options = list(dom = "t"))
  })

  # ── HANDICAP (WASHINGTON GROUP) ────────────────────────────────────────────
  observe({
    d <- df_f(); req(d)
    output$wg_voir_ui     <- renderUI(sens_select("wg_voir", "Vision", "handicap_voir", d))
    output$wg_entendre_ui <- renderUI(sens_select("wg_entendre", "Audition", "handicap_entendre", d))
    output$wg_marcher_ui  <- renderUI(sens_select("wg_marcher", "Mobilité", "handicap_marcher", d))
    output$wg_memoire_ui  <- renderUI(sens_select("wg_memoire", "Cognition", "handicap_memoire", d))
    output$wg_soins_ui    <- renderUI(sens_select("wg_soins", "Soins personnels", "handicap_soins_personnels", d))
    output$wg_comm_ui     <- renderUI(sens_select("wg_comm", "Communication", "handicap_communication", d))
  }) |> bindEvent(df_f())

  wg_res <- reactiveVal(NULL)

  observeEvent(input$run_wg, {
    d <- df_f(); req(d)
    g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
    result <- calc_wg_prevalence(d, g("wg_voir"), g("wg_entendre"), g("wg_marcher"),
      g("wg_memoire"), g("wg_soins"), g("wg_comm"), threshold = input$wg_threshold)
    wg_res(result)
    showNotification(paste0("Handicap: ", result$prevalence, "% ✓"), type = "message")
  })

  output$wg_kpi <- renderUI({ req(wg_res()); HTML(render_wg_kpi(wg_res())) })
  output$wg_domain_table <- DT::renderDT({
    req(wg_res()); DT::datatable(wg_res()$domain_summary, options = list(dom = "t"))
  })
  output$wg_domain_plot <- renderPlot({ req(wg_res()); plot_wg_radar(wg_res()) })
  output$wg_severity_plot <- renderPlot({ req(wg_res()); plot_wg_severity(wg_res()) })

