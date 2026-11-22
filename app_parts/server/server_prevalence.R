# Extrait de app_v2_final.R — prévalences SMART pondérées
  # ══════════════════════════════════════════════════════════════════
  # PRÉVALENCES SMART — Pondérées + DEFF + IC 95%
  # ══════════════════════════════════════════════════════════════════

  # UI dynamiques pour le plan de sondage
  output$sp_col_cluster_ui <- renderUI({
    all_v <- c(vars_cat(), vars_num())
    d <- df_f()
    guess <- guess_col(d, c("^cluster","^grappe","^psu","^ea$","^zd$"), type = "cat")
    sel <- guess %||% ""
    selectInput("sp_col_cluster", "Cluster / Grappe (PSU) :",
                choices = c("— Aucun —" = "", all_v), selected = sel, width = "100%")
  })
  output$sp_col_strata_ui <- renderUI({
    all_v <- c(vars_cat(), vars_num())
    d <- df_f()
    guess <- guess_col(d, c("^strat","^region","^département","^zone"), type = "cat")
    sel <- guess %||% ""
    selectInput("sp_col_strata", "Strate :",
                choices = c("— Aucune —" = "", all_v), selected = sel, width = "100%")
  })
  output$sp_col_weights_ui <- renderUI({
    vn <- vars_num()
    d <- df_f()
    guess <- guess_col(d, c("^weight$","^poids_sond","^wt$","^sampling.*weight","^ponder"))
    sel <- guess %||% ""
    selectInput("sp_col_weights", "Poids d'échantillonnage :",
                choices = c("— Aucun (poids = 1) —" = "", vn), selected = sel, width = "100%")
  })

  # UI : Indicateurs à calculer (auto-détection des colonnes binaires OMS)
  output$sp_indicators_ui <- renderUI({
    d <- df_f()
    # Chercher les colonnes binaires (0/1) qui ressemblent à des indicateurs
    binary_cols <- names(d)[vapply(names(d), function(v) {
      vals <- na.omit(d[[v]])
      is.numeric(vals) && all(vals %in% c(0, 1)) && length(vals) > 10
    }, logical(1))]
    # Prioriser les noms connus
    known <- c("GAM", "SAM", "MAM", "Stunting", "Underweight", "MUAC_GAM",
               "gam", "sam", "mam", "stunting", "underweight")
    selected <- intersect(known, binary_cols)
    if (length(selected) == 0) selected <- head(binary_cols, 5)

    checkboxGroupInput("sp_indicators", "Indicateurs binaires (0/1) :",
                       choices = binary_cols, selected = selected, width = "100%")
  })

  # UI : Stratifications
  output$sp_stratifications_ui <- renderUI({
    d <- df_f()
    vc <- vars_cat()
    # Suggestions intelligentes
    known_strats <- c("Sexe", "sexe", "sex", "Sex", "Groupe_age", "age_group",
                      "Region", "region", "Strate", "strate", "Deplacement",
                      "deplacement", "displacement")
    suggested <- intersect(known_strats, vc)
    if (length(suggested) == 0) suggested <- head(vc, 3)

    checkboxGroupInput("sp_stratifications", "Stratifier par :",
                       choices = vc, selected = suggested, width = "100%")
  })

  # Stockage des résultats
  sp_results <- reactiveVal(NULL)

  # Lancer le calcul
  observeEvent(input$run_prevalence, {
    d <- df_f()
    req(nrow(d) > 0, input$sp_indicators)

    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    col_cluster <- nullify(input$sp_col_cluster)
    col_strata  <- nullify(input$sp_col_strata)
    col_weights <- nullify(input$sp_col_weights)

    # Construire la liste d'indicateurs
    indicators <- setNames(
      as.list(input$sp_indicators),
      input$sp_indicators
    )

    # Construire la liste de stratifications
    stratifications <- if (!is.null(input$sp_stratifications) && length(input$sp_stratifications) > 0) {
      setNames(as.list(input$sp_stratifications), input$sp_stratifications)
    } else NULL

    withProgress(message = "Calcul des prévalences SMART...", value = 0, {
      incProgress(0.2, detail = "Création du design de sondage...")
      res <- tryCatch(
        run_smart_prevalences(d, indicators, col_cluster, col_strata,
                               col_weights, stratifications),
        error = function(e) list(error = e$message)
      )
      incProgress(0.8, detail = "Terminé ✅")
    })

    sp_results(res)
  })

  # ── Rendu : Info design ──
  output$sp_design_info <- renderUI({
    res <- sp_results(); req(!is.null(res), is.null(res$error))
    render_design_info_html(res$design_info, res$method)
  })

  # ── Rendu : Tableau global HTML ──
  output$sp_global_table_html <- renderUI({
    res <- sp_results(); req(!is.null(res), !is.null(res$global))
    if (!is.null(res$error)) return(tags$p(style="color:#e74c3c;", res$error))
    render_smart_table_html(res$global)
  })

  # ── Rendu : Forest plot global ──
  output$sp_forest_global <- renderPlotly({
    res <- sp_results(); req(!is.null(res), !is.null(res$global), nrow(res$global) > 0)
    plot_prevalence_forest(res$global, "Prévalences SMART globales — IC 95%")
  })

  # ── Rendu : Tableau par strate ──
  output$sp_strat_table <- renderDT({
    res <- sp_results(); req(!is.null(res), !is.null(res$strat), nrow(res$strat) > 0)
    datatable(res$strat, rownames = FALSE,
              options = list(dom = "ftip", pageLength = 20, scrollX = TRUE),
              class = "compact stripe hover") |>
      formatStyle("Prévalence (%)",
                  background = styleColorBar(c(0, max(res$strat$`Prévalence (%)`, 30)), "#e74c3c"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatRound(c("Prévalence (%)", "IC bas", "IC haut", "DEFF"), digits = 2)
  })

  # ── Rendu : Forest plot par strate ──
  output$sp_forest_strat <- renderPlotly({
    res <- sp_results(); req(!is.null(res), !is.null(res$strat), nrow(res$strat) > 0)
    plot_prevalence_forest(res$strat, "Prévalences par strate — IC 95%")
  })

  # ── Rendu : Tableau complet (toutes données combinées) ──
  output$sp_full_table <- renderDT({
    res <- sp_results(); req(!is.null(res))
    # Combiner global + strat
    all_data <- bind_rows(
      if (!is.null(res$global)) res$global |> mutate(Strate = "Global", Groupe = "Tous"),
      res$strat
    )
    req(nrow(all_data) > 0)
    datatable(all_data, rownames = FALSE,
              options = list(dom = "ftip", pageLength = 50, scrollX = TRUE),
              class = "compact stripe hover",
              filter = "top") |>
      formatRound(c("Prévalence (%)", "IC bas", "IC haut", "DEFF"), digits = 2)
  })

  # ── Download CSV ──
  output$dl_smart_table <- downloadHandler(
    filename = function() paste0("prevalences_smart_", Sys.Date(), ".csv"),
    content = function(file) {
      res <- sp_results(); req(!is.null(res))
      all_data <- bind_rows(
        if (!is.null(res$global)) res$global |> mutate(Strate = "Global", Groupe = "Tous"),
        res$strat
      )
      write.csv(all_data, file, row.names = FALSE)
    }
  )

  # ── Export ENA/SMART ──
  output$dl_smart_ena <- downloadHandler(
    filename = function() paste0("SMART_export_", Sys.Date(), ".csv"),
    content  = function(file) {
      res <- sp_results(); req(!is.null(res))
      out <- tryCatch(
        format_smart_export(res$global, res$strat, res$design_info, res$method),
        error = function(e) res$global
      )
      write.csv(out, file, row.names = FALSE)
    }
  )

