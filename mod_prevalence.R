# Module Shiny — Prévalences SMART pondérées (design-based)
# UI et serveur namespacés (ns). Les réactifs partagés sont passés en arguments.

mod_prevalence_ui <- function(id) {
  ns <- shiny::NS(id)

      tabItem(tabName="prevalence",
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=tags$span(icon("chart-line"),
                " Prévalences SMART — Pondérées · DEFF · IC 95% design-based"),
              tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;margin-bottom:12px;font-size:12px;",
                tags$b("Méthodologie SMART"), " — Calcul des prévalences de malnutrition ",
                "avec prise en compte du plan de sondage (clusters, strates, poids). ",
                "L'effet de grappe (DEFF) et les IC 95% sont calculés selon la méthodologie ",
                "recommandée par l'OMS, l'UNICEF et le PAM. ",
                tags$span(style="color:#e67e22;font-weight:600;",
                  if (!HAS_SURVEY) "⚠ Package 'survey' non installé — fallback avec DEFF approximatif. Installer avec install.packages('survey')" else "✅ Package 'survey' détecté — calculs design-based exacts")
              )
          )
        ),
        fluidRow(
          box(width=3, status="primary", solidHeader=TRUE,
              title="Plan de sondage",
              tags$div(class="section-header","Design"),
              uiOutput(ns("sp_col_cluster_ui")),
              uiOutput(ns("sp_col_strata_ui")),
              uiOutput(ns("sp_col_weights_ui")),
              hr(),
              tags$div(class="section-header","Indicateurs à calculer"),
              uiOutput(ns("sp_indicators_ui")),
              hr(),
              tags$div(class="section-header","Stratifications"),
              uiOutput(ns("sp_stratifications_ui")),
              hr(),
              actionButton(ns("run_prevalence"), "📈 Calculer les prévalences",
                           class="btn btn-oms btn-block", width="100%",
                           style="font-size:14px;padding:10px;margin-top:8px;")
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Résultats — Prévalences SMART",
              tabsetPanel(
                tabPanel(tags$span(icon("globe"), " Prévalences globales"),
                  uiOutput(ns("sp_design_info")),
                  br(),
                  uiOutput(ns("sp_global_table_html")),
                  br(),
                  plotlyOutput(ns("sp_forest_global"), height="350px")
                ),
                tabPanel(tags$span(icon("layer-group"), " Par strate"),
                  DTOutput(ns("sp_strat_table")),
                  br(),
                  plotlyOutput(ns("sp_forest_strat"), height="450px")
                ),
                tabPanel(tags$span(icon("table"), " Tableau SMART standard"),
                  tags$div(style="margin-bottom:10px;",
                    downloadButton(ns("dl_smart_table"), "📥 Télécharger (CSV)",
                      style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:6px 14px;"),
                    downloadButton(ns("dl_smart_ena"), "📥 Export ENA/SMART",
                      style="background:#27AE60;color:white;border:none;border-radius:6px;padding:6px 14px;margin-left:8px;")
                  ),
                  DTOutput(ns("sp_full_table"))
                ),

                # ── NOUVEAU : Moyennes SA design-based ──
                tabPanel(tags$span(icon("chart-line"), " Moyennes SA (IC design)"),
                  br(),
                  tags$div(
                    style = "background:#f0fff4;border-left:4px solid #27AE60;padding:10px 14px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:12px;",
                    icon("info-circle", style = "color:#27AE60;"),
                    " Moyennes de FCS, rCSI, HDDS et HHS avec IC 95% ajustés pour le plan de sondage complexe (grappes + strates). ",
                    "Ces estimations sont plus précises qu'un simple IC basé sur l'hypothèse SRS."
                  ),
                  fluidRow(
                    column(4,
                      uiOutput(ns("sp_means_sa_cols_ui")),
                      hr(),
                      uiOutput(ns("sp_means_group_ui"))
                    ),
                    column(8, plotlyOutput(ns("sp_means_forest"), height = "360px"))
                  ),
                  br(),
                  DTOutput(ns("sp_means_table"))
                ),

                # ── NOUVEAU : Comparaison de designs ──
                tabPanel(tags$span(icon("balance-scale"), " Comparaison Designs"),
                  br(),
                  tags$div(
                    style = "background:#fff8e1;border-left:4px solid #FBC02D;padding:10px 14px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:12px;",
                    icon("exclamation-triangle", style = "color:#FBC02D;"),
                    " Compare les IC 95% et DEFF obtenus avec 3 designs : ",
                    tags$b("Stratifié + Grappes"), " (recommandé SMART), ",
                    tags$b("Grappes seules"), ", et ",
                    tags$b("SRS naïf"), " (ignore la structure de sondage). ",
                    "Un DEFF > 1 signifie que les IC naïfs sous-estiment l'incertitude réelle."
                  ),
                  fluidRow(
                    column(4,
                      checkboxGroupInput(ns("sp_cmp_inds"), "Indicateurs à comparer :",
                        choices = c("GAM" = "GAM", "SAM" = "SAM",
                                    "MAG MUAC" = "MAG_muac",
                                    "Stunting" = "Stunting",
                                    "Underweight" = "Underweight"),
                        selected = c("GAM", "MAG_muac"), width = "100%")
                    ),
                    column(8, plotlyOutput(ns("sp_cmp_forest"), height = "380px"))
                  ),
                  br(),
                  DTOutput(ns("sp_cmp_table"))
                ),

                # ── NOUVEAU : ICC et effets de sondage ──
                tabPanel(tags$span(icon("project-diagram"), " ICC & DEFF"),
                  br(),
                  tags$div(
                    style = "background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;font-size:11px;margin-bottom:12px;",
                    icon("info-circle"),
                    " L'ICC (Corrélation Intra-Classe) mesure la ressemblance entre enfants d'une même grappe. ",
                    "DEFF = 1 + (m̄ − 1) × ICC. Un ICC élevé implique un DEFF élevé et donc des IC plus larges.",
                    tags$b(" DEFF > 2 : IC doublé par rapport au SRS.")
                  ),
                  DTOutput(ns("sp_icc_table")),
                  br(),
                  plotlyOutput(ns("sp_deff_bar"), height = "300px")
                ),

                # ── NOUVEAU : Diagnostics du plan ──
                tabPanel(tags$span(icon("stethoscope"), " Diagnostics"),
                  br(),
                  uiOutput(ns("sp_diag_alerts")),
                  br(),
                  fluidRow(
                    column(6, plotlyOutput(ns("sp_cluster_sizes_hist"), height = "280px")),
                    column(6, DTOutput(ns("sp_cluster_sizes_table")))
                  )
                )
              )
          )
        )
      )
}

mod_prevalence_server <- function(id, df_f, vars_cat, vars_num) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # ══════════════════════════════════════════════════════════════════
  # PRÉVALENCES SMART — Pondérées + DEFF + IC 95%
  # ══════════════════════════════════════════════════════════════════

  # UI dynamiques pour le plan de sondage
  output$sp_col_cluster_ui <- renderUI({
    all_v <- c(vars_cat(), vars_num())
    d <- df_f()
    guess <- guess_col(d, c("^cluster","^grappe","^psu","^ea$","^zd$"), type = "cat")
    sel <- guess %||% ""
    selectInput(ns("sp_col_cluster"), "Cluster / Grappe (PSU) :",
                choices = c("— Aucun —" = "", all_v), selected = sel, width = "100%")
  })
  output$sp_col_strata_ui <- renderUI({
    all_v <- c(vars_cat(), vars_num())
    d <- df_f()
    guess <- guess_col(d, c("^strat","^region","^département","^zone"), type = "cat")
    sel <- guess %||% ""
    selectInput(ns("sp_col_strata"), "Strate :",
                choices = c("— Aucune —" = "", all_v), selected = sel, width = "100%")
  })
  output$sp_col_weights_ui <- renderUI({
    vn <- vars_num()
    d <- df_f()
    guess <- guess_col(d, c("^weight$","^poids_sond","^wt$","^sampling.*weight","^ponder"))
    sel <- guess %||% ""
    selectInput(ns("sp_col_weights"), "Poids d'échantillonnage :",
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

    checkboxGroupInput(ns("sp_indicators"), "Indicateurs binaires (0/1) :",
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

    checkboxGroupInput(ns("sp_stratifications"), "Stratifier par :",
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
    checkboxGroupInput(ns("sp_means_sa_cols"), "Indicateurs continus :",
      choices  = setNames(cont_cols, cont_cols),
      selected = na.omit(defaults),
      width    = "100%")
  })

  output$sp_means_group_ui <- renderUI({
    d <- df_f(); req(d)
    cat_cols <- names(d)[sapply(names(d), function(v) is.character(d[[v]]) || is.factor(d[[v]]))]
    choices <- c("Global" = "", setNames(cat_cols, cat_cols))
    selectInput(ns("sp_means_group_var"), "Grouper par :",
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
      layout(
        # add_vline n'est pas exporté par plotly 4.12 : on utilise une forme.
        shapes = list(list(
          type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#888", dash = "dash", width = 1)
        )),
        annotations = list(list(
          x = 1, y = 1, yref = "paper", text = "DEFF = 1 (SRS)",
          showarrow = FALSE, xanchor = "left",
          font = list(size = 10, color = "#888")
        )),
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

  })
}
