# Module Shiny — Handicap (Washington Group Short Set, 6 domaines)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.
# Niveau expert : IC Wilson / design-based, strates, qualité, exports.

mod_handicap_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "handicap",
    fluidRow(box(width = 12, status = "warning", solidHeader = TRUE,
      title = tags$span(icon("wheelchair"), " Handicap — Washington Group Short Set (6 domaines)"),
      tags$div(style = "background:#fef9e7;border-left:4px solid #e67e22;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Domaines :"), " Vision, Audition, Mobilité, Cognition, Soins personnels, Communication.",
        tags$br(), "Seuil OMS : ≥ 'Beaucoup de difficultés' dans au moins 1 domaine.",
        tags$br(), "Expert : IC 95 % Wilson ou design-based, stratification, qualité et exports.")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Colonnes WG", status = "warning", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("wg_voir_ui")), uiOutput(ns("wg_entendre_ui")), uiOutput(ns("wg_marcher_ui")),
          uiOutput(ns("wg_memoire_ui")), uiOutput(ns("wg_soins_ui")), uiOutput(ns("wg_comm_ui")),
          sliderInput(ns("wg_threshold"), "Seuil de sévérité", min = 2, max = 4, value = 3, step = 1),
          tags$hr(style = "margin:8px 0;border-color:#fdebd0;"),
          uiOutput(ns("wg_strat_ui")),
          expert_design_controls_ui(ns, "wg"),
          actionButton(ns("run_wg"), "♿  Calculer", class = "btn-warning btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("wg_tabs"),
          tabPanel("Synthèse", uiOutput(ns("wg_kpi")), br(), DT::DTOutput(ns("wg_domain_table"))),
          tabPanel("Par domaine", plotOutput(ns("wg_domain_plot"), height = "450px")),
          tabPanel("Sévérité", plotOutput(ns("wg_severity_plot"), height = "500px")),
          tabPanel("Prévalences & IC",
            uiOutput(ns("wg_expert_note")), br(),
            DT::DTOutput(ns("wg_expert_table")), br(),
            plotly::plotlyOutput(ns("wg_expert_plot"), height = "380px"), br(),
            DT::DTOutput(ns("wg_test_table"))),
          tabPanel("Qualité",
            uiOutput(ns("wg_quality_note")), br(),
            DT::DTOutput(ns("wg_quality_table"))),
          tabPanel("Export", expert_exports_ui(ns, "wg"))
        )
      )
    )
  )
}

mod_handicap_server <- function(id, df_f) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      d <- df_f(); req(d)
      output$wg_voir_ui     <- renderUI(sens_select_ns(ns, "wg_voir", "Vision", "handicap_voir", d))
      output$wg_entendre_ui <- renderUI(sens_select_ns(ns, "wg_entendre", "Audition", "handicap_entendre", d))
      output$wg_marcher_ui  <- renderUI(sens_select_ns(ns, "wg_marcher", "Mobilité", "handicap_marcher", d))
      output$wg_memoire_ui  <- renderUI(sens_select_ns(ns, "wg_memoire", "Cognition", "handicap_memoire", d))
      output$wg_soins_ui    <- renderUI(sens_select_ns(ns, "wg_soins", "Soins personnels", "handicap_soins_personnels", d))
      output$wg_comm_ui     <- renderUI(sens_select_ns(ns, "wg_comm", "Communication", "handicap_communication", d))
      cats <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      cats <- setdiff(cats, c("household_id", "child_id", "id_menage"))
      sel <- intersect(c("region", "province", "departement", "dept", "sexe",
                         "milieu", "strate", "survey_round"), cats)
      selectInput(ns("wg_strat"), "Stratification (optionnelle)",
                  choices = c("— Aucune —" = "", stats::setNames(cats, cats)),
                  selected = if (length(sel) > 0) sel[1] else "")
    }) |> bindEvent(df_f())

    expert_design_controls_server(input, output, session, "wg", df_f)

    wg_res <- reactiveVal(NULL)
    wg_expert <- reactiveVal(NULL); wg_quality <- reactiveVal(NULL)
    wg_tests <- reactiveVal(NULL); wg_method <- reactiveVal("Wilson")

    observeEvent(input$run_wg, {
      d <- df_f(); req(d)
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      result <- calc_wg_prevalence(d, g("wg_voir"), g("wg_entendre"), g("wg_marcher"),
        g("wg_memoire"), g("wg_soins"), g("wg_comm"), threshold = input$wg_threshold)
      wg_res(result)

      doms <- c(Vision = g("wg_voir"), Audition = g("wg_entendre"),
                Mobilité = g("wg_marcher"), Cognition = g("wg_memoire"),
                `Soins personnels` = g("wg_soins"), Communication = g("wg_comm"))
      doms <- doms[!vapply(doms, is.null, logical(1))]
      d2 <- as.data.frame(d)
      mat <- NULL
      for (nm in names(doms)) {
        v <- suppressWarnings(as.numeric(d[[doms[[nm]]]]))
        d2[[paste0(".wg_", nm)]] <- as.integer(!is.na(v) & v >= input$wg_threshold)
        mat <- cbind(mat, v)
      }
      if (!is.null(mat)) {
        mx <- apply(mat, 1, function(z) if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE))
        d2$.wg_any <- as.integer(!is.na(mx) & mx >= input$wg_threshold)
      }
      vars <- c()
      if (".wg_any" %in% names(d2)) vars <- c(vars, `Handicap (≥ 1 domaine)` = ".wg_any")
      for (nm in names(doms)) vars <- c(vars, stats::setNames(paste0(".wg_", nm), nm))

      da <- expert_design_args(input, "wg")
      strat <- g("wg_strat")
      method <- if (!is.null(da$cluster) || !is.null(da$strata) || !is.null(da$weights)) "Design-based" else "Wilson"
      if (length(vars) == 0) {
        wg_expert(NULL); wg_tests(NULL)
      } else if (is.null(strat)) {
        wg_expert(expert_prev_multi(d2, vars, cluster = da$cluster,
                                    strata = da$strata, weights = da$weights))
        wg_tests(NULL)
      } else {
        wg_expert(purrr::map_dfr(seq_along(vars), function(i)
          expert_prev_by(d2, vars[[i]], strat, names(vars)[i], da$cluster, da$strata, da$weights)))
        tt <- purrr::map_dfr(seq_along(vars), function(i) expert_strat_test(d2, vars[[i]], strat))
        if (nrow(tt) > 0) tt$Indicateur <- names(vars)
        wg_tests(tt)
      }
      wg_method(method)
      wg_quality(expert_quality(d, unique(doms)))
      showNotification(paste0("Handicap: ", result$prevalence, "% ✓"), type = "message")
    })

    output$wg_kpi <- renderUI({ req(wg_res()); HTML(render_wg_kpi(wg_res())) })
    output$wg_domain_table <- DT::renderDT({
      req(wg_res()); DT::datatable(wg_res()$domain_summary, options = list(dom = "t"))
    })
    output$wg_domain_plot <- renderPlot({ req(wg_res()); plot_wg_radar(wg_res()) })
    output$wg_severity_plot <- renderPlot({ req(wg_res()); plot_wg_severity(wg_res()) })

    output$wg_expert_note <- renderUI({
      req(wg_expert())
      tags$div(style = "font-size:12px;color:#555;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
        icon("circle-info"), " Méthode : ", tags$b(wg_method()),
        if (!is.null(input$wg_strat) && nzchar(input$wg_strat))
          paste0(" | strate : ", input$wg_strat) else " | global",
        " | seuil : ", input$wg_threshold, " (", WG_SEVERITY$labels[input$wg_threshold], ").")
    })
    output$wg_expert_table <- DT::renderDT({ req(wg_expert()); expert_dt(wg_expert()) })
    output$wg_expert_plot <- plotly::renderPlotly({
      req(wg_expert()); p <- expert_prev_plot(wg_expert(), "Handicap — prévalences et IC 95 %")
      if (is.null(p)) plotly::plotly_empty() else p
    })
    output$wg_test_table <- DT::renderDT({
      req(wg_tests()); if (is.null(wg_tests()) || nrow(wg_tests()) == 0) return(NULL)
      expert_dt(wg_tests()[c("Indicateur", "Test", "Statistique", "ddl", "p", "V de Cramér", "N")])
    })
    output$wg_quality_note <- renderUI({
      tags$div(style = "font-size:12px;color:#555;",
        icon("shield-halved"), " Distribution des scores WG et complétude des 6 domaines.")
    })
    output$wg_quality_table <- DT::renderDT({ req(wg_quality()); expert_dt(wg_quality()) })

    expert_export_server(input, output, session, "wg", reactive({
      list(`Domaines` = wg_res()$domain_summary, `Prévalences IC` = wg_expert(),
           `Tests` = wg_tests(), `Qualité` = wg_quality())
    }))
  })
}
