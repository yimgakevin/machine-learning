# Module Shiny — Paludisme & Moustiquaires SENS (LLIN / TDR / ACT)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.
# Niveau expert : IC Wilson / design-based, strates, qualité, exports.

mod_paludisme_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "paludisme",
    fluidRow(box(width = 12, status = "danger", solidHeader = TRUE,
      title = tags$span(icon("bug"), " Paludisme & Moustiquaires SENS (Roll Back Malaria / OMS)"),
      tags$div(style = "background:#fdf2f2;border-left:4px solid #c0392b;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Indicateurs :"), " Possession / Accès / Utilisation LLIN, Prévalence TDR+, Traitement ACT.",
        tags$br(), "Expert : IC 95 % Wilson ou design-based (cluster / strates / poids), stratification, qualité et exports.")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Configuration", status = "danger", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("palu_poss_ui")), uiOutput(ns("palu_use_ui")), uiOutput(ns("palu_access_ui")),
          uiOutput(ns("palu_nb_nets_ui")), uiOutput(ns("palu_hh_size_ui")),
          uiOutput(ns("palu_tdr_ui")), uiOutput(ns("palu_fever_ui")), uiOutput(ns("palu_strat_ui")),
          tags$hr(style = "margin:8px 0;border-color:#fadbd8;"),
          expert_design_controls_ui(ns, "palu"),
          actionButton(ns("run_palu"), "🦟  Calculer", class = "btn-danger btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("palu_tabs"),
          tabPanel("Synthèse", uiOutput(ns("palu_kpi"))),
          tabPanel("Cascade LLIN", plotOutput(ns("palu_cascade_plot"), height = "400px")),
          tabPanel("TDR par groupe", DT::DTOutput(ns("palu_by_fever")), br(), DT::DTOutput(ns("palu_by_group"))),
          tabPanel("Prévalences & IC",
            uiOutput(ns("palu_expert_note")), br(),
            DT::DTOutput(ns("palu_expert_table")), br(),
            plotly::plotlyOutput(ns("palu_expert_plot"), height = "380px"), br(),
            DT::DTOutput(ns("palu_test_table"))),
          tabPanel("Qualité",
            uiOutput(ns("palu_quality_note")), br(),
            DT::DTOutput(ns("palu_quality_table"))),
          tabPanel("Export", expert_exports_ui(ns, "palu"))
        )
      )
    )
  )
}

mod_paludisme_server <- function(id, df_f) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      d <- df_f(); req(d)
      output$palu_poss_ui    <- renderUI(sens_select_ns(ns, "palu_poss", "Possession LLIN", "llin_possession", d))
      output$palu_use_ui     <- renderUI(sens_select_ns(ns, "palu_use", "Utilisation LLIN", "llin_utilisation", d))
      output$palu_access_ui  <- renderUI(sens_select_ns(ns, "palu_access", "Accès LLIN", "llin_acces", d))
      output$palu_nb_nets_ui <- renderUI(sens_select_ns(ns, "palu_nb_nets", "Nb moustiquaires", "nb_moustiquaires", d))
      output$palu_hh_size_ui <- renderUI(sens_select_ns(ns, "palu_hh_size", "Taille ménage", "taille_menage", d))
      output$palu_tdr_ui     <- renderUI(sens_select_ns(ns, "palu_tdr", "Résultat TDR", "tdr_malaria", d))
      output$palu_fever_ui   <- renderUI(sens_select_ns(ns, "palu_fever", "Fièvre", "fievre", d))
      output$palu_strat_ui   <- renderUI(sens_select_ns(ns, "palu_strat", "Stratification", "region", d))
    }) |> bindEvent(df_f())

    expert_design_controls_server(input, output, session, "palu", df_f)

    palu_llin <- reactiveVal(NULL); palu_malaria <- reactiveVal(NULL)
    palu_expert <- reactiveVal(NULL); palu_quality <- reactiveVal(NULL)
    palu_tests <- reactiveVal(NULL); palu_method <- reactiveVal("Wilson")

    observeEvent(input$run_palu, {
      d <- df_f(); req(d)
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      withProgress(message = "Calcul paludisme...", {
        llin <- calc_llin_indicators(d, g("palu_poss"), g("palu_use"), g("palu_access"),
                                     g("palu_nb_nets"), g("palu_hh_size"))
        palu_llin(llin)
        if (!is.null(g("palu_tdr"))) palu_malaria(calc_malaria_prevalence(d, g("palu_tdr"), g("palu_fever"), g("palu_strat")))

        d2 <- as.data.frame(d)
        vars <- c()
        if (!is.null(g("palu_poss")))   vars <- c(vars, `Possession LLIN` = g("palu_poss"))
        if (!is.null(g("palu_access"))) vars <- c(vars, `Accès LLIN` = g("palu_access"))
        if (!is.null(g("palu_use")))    vars <- c(vars, `Utilisation LLIN` = g("palu_use"))
        if (!is.null(g("palu_tdr")))    vars <- c(vars, `TDR positif` = g("palu_tdr"))
        if (!is.null(g("palu_nb_nets")) && !is.null(g("palu_hh_size"))) {
          nets <- suppressWarnings(as.numeric(d[[g("palu_nb_nets")]]))
          hh <- suppressWarnings(as.numeric(d[[g("palu_hh_size")]]))
          d2$.ratio_ok <- as.integer(!is.na(nets) & !is.na(hh) & hh > 0 & (nets / hh) >= 0.5)
          vars <- c(vars, `≥ 1 LLIN / 2 personnes` = ".ratio_ok")
        }

        da <- expert_design_args(input, "palu")
        strat <- g("palu_strat")
        method <- if (!is.null(da$cluster) || !is.null(da$strata) || !is.null(da$weights)) "Design-based" else "Wilson"
        if (length(vars) == 0) {
          palu_expert(NULL); palu_tests(NULL)
        } else if (is.null(strat)) {
          palu_expert(expert_prev_multi(d2, vars, cluster = da$cluster,
                                        strata = da$strata, weights = da$weights))
          palu_tests(NULL)
        } else {
          palu_expert(purrr::map_dfr(seq_along(vars), function(i)
            expert_prev_by(d2, vars[[i]], strat, names(vars)[i], da$cluster, da$strata, da$weights)))
          tt <- purrr::map_dfr(seq_along(vars), function(i) expert_strat_test(d2, vars[[i]], strat))
          if (nrow(tt) > 0) tt$Indicateur <- names(vars)
          palu_tests(tt)
        }
        palu_method(method)
        palu_quality(expert_quality(d, unique(c(g("palu_poss"), g("palu_use"),
          g("palu_access"), g("palu_tdr"), g("palu_nb_nets"), g("palu_hh_size")))))
      })
      showNotification("Paludisme calculé ✓", type = "message")
    })

    output$palu_kpi <- renderUI({ HTML(render_malaria_kpi(palu_malaria(), palu_llin())) })
    output$palu_cascade_plot <- renderPlot({ req(palu_llin()); plot_llin_cascade(palu_llin()) })
    output$palu_by_fever <- DT::renderDT({
      req(palu_malaria()); if (!is.null(palu_malaria()$by_fever)) DT::datatable(palu_malaria()$by_fever, options = list(dom = "t"))
    })
    output$palu_by_group <- DT::renderDT({
      req(palu_malaria()); if (!is.null(palu_malaria()$by_group)) DT::datatable(palu_malaria()$by_group, options = list(dom = "t"))
    })

    output$palu_expert_note <- renderUI({
      req(palu_expert())
      tags$div(style = "font-size:12px;color:#555;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
        icon("circle-info"), " Méthode : ", tags$b(palu_method()),
        if (!is.null(input$palu_strat) && nzchar(input$palu_strat))
          paste0(" | strate : ", input$palu_strat) else " | global",
        ". DEFF calculé quand le plan de sondage est actif.")
    })
    output$palu_expert_table <- DT::renderDT({ req(palu_expert()); expert_dt(palu_expert()) })
    output$palu_expert_plot <- plotly::renderPlotly({
      req(palu_expert()); p <- expert_prev_plot(palu_expert(), "Paludisme — prévalences et IC 95 %")
      if (is.null(p)) plotly::plotly_empty() else p
    })
    output$palu_test_table <- DT::renderDT({
      req(palu_tests()); if (is.null(palu_tests()) || nrow(palu_tests()) == 0) return(NULL)
      expert_dt(palu_tests()[c("Indicateur", "Test", "Statistique", "ddl", "p", "V de Cramér", "N")])
    })
    output$palu_quality_note <- renderUI({
      tags$div(style = "font-size:12px;color:#555;",
        icon("shield-halved"), " Complétude et plausibilité des colonnes LLIN / TDR.")
    })
    output$palu_quality_table <- DT::renderDT({ req(palu_quality()); expert_dt(palu_quality()) })

    expert_export_server(input, output, session, "palu", reactive({
      list(`Cascade LLIN` = palu_llin()$cascade, `Prévalences IC` = palu_expert(),
           `Tests` = palu_tests(), `Qualité` = palu_quality(),
           `TDR fièvre` = palu_malaria()$by_fever, `TDR groupe` = palu_malaria()$by_group)
    }))
  })
}
