# Module Shiny — WASH (Eau, Hygiène, Assainissement — JMP / Sphère)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.
# Niveau expert : IC Wilson / design-based, strates, qualité, exports.

mod_wash_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "wash",
    fluidRow(box(width = 12, status = "info", solidHeader = TRUE,
      title = tags$span(icon("shower"), " WASH — Eau, Hygiène, Assainissement (JMP / Sphère)"),
      tags$div(style = "background:#eaf6fd;border-left:4px solid #3498db;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Standards :"), " Classification JMP (amélioré/non amélioré), Sphère (≥ 15 L/p/j, ≤ 30 min).",
        tags$br(), "Expert : IC 95 % Wilson ou design-based, stratification, qualité et exports.")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Colonnes WASH", status = "info", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("wash_source_ui")), uiOutput(ns("wash_time_ui")), uiOutput(ns("wash_qty_ui")),
          uiOutput(ns("wash_treat_ui")), uiOutput(ns("wash_latrine_ui")), uiOutput(ns("wash_shared_ui")),
          uiOutput(ns("wash_oda_ui")), uiOutput(ns("wash_hw_ui")), uiOutput(ns("wash_soap_ui")),
          uiOutput(ns("wash_hwsoap_ui")),
          tags$hr(style = "margin:8px 0;border-color:#d6eaf8;"),
          uiOutput(ns("wash_strat_ui")),
          expert_design_controls_ui(ns, "wash"),
          actionButton(ns("run_wash"), "🚰  Calculer WASH", class = "btn-info btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("wash_tabs"),
          tabPanel("Synthèse", uiOutput(ns("wash_kpi"))),
          tabPanel("Échelle JMP", plotOutput(ns("wash_ladder_plot"), height = "450px")),
          tabPanel("Sources", DT::DTOutput(ns("wash_sources_table")), br(), DT::DTOutput(ns("wash_treatment_table"))),
          tabPanel("Prévalences & IC",
            uiOutput(ns("wash_expert_note")), br(),
            DT::DTOutput(ns("wash_expert_table")), br(),
            plotly::plotlyOutput(ns("wash_expert_plot"), height = "380px"), br(),
            DT::DTOutput(ns("wash_test_table"))),
          tabPanel("Qualité",
            uiOutput(ns("wash_quality_note")), br(),
            DT::DTOutput(ns("wash_quality_table"))),
          tabPanel("Export", expert_exports_ui(ns, "wash"))
        )
      )
    )
  )
}

mod_wash_server <- function(id, df_f) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      d <- df_f(); req(d)
      output$wash_source_ui  <- renderUI(sens_select_ns(ns, "wash_source", "Source d'eau", "water_source", d))
      output$wash_time_ui    <- renderUI(sens_select_ns(ns, "wash_time", "Temps collecte (min)", "water_time", d))
      output$wash_qty_ui     <- renderUI(sens_select_ns(ns, "wash_qty", "Quantité (L/p/j)", "water_quantity", d))
      output$wash_treat_ui   <- renderUI(sens_select_ns(ns, "wash_treat", "Traitement de l'eau", "water_treatment", d))
      output$wash_latrine_ui <- renderUI(sens_select_ns(ns, "wash_latrine", "Type de latrine", "latrine_type", d))
      output$wash_shared_ui  <- renderUI(sens_select_ns(ns, "wash_shared", "Latrine partagée", "latrine_shared", d))
      output$wash_oda_ui     <- renderUI(sens_select_ns(ns, "wash_oda", "Défécation à l'air libre", "open_defecation", d))
      output$wash_hw_ui      <- renderUI(sens_select_ns(ns, "wash_hw", "Dispositif lavage mains", "handwashing_facility", d))
      output$wash_soap_ui    <- renderUI(sens_select_ns(ns, "wash_soap", "Savon disponible", "soap_available", d))
      output$wash_hwsoap_ui  <- renderUI(sens_select_ns(ns, "wash_hwsoap", "Lavage + savon", "handwash_soap", d))
      cats <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      cats <- setdiff(cats, c("household_id", "child_id", "id_menage"))
      sel <- intersect(c("region", "province", "departement", "dept", "milieu",
                         "strate", "survey_round"), cats)
      selectInput(ns("wash_strat"), "Stratification (optionnelle)",
                  choices = c("— Aucune —" = "", stats::setNames(cats, cats)),
                  selected = if (length(sel) > 0) sel[1] else "")
    }) |> bindEvent(df_f())

    expert_design_controls_server(input, output, session, "wash", df_f)

    wash_res <- reactiveVal(NULL)
    wash_expert <- reactiveVal(NULL); wash_quality <- reactiveVal(NULL)
    wash_tests <- reactiveVal(NULL); wash_method <- reactiveVal("Wilson")

    observeEvent(input$run_wash, {
      d <- df_f(); req(d)
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      withProgress(message = "Calcul WASH...", {
        res <- calc_wash_indicators(d, g("wash_source"), g("wash_time"), g("wash_qty"),
          g("wash_treat"), g("wash_latrine"), g("wash_shared"), g("wash_oda"),
          g("wash_hw"), g("wash_soap"), g("wash_hwsoap"))
        wash_res(res)

        d2 <- as.data.frame(d)
        vars <- c()
        if (!is.null(g("wash_source"))) {
          ws <- tolower(as.character(d[[g("wash_source")]]))
          d2$.water_improved <- as.integer(!is.na(ws) & ws %in% WATER_IMPROVED)
          vars <- c(vars, `Eau améliorée (JMP)` = ".water_improved")
        }
        if (!is.null(g("wash_time"))) {
          wt <- suppressWarnings(as.numeric(d[[g("wash_time")]]))
          d2$.time_ok <- as.integer(!is.na(wt) & wt <= 30)
          vars <- c(vars, `Eau ≤ 30 min` = ".time_ok")
        }
        if (!is.null(g("wash_qty"))) {
          wq <- suppressWarnings(as.numeric(d[[g("wash_qty")]]))
          d2$.qty_ok <- as.integer(!is.na(wq) & wq >= 15)
          vars <- c(vars, `≥ 15 L/personne/jour` = ".qty_ok")
        }
        if (!is.null(g("wash_latrine"))) {
          lat <- tolower(as.character(d[[g("wash_latrine")]]))
          d2$.sanit_improved <- as.integer(!is.na(lat) & lat %in% SANITATION_IMPROVED)
          vars <- c(vars, `Assainissement amélioré` = ".sanit_improved")
        }
        if (!is.null(g("wash_oda"))) {
          oda <- tolower(as.character(d[[g("wash_oda")]]))
          d2$.no_oda <- as.integer(!is.na(oda) & !oda %in% c("oui", "yes", "1", "true"))
          vars <- c(vars, `Pas de DAL` = ".no_oda")
        }
        if (!is.null(g("wash_hw")))   vars <- c(vars, `Dispositif lavage` = g("wash_hw"))
        if (!is.null(g("wash_soap"))) vars <- c(vars, `Savon disponible` = g("wash_soap"))
        if (!is.null(g("wash_hwsoap"))) vars <- c(vars, `Lavage + savon` = g("wash_hwsoap"))
        if (!is.null(g("wash_treat"))) {
          tr <- tolower(as.character(d[[g("wash_treat")]]))
          d2$.treated <- as.integer(!is.na(tr) & !tr %in% c("aucun traitement", "aucun", "none", "no treatment", "rien", ""))
          vars <- c(vars, `Eau traitée` = ".treated")
        }

        da <- expert_design_args(input, "wash")
        strat <- g("wash_strat")
        method <- if (!is.null(da$cluster) || !is.null(da$strata) || !is.null(da$weights)) "Design-based" else "Wilson"
        if (length(vars) == 0) {
          wash_expert(NULL); wash_tests(NULL)
        } else if (is.null(strat)) {
          wash_expert(expert_prev_multi(d2, vars, cluster = da$cluster,
                                        strata = da$strata, weights = da$weights))
          wash_tests(NULL)
        } else {
          wash_expert(purrr::map_dfr(seq_along(vars), function(i)
            expert_prev_by(d2, vars[[i]], strat, names(vars)[i], da$cluster, da$strata, da$weights)))
          tt <- purrr::map_dfr(seq_along(vars), function(i) expert_strat_test(d2, vars[[i]], strat))
          if (nrow(tt) > 0) tt$Indicateur <- names(vars)
          wash_tests(tt)
        }
        wash_method(method)
        wash_quality(expert_quality(d, unique(c(g("wash_source"), g("wash_time"), g("wash_qty"),
          g("wash_treat"), g("wash_latrine"), g("wash_oda"), g("wash_hw"), g("wash_soap"),
          g("wash_hwsoap")))))
      })
      showNotification("WASH calculé ✓", type = "message")
    })

    output$wash_kpi <- renderUI({ req(wash_res()); HTML(render_wash_kpi(wash_res())) })
    output$wash_ladder_plot <- renderPlot({ req(wash_res()); plot_wash_ladder(wash_res()) })
    output$wash_sources_table <- DT::renderDT({
      req(wash_res()); if (!is.null(wash_res()$results$water_sources)) DT::datatable(wash_res()$results$water_sources)
    })
    output$wash_treatment_table <- DT::renderDT({
      req(wash_res()); if (!is.null(wash_res()$results$treatment_methods)) DT::datatable(wash_res()$results$treatment_methods)
    })

    output$wash_expert_note <- renderUI({
      req(wash_expert())
      tags$div(style = "font-size:12px;color:#555;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
        icon("circle-info"), " Méthode : ", tags$b(wash_method()),
        if (!is.null(input$wash_strat) && nzchar(input$wash_strat))
          paste0(" | strate : ", input$wash_strat) else " | global",
        ". Indicateurs recodés selon JMP / Sphère.")
    })
    output$wash_expert_table <- DT::renderDT({ req(wash_expert()); expert_dt(wash_expert()) })
    output$wash_expert_plot <- plotly::renderPlotly({
      req(wash_expert()); p <- expert_prev_plot(wash_expert(), "WASH — prévalences et IC 95 %")
      if (is.null(p)) plotly::plotly_empty() else p
    })
    output$wash_test_table <- DT::renderDT({
      req(wash_tests()); if (is.null(wash_tests()) || nrow(wash_tests()) == 0) return(NULL)
      expert_dt(wash_tests()[c("Indicateur", "Test", "Statistique", "ddl", "p", "V de Cramér", "N")])
    })
    output$wash_quality_note <- renderUI({
      tags$div(style = "font-size:12px;color:#555;",
        icon("shield-halved"), " Complétude et plausibilité des colonnes WASH.")
    })
    output$wash_quality_table <- DT::renderDT({ req(wash_quality()); expert_dt(wash_quality()) })

    expert_export_server(input, output, session, "wash", reactive({
      list(`Échelle JMP` = wash_res()$jmp_ladder, `Sources` = wash_res()$results$water_sources,
           `Traitements` = wash_res()$results$treatment_methods,
           `Prévalences IC` = wash_expert(), `Tests` = wash_tests(), `Qualité` = wash_quality())
    }))
  })
}
