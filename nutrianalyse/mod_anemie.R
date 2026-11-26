# Module Shiny — Anémie SENS (hémoglobine, classification OMS)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.
# Niveau expert : IC Wilson / design-based, stratification, qualité, exports.

mod_anemie_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "anemie",
    fluidRow(box(width = 12, status = "warning", solidHeader = TRUE,
      title = tags$span(icon("tint"), " Anémie SENS — Hémoglobine & Classification OMS"),
      tags$div(style = "background:#fef9e7;border-left:4px solid #f39c12;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Standards OMS 2011 :"), " Enfants < 11 g/dL | Femmes NE < 12 g/dL | Femmes enceintes < 11 g/dL.",
        tags$br(), "Ajustements : altitude + tabac (tables OMS). ",
        tags$br(), "Expert : IC 95 % Wilson ou design-based (cluster / strates / poids), stratification, qualité et exports.")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Configuration", status = "warning", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("anem_hb_ui")), uiOutput(ns("anem_alt_ui")), uiOutput(ns("anem_smoke_ui")),
          uiOutput(ns("anem_preg_ui")),
          selectInput(ns("anem_pop"), "Population cible", choices = c(
            "Enfants 6-59 mois" = "enfant_6_59", "Femmes non enceintes" = "femme_np",
            "Femmes enceintes" = "femme_enc", "Hommes adultes" = "homme")),
          tags$hr(style = "margin:8px 0;border-color:#fdebd0;"),
          uiOutput(ns("anem_strat_ui")),
          expert_design_controls_ui(ns, "anem"),
          actionButton(ns("run_anem"), "🩸  Calculer", class = "btn-warning btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("anem_tabs"),
          tabPanel("Synthèse", uiOutput(ns("anem_kpi")), br(), DT::DTOutput(ns("anem_summary_table"))),
          tabPanel("Distribution Hb", plotOutput(ns("anem_hb_dist"), height = "450px")),
          tabPanel("Classification", plotly::plotlyOutput(ns("anem_donut"), height = "400px")),
          tabPanel("Prévalences & IC",
            uiOutput(ns("anem_expert_note")), br(),
            DT::DTOutput(ns("anem_expert_table")), br(),
            plotly::plotlyOutput(ns("anem_expert_plot"), height = "360px"), br(),
            DT::DTOutput(ns("anem_test_table"))),
          tabPanel("Qualité",
            uiOutput(ns("anem_quality_note")), br(),
            DT::DTOutput(ns("anem_quality_table"))),
          tabPanel("Export", expert_exports_ui(ns, "anem"))
        )
      )
    )
  )
}

mod_anemie_server <- function(id, df_f) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      d <- df_f(); req(d)
      output$anem_hb_ui    <- renderUI(sens_select_ns(ns, "anem_hb", "Hémoglobine (g/dL)", "hemoglobine", d))
      output$anem_alt_ui   <- renderUI(sens_select_ns(ns, "anem_alt", "Altitude (m)", "altitude", d))
      output$anem_smoke_ui <- renderUI(sens_select_ns(ns, "anem_smoke", "Fumeur", "fumeur", d))
      output$anem_preg_ui  <- renderUI(sens_select_ns(ns, "anem_preg", "Enceinte", "enceinte", d))
      cats <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      cats <- setdiff(cats, c("household_id", "child_id", "id_menage"))
      sel <- intersect(c("region", "province", "departement", "dept", "sexe",
                         "milieu", "strate", "survey_round"), cats)
      selectInput(ns("anem_strat"), "Stratification (optionnelle)",
                  choices = c("— Aucune —" = "", stats::setNames(cats, cats)),
                  selected = if (length(sel) > 0) sel[1] else "")
    }) |> bindEvent(df_f())

    expert_design_controls_server(input, output, session, "anem", df_f)

    anem_res <- reactiveVal(NULL)
    anem_expert <- reactiveVal(NULL)
    anem_quality <- reactiveVal(NULL)
    anem_tests <- reactiveVal(NULL)
    anem_method <- reactiveVal("Wilson")

    observeEvent(input$run_anem, {
      d <- df_f(); req(d)
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      result <- calc_anemia_prevalence(d, col_hb = g("anem_hb"), col_altitude = g("anem_alt"),
        col_smoker = g("anem_smoke"), col_pregnant = g("anem_preg"), population = input$anem_pop)
      anem_res(result)

      cls <- result$classification
      d2 <- as.data.frame(d)
      d2$.anemie_tot <- as.integer(cls %in% c("Légère", "Modérée", "Sévère"))
      d2$.anemie_leg <- as.integer(cls == "Légère")
      d2$.anemie_mod <- as.integer(cls == "Modérée")
      d2$.anemie_sev <- as.integer(cls == "Sévère")

      da <- expert_design_args(input, "anem")
      strat <- g("anem_strat")
      vars <- c(`Anémie totale` = ".anemie_tot", `Anémie légère` = ".anemie_leg",
                `Anémie modérée` = ".anemie_mod", `Anémie sévère` = ".anemie_sev")
      if (is.null(strat)) {
        tab <- expert_prev_multi(d2, vars, cluster = da$cluster,
                                 strata = da$strata, weights = da$weights)
        tests <- NULL
      } else {
        tab <- purrr::map_dfr(seq_along(vars), function(i)
          expert_prev_by(d2, vars[[i]], strat, names(vars)[i], da$cluster, da$strata, da$weights))
        tests <- purrr::map_dfr(seq_along(vars), function(i)
          expert_strat_test(d2, vars[[i]], strat))
        if (nrow(tests) > 0) tests$Indicateur <- names(vars)
      }
      anem_expert(tab)
      anem_tests(tests)
      anem_method(if (!is.null(da$cluster) || !is.null(da$strata) || !is.null(da$weights))
        "Design-based" else "Wilson")
      anem_quality(expert_quality(d, unique(c(g("anem_hb"), g("anem_alt"),
        g("anem_smoke"), g("anem_preg")))))
      showNotification(paste0("Anémie totale: ", result$total_anemia, "% ✓"), type = "message")
    })

    output$anem_kpi <- renderUI({ req(anem_res()); HTML(render_anemia_kpi(anem_res())) })
    output$anem_summary_table <- DT::renderDT({
      req(anem_res()); DT::datatable(anem_res()$summary, options = list(dom = "t"))
    })
    output$anem_hb_dist <- renderPlot({ req(anem_res()); plot_hb_distribution(anem_res()) })
    output$anem_donut <- plotly::renderPlotly({ req(anem_res()); plot_anemia_donut(anem_res()) })

    output$anem_expert_note <- renderUI({
      req(anem_expert())
      strat <- input$anem_strat
      tags$div(style = "font-size:12px;color:#555;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
        icon("circle-info"), " Méthode : ", tags$b(anem_method()),
        if (!is.null(strat) && nzchar(strat)) paste0(" | strate : ", strat) else " | global",
        ". IC 95 % Wilson si aucun plan de sondage, sinon svyciprop logit avec DEFF.")
    })
    output$anem_expert_table <- DT::renderDT({ req(anem_expert()); expert_dt(anem_expert()) })
    output$anem_expert_plot <- plotly::renderPlotly({
      req(anem_expert()); p <- expert_prev_plot(anem_expert(), "Anémie — prévalences et IC 95 %")
      if (is.null(p)) plotly::plotly_empty() else p
    })
    output$anem_test_table <- DT::renderDT({
      req(anem_tests()); if (nrow(anem_tests()) == 0) return(NULL)
      expert_dt(anem_tests()[c("Indicateur", "Test", "Statistique", "ddl", "p", "V de Cramér", "N")])
    })
    output$anem_quality_note <- renderUI({
      tags$div(style = "font-size:12px;color:#555;",
        icon("shield-halved"), " Contrôle de complétude et de plausibilité des colonnes utilisées.")
    })
    output$anem_quality_table <- DT::renderDT({ req(anem_quality()); expert_dt(anem_quality()) })

    expert_export_server(input, output, session, "anem", reactive({
      list(`Synthèse` = anem_res()$summary, `Prévalences IC` = anem_expert(),
           `Tests` = anem_tests(), `Qualité` = anem_quality())
    }))
  })
}
