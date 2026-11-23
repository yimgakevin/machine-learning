# Module Shiny — Anémie SENS (hémoglobine, classification OMS)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.

mod_anemie_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "anemie",
    fluidRow(box(width = 12, status = "warning", solidHeader = TRUE,
      title = tags$span(icon("tint"), " Anémie SENS — Hémoglobine & Classification OMS"),
      tags$div(style = "background:#fef9e7;border-left:4px solid #f39c12;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Standards OMS 2011 :"), " Enfants < 11 g/dL | Femmes NE < 12 g/dL | Femmes enceintes < 11 g/dL.",
        tags$br(), "Ajustements : altitude + tabac (tables OMS)")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Configuration", status = "warning", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("anem_hb_ui")), uiOutput(ns("anem_alt_ui")), uiOutput(ns("anem_smoke_ui")),
          uiOutput(ns("anem_preg_ui")),
          selectInput(ns("anem_pop"), "Population cible", choices = c(
            "Enfants 6-59 mois" = "enfant_6_59", "Femmes non enceintes" = "femme_np",
            "Femmes enceintes" = "femme_enc", "Hommes adultes" = "homme")),
          actionButton(ns("run_anem"), "🩸  Calculer", class = "btn-warning btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("anem_tabs"),
          tabPanel("Synthèse", uiOutput(ns("anem_kpi")), br(), DT::DTOutput(ns("anem_summary_table"))),
          tabPanel("Distribution Hb", plotOutput(ns("anem_hb_dist"), height = "450px")),
          tabPanel("Classification", plotly::plotlyOutput(ns("anem_donut"), height = "400px"))
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
  })
}
