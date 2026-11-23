# Module Shiny — WASH (Eau, Hygiène, Assainissement — JMP / Sphère)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.

mod_wash_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "wash",
    fluidRow(box(width = 12, status = "info", solidHeader = TRUE,
      title = tags$span(icon("shower"), " WASH — Eau, Hygiène, Assainissement (JMP / Sphère)"),
      tags$div(style = "background:#eaf6fd;border-left:4px solid #3498db;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Standards :"), " Classification JMP (amélioré/non amélioré), Sphère (≥ 15 L/p/j, ≤ 30 min)")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Colonnes WASH", status = "info", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("wash_source_ui")), uiOutput(ns("wash_time_ui")), uiOutput(ns("wash_qty_ui")),
          uiOutput(ns("wash_treat_ui")), uiOutput(ns("wash_latrine_ui")), uiOutput(ns("wash_shared_ui")),
          uiOutput(ns("wash_oda_ui")), uiOutput(ns("wash_hw_ui")), uiOutput(ns("wash_soap_ui")),
          uiOutput(ns("wash_hwsoap_ui")),
          actionButton(ns("run_wash"), "🚰  Calculer WASH", class = "btn-info btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("wash_tabs"),
          tabPanel("Synthèse", uiOutput(ns("wash_kpi"))),
          tabPanel("Échelle JMP", plotOutput(ns("wash_ladder_plot"), height = "500px")),
          tabPanel("Sources d'eau", DT::DTOutput(ns("wash_sources_table"))),
          tabPanel("Traitement", DT::DTOutput(ns("wash_treatment_table")))
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
      output$wash_source_ui  <- renderUI(sens_select_ns(ns, "wash_source", "Source d'eau", "source_eau", d))
      output$wash_time_ui    <- renderUI(sens_select_ns(ns, "wash_time", "Temps collecte (min)", "temps_collecte_eau", d))
      output$wash_qty_ui     <- renderUI(sens_select_ns(ns, "wash_qty", "Quantité eau (L/p/j)", "quantite_eau", d))
      output$wash_treat_ui   <- renderUI(sens_select_ns(ns, "wash_treat", "Traitement eau", "traitement_eau", d))
      output$wash_latrine_ui <- renderUI(sens_select_ns(ns, "wash_latrine", "Type latrine", "type_latrine", d))
      output$wash_shared_ui  <- renderUI(sens_select_ns(ns, "wash_shared", "Latrine partagée", "latrine_partagee", d))
      output$wash_oda_ui     <- renderUI(sens_select_ns(ns, "wash_oda", "DAL", "defecation_air_libre", d))
      output$wash_hw_ui      <- renderUI(sens_select_ns(ns, "wash_hw", "Dispositif lavage", "lavage_mains", d))
      output$wash_soap_ui    <- renderUI(sens_select_ns(ns, "wash_soap", "Savon", "savon_disponible", d))
      output$wash_hwsoap_ui  <- renderUI(sens_select_ns(ns, "wash_hwsoap", "Lavage + savon", "eau_savon_lavage", d))
    }) |> bindEvent(df_f())

    wash_res <- reactiveVal(NULL)

    observeEvent(input$run_wash, {
      d <- df_f(); req(d)
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      result <- calc_wash_indicators(d, g("wash_source"), g("wash_time"), g("wash_qty"), g("wash_treat"),
        g("wash_latrine"), g("wash_shared"), g("wash_oda"), g("wash_hw"), g("wash_soap"), g("wash_hwsoap"))
      wash_res(result)
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
  })
}
