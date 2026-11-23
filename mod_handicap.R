# Module Shiny — Handicap (Washington Group Short Set, 6 domaines)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.

mod_handicap_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "handicap",
    fluidRow(box(width = 12, status = "warning", solidHeader = TRUE,
      title = tags$span(icon("wheelchair"), " Handicap — Washington Group Short Set (6 domaines)"),
      tags$div(style = "background:#fef9e7;border-left:4px solid #e67e22;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Domaines :"), " Vision, Audition, Mobilité, Cognition, Soins personnels, Communication.",
        tags$br(), "Seuil OMS : ≥ 'Beaucoup de difficultés' dans au moins 1 domaine")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Colonnes WG", status = "warning", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("wg_voir_ui")), uiOutput(ns("wg_entendre_ui")), uiOutput(ns("wg_marcher_ui")),
          uiOutput(ns("wg_memoire_ui")), uiOutput(ns("wg_soins_ui")), uiOutput(ns("wg_comm_ui")),
          sliderInput(ns("wg_threshold"), "Seuil de sévérité", min = 2, max = 4, value = 3, step = 1),
          actionButton(ns("run_wg"), "♿  Calculer", class = "btn-warning btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("wg_tabs"),
          tabPanel("Synthèse", uiOutput(ns("wg_kpi")), br(), DT::DTOutput(ns("wg_domain_table"))),
          tabPanel("Par domaine", plotOutput(ns("wg_domain_plot"), height = "450px")),
          tabPanel("Sévérité", plotOutput(ns("wg_severity_plot"), height = "500px"))
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
  })
}
