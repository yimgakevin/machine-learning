# Module Shiny — Mortalité SMART (CMR / U5MR)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.

mod_mortalite_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "mortalite",
    fluidRow(box(width = 12, status = "danger", solidHeader = TRUE,
      title = tags$span(icon("skull-crossbones"), " Mortalité SMART — CMR / U5MR (Standards SMART v2 / Sphère)"),
      tags$div(style = "background:#fdf2f2;border-left:4px solid #e74c3c;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Indicateurs :"), " Taux de mortalité brut (CMR) et < 5 ans (U5MR) — Seuils Sphère.",
        tags$br(), "CMR ≥ 1/10 000/jour = urgence | U5MR ≥ 2/10 000/jour = urgence")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Configuration", status = "warning", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("mort_deaths_ui")), uiOutput(ns("mort_pop_start_ui")), uiOutput(ns("mort_pop_end_ui")),
          uiOutput(ns("mort_u5_pop_ui")), uiOutput(ns("mort_age_dec_ui")), uiOutput(ns("mort_cause_ui")),
          uiOutput(ns("mort_sex_dec_ui")), uiOutput(ns("mort_recall_ui")),
          numericInput(ns("mort_recall_days"), "Jours de rappel (défaut)", 90, min = 30, max = 365),
          actionButton(ns("run_mort"), "💀  Calculer", class = "btn-danger btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("mort_tabs"),
          tabPanel("Synthèse", uiOutput(ns("mort_kpi")), br(), DT::DTOutput(ns("mort_summary_table"))),
          tabPanel("Causes", plotOutput(ns("mort_cause_plot"), height = "450px"), DT::DTOutput(ns("mort_cause_table"))),
          tabPanel("Pyramide", plotOutput(ns("mort_pyramid_plot"), height = "500px"))
        )
      )
    )
  )
}

mod_mortalite_server <- function(id, df_f) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      d <- df_f(); req(d)
      output$mort_deaths_ui    <- renderUI(sens_select_ns(ns, "mort_deaths", "Nb décès", "nb_deces", d))
      output$mort_pop_start_ui <- renderUI(sens_select_ns(ns, "mort_pop_start", "Pop début rappel", "pop_debut", d))
      output$mort_pop_end_ui   <- renderUI(sens_select_ns(ns, "mort_pop_end", "Pop fin rappel", "pop_fin", d))
      output$mort_u5_pop_ui    <- renderUI(sens_select_ns(ns, "mort_u5_pop", "Enfants < 5 ans", "taille_menage_0_5", d))
      output$mort_age_dec_ui   <- renderUI(sens_select_ns(ns, "mort_age_dec", "Âge décédé", "age_decede", d))
      output$mort_cause_ui     <- renderUI(sens_select_ns(ns, "mort_cause", "Cause décès", "cause_deces", d))
      output$mort_sex_dec_ui   <- renderUI(sens_select_ns(ns, "mort_sex_dec", "Sexe décédé", "sexe_decede", d))
      output$mort_recall_ui    <- renderUI(sens_select_ns(ns, "mort_recall", "Période rappel (col)", "recall_period", d))
    }) |> bindEvent(df_f())

    mort_res <- reactiveValues(cmr = NULL, u5mr = NULL, cause = NULL)

    observeEvent(input$run_mort, {
      d <- df_f(); req(d)
      withProgress(message = "Calcul mortalité...", {
        col_d <- if (!is.null(input$mort_deaths) && input$mort_deaths != "") input$mort_deaths else NULL
        col_ps <- if (!is.null(input$mort_pop_start) && input$mort_pop_start != "") input$mort_pop_start else NULL
        col_pe <- if (!is.null(input$mort_pop_end) && input$mort_pop_end != "") input$mort_pop_end else NULL
        col_u5 <- if (!is.null(input$mort_u5_pop) && input$mort_u5_pop != "") input$mort_u5_pop else NULL
        col_ad <- if (!is.null(input$mort_age_dec) && input$mort_age_dec != "") input$mort_age_dec else NULL
        col_rc <- if (!is.null(input$mort_recall) && input$mort_recall != "") input$mort_recall else NULL
        col_ca <- if (!is.null(input$mort_cause) && input$mort_cause != "") input$mort_cause else NULL

        if (!is.null(col_d) && !is.null(col_ps) && !is.null(col_pe)) {
          mort_res$cmr <- calc_cmr(d, col_d, col_ps, col_pe, col_rc, input$mort_recall_days)
        }
        if (!is.null(col_d) && !is.null(col_u5) && !is.null(col_ad)) {
          mort_res$u5mr <- calc_u5mr(d, col_d, col_ad, col_u5, col_recall_days = col_rc, recall_days = input$mort_recall_days)
        }
        if (!is.null(col_ca)) mort_res$cause <- calc_cause_death(d, col_ca)
      })
      showNotification("Mortalité calculée ✓", type = "message")
    })

    output$mort_kpi <- renderUI({ req(mort_res$cmr); HTML(render_mortality_kpi(mort_res$cmr, mort_res$u5mr)) })
    output$mort_summary_table <- DT::renderDT({
      req(mort_res$cmr)
      tbl <- render_mortality_summary_table(mort_res$cmr, mort_res$u5mr)
      if (nrow(tbl) > 0) DT::datatable(tbl, options = list(dom = "t"))
    })
    output$mort_cause_plot <- renderPlot({ req(mort_res$cause); plot_cause_death(mort_res$cause) })
    output$mort_cause_table <- DT::renderDT({
      req(mort_res$cause); DT::datatable(mort_res$cause$table, options = list(pageLength = 15))
    })
    output$mort_pyramid_plot <- renderPlot({
      d <- df_f(); req(d)
      col_age <- if (!is.null(input$mort_age_dec) && input$mort_age_dec != "") input$mort_age_dec else NULL
      col_sex <- if (!is.null(input$mort_sex_dec) && input$mort_sex_dec != "") input$mort_sex_dec else NULL
      req(col_age, col_sex)
      plot_mortality_pyramid(d[!is.na(d[[col_age]]), ], col_age, col_sex)
    })
  })
}
