# Module Shiny — Paludisme & Moustiquaires SENS (LLIN / TDR / ACT)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.

mod_paludisme_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "paludisme",
    fluidRow(box(width = 12, status = "danger", solidHeader = TRUE,
      title = tags$span(icon("bug"), " Paludisme & Moustiquaires SENS (Roll Back Malaria / OMS)"),
      tags$div(style = "background:#fdf2f2;border-left:4px solid #c0392b;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Indicateurs :"), " Possession / Accès / Utilisation LLIN, Prévalence TDR+, Traitement ACT")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Configuration", status = "danger", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("palu_poss_ui")), uiOutput(ns("palu_use_ui")), uiOutput(ns("palu_access_ui")),
          uiOutput(ns("palu_nb_nets_ui")), uiOutput(ns("palu_hh_size_ui")),
          uiOutput(ns("palu_tdr_ui")), uiOutput(ns("palu_fever_ui")), uiOutput(ns("palu_strat_ui")),
          actionButton(ns("run_palu"), "🦟  Calculer", class = "btn-danger btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("palu_tabs"),
          tabPanel("Synthèse", uiOutput(ns("palu_kpi"))),
          tabPanel("Cascade LLIN", plotOutput(ns("palu_cascade_plot"), height = "400px")),
          tabPanel("TDR par groupe", DT::DTOutput(ns("palu_by_fever")), br(), DT::DTOutput(ns("palu_by_group")))
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

    palu_llin <- reactiveVal(NULL); palu_malaria <- reactiveVal(NULL)

    observeEvent(input$run_palu, {
      d <- df_f(); req(d)
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      withProgress(message = "Calcul paludisme...", {
        palu_llin(calc_llin_indicators(d, g("palu_poss"), g("palu_use"), g("palu_access"), g("palu_nb_nets"), g("palu_hh_size")))
        if (!is.null(g("palu_tdr"))) palu_malaria(calc_malaria_prevalence(d, g("palu_tdr"), g("palu_fever"), g("palu_strat")))
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
  })
}
