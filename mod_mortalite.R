# Module Shiny — Mortalité SMART (CMR / U5MR)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.
# Niveau expert : IC Poisson / design-based, strates, qualité, exports.

mod_mortalite_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "mortalite",
    fluidRow(box(width = 12, status = "danger", solidHeader = TRUE,
      title = tags$span(icon("skull-crossbones"), " Mortalité SMART — CMR / U5MR (Standards SMART v2 / Sphère)"),
      tags$div(style = "background:#fdf2f2;border-left:4px solid #e74c3c;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Indicateurs :"), " Taux de mortalité brut (CMR) et < 5 ans (U5MR) — Seuils Sphère.",
        tags$br(), "CMR ≥ 1/10 000/jour = urgence | U5MR ≥ 2/10 000/jour = urgence.",
        tags$br(), "Expert : IC 95 % Poisson exact ou design-based (svyratio), strates, qualité et exports.")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Configuration", status = "warning", solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("mort_deaths_ui")), uiOutput(ns("mort_pop_start_ui")), uiOutput(ns("mort_pop_end_ui")),
          uiOutput(ns("mort_u5_pop_ui")), uiOutput(ns("mort_age_dec_ui")), uiOutput(ns("mort_cause_ui")),
          uiOutput(ns("mort_sex_dec_ui")), uiOutput(ns("mort_recall_ui")),
          numericInput(ns("mort_recall_days"), "Jours de rappel (défaut)", 90, min = 30, max = 365),
          tags$hr(style = "margin:8px 0;border-color:#fadbd8;"),
          uiOutput(ns("mort_strat_ui")),
          expert_design_controls_ui(ns, "mort"),
          actionButton(ns("run_mort"), "💀  Calculer", class = "btn-danger btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("mort_tabs"),
          tabPanel("Synthèse", uiOutput(ns("mort_kpi")), br(), DT::DTOutput(ns("mort_summary_table"))),
          tabPanel("Causes", plotOutput(ns("mort_cause_plot"), height = "450px"), DT::DTOutput(ns("mort_cause_table"))),
          tabPanel("Pyramide", plotOutput(ns("mort_pyramid_plot"), height = "500px")),
          tabPanel("Taux & IC",
            uiOutput(ns("mort_expert_note")), br(),
            DT::DTOutput(ns("mort_expert_table")), br(),
            plotly::plotlyOutput(ns("mort_expert_plot"), height = "320px"), br(),
            DT::DTOutput(ns("mort_test_table"))),
          tabPanel("Qualité",
            uiOutput(ns("mort_quality_note")), br(),
            DT::DTOutput(ns("mort_quality_table"))),
          tabPanel("Export", expert_exports_ui(ns, "mort"))
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
      cats <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      cats <- setdiff(cats, c("household_id", "child_id", "id_menage"))
      sel <- intersect(c("region", "province", "departement", "dept", "milieu",
                         "strate", "survey_round"), cats)
      selectInput(ns("mort_strat"), "Stratification (optionnelle)",
                  choices = c("— Aucune —" = "", stats::setNames(cats, cats)),
                  selected = if (length(sel) > 0) sel[1] else "")
    }) |> bindEvent(df_f())

    expert_design_controls_server(input, output, session, "mort", df_f)

    mort_res <- reactiveValues(cmr = NULL, u5mr = NULL, cause = NULL)
    mort_expert <- reactiveVal(NULL); mort_quality <- reactiveVal(NULL)
    mort_tests <- reactiveVal(NULL); mort_method <- reactiveVal("Poisson exact")

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

        d2 <- as.data.frame(d)
        rdays <- if (!is.null(col_rc) && col_rc %in% names(d))
          suppressWarnings(as.numeric(d[[col_rc]])) else rep(input$mort_recall_days, nrow(d))
        if (!is.null(col_d)) d2$.deaths <- suppressWarnings(as.numeric(d[[col_d]]))
        if (!is.null(col_ps) && !is.null(col_pe)) {
          d2$.pt <- ((suppressWarnings(as.numeric(d[[col_ps]])) +
                        suppressWarnings(as.numeric(d[[col_pe]]))) / 2) * rdays
        }
        if (!is.null(col_u5)) d2$.u5_pt <- suppressWarnings(as.numeric(d[[col_u5]])) * rdays
        if (!is.null(col_d) && !is.null(col_ad)) {
          age_dec <- suppressWarnings(as.numeric(d[[col_ad]]))
          d2$.u5_deaths <- ifelse(!is.na(d2$.deaths) & d2$.deaths > 0 & !is.na(age_dec) & age_dec < 5,
                                  d2$.deaths, 0)
        }

        da <- expert_design_args(input, "mort")
        strat <- if (!is.null(input$mort_strat) && nzchar(input$mort_strat)) input$mort_strat else NULL
        method <- if (!is.null(da$cluster) || !is.null(da$strata) || !is.null(da$weights))
          "Design-based (svyratio)" else "Poisson exact"
        tabs <- list(); tests <- list()
        if (".deaths" %in% names(d2) && ".pt" %in% names(d2)) {
          if (is.null(strat)) {
            tabs[["CMR"]] <- expert_rate(d2, ".deaths", ".pt", "CMR (/10 000/jour)",
                                         da$cluster, da$strata, da$weights)
          } else {
            r <- expert_rate_by(d2, ".deaths", ".pt", strat, "CMR (/10 000/jour)",
                                da$cluster, da$strata, da$weights)
            tabs[["CMR"]] <- r$table
            if (!is.null(r$test)) tests[["CMR"]] <- r$test
          }
        }
        if (".u5_deaths" %in% names(d2) && ".u5_pt" %in% names(d2)) {
          if (is.null(strat)) {
            tabs[["U5MR"]] <- expert_rate(d2, ".u5_deaths", ".u5_pt", "U5MR (/10 000/jour)",
                                          da$cluster, da$strata, da$weights)
          } else {
            r <- expert_rate_by(d2, ".u5_deaths", ".u5_pt", strat, "U5MR (/10 000/jour)",
                                da$cluster, da$strata, da$weights)
            tabs[["U5MR"]] <- r$table
            if (!is.null(r$test)) tests[["U5MR"]] <- r$test
          }
        }
        mort_expert(if (length(tabs) > 0) dplyr::bind_rows(tabs) else NULL)
        mort_tests(if (length(tests) > 0) dplyr::bind_rows(tests) else NULL)
        mort_method(method)
        mort_quality(expert_quality(d, unique(c(col_d, col_ps, col_pe, col_u5, col_ad, col_rc, col_ca))))
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

    output$mort_expert_note <- renderUI({
      req(mort_expert())
      tags$div(style = "font-size:12px;color:#555;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
        icon("circle-info"), " Méthode : ", tags$b(mort_method()),
        if (!is.null(input$mort_strat) && nzchar(input$mort_strat))
          paste0(" | strate : ", input$mort_strat) else " | global",
        ". Unités : décès pour 10 000 personnes-jours.")
    })
    output$mort_expert_table <- DT::renderDT({ req(mort_expert()); expert_dt(mort_expert()) })
    output$mort_expert_plot <- plotly::renderPlotly({
      req(mort_expert())
      df <- mort_expert()
      df$Label <- if ("Groupe" %in% names(df) && any(!is.na(df$Groupe)))
        paste0(df$Indicateur, " — ", df$Groupe) else df$Indicateur
      p <- ggplot2::ggplot(df, ggplot2::aes(x = `Taux`, y = Label)) +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = `IC bas`, xmax = `IC haut`),
                               width = .25, orientation = "y", color = "#C0392B") +
        ggplot2::geom_point(size = 3, color = "#0077B6") +
        ggplot2::labs(x = "Taux / 10 000 personnes-jours", y = "") +
        ggplot2::theme_minimal(base_size = 11)
      plotly::ggplotly(p)
    })
    output$mort_test_table <- DT::renderDT({
      req(mort_tests()); if (is.null(mort_tests()) || nrow(mort_tests()) == 0) return(NULL)
      expert_dt(mort_tests())
    })
    output$mort_quality_note <- renderUI({
      tags$div(style = "font-size:12px;color:#555;",
        icon("shield-halved"), " Complétude des décès, populations et période de rappel.")
    })
    output$mort_quality_table <- DT::renderDT({ req(mort_quality()); expert_dt(mort_quality()) })

    expert_export_server(input, output, session, "mort", reactive({
      list(`Synthèse` = if (!is.null(mort_res$cmr)) render_mortality_summary_table(mort_res$cmr, mort_res$u5mr) else NULL,
           `Taux IC` = mort_expert(), `Tests` = mort_tests(),
           `Causes` = mort_res$cause$table, `Qualité` = mort_quality())
    }))
  })
}
