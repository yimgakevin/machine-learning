# Module Shiny — Ménage & Protection (SENS / UNHCR)
# UI namespacée + serveur moduleServer. Réactif partagé : df_f.

mod_menage_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "menage",
    fluidRow(box(width = 12, status = "primary", solidHeader = TRUE,
      title = tags$span(icon("house-user"), " Ménage & Protection (SENS / UNHCR)"),
      tags$div(style = "background:#eaf0f8;border-left:4px solid #2c3e50;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
        tags$b("Modules :"), " Profil démographique, Économie ménage, Abri, Protection")
    )),
    fluidRow(
      column(3,
        box(width = 12, title = "Configuration", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          tags$h5("Composition"), uiOutput(ns("hh_size_ui")), uiOutput(ns("hh_u5_ui")),
          uiOutput(ns("hh_5_17_ui")), uiOutput(ns("hh_adults_ui")),
          tags$h5("Chef de ménage"), uiOutput(ns("hh_head_sex_ui")), uiOutput(ns("hh_head_age_ui")),
          uiOutput(ns("hh_education_ui")),
          tags$h5("Économie"), uiOutput(ns("hh_income_ui")), uiOutput(ns("hh_income_src_ui")),
          uiOutput(ns("hh_expenditure_ui")), uiOutput(ns("hh_food_exp_ui")), uiOutput(ns("hh_livelihood_ui")),
          tags$h5("Abri"), uiOutput(ns("hh_shelter_ui")), uiOutput(ns("hh_tenure_ui")),
          tags$h5("Protection"), uiOutput(ns("hh_registration_ui")), uiOutput(ns("hh_documentation_ui")),
          uiOutput(ns("hh_incident_ui")), uiOutput(ns("hh_child_labor_ui")), uiOutput(ns("hh_marriage_ui")),
          actionButton(ns("run_menage"), "🏠  Analyser", class = "btn-primary btn-block", style = "margin-top:10px;")
        )
      ),
      column(9,
        tabsetPanel(id = ns("menage_tabs"),
          tabPanel("Profil", uiOutput(ns("hh_kpi")), br(), DT::DTOutput(ns("hh_composition_table"))),
          tabPanel("Économie", plotOutput(ns("hh_income_plot"), height = "400px"), DT::DTOutput(ns("hh_livelihood_table"))),
          tabPanel("Abri", DT::DTOutput(ns("hh_shelter_table")), DT::DTOutput(ns("hh_tenure_table"))),
          tabPanel("Protection", plotOutput(ns("hh_protection_plot"), height = "400px"), DT::DTOutput(ns("hh_protection_table")))
        )
      )
    )
  )
}

mod_menage_server <- function(id, df_f) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      d <- df_f(); req(d)
      output$hh_size_ui       <- renderUI(sens_select_ns(ns, "hh_size", "Taille ménage", "taille_menage", d))
      output$hh_u5_ui         <- renderUI(sens_select_ns(ns, "hh_u5", "Enfants < 5", "taille_menage_0_5", d))
      output$hh_5_17_ui       <- renderUI(sens_select_ns(ns, "hh_5_17", "Enfants 5-17", "taille_menage_5_17", d))
      output$hh_adults_ui     <- renderUI(sens_select_ns(ns, "hh_adults", "Adultes", "taille_menage_adultes", d))
      output$hh_head_sex_ui   <- renderUI(sens_select_ns(ns, "hh_head_sex", "Sexe chef ménage", "chef_menage_sexe", d))
      output$hh_head_age_ui   <- renderUI(sens_select_ns(ns, "hh_head_age", "Âge chef ménage", "chef_menage_age", d))
      output$hh_education_ui  <- renderUI(sens_select_ns(ns, "hh_education", "Éducation chef", "education_cm", d))
      output$hh_income_ui     <- renderUI(sens_select_ns(ns, "hh_income", "Revenus", "revenus", d))
      output$hh_income_src_ui <- renderUI(sens_select_ns(ns, "hh_income_src", "Source revenus", "source_revenus", d))
      output$hh_expenditure_ui<- renderUI(sens_select_ns(ns, "hh_expenditure", "Dépenses totales", "depenses", d))
      output$hh_food_exp_ui   <- renderUI(sens_select_ns(ns, "hh_food_exp", "Dépenses alim.", "depenses_alim", d))
      output$hh_livelihood_ui <- renderUI(sens_select_ns(ns, "hh_livelihood", "Moyens subsistance", "moyens_subsistance", d))
      output$hh_shelter_ui    <- renderUI(sens_select_ns(ns, "hh_shelter", "Type abri", "type_abri", d))
      output$hh_tenure_ui     <- renderUI(sens_select_ns(ns, "hh_tenure", "Statut propriété", "statut_propriete", d))
      output$hh_registration_ui  <- renderUI(sens_select_ns(ns, "hh_registration", "Enregistrement", "enregistrement", d))
      output$hh_documentation_ui <- renderUI(sens_select_ns(ns, "hh_documentation", "Documentation", "documentation", d))
      output$hh_incident_ui      <- renderUI(sens_select_ns(ns, "hh_incident", "Incident protection", "incident_protection", d))
      output$hh_child_labor_ui   <- renderUI(sens_select_ns(ns, "hh_child_labor", "Travail enfant", "travail_enfant", d))
      output$hh_marriage_ui      <- renderUI(sens_select_ns(ns, "hh_marriage", "Mariage précoce", "mariage_precoce", d))
    }) |> bindEvent(df_f())

    hh_profile_res <- reactiveVal(NULL); hh_econ_res <- reactiveVal(NULL)
    hh_shelter_res <- reactiveVal(NULL); hh_prot_res <- reactiveVal(NULL)

    observeEvent(input$run_menage, {
      d <- df_f(); req(d)
      g <- function(id) { v <- input[[id]]; if (!is.null(v) && v != "") v else NULL }
      withProgress(message = "Analyse ménage...", {
        hh_profile_res(calc_household_profile(d, g("hh_size"), g("hh_u5"), g("hh_5_17"), g("hh_adults"),
          g("hh_head_sex"), g("hh_head_age"), g("hh_education")))
        hh_econ_res(calc_household_economy(d, g("hh_income"), g("hh_income_src"),
          g("hh_expenditure"), g("hh_food_exp"), g("hh_livelihood")))
        hh_shelter_res(calc_shelter(d, g("hh_shelter"), g("hh_tenure")))
        hh_prot_res(calc_protection(d, g("hh_registration"), g("hh_documentation"),
          g("hh_incident"), g("hh_child_labor"), g("hh_marriage")))
      })
      showNotification("Analyse ménage terminée ✓", type = "message")
    })

    output$hh_kpi <- renderUI({ req(hh_profile_res()); HTML(render_household_kpi(hh_profile_res(), hh_econ_res())) })
    output$hh_composition_table <- DT::renderDT({
      req(hh_profile_res()); DT::datatable(hh_profile_res()$composition, options = list(dom = "t"))
    })
    output$hh_income_plot <- renderPlot({ req(hh_econ_res()); plot_income_expenditure(hh_econ_res()) })
    output$hh_livelihood_table <- DT::renderDT({
      req(hh_econ_res()); r <- hh_econ_res()$results
      if (!is.null(r$livelihood)) DT::datatable(r$livelihood, options = list(pageLength = 10))
    })
    output$hh_shelter_table <- DT::renderDT({
      req(hh_shelter_res()); r <- hh_shelter_res()$results
      if (!is.null(r$shelter_types)) DT::datatable(r$shelter_types, options = list(dom = "t"))
    })
    output$hh_tenure_table <- DT::renderDT({
      req(hh_shelter_res()); r <- hh_shelter_res()$results
      if (!is.null(r$tenure_status)) DT::datatable(r$tenure_status, options = list(dom = "t"))
    })
    output$hh_protection_plot <- renderPlot({ req(hh_prot_res()); plot_protection_indicators(hh_prot_res()) })
    output$hh_protection_table <- DT::renderDT({
      req(hh_prot_res()); DT::datatable(hh_prot_res()$summary, options = list(dom = "t"))
    })
  })
}
