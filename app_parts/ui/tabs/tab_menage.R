# Extrait de app_parts/ui/ui_body.R — onglet menage
ui_tab_menage <- function() {
      tabItem(tabName="menage",
        fluidRow(box(width=12, status="primary", solidHeader=TRUE,
          title=tags$span(icon("house-user"), " Ménage & Protection (SENS / UNHCR)"),
          tags$div(style="background:#eaf0f8;border-left:4px solid #2c3e50;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
            tags$b("Modules :"), " Profil démographique, Économie ménage, Abri, Protection")
        )),
        fluidRow(
          column(3,
            box(width=12, title="Configuration", status="primary", solidHeader=TRUE, collapsible=TRUE,
              tags$h5("Composition"), uiOutput("hh_size_ui"), uiOutput("hh_u5_ui"),
              uiOutput("hh_5_17_ui"), uiOutput("hh_adults_ui"),
              tags$h5("Chef de ménage"), uiOutput("hh_head_sex_ui"), uiOutput("hh_head_age_ui"),
              uiOutput("hh_education_ui"),
              tags$h5("Économie"), uiOutput("hh_income_ui"), uiOutput("hh_income_src_ui"),
              uiOutput("hh_expenditure_ui"), uiOutput("hh_food_exp_ui"), uiOutput("hh_livelihood_ui"),
              tags$h5("Abri"), uiOutput("hh_shelter_ui"), uiOutput("hh_tenure_ui"),
              tags$h5("Protection"), uiOutput("hh_registration_ui"), uiOutput("hh_documentation_ui"),
              uiOutput("hh_incident_ui"), uiOutput("hh_child_labor_ui"), uiOutput("hh_marriage_ui"),
              actionButton("run_menage", "🏠  Analyser", class="btn-primary btn-block", style="margin-top:10px;")
            )
          ),
          column(9,
            tabsetPanel(id="menage_tabs",
              tabPanel("Profil", uiOutput("hh_kpi"), br(), DT::DTOutput("hh_composition_table")),
              tabPanel("Économie", plotOutput("hh_income_plot", height="400px"), DT::DTOutput("hh_livelihood_table")),
              tabPanel("Abri", DT::DTOutput("hh_shelter_table"), DT::DTOutput("hh_tenure_table")),
              tabPanel("Protection", plotOutput("hh_protection_plot", height="400px"), DT::DTOutput("hh_protection_table"))
            )
          )
        )
      )
}
