# Extrait de app_parts/ui/ui_body.R — onglet handicap
ui_tab_handicap <- function() {
      tabItem(tabName="handicap",
        fluidRow(box(width=12, status="warning", solidHeader=TRUE,
          title=tags$span(icon("wheelchair"), " Handicap — Washington Group Short Set (6 domaines)"),
          tags$div(style="background:#fef9e7;border-left:4px solid #e67e22;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
            tags$b("Domaines :"), " Vision, Audition, Mobilité, Cognition, Soins personnels, Communication.",
            tags$br(), "Seuil OMS : ≥ 'Beaucoup de difficultés' dans au moins 1 domaine")
        )),
        fluidRow(
          column(3,
            box(width=12, title="Colonnes WG", status="warning", solidHeader=TRUE, collapsible=TRUE,
              uiOutput("wg_voir_ui"), uiOutput("wg_entendre_ui"), uiOutput("wg_marcher_ui"),
              uiOutput("wg_memoire_ui"), uiOutput("wg_soins_ui"), uiOutput("wg_comm_ui"),
              sliderInput("wg_threshold", "Seuil de sévérité", min=2, max=4, value=3, step=1),
              actionButton("run_wg", "♿  Calculer", class="btn-warning btn-block", style="margin-top:10px;")
            )
          ),
          column(9,
            tabsetPanel(id="wg_tabs",
              tabPanel("Synthèse", uiOutput("wg_kpi"), br(), DT::DTOutput("wg_domain_table")),
              tabPanel("Par domaine", plotOutput("wg_domain_plot", height="450px")),
              tabPanel("Sévérité", plotOutput("wg_severity_plot", height="500px"))
            )
          )
        )
      )
}
