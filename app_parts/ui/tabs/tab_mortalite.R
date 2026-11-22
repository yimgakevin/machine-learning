# Extrait de app_parts/ui/ui_body.R — onglet mortalite
ui_tab_mortalite <- function() {
      tabItem(tabName="mortalite",
        fluidRow(box(width=12, status="danger", solidHeader=TRUE,
          title=tags$span(icon("skull-crossbones"), " Mortalité SMART — CMR / U5MR (Standards SMART v2 / Sphère)"),
          tags$div(style="background:#fdf2f2;border-left:4px solid #e74c3c;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
            tags$b("Indicateurs :"), " Taux de mortalité brut (CMR) et < 5 ans (U5MR) — Seuils Sphère.",
            tags$br(), "CMR ≥ 1/10 000/jour = urgence | U5MR ≥ 2/10 000/jour = urgence")
        )),
        fluidRow(
          column(3,
            box(width=12, title="Configuration", status="warning", solidHeader=TRUE, collapsible=TRUE,
              uiOutput("mort_deaths_ui"), uiOutput("mort_pop_start_ui"), uiOutput("mort_pop_end_ui"),
              uiOutput("mort_u5_pop_ui"), uiOutput("mort_age_dec_ui"), uiOutput("mort_cause_ui"),
              uiOutput("mort_sex_dec_ui"), uiOutput("mort_recall_ui"),
              numericInput("mort_recall_days", "Jours de rappel (défaut)", 90, min=30, max=365),
              actionButton("run_mort", "💀  Calculer", class="btn-danger btn-block", style="margin-top:10px;")
            )
          ),
          column(9,
            tabsetPanel(id="mort_tabs",
              tabPanel("Synthèse", uiOutput("mort_kpi"), br(), DT::DTOutput("mort_summary_table")),
              tabPanel("Causes", plotOutput("mort_cause_plot", height="450px"), DT::DTOutput("mort_cause_table")),
              tabPanel("Pyramide", plotOutput("mort_pyramid_plot", height="500px"))
            )
          )
        )
      )
}
