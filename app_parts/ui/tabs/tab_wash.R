# Extrait de app_parts/ui/ui_body.R — onglet wash
ui_tab_wash <- function() {
      tabItem(tabName="wash",
        fluidRow(box(width=12, status="info", solidHeader=TRUE,
          title=tags$span(icon("shower"), " WASH — Eau, Hygiène, Assainissement (JMP / Sphère)"),
          tags$div(style="background:#eaf6fd;border-left:4px solid #3498db;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
            tags$b("Standards :"), " Classification JMP (amélioré/non amélioré), Sphère (≥ 15 L/p/j, ≤ 30 min)")
        )),
        fluidRow(
          column(3,
            box(width=12, title="Colonnes WASH", status="info", solidHeader=TRUE, collapsible=TRUE,
              uiOutput("wash_source_ui"), uiOutput("wash_time_ui"), uiOutput("wash_qty_ui"),
              uiOutput("wash_treat_ui"), uiOutput("wash_latrine_ui"), uiOutput("wash_shared_ui"),
              uiOutput("wash_oda_ui"), uiOutput("wash_hw_ui"), uiOutput("wash_soap_ui"),
              uiOutput("wash_hwsoap_ui"),
              actionButton("run_wash", "🚰  Calculer WASH", class="btn-info btn-block", style="margin-top:10px;")
            )
          ),
          column(9,
            tabsetPanel(id="wash_tabs",
              tabPanel("Synthèse", uiOutput("wash_kpi")),
              tabPanel("Échelle JMP", plotOutput("wash_ladder_plot", height="500px")),
              tabPanel("Sources d'eau", DT::DTOutput("wash_sources_table")),
              tabPanel("Traitement", DT::DTOutput("wash_treatment_table"))
            )
          )
        )
      )
}
