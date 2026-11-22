# Extrait de app_parts/ui/ui_body.R — onglet anemie
ui_tab_anemie <- function() {
      tabItem(tabName="anemie",
        fluidRow(box(width=12, status="warning", solidHeader=TRUE,
          title=tags$span(icon("tint"), " Anémie SENS — Hémoglobine & Classification OMS"),
          tags$div(style="background:#fef9e7;border-left:4px solid #f39c12;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
            tags$b("Standards OMS 2011 :"), " Enfants < 11 g/dL | Femmes NE < 12 g/dL | Femmes enceintes < 11 g/dL.",
            tags$br(), "Ajustements : altitude + tabac (tables OMS)")
        )),
        fluidRow(
          column(3,
            box(width=12, title="Configuration", status="warning", solidHeader=TRUE, collapsible=TRUE,
              uiOutput("anem_hb_ui"), uiOutput("anem_alt_ui"), uiOutput("anem_smoke_ui"),
              uiOutput("anem_preg_ui"),
              selectInput("anem_pop", "Population cible", choices=c(
                "Enfants 6-59 mois"="enfant_6_59", "Femmes non enceintes"="femme_np",
                "Femmes enceintes"="femme_enc", "Hommes adultes"="homme")),
              actionButton("run_anem", "🩸  Calculer", class="btn-warning btn-block", style="margin-top:10px;")
            )
          ),
          column(9,
            tabsetPanel(id="anem_tabs",
              tabPanel("Synthèse", uiOutput("anem_kpi"), br(), DT::DTOutput("anem_summary_table")),
              tabPanel("Distribution Hb", plotOutput("anem_hb_dist", height="450px")),
              tabPanel("Classification", plotly::plotlyOutput("anem_donut", height="400px"))
            )
          )
        )
      )
}
