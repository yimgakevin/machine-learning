# Extrait de app_parts/ui/ui_body.R — onglet explor
ui_tab_explor <- function() {
      tabItem(tabName="explor",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("search"), "Exploration interactive des données"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Découvrez la structure de vos données : aperçu des premières lignes, types de variables, ",
                 "valeurs manquantes et résumés statistiques. ",
                 tags$b("Point de départ recommandé"), " avant toute analyse.")
        ),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=icon("search"), " Exploration interactive des données",
              fluidRow(
                column(5, uiOutput("explor_cols_ui")),
                column(2, numericInput("explor_n","Lignes/page:",15,5,200,5)),
                column(2, br(),
                       downloadButton("dl_csv","📥 CSV",
                                      style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:6px 12px;width:100%;")),
                column(2, br(),
                       downloadButton("dl_xlsx","📥 Excel",
                                      style="background:#2E7D52;color:white;border:none;border-radius:6px;padding:6px 12px;width:100%;"))
              ),
              br(),
              DTOutput("explor_table")
          )
        ),
        fluidRow(
          box(width=4, status="info", solidHeader=TRUE,
              title=icon("calculator"), " Statistiques complètes",
              selectInput("explor_var","Variable :",choices=NULL,width="100%"),
              div(style="max-height:400px;overflow-y:auto;",
                  tableOutput("explor_stats"))
          ),
          box(width=8, status="info", solidHeader=TRUE,
              title=icon("chart-bar"), " Graphique automatique selon le type de variable",
              plotlyOutput("explor_auto_plot", height="360px")
          )
        )
      )
}
