# Extrait de app_parts/ui/ui_body.R — onglet oms
ui_tab_oms <- function() {
      tabItem(tabName="oms",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("heartbeat"), "Indicateurs OMS / PAM / UNICEF"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Calculez les prévalences nutritionnelles standardisées avec intervalles de confiance à 95% (Wilson) : ",
                 "GAM, SAM, retard de croissance, insuffisance pondérale. ",
                 "Stratification possible par groupe selon les normes OMS 2006, PAM et UNICEF.")
        ),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=icon("heartbeat"), " Prévalences avec IC 95% (Wilson) — Normes OMS 2006 · PAM · UNICEF",
              fluidRow(
                column(3,
                  checkboxGroupInput("oms_groupes","Stratifications :",
                    choices=c("Global","Région","Sexe","Groupe d'âge","Déplacement","FCS"),
                    selected=c("Global","Région"))
                ),
                column(9,
                  DTOutput("oms_table"),
                  br(),
                  downloadButton("dl_oms_csv","📥 Télécharger",
                    style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:6px 14px;")
                )
              )
          )
        ),
        fluidRow(
          box(width=8, status="info", solidHeader=TRUE,
              title="Forest plot — Prévalences IC 95% toutes stratifications",
              plotlyOutput("oms_forest", height="450px")),
          box(width=4, status="warning", solidHeader=TRUE,
              title="Jauges OMS",
              plotlyOutput("oms_gauge_gam", height="200px"),
              plotlyOutput("oms_gauge_stunt", height="200px"))
        ),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title="Scores Z — Comparaison à la distribution de référence OMS N(0,1)",
              plotlyOutput("oms_zscore_dist", height="300px"))
        )
      )
}
