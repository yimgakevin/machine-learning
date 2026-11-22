# Extrait de app_parts/ui/ui_body.R — onglet qualite
ui_tab_qualite <- function() {
      tabItem(tabName="qualite",
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=tags$span(icon("check-double"),
                " Contrôle Qualité SMART — Plausibility Check (style ENA)"),
              tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;margin-bottom:12px;font-size:12px;",
                tags$b("Méthodologie SMART/ENA"), " — Ce module vérifie automatiquement la qualité des données anthropométriques ",
                "selon les critères standardisés utilisés par l'OMS, l'UNICEF et le PAM. ",
                "Score de plausibilité : 0-9 Excellent | 10-14 Bon | 15-24 Acceptable | ≥25 Problématique."
              )
          )
        ),
        fluidRow(
          box(width=3, status="primary", solidHeader=TRUE,
              title="Paramètres",
              tags$div(class="section-header","Variables anthropométriques"),
              uiOutput("sq_col_whz_ui"),
              uiOutput("sq_col_haz_ui"),
              uiOutput("sq_col_waz_ui"),
              uiOutput("sq_col_muac_ui"),
              hr(),
              tags$div(class="section-header","Variables contextuelles"),
              uiOutput("sq_col_sex_ui"),
              uiOutput("sq_col_age_ui"),
              uiOutput("sq_col_weight_ui"),
              uiOutput("sq_col_height_ui"),
              uiOutput("sq_col_team_ui"),
              hr(),
              actionButton("run_qualite", "✅ Lancer le contrôle qualité",
                           class="btn btn-oms btn-block", width="100%",
                           style="font-size:14px;padding:10px;margin-top:8px;")
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Résultats du contrôle qualité",
              tabsetPanel(
                tabPanel(tags$span(icon("trophy"), " Score de plausibilité"),
                  uiOutput("sq_plausibility_html")
                ),

                tabPanel(tags$span(icon("flag"), " SMART Flags"),
                  DTOutput("sq_flags_table"),
                  br(),
                  plotlyOutput("sq_flags_plot", height="350px")
                ),

                tabPanel(tags$span(icon("hashtag"), " Digit Preference"),
                  fluidRow(
                    column(6,
                      uiOutput("sq_dp_var_ui"),
                      DTOutput("sq_dp_table")
                    ),
                    column(6,
                      plotlyOutput("sq_dp_plot", height="320px")
                    )
                  )
                ),

                tabPanel(tags$span(icon("child"), " Age Heaping"),
                  fluidRow(
                    column(4, uiOutput("sq_age_summary")),
                    column(8, plotlyOutput("sq_age_plot", height="350px"))
                  )
                ),

                tabPanel(tags$span(icon("users"), " SD par équipe"),
                  DTOutput("sq_team_table"),
                  br(),
                  plotlyOutput("sq_team_plot", height="350px")
                ),

                tabPanel(tags$span(icon("tasks"), " Complétude"),
                  DTOutput("sq_completude_table")
                )
              )
          )
        )
      )
}
