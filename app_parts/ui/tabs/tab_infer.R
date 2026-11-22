# Extrait de app_parts/ui/ui_body.R — onglet infer
ui_tab_infer <- function() {
      tabItem(tabName="infer",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("flask"), "Statistiques inférentielles"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Vérifiez si les différences observées sont statistiquement significatives : ",
                 "t-test, ANOVA, Chi², corrélations de Pearson/Spearman. ",
                 "Les hypothèses de chaque test sont vérifiées automatiquement.")
        ),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=tags$span(icon("flask"), " Tests statistiques — Tous les tests à la fois"),
              tabsetPanel(

            tabPanel(tags$span(icon("vials"), " Tests bivariés"),
              fluidRow(
                column(3,
                  tags$div(style="padding:8px;",
                    tags$div(class="section-header","Variables à comparer"),
                    uiOutput("infer_x_ui"),
                    uiOutput("infer_y_ui"),
                    hr(),
                    sliderInput("infer_alpha", "Seuil de significativité (α) :",
                                min = 0.01, max = 0.10, value = 0.05, step = 0.01, width = "100%"),
                    actionButton("run_infer", "🧪 Lancer TOUS les tests",
                                 class="btn btn-oms btn-block", width="100%",
                                 style="margin-top:8px;font-size:14px;padding:10px;"),
                    hr(),
                    uiOutput("infer_type_badge"),
                    hr(),
                    tags$div(class="section-header","Légende"),
                    tags$div(style="font-size:11px;color:#555;line-height:1.8;",
                      tags$div("✅ Condition remplie → Test valide"),
                      tags$div("❌ Condition non remplie → Test non applicable"),
                      tags$div("🔴 p < α → Résultat significatif"),
                      tags$div("🟢 p ≥ α → Résultat non significatif")
                    )
                  )
                ),
                column(9,
                  tags$div(style="padding:4px;",
                    tags$div(class="section-header","Tableau récapitulatif"),
                    DTOutput("infer_summary_table"),
                    br(),
                    tags$div(class="section-header","Détail de chaque test"),
                    uiOutput("infer_all_cards"),
                    br(),
                    tags$div(class="section-header","Graphique"),
                    plotlyOutput("infer_plot", height="420px")
                  )
                )
              )
            ),

            tabPanel(tags$span(icon("project-diagram"), " Régression logistique"),
              fluidRow(
                column(4,
                  uiOutput("multi_outcome_ui"),
                  uiOutput("multi_vars_ui"),
                  verbatimTextOutput("multi_summary")
                ),
                column(8,
                  plotlyOutput("multi_forest", height="420px"))
              )
            )
          )
          )
        ),

        # ── TABS EXPERT INFÉRENTIEL ─────────────────────────────────────────────
        fluidRow(
          box(width = 12, status = "danger", solidHeader = TRUE,
              title = tags$span(icon("brain"), " Statistiques inférentielles expert"),
              do.call(tabsetPanel, c(list(id = "infer_expert_tabs", type = "pills"), infer_expert_tabs()))
          )
        )
      )
}
