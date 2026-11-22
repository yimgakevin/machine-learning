# Extrait de app_parts/ui/ui_body.R — onglet descoms
ui_tab_descoms <- function() {
      tabItem(tabName="descoms",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("stethoscope"), "Tableau descriptif — Standards OMS"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Générez un tableau récapitulatif de vos indicateurs selon les standards OMS : ",
                 "moyennes, écarts-types, médianes et intervalles de confiance, ",
                 "avec possibilité de stratifier par groupe (via gtsummary).")
        ),
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=icon("stethoscope"), " Tableau descriptif complet — Style OMS (gtsummary)",
              fluidRow(
                column(3, uiOutput("desc_by_ui")),
                column(5, uiOutput("desc_vars_ui")),
                column(2, br(),
                       checkboxInput("desc_add_p","Ajouter p-values",TRUE)),
                column(2, br(),
                       downloadButton("dl_gt","📥 Word",
                         style="background:#2E7D52;color:white;border:none;border-radius:6px;padding:6px 12px;width:100%;"))
              ),
              uiOutput("desc_gt_out")
          )
        ),
        fluidRow(
          box(width=6, status="info", solidHeader=TRUE,
              title="Matrice corrélation Spearman",
              uiOutput("desc_corr_vars_ui"),
              plotOutput("desc_corr_plot", height="400px")),
          box(width=6, status="warning", solidHeader=TRUE,
              title="Scores Z par groupe vs seuils OMS",
              uiOutput("desc_grp_ui"),
              plotlyOutput("desc_zscore_plot", height="380px"))
        ),

        # ── TABS EXPERT DESCRIPTIF ──────────────────────────────────────────────
        fluidRow(
          box(width = 12, status = "success", solidHeader = TRUE,
              title = tags$span(icon("flask"), " Analyses descriptives expert"),
              do.call(tabsetPanel, c(list(id = "desc_expert_tabs", type = "pills"), desc_expert_tabs()))
          )
        )
      )
}
