# Extrait de app_parts/ui/ui_body.R — onglet iycf
ui_tab_iycf <- function() {
      tabItem(tabName="iycf",
        fluidRow(box(width=12, status="success", solidHeader=TRUE,
          title=tags$span(icon("baby"), " IYCF — Alimentation du Nourrisson et du Jeune Enfant (OMS/UNICEF 2021)"),
          tags$div(style="background:#f0faf0;border-left:4px solid #27ae60;padding:12px 16px;border-radius:0 8px 8px 0;font-size:12px;",
            tags$b("14 indicateurs experts :"), " EBF, EIBF, CBF, ISF, MDD, MMF, MAD, Fer, Vitamine A, ANC 4+, Biberon, Boissons sucrées, Ultra-transformés, Zéro légumes/fruits.",
            tags$br(), "IC 95 % design-based ou Wilson, cibles OMS/UNICEF, verdict composite, qualité/mapping, χ²/V de Cramér, équité, tendances inter-rounds, table enfants et exports.")
        )),
        fluidRow(
          column(3,
            box(width=12, title="Colonnes IYCF", status="success", solidHeader=TRUE, collapsible=TRUE,
              uiOutput("iycf_age_ui"),
              uiOutput("iycf_bfstatus_ui"),
              checkboxInput("iycf_use_design", "Utiliser le plan de sondage", value = TRUE),
              uiOutput("iycf_cluster_ui"),
              uiOutput("iycf_strata_ui"),
              uiOutput("iycf_weights_ui"),
              tags$hr(style="margin:6px 0;border-color:#e8f5e9;"),
              uiOutput("iycf_ebf_ui"), uiOutput("iycf_cbf_ui"),
              uiOutput("iycf_eibf_ui"), uiOutput("iycf_isf_ui"), uiOutput("iycf_mdd_ui"),
              uiOutput("iycf_mmf_ui"), uiOutput("iycf_mad_ui"), uiOutput("iycf_iron_ui"),
              uiOutput("iycf_vita_ui"), uiOutput("iycf_anc_ui"),
              tags$hr(style="margin:6px 0;border-color:#e8f5e9;"),
              uiOutput("iycf_bottle_ui"), uiOutput("iycf_sweet_ui"), uiOutput("iycf_upt_ui"),
              uiOutput("iycf_zvf_ui"),
              tags$hr(style="margin:6px 0;border-color:#e8f5e9;"),
              uiOutput("iycf_focus_ui"), uiOutput("iycf_stratvar_ui"),
              actionButton("run_iycf", "👶  Calculer IYCF", class="btn-success btn-block", style="margin-top:10px;")
            )
          ),
          column(9,
            tabsetPanel(id="iycf_tabs",
              tabPanel("Synthèse",
                uiOutput("iycf_verdict"),
                br(),
                uiOutput("iycf_kpi"),
                br(),
                fluidRow(
                  column(5,
                    tags$div(class="section-header", "Indice d'atteinte des cibles"),
                    plotlyOutput("iycf_gauge", height="260px")
                  ),
                  column(7,
                    tags$div(class="section-header", "Lecture experte"),
                    uiOutput("iycf_reading_synth")
                  )
                ),
                br(),
                DT::DTOutput("iycf_summary_table")
              ),
              tabPanel("Dashboard",
                plotOutput("iycf_dashboard_plot", height="550px"), br(),
                uiOutput("iycf_reading_dash")
              ),
              tabPanel("Allaitement",
                plotOutput("iycf_bf_cascade_plot", height="400px"), br(),
                uiOutput("iycf_reading_bf")
              ),
              tabPanel("Alimentation",
                plotlyOutput("iycf_diet_plot", height="380px"), br(),
                uiOutput("iycf_reading_diet")
              ),
              tabPanel("Risques",
                plotlyOutput("iycf_risk_plot", height="340px"), br(),
                uiOutput("iycf_reading_risks")
              ),
              tabPanel("Écarts vs cibles",
                plotlyOutput("iycf_gap_plot", height="420px"), br(),
                DT::DTOutput("iycf_gap_table"), br(),
                uiOutput("iycf_reading_gaps")
              ),
              tabPanel("Par strate",
                plotlyOutput("iycf_strat_plot", height="400px"), br(),
                uiOutput("iycf_strat_test_out"), br(),
                DT::DTOutput("iycf_strat_table"), br(),
                uiOutput("iycf_reading_strat")
              ),
              tabPanel("Évolution",
                plotlyOutput("iycf_rounds_plot", height="400px"), br(),
                DT::DTOutput("iycf_rounds_table"), br(),
                uiOutput("iycf_reading_rounds")
              ),
              tabPanel("Qualité & mapping",
                uiOutput("iycf_quality_summary"), br(),
                tags$div(class="section-header", "Contrôles qualité"),
                DT::DTOutput("iycf_quality_controls"), br(),
                tags$div(class="section-header", "Qualité par indicateur"),
                DT::DTOutput("iycf_quality_indicators"), br(),
                tags$div(class="section-header", "Mapping des colonnes"),
                DT::DTOutput("iycf_mapping_table"), br(),
                uiOutput("iycf_reading_quality")
              ),
              tabPanel("Profil âge",
                plotOutput("iycf_age_plot", height="420px"), br(),
                DT::DTOutput("iycf_age_table"), br(),
                uiOutput("iycf_reading_age")
              ),
              tabPanel("Équité",
                uiOutput("iycf_equity_var_ui"), br(),
                plotlyOutput("iycf_equity_plot", height="420px"), br(),
                DT::DTOutput("iycf_equity_table"), br(),
                uiOutput("iycf_reading_equity")
              ),
              tabPanel("Enfants",
                tags$div(style="font-size:12px;color:#555;background:#f8f9fa;padding:8px 12px;border-radius:6px;margin-bottom:8px;",
                  icon("circle-info"), " Une ligne par enfant ; colonnes Oui/Non = indicateur atteint ou non. Filtres actifs en haut de table."),
                DT::DTOutput("iycf_child_table"), br(),
                downloadButton("iycf_dl_csv_child", "Télécharger la table enfants (CSV)")
              ),
              tabPanel("Export",
                tags$div(style="font-size:12px;color:#555;",
                  tags$b("Classeur Excel :"), " toutes les feuilles (synthèse, allaitement, alimentation, risques, écarts, strate, évolution, enfants, verdict)."),
                br(),
                downloadButton("iycf_dl_xlsx", "Excel (xlsx)"),
                downloadButton("iycf_dl_csv_summary", "Synthèse (CSV)"),
                br(), br(),
                uiOutput("iycf_export_info")
              )
            )
          )
        )
      )
}
