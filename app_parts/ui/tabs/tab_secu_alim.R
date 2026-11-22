# Extrait de app_parts/ui/ui_body.R — onglet secu_alim
ui_tab_secu_alim <- function() {
      tabItem(tabName="secu_alim",
        fluidRow(
          box(width=12, status="primary", solidHeader=TRUE,
              title=tags$span(icon("utensils"),
                " Sécurité Alimentaire Expert — FCS · rCSI · HDDS · HHS · LCSI · FIES · MDD-W"),
              tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:10px 14px;border-radius:0 8px 8px 0;margin-bottom:12px;font-size:12px;",
                tags$b("Standards PAM/FAO/IPC"), " — 7 indicateurs : Food Consumption Score (FCS), ",
                "reduced Coping Strategy Index (rCSI), Household Dietary Diversity Score (HDDS), ",
                "Household Hunger Scale (HHS), Livelihood Coping Strategies (LCSI), ",
                "Food Insecurity Experience Scale (FIES) et Minimum Dietary Diversity for Women (MDD-W). ",
                tags$br(),
                tags$b("Couche experte :"), " intervalle de confiance de Wilson, indice composite de sévérité, ",
                "verdict automatique, tests du χ² (V de Cramér), tendances inter-rounds, ",
                "estimation IPC/CH, table ménages et exports Excel/CSV."
              )
          )
        ),
        fluidRow(
          box(width=3, status="primary", solidHeader=TRUE,
              title="Configuration",
              tags$div(class="section-header","Mode de calcul"),
              radioButtons("sa_mode", NULL,
                choices = c(
                  "FCS pré-calculé (colonne existante)" = "fcs_precalc",
                  "Calculer FCS depuis les groupes alimentaires" = "fcs_calc",
                  "rCSI (Coping strategies)" = "rcsi",
                  "HDDS (Diversité alimentaire)" = "hdds",
                  "HHS (Échelle de la faim)" = "hhs",
                  "LCSI (Stratégies de subsistance)" = "lcsi",
                  "FIES (Expérience de l'insécurité)" = "fies",
                  "MDD-W (Diversité des femmes)" = "mddw",
                  "Tous les indicateurs disponibles" = "all"
                ), selected = "all"),
              hr(),

              # ── FCS pré-calculé ──
              conditionalPanel("input.sa_mode == 'fcs_precalc' || input.sa_mode == 'all'",
                tags$div(class="section-header","FCS pré-calculé"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  "Si votre jeu de données contient déjà une colonne avec le score FCS calculé, sélectionnez-la ici."
                ),
                uiOutput("sa_fcs_precalc_ui")
              ),

              # ── FCS depuis groupes ──
              conditionalPanel("input.sa_mode == 'fcs_calc' || input.sa_mode == 'all'",
                tags$div(class="section-header","Groupes FCS (jours/semaine)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Food Consumption Score (PAM)"), tags$br(),
                  "Chaque colonne = nombre de jours de consommation (0-7) par groupe alimentaire. ",
                  "Le FCS est la somme pondérée : ",
                  tags$span(style="font-family:monospace;font-size:9px;",
                    "Céréales×2 + Légumineuses×3 + Légumes×1 + Fruits×1 + Viande×4 + Lait×4 + Sucre×0.5 + Huile×0.5"),
                  tags$br(),
                  tags$span(style="color:#0077B6;", "Seuils : Pauvre <21 | Limite 21-35 | Acceptable >35"),
                  tags$br(), "Minimum 4 groupes requis."
                ),
                uiOutput("sa_fcs_cereales_ui"),
                uiOutput("sa_fcs_legumineuses_ui"),
                uiOutput("sa_fcs_legumes_ui"),
                uiOutput("sa_fcs_fruits_ui"),
                uiOutput("sa_fcs_viande_ui"),
                uiOutput("sa_fcs_lait_ui"),
                uiOutput("sa_fcs_sucre_ui"),
                uiOutput("sa_fcs_huile_ui")
              ),

              # ── rCSI ──
              conditionalPanel("input.sa_mode == 'rcsi' || input.sa_mode == 'all'",
                tags$div(class="section-header","rCSI (jours/semaine)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("reduced Coping Strategy Index (PAM)"), tags$br(),
                  "Chaque colonne = nombre de jours (0-7) où le ménage a eu recours à cette stratégie. ",
                  "Le rCSI est la somme pondérée : ",
                  tags$span(style="font-family:monospace;font-size:9px;",
                    "Moins préférés×1 + Emprunter×2 + Réduire repas×1 + Réduire portions×1 + Restreindre adultes×3"),
                  tags$br(),
                  tags$span(style="color:#0077B6;", "Seuils : Faible 0-3 | Moyen 4-18 | Élevé ≥19"),
                  tags$br(), "Minimum 3 stratégies requises.",
                  tags$br(), tags$b("Option :"), " score rCSI déjà calculé."
                ),
                uiOutput("sa_rcsi_precalc_ui"),
                uiOutput("sa_rcsi_prefere_ui"),
                uiOutput("sa_rcsi_emprunter_ui"),
                uiOutput("sa_rcsi_repas_ui"),
                uiOutput("sa_rcsi_portions_ui"),
                uiOutput("sa_rcsi_adultes_ui")
              ),

              # ── HDDS ──
              conditionalPanel("input.sa_mode == 'hdds' || input.sa_mode == 'all'",
                tags$div(class="section-header","HDDS (groupes 0/1)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Household Dietary Diversity Score (FAO)"), tags$br(),
                  "Chaque colonne = variable binaire (0/1) indiquant si le ménage a consommé ce groupe ",
                  "alimentaire dans les dernières 24h. Le HDDS = somme des groupes consommés (0-12). ",
                  tags$br(),
                  tags$span(style="color:#0077B6;", "Seuils : Faible 0-3 | Moyen 4-5 | Élevé ≥6"),
                  tags$br(), "Cochez au minimum 4 colonnes binaires.",
                  tags$br(), tags$b("Option :"), " score HDDS déjà calculé (utilisé en priorité)."
                ),
                uiOutput("sa_hdds_precalc_ui"),
                uiOutput("sa_hdds_cols_ui")
              ),

              # ── HHS ──
              conditionalPanel("input.sa_mode == 'hhs' || input.sa_mode == 'all'",
                tags$div(class="section-header","HHS (fréquence 0-2)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Household Hunger Scale (FAO/USAID)"), tags$br(),
                  "3 questions sur l'expérience de la faim au cours des 30 derniers jours. ",
                  "Chaque réponse est codée : 0 = Jamais, 1 = Rarement/Parfois, 2 = Souvent. ",
                  "Le score HHS = somme des 3 réponses (0-6). ",
                  tags$br(),
                  tags$span(style="color:#0077B6;",
                    "Seuils : Aucune faim 0-1 | Faim modérée 2-3 | Faim sévère 4-6"),
                  tags$br(), "Minimum 2 questions requises.",
                  tags$br(), tags$b("Option :"), " score HHS déjà calculé (utilisé en priorité)."
                ),
                uiOutput("sa_hhs_precalc_ui"),
                uiOutput("sa_hhs_nofood_ui"),
                uiOutput("sa_hhs_sleep_ui"),
                uiOutput("sa_hhs_wholeday_ui")
              ),

              # ── LCSI ──
              conditionalPanel("input.sa_mode == 'lcsi' || input.sa_mode == 'all'",
                tags$div(class="section-header","LCSI (stratégies de subsistance)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Livelihood Coping Strategy Index (PAM)"), tags$br(),
                  "Sélectionnez une classe pré-calculée, ou les 3 familles de stratégies (oui/non). ",
                  tags$span(style="color:#0077B6;", "Catégories : Aucune · Stress · Crise · Urgence")
                ),
                uiOutput("sa_lcsi_class_ui"),
                uiOutput("sa_lcsi_stress_ui"),
                uiOutput("sa_lcsi_crisis_ui"),
                uiOutput("sa_lcsi_emerg_ui")
              ),

              # ── FIES ──
              conditionalPanel("input.sa_mode == 'fies' || input.sa_mode == 'all'",
                tags$div(class="section-header","FIES (score 0-8)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Food Insecurity Experience Scale (FAO)"), tags$br(),
                  "Colonne du score FIES (somme des 8 questions d'expérience). ",
                  tags$span(style="color:#0077B6;",
                    "Seuils : légère 1-3 | modérée 4-6 | sévère 7-8")
                ),
                uiOutput("sa_fies_col_ui")
              ),

              # ── MDD-W ──
              conditionalPanel("input.sa_mode == 'mddw' || input.sa_mode == 'all'",
                tags$div(class="section-header","MDD-W (binaire oui/non)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Minimum Dietary Diversity for Women (FAO/OMS)"), tags$br(),
                  "Colonne indiquant si la femme a consommé ≥ 5 groupes alimentaires sur 24 h. ",
                  tags$span(style="color:#0077B6;", "Seuil OMS : ≥ 5 groupes = adéquat")
                ),
                uiOutput("sa_mddw_col_ui")
              ),
              hr(),
              tags$div(class="section-header","Stratification"),
              uiOutput("sa_strat_ui"),
              hr(),
              actionButton("run_sa", "🍽️ Calculer les indicateurs",
                           class="btn btn-oms btn-block", width="100%",
                           style="font-size:14px;padding:10px;margin-top:8px;")
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Résultats — Sécurité Alimentaire",
              tabsetPanel(
                tabPanel(tags$span(icon("tachometer-alt"), " Synthèse"),
                  uiOutput("sa_verdict"),
                  br(),
                  uiOutput("sa_summary_cards"),
                  fluidRow(
                    column(7,
                      tags$div(class="section-header", "Indice composite de sévérité"),
                      plotlyOutput("sa_severity_plot", height="260px")
                    ),
                    column(5,
                      tags$div(class="section-header", "Lecture experte"),
                      uiOutput("sa_synthesis_reading")
                    )
                  ),
                  br(),
                  uiOutput("sa_synthesis_details")
                ),
                tabPanel(tags$span(icon("chart-pie"), " FCS"),
                  fluidRow(
                    column(5, plotlyOutput("sa_fcs_donut", height="300px")),
                    column(7, plotlyOutput("sa_fcs_hist", height="300px"))
                  ),
                  DTOutput("sa_fcs_stats"),
                  uiOutput("sa_fcs_reading")
                ),
                tabPanel(tags$span(icon("exclamation-triangle"), " rCSI"),
                  fluidRow(
                    column(5, plotlyOutput("sa_rcsi_donut", height="300px")),
                    column(7, plotlyOutput("sa_rcsi_hist", height="300px"))
                  ),
                  DTOutput("sa_rcsi_stats"),
                  uiOutput("sa_rcsi_reading")
                ),
                tabPanel(tags$span(icon("seedling"), " HDDS"),
                  fluidRow(
                    column(5, plotlyOutput("sa_hdds_donut", height="300px")),
                    column(7, plotlyOutput("sa_hdds_hist", height="300px"))
                  ),
                  DTOutput("sa_hdds_groups"),
                  uiOutput("sa_hdds_reading")
                ),
                tabPanel(tags$span(icon("heartbeat"), " HHS"),
                  fluidRow(
                    column(5, plotlyOutput("sa_hhs_donut", height="300px")),
                    column(7, plotlyOutput("sa_hhs_hist", height="300px"))
                  ),
                  DTOutput("sa_hhs_stats"),
                  uiOutput("sa_hhs_reading")
                ),
                tabPanel(tags$span(icon("layer-group"), " Par strate"),
                  plotlyOutput("sa_strat_plot", height="420px"),
                  DTOutput("sa_strat_table"),
                  uiOutput("sa_strat_reading")
                ),

                # ── NOUVEAU : Composition FCS ──
                tabPanel(tags$span(icon("chart-bar"), " Composition FCS"),
                  br(),
                  tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:8px 12px;border-radius:0 8px 8px 0;font-size:11px;margin-bottom:10px;",
                    icon("info-circle"), " Contribution de chaque groupe alimentaire au score FCS moyen, pondérée par les poids PAM."
                  ),
                  fluidRow(
                    column(8, plotlyOutput("sa_fcs_composition_bar", height="340px")),
                    column(4, plotlyOutput("sa_radar", height="340px"))
                  ),
                  br(),
                  DTOutput("sa_fcs_composition_table"),
                  uiOutput("sa_fcs_comp_reading")
                ),

                # ── NOUVEAU : rCSI Détail par stratégie ──
                tabPanel(tags$span(icon("exclamation-circle"), " rCSI Détail"),
                  br(),
                  tags$div(style="background:#fff3e0;border-left:4px solid #E67E22;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:10px;",
                    icon("exclamation-triangle", style="color:#E67E22;"),
                    " Analyse du recours à chaque stratégie d'adaptation : fréquence, poids de sévérité et contribution au score rCSI."
                  ),
                  plotlyOutput("sa_rcsi_components_plot", height="380px"),
                  br(),
                  DTOutput("sa_rcsi_components_table"),
                  uiOutput("sa_rcsi_comp_reading")
                ),

                # ── NOUVEAU : Phases IPC/CH ──
                tabPanel(tags$span(icon("globe"), " Phases IPC/CH"),
                  br(),
                  tags$div(style="background:#fff8e1;border-left:4px solid #FBC02D;padding:10px 14px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:12px;",
                    tags$b(icon("exclamation-triangle", style="color:#FBC02D;"), " Estimation IPC/Cadre Harmonisé"),
                    tags$br(),
                    "Classification approximative basée sur FCS, rCSI et HHS disponibles. ",
                    tags$b("Une triangulation terrain avec d'autres sources (prévalences, marchés, accès) est indispensable pour une classification IPC officielle.")
                  ),
                  fluidRow(
                    column(4, uiOutput("sa_ipc_kpi_cards")),
                    column(8, plotlyOutput("sa_ipc_phases_plot", height="320px"))
                  ),
                  br(),
                  fluidRow(
                    column(6,
                      selectInput("sa_ipc_group_var", "Grouper par :",
                        choices = c("Global" = "", "Région" = "region",
                                    "Département" = "departement", "Round" = "survey_round",
                                    "Sexe" = "sexe"),
                        selected = "", width = "100%")
                    )
                  ),
                  plotlyOutput("sa_ipc_by_group_plot", height="340px"),
                  br(),
                  DTOutput("sa_ipc_table"),
                  uiOutput("sa_ipc_reading")
                ),

                # ── NOUVEAU : Comparaison inter-rounds ──
                tabPanel(tags$span(icon("calendar-alt"), " Comparaison Rounds"),
                  br(),
                  tags$div(style="background:#f0fff4;border-left:4px solid #27AE60;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:10px;",
                    icon("chart-line", style="color:#27AE60;"),
                    " Évolution des indicateurs de sécurité alimentaire entre les rounds de collecte. Les rubans représentent l'IC à 95%, complétés par un test de comparaison 1er/dernier round."
                  ),
                  plotlyOutput("sa_rounds_plot", height="380px"),
                  br(),
                  DTOutput("sa_rounds_table"),
                  uiOutput("sa_rounds_reading")
                ),

                # ── NOUVEAU : Indicateurs OMS/FAO avancés ──
                tabPanel(tags$span(icon("globe-africa"), " Indicateurs OMS"),
                  br(),
                  tags$div(style="background:#eef6ff;border-left:4px solid #0077B6;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:10px;",
                    icon("award", style="color:#0077B6;"),
                    " LCSI (PAM), FIES (FAO) et MDD-W (FAO/OMS 2021) — indicateurs complémentaires du cadre IPC et des enquêtes SMART."
                  ),
                  fluidRow(
                    column(8, plotlyOutput("sa_lcsi_plot", height="280px")),
                    column(4, plotlyOutput("sa_mddw_plot", height="280px"))
                  ),
                  DTOutput("sa_lcsi_table"),
                  br(),
                  plotlyOutput("sa_fies_plot", height="300px"),
                  DTOutput("sa_fies_table"),
                  br(),
                  uiOutput("sa_oms_reading")
                ),

                # ── NOUVEAU : Table ménages ──
                tabPanel(tags$span(icon("table"), " Ménages"),
                  br(),
                  tags$div(style="background:#f8f9fa;border-left:4px solid #0B3954;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:10px;",
                    icon("users", style="color:#0B3954;"),
                    " Résultats individuels par ménage : scores, classifications, indice de sévérité et phase IPC estimée. Table filtrable et exportable."
                  ),
                  DTOutput("sa_household_table"),
                  br(),
                  downloadButton("sa_dl_csv_hh", "Télécharger la table ménages (CSV)",
                                 class="btn btn-sm btn-default")
                ),

                # ── NOUVEAU : Export ──
                tabPanel(tags$span(icon("file-export"), " Export"),
                  br(),
                  tags$div(style="background:#fffef5;border-left:4px solid #D4A843;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:12px;",
                    icon("file-excel", style="color:#D4A843;"),
                    " Exportez l'ensemble des résultats : synthèse, indicateurs détaillés, indice de sévérité, phases IPC, verdict et table ménages."
                  ),
                  fluidRow(
                    column(4, downloadButton("sa_dl_xlsx", "Classeur Excel complet (.xlsx)",
                                             class="btn btn-success btn-block")),
                    column(4, downloadButton("sa_dl_csv_summary", "Synthèse indicateurs (.csv)",
                                             class="btn btn-default btn-block")),
                    column(4, downloadButton("sa_dl_csv_hh2", "Table ménages (.csv)",
                                             class="btn btn-default btn-block"))
                  ),
                  br(),
                  uiOutput("sa_export_info")
                )
              )
          )
        )
      )
}
