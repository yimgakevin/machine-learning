# Module Shiny — Sécurité alimentaire (FCS, rCSI, HDDS, HHS, LCSI, FIES, MDD-W)
# UI et serveur namespacés (ns). Les réactifs partagés sont passés en arguments.

mod_secu_alim_ui <- function(id) {
  ns <- shiny::NS(id)

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
              radioButtons(ns("sa_mode"), NULL,
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
              conditionalPanel(ns = ns, "input.sa_mode == 'fcs_precalc' || input.sa_mode == 'all'",
                tags$div(class="section-header","FCS pré-calculé"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  "Si votre jeu de données contient déjà une colonne avec le score FCS calculé, sélectionnez-la ici."
                ),
                uiOutput(ns("sa_fcs_precalc_ui"))
              ),

              # ── FCS depuis groupes ──
              conditionalPanel(ns = ns, "input.sa_mode == 'fcs_calc' || input.sa_mode == 'all'",
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
                uiOutput(ns("sa_fcs_cereales_ui")),
                uiOutput(ns("sa_fcs_legumineuses_ui")),
                uiOutput(ns("sa_fcs_legumes_ui")),
                uiOutput(ns("sa_fcs_fruits_ui")),
                uiOutput(ns("sa_fcs_viande_ui")),
                uiOutput(ns("sa_fcs_lait_ui")),
                uiOutput(ns("sa_fcs_sucre_ui")),
                uiOutput(ns("sa_fcs_huile_ui"))
              ),

              # ── rCSI ──
              conditionalPanel(ns = ns, "input.sa_mode == 'rcsi' || input.sa_mode == 'all'",
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
                uiOutput(ns("sa_rcsi_precalc_ui")),
                uiOutput(ns("sa_rcsi_prefere_ui")),
                uiOutput(ns("sa_rcsi_emprunter_ui")),
                uiOutput(ns("sa_rcsi_repas_ui")),
                uiOutput(ns("sa_rcsi_portions_ui")),
                uiOutput(ns("sa_rcsi_adultes_ui"))
              ),

              # ── HDDS ──
              conditionalPanel(ns = ns, "input.sa_mode == 'hdds' || input.sa_mode == 'all'",
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
                uiOutput(ns("sa_hdds_precalc_ui")),
                uiOutput(ns("sa_hdds_cols_ui"))
              ),

              # ── HHS ──
              conditionalPanel(ns = ns, "input.sa_mode == 'hhs' || input.sa_mode == 'all'",
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
                uiOutput(ns("sa_hhs_precalc_ui")),
                uiOutput(ns("sa_hhs_nofood_ui")),
                uiOutput(ns("sa_hhs_sleep_ui")),
                uiOutput(ns("sa_hhs_wholeday_ui"))
              ),

              # ── LCSI ──
              conditionalPanel(ns = ns, "input.sa_mode == 'lcsi' || input.sa_mode == 'all'",
                tags$div(class="section-header","LCSI (stratégies de subsistance)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Livelihood Coping Strategy Index (PAM)"), tags$br(),
                  "Sélectionnez une classe pré-calculée, ou les 3 familles de stratégies (oui/non). ",
                  tags$span(style="color:#0077B6;", "Catégories : Aucune · Stress · Crise · Urgence")
                ),
                uiOutput(ns("sa_lcsi_class_ui")),
                uiOutput(ns("sa_lcsi_stress_ui")),
                uiOutput(ns("sa_lcsi_crisis_ui")),
                uiOutput(ns("sa_lcsi_emerg_ui"))
              ),

              # ── FIES ──
              conditionalPanel(ns = ns, "input.sa_mode == 'fies' || input.sa_mode == 'all'",
                tags$div(class="section-header","FIES (score 0-8)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Food Insecurity Experience Scale (FAO)"), tags$br(),
                  "Colonne du score FIES (somme des 8 questions d'expérience). ",
                  tags$span(style="color:#0077B6;",
                    "Seuils : légère 1-3 | modérée 4-6 | sévère 7-8")
                ),
                uiOutput(ns("sa_fies_col_ui"))
              ),

              # ── MDD-W ──
              conditionalPanel(ns = ns, "input.sa_mode == 'mddw' || input.sa_mode == 'all'",
                tags$div(class="section-header","MDD-W (binaire oui/non)"),
                tags$div(style="font-size:10px;color:#555;margin-bottom:6px;line-height:1.4;",
                  tags$b("Minimum Dietary Diversity for Women (FAO/OMS)"), tags$br(),
                  "Colonne indiquant si la femme a consommé ≥ 5 groupes alimentaires sur 24 h. ",
                  tags$span(style="color:#0077B6;", "Seuil OMS : ≥ 5 groupes = adéquat")
                ),
                uiOutput(ns("sa_mddw_col_ui"))
              ),
              hr(),
              tags$div(class="section-header","Stratification"),
              uiOutput(ns("sa_strat_ui")),
              hr(),
              actionButton(ns("run_sa"), "🍽️ Calculer les indicateurs",
                           class="btn btn-oms btn-block", width="100%",
                           style="font-size:14px;padding:10px;margin-top:8px;")
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Résultats — Sécurité Alimentaire",
              tabsetPanel(
                tabPanel(tags$span(icon("tachometer-alt"), " Synthèse"),
                  uiOutput(ns("sa_verdict")),
                  br(),
                  uiOutput(ns("sa_summary_cards")),
                  fluidRow(
                    column(7,
                      tags$div(class="section-header", "Indice composite de sévérité"),
                      plotlyOutput(ns("sa_severity_plot"), height="260px")
                    ),
                    column(5,
                      tags$div(class="section-header", "Lecture experte"),
                      uiOutput(ns("sa_synthesis_reading"))
                    )
                  ),
                  br(),
                  uiOutput(ns("sa_synthesis_details"))
                ),
                tabPanel(tags$span(icon("chart-pie"), " FCS"),
                  fluidRow(
                    column(5, plotlyOutput(ns("sa_fcs_donut"), height="300px")),
                    column(7, plotlyOutput(ns("sa_fcs_hist"), height="300px"))
                  ),
                  DTOutput(ns("sa_fcs_stats")),
                  uiOutput(ns("sa_fcs_reading"))
                ),
                tabPanel(tags$span(icon("exclamation-triangle"), " rCSI"),
                  fluidRow(
                    column(5, plotlyOutput(ns("sa_rcsi_donut"), height="300px")),
                    column(7, plotlyOutput(ns("sa_rcsi_hist"), height="300px"))
                  ),
                  DTOutput(ns("sa_rcsi_stats")),
                  uiOutput(ns("sa_rcsi_reading"))
                ),
                tabPanel(tags$span(icon("seedling"), " HDDS"),
                  fluidRow(
                    column(5, plotlyOutput(ns("sa_hdds_donut"), height="300px")),
                    column(7, plotlyOutput(ns("sa_hdds_hist"), height="300px"))
                  ),
                  DTOutput(ns("sa_hdds_groups")),
                  uiOutput(ns("sa_hdds_reading"))
                ),
                tabPanel(tags$span(icon("heartbeat"), " HHS"),
                  fluidRow(
                    column(5, plotlyOutput(ns("sa_hhs_donut"), height="300px")),
                    column(7, plotlyOutput(ns("sa_hhs_hist"), height="300px"))
                  ),
                  DTOutput(ns("sa_hhs_stats")),
                  uiOutput(ns("sa_hhs_reading"))
                ),
                tabPanel(tags$span(icon("layer-group"), " Par strate"),
                  plotlyOutput(ns("sa_strat_plot"), height="420px"),
                  DTOutput(ns("sa_strat_table")),
                  uiOutput(ns("sa_strat_reading"))
                ),

                # ── NOUVEAU : Composition FCS ──
                tabPanel(tags$span(icon("chart-bar"), " Composition FCS"),
                  br(),
                  tags$div(style="background:#F0F7FF;border-left:4px solid #0077B6;padding:8px 12px;border-radius:0 8px 8px 0;font-size:11px;margin-bottom:10px;",
                    icon("info-circle"), " Contribution de chaque groupe alimentaire au score FCS moyen, pondérée par les poids PAM."
                  ),
                  fluidRow(
                    column(8, plotlyOutput(ns("sa_fcs_composition_bar"), height="340px")),
                    column(4, plotlyOutput(ns("sa_radar"), height="340px"))
                  ),
                  br(),
                  DTOutput(ns("sa_fcs_composition_table")),
                  uiOutput(ns("sa_fcs_comp_reading"))
                ),

                # ── NOUVEAU : rCSI Détail par stratégie ──
                tabPanel(tags$span(icon("exclamation-circle"), " rCSI Détail"),
                  br(),
                  tags$div(style="background:#fff3e0;border-left:4px solid #E67E22;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:10px;",
                    icon("exclamation-triangle", style="color:#E67E22;"),
                    " Analyse du recours à chaque stratégie d'adaptation : fréquence, poids de sévérité et contribution au score rCSI."
                  ),
                  plotlyOutput(ns("sa_rcsi_components_plot"), height="380px"),
                  br(),
                  DTOutput(ns("sa_rcsi_components_table")),
                  uiOutput(ns("sa_rcsi_comp_reading"))
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
                    column(4, uiOutput(ns("sa_ipc_kpi_cards"))),
                    column(8, plotlyOutput(ns("sa_ipc_phases_plot"), height="320px"))
                  ),
                  br(),
                  fluidRow(
                    column(6,
                      selectInput(ns("sa_ipc_group_var"), "Grouper par :",
                        choices = c("Global" = "", "Région" = "region",
                                    "Département" = "departement", "Round" = "survey_round",
                                    "Sexe" = "sexe"),
                        selected = "", width = "100%")
                    )
                  ),
                  plotlyOutput(ns("sa_ipc_by_group_plot"), height="340px"),
                  br(),
                  DTOutput(ns("sa_ipc_table")),
                  uiOutput(ns("sa_ipc_reading"))
                ),

                # ── NOUVEAU : Comparaison inter-rounds ──
                tabPanel(tags$span(icon("calendar-alt"), " Comparaison Rounds"),
                  br(),
                  tags$div(style="background:#f0fff4;border-left:4px solid #27AE60;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:10px;",
                    icon("chart-line", style="color:#27AE60;"),
                    " Évolution des indicateurs de sécurité alimentaire entre les rounds de collecte. Les rubans représentent l'IC à 95%, complétés par un test de comparaison 1er/dernier round."
                  ),
                  plotlyOutput(ns("sa_rounds_plot"), height="380px"),
                  br(),
                  DTOutput(ns("sa_rounds_table")),
                  uiOutput(ns("sa_rounds_reading"))
                ),

                # ── NOUVEAU : Indicateurs OMS/FAO avancés ──
                tabPanel(tags$span(icon("globe-africa"), " Indicateurs OMS"),
                  br(),
                  tags$div(style="background:#eef6ff;border-left:4px solid #0077B6;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:10px;",
                    icon("award", style="color:#0077B6;"),
                    " LCSI (PAM), FIES (FAO) et MDD-W (FAO/OMS 2021) — indicateurs complémentaires du cadre IPC et des enquêtes SMART."
                  ),
                  fluidRow(
                    column(8, plotlyOutput(ns("sa_lcsi_plot"), height="280px")),
                    column(4, plotlyOutput(ns("sa_mddw_plot"), height="280px"))
                  ),
                  DTOutput(ns("sa_lcsi_table")),
                  br(),
                  plotlyOutput(ns("sa_fies_plot"), height="300px"),
                  DTOutput(ns("sa_fies_table")),
                  br(),
                  uiOutput(ns("sa_oms_reading"))
                ),

                # ── NOUVEAU : Table ménages ──
                tabPanel(tags$span(icon("table"), " Ménages"),
                  br(),
                  tags$div(style="background:#f8f9fa;border-left:4px solid #0B3954;padding:8px 12px;border-radius:0 6px 6px 0;font-size:11px;margin-bottom:10px;",
                    icon("users", style="color:#0B3954;"),
                    " Résultats individuels par ménage : scores, classifications, indice de sévérité et phase IPC estimée. Table filtrable et exportable."
                  ),
                  DTOutput(ns("sa_household_table")),
                  br(),
                  downloadButton(ns("sa_dl_csv_hh"), "Télécharger la table ménages (CSV)",
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
                    column(4, downloadButton(ns("sa_dl_xlsx"), "Classeur Excel complet (.xlsx)",
                                             class="btn btn-success btn-block")),
                    column(4, downloadButton(ns("sa_dl_csv_summary"), "Synthèse indicateurs (.csv)",
                                             class="btn btn-default btn-block")),
                    column(4, downloadButton(ns("sa_dl_csv_hh2"), "Table ménages (.csv)",
                                             class="btn btn-default btn-block"))
                  ),
                  br(),
                  uiOutput(ns("sa_export_info"))
                )
              )
          )
        )
      )
}

mod_secu_alim_server <- function(id, df_f, vars_cat, vars_num) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # ══════════════════════════════════════════════════════════════════
  # SÉCURITÉ ALIMENTAIRE — FCS · rCSI · HDDS · HHS
  # ══════════════════════════════════════════════════════════════════

  # Helper : sélecteur rapide de colonne numérique avec guess
  sa_select <- function(id, label, patterns, d, vn) {
    guess <- guess_col(d, patterns)
    sel <- guess %||% ""
    selectInput(ns(id), label, choices = c("— Aucun —" = "", vn),
                selected = sel, width = "100%")
  }

  # ── UI dynamiques FCS pré-calculé ──
  output$sa_fcs_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_precalc", "Colonne FCS :",
              c("^fcs$","^FCS$","^food.*cons.*score","^fcs_score","^score.*fcs",
                "^fcscore","FCSScore","food_consumption","sca$","^sca_score",
                "^score_consommation"), d, vn)
  })

  # ── UI dynamiques FCS groupes ──
  output$sa_fcs_cereales_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_cereales", "Céréales/Tubercules :",
              c("^cereale","^cereal","^staple","^fcs.*cereal","^fcs.*staple",
                "FCSStap","fcs_staple","tubercul","main_staple","grp.*cereal",
                "g1.*cereal","food.*cereal","alim.*cereal"), d, vn)
  })
  output$sa_fcs_legumineuses_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_legumineuses", "Légumineuses :",
              c("^legum","^pulse","^fcs.*pulse","^fcs.*legum","FCSPulse",
                "fcs_pulse","bean","haricot","grp.*legum","g2.*pulse",
                "food.*pulse","alim.*legum","noix_legum"), d, vn)
  })
  output$sa_fcs_legumes_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_legumes", "Légumes :",
              c("^legume","^veget","^fcs.*veg","^fcs.*legum","FCSVeg","fcs_veg",
                "grp.*legume","g3.*veg","food.*veg","alim.*legume"), d, vn)
  })
  output$sa_fcs_fruits_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_fruits", "Fruits :",
              c("^fruit","^fcs.*fruit","FCSFruit","fcs_fruit",
                "grp.*fruit","g4.*fruit","food.*fruit","alim.*fruit"), d, vn)
  })
  output$sa_fcs_viande_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_viande", "Viande/Poisson :",
              c("^viande","^meat","^protein","^fcs.*meat","^fcs.*prot",
                "FCSPr","fcs_meat","fcs_protein","poisson","fish",
                "grp.*viande","grp.*meat","g5.*meat","food.*meat",
                "alim.*viande","animal_protein"), d, vn)
  })
  output$sa_fcs_lait_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_lait", "Lait/Prod. laitiers :",
              c("^lait","^milk","^dairy","^fcs.*milk","^fcs.*dairy","^fcs.*lait",
                "FCSDairy","fcs_dairy","prod.*lait","grp.*lait",
                "g6.*dairy","g6.*milk","food.*dairy","alim.*lait"), d, vn)
  })
  output$sa_fcs_sucre_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_sucre", "Sucre :",
              c("^sucre","^sugar","^fcs.*sugar","^fcs.*sucre","FCSSugar","fcs_sugar",
                "grp.*sucre","g7.*sugar","food.*sugar","alim.*sucre",
                "sweet","sucrerie"), d, vn)
  })
  output$sa_fcs_huile_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_huile", "Huile/Graisse :",
              c("^huile","^oil","^fat","^fcs.*oil","^fcs.*fat","^fcs.*huile",
                "FCSFat","FCSOil","fcs_oil","fcs_fat","graisse",
                "grp.*huile","g8.*oil","g8.*fat","food.*oil","alim.*huile"), d, vn)
  })

  # ── UI dynamiques rCSI ──
  output$sa_rcsi_prefere_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_prefere", "Aliments moins préférés :",
              c("^rcsi.*pref","^less.*prefer","^moins.*pref","rcsi1",
                "rCSILessQlty","rcsi_less","coping.*pref","strat.*pref",
                "alim.*moins.*pref","less_preferred"), d, vn)
  })
  output$sa_rcsi_emprunter_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_emprunter", "Emprunter nourriture :",
              c("^rcsi.*borrow","^emprunt","rcsi2","rCSIBorrow",
                "rcsi_borrow","coping.*borrow","coping.*emprunt",
                "strat.*emprunt","borrow_food"), d, vn)
  })
  output$sa_rcsi_repas_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_repas", "Réduire nb de repas :",
              c("^rcsi.*meal","^redui.*repas","rcsi3","rCSIMealNb",
                "rcsi_meal","reduce.*meal","coping.*meal","coping.*repas",
                "strat.*repas","limit.*meal","fewer_meal"), d, vn)
  })
  output$sa_rcsi_portions_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_portions", "Réduire les portions :",
              c("^rcsi.*port","^redui.*port","rcsi4","rCSIMealSize","meal.*size","meal.*si",
                "rcsi_portion","reduce.*portion","coping.*portion",
                "strat.*portion","limit.*portion","smaller_meal"), d, vn)
  })
  output$sa_rcsi_adultes_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_adultes", "Réduire conso adultes :",
              c("^rcsi.*adult","^redui.*adult","rcsi5","rCSIMealAdult",
                "rcsi_adult","restrict.*adult","coping.*adult",
                "strat.*adult","adults_restrict"), d, vn)
  })

  # ── UI dynamiques HDDS ──
  output$sa_hdds_cols_ui <- renderUI({
    vn <- vars_num()
    hdds_guess <- setdiff(
      grep("^group|^grp.*alim|^hdds_|^dd_|^dietary|^diversit",
           vn, ignore.case = TRUE, value = TRUE),
      c("hdds", "hdds_score"))
    checkboxGroupInput(ns("sa_hdds_cols"), "Colonnes des groupes alimentaires (0/1) :",
                       choices = vn,
                       selected = if (length(hdds_guess) > 0) hdds_guess else NULL,
                       width = "100%")
  })

  # ── UI dynamiques HHS ──
  output$sa_hhs_nofood_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_nofood", "Pas de nourriture :",
              c("^hhs.*nofood","^hhs.*no.*food","^hhs1","pas.*nourr",
                "HHSNoFood","hhs_nofood","no_food_home","hunger.*nofood",
                "faim.*nourr","manque.*nourr"), d, vn)
  })
  output$sa_hhs_sleep_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_sleep", "Dormir affamé :",
              c("^hhs.*sleep","^hhs.*bed","^hhs2","dormir.*affam",
                "HHSBedHungry","hhs_sleep","hhs_bed","sleep.*hungry",
                "bed_hungry","coucher.*faim"), d, vn)
  })
  output$sa_hhs_wholeday_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_wholeday", "Journée sans manger :",
              c("^hhs.*whole","^hhs.*day","^hhs3","journee.*sans",
                "HHSNoMealDay","hhs_wholeday","hhs_allday","whole_day",
                "no_eat_day","jour.*sans.*mang"), d, vn)
  })

  # ── UI dynamiques : scores pré-calculés (HDDS · rCSI · HHS) ──
  output$sa_hdds_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hdds_precalc", "Score HDDS (0-12) :",
              c("^hdds$","^hdds_score$","^score.*hdds","^dietary.*diversity",
                "^dd_score","^dds$","diversit"), d, vn)
  })
  output$sa_rcsi_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_precalc", "Score rCSI (0-56) :",
              c("^coping.*strategy","^rcsi$","^rcsi_score$","^score.*rcsi",
                "casesi","^csi$","coping.*index"), d, vn)
  })
  output$sa_hhs_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_precalc", "Score HHS (0-6) :",
              c("^household.*hunger","^hhs$","^hhs_score$","^score.*hhs",
                "^hhs_total","faim.*score","hunger.*score"), d, vn)
  })

  # ── UI dynamiques : LCSI · FIES · MDD-W ──
  sa_select_any <- function(id, label, patterns) {
    d <- df_f()
    guess <- guess_col(d, patterns, type = "any")
    selectInput(ns(id), label, choices = c("— Aucun —" = "", names(d)),
                selected = guess %||% "", width = "100%")
  }
  output$sa_lcsi_class_ui <- renderUI({
    sa_select_any("sa_lcsi_class", "Classe LCSI pré-calculée :",
                  c("^lcsi.*class","^lcsi$","live.*coping.*index","lcs.*class",
                    "coping.*subsistance","strategie.*subsistance"))
  })
  output$sa_lcsi_stress_ui <- renderUI({
    sa_select_any("sa_lcsi_stress", "Stratégies de stress (oui/non) :",
                  c("^lcsi.*stress","stress.*lcsi","^stress","lcsi.*s1"))
  })
  output$sa_lcsi_crisis_ui <- renderUI({
    sa_select_any("sa_lcsi_crisis", "Stratégies de crise (oui/non) :",
                  c("^lcsi.*crise","^lcsi.*crisis","crise.*lcsi","^crise","^crisis"))
  })
  output$sa_lcsi_emerg_ui <- renderUI({
    sa_select_any("sa_lcsi_emerg", "Stratégies d'urgence (oui/non) :",
                  c("^lcsi.*urg","urg.*lcsi","^urgence","^emerg","emergency.*lcsi"))
  })
  output$sa_fies_col_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fies_col", "Score FIES (0-8) :",
              c("^fies","food.*insecurity.*experience","^score.*fies","insecurite.*experience"),
              d, vn)
  })
  output$sa_mddw_col_ui <- renderUI({
    sa_select_any("sa_mddw_col", "MDD-W (≥5 groupes, oui/non) :",
                  c("^mdd.*w","^mddw","mdd_w","dietary.*diversity.*wom",
                    "women.*dietary","diversite.*femme"))
  })

  # ── UI stratification ──
  output$sa_strat_ui <- renderUI({
    vc <- vars_cat()
    selectInput(ns("sa_strat_var"), "Stratifier par :",
                choices = c("— Aucune —" = "", vc), selected = "", width = "100%")
  })

  # ── Stockage résultats ──
  sa_res <- reactiveValues(fcs = NULL, rcsi = NULL, hdds = NULL, hhs = NULL,
                           lcsi = NULL, fies = NULL, mddw = NULL)

  # ── Calcul ──
  observeEvent(input$run_sa, {
    d <- df_f(); req(nrow(d) > 0)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    mode <- input$sa_mode

    # Reset des resultats
    sa_res$fcs <- NULL; sa_res$rcsi <- NULL; sa_res$hdds <- NULL; sa_res$hhs <- NULL
    sa_res$lcsi <- NULL; sa_res$fies <- NULL; sa_res$mddw <- NULL
    computed <- character(0)
    skipped  <- character(0)

    # Compteur de colonnes mappées par indicateur (pour diagnostic)
    sa_diag <- list()

    withProgress(message = "Calcul des indicateurs de s\u00e9curit\u00e9 alimentaire...", value = 0, {

      # FCS pre-calcule
      if (mode %in% c("fcs_precalc", "all")) {
        incProgress(0.1, detail = "FCS (pr\u00e9-calcul\u00e9)...")
        col_fcs <- nullify(input$sa_fcs_precalc)
        if (!is.null(col_fcs) && col_fcs %in% names(d)) {
          sa_res$fcs <- classify_fcs_precalc(d[[col_fcs]])
          d$FCS_class <- sa_res$fcs$class
          computed <- c(computed, "FCS (pr\u00e9-calcul\u00e9)")
        } else {
          sa_diag[["FCS pr\u00e9-calcul\u00e9"]] <- "aucune colonne FCS s\u00e9lectionn\u00e9e"
          if (mode == "fcs_precalc") {
            skipped <- c(skipped, "FCS pr\u00e9-calcul\u00e9 : s\u00e9lectionnez une colonne contenant le score FCS d\u00e9j\u00e0 calcul\u00e9")
          }
        }
      }

      # FCS calcule depuis groupes
      if (mode %in% c("fcs_calc", "all")) {
        incProgress(0.15, detail = "FCS (groupes alimentaires)...")
        fcs_cols <- list(
          cereales = nullify(input$sa_fcs_cereales),
          legumineuses = nullify(input$sa_fcs_legumineuses),
          legumes = nullify(input$sa_fcs_legumes),
          fruits = nullify(input$sa_fcs_fruits),
          viande = nullify(input$sa_fcs_viande),
          lait = nullify(input$sa_fcs_lait),
          sucre = nullify(input$sa_fcs_sucre),
          huile = nullify(input$sa_fcs_huile)
        )
        n_fcs_mapped <- sum(!sapply(fcs_cols, is.null))
        fcs <- calc_fcs(d,
          col_cereales = fcs_cols$cereales,
          col_legumineuses = fcs_cols$legumineuses,
          col_legumes = fcs_cols$legumes,
          col_fruits = fcs_cols$fruits,
          col_viande = fcs_cols$viande,
          col_lait = fcs_cols$lait,
          col_sucre = fcs_cols$sucre,
          col_huile = fcs_cols$huile
        )
        if (!is.null(fcs)) {
          sa_res$fcs <- fcs
          d$FCS_class <- fcs$class
          computed <- c(computed, paste0("FCS (", fcs$n_groups, " groupes)"))
        } else {
          sa_diag[["FCS groupes"]] <- paste0(n_fcs_mapped, "/8 groupes mapp\u00e9s (min 4)")
          if (mode == "fcs_calc") {
            skipped <- c(skipped, paste0("FCS : ", n_fcs_mapped, "/8 groupes mapp\u00e9s (minimum 4 requis). ",
                         "S\u00e9lectionnez les colonnes de fr\u00e9quence alimentaire (0-7 jours/semaine)"))
          }
        }
      }

      # rCSI
      if (mode %in% c("rcsi", "all")) {
        incProgress(0.2, detail = "rCSI...")
        col_rcsi_pre <- nullify(input$sa_rcsi_precalc)
        rcsi_cols <- list(
          prefere = nullify(input$sa_rcsi_prefere),
          emprunter = nullify(input$sa_rcsi_emprunter),
          repas = nullify(input$sa_rcsi_repas),
          portions = nullify(input$sa_rcsi_portions),
          adultes = nullify(input$sa_rcsi_adultes)
        )
        n_rcsi_mapped <- sum(!sapply(rcsi_cols, is.null))
        rcsi <- if (!is.null(col_rcsi_pre) && col_rcsi_pre %in% names(d)) {
          calc_rcsi_precalc(d, col_rcsi_pre)
        } else {
          calc_rcsi(d,
            col_moins_prefere = rcsi_cols$prefere,
            col_emprunter = rcsi_cols$emprunter,
            col_reduire_repas = rcsi_cols$repas,
            col_reduire_portions = rcsi_cols$portions,
            col_reduire_adultes = rcsi_cols$adultes
          )
        }
        if (!is.null(rcsi)) {
          sa_res$rcsi <- rcsi
          d$rCSI_class <- rcsi$class
          computed <- c(computed, if (isTRUE(rcsi$precalc)) "rCSI (pr\u00e9-calcul\u00e9)" else "rCSI")
        } else {
          sa_diag[["rCSI"]] <- paste0(n_rcsi_mapped, "/5 strat\u00e9gies mapp\u00e9es (min 3)")
          if (mode == "rcsi") {
            skipped <- c(skipped, paste0("rCSI : ", n_rcsi_mapped, "/5 strat\u00e9gies mapp\u00e9es (minimum 3 requises). ",
                         "S\u00e9lectionnez les colonnes de strat\u00e9gies d'adaptation (0-7 jours)"))
          }
        }
      }

      # HDDS
      if (mode %in% c("hdds", "all")) {
        incProgress(0.2, detail = "HDDS...")
        col_hdds_pre <- nullify(input$sa_hdds_precalc)
        cols_hdds <- input$sa_hdds_cols
        n_hdds <- if (!is.null(cols_hdds)) length(cols_hdds) else 0
        if (!is.null(col_hdds_pre) && col_hdds_pre %in% names(d)) {
          hdds <- calc_hdds_precalc(d, col_hdds_pre)
          sa_res$hdds <- hdds
          d$HDDS_class <- hdds$class
          computed <- c(computed, "HDDS (pr\u00e9-calcul\u00e9)")
        } else if (n_hdds >= 4) {
          hdds <- calc_hdds(d, cols_hdds)
          if (!is.null(hdds)) {
            sa_res$hdds <- hdds
            d$HDDS_class <- hdds$class
            computed <- c(computed, paste0("HDDS (", hdds$n_groups_used, " groupes)"))
          }
        } else {
          sa_diag[["HDDS"]] <- paste0(n_hdds, " groupes coch\u00e9s (min 4)")
          if (mode == "hdds") {
            skipped <- c(skipped, paste0("HDDS : ", n_hdds, " groupes coch\u00e9s (minimum 4 requis). ",
                         "Cochez les colonnes binaires (0/1) des groupes alimentaires"))
          }
        }
      }

      # HHS
      if (mode %in% c("hhs", "all")) {
        incProgress(0.2, detail = "HHS...")
        col_hhs_pre <- nullify(input$sa_hhs_precalc)
        hhs_cols <- list(
          nofood = nullify(input$sa_hhs_nofood),
          sleep = nullify(input$sa_hhs_sleep),
          wholeday = nullify(input$sa_hhs_wholeday)
        )
        n_hhs_mapped <- sum(!sapply(hhs_cols, is.null))
        hhs <- if (!is.null(col_hhs_pre) && col_hhs_pre %in% names(d)) {
          calc_hhs_precalc(d, col_hhs_pre)
        } else {
          calc_hhs(d,
            col_no_food = hhs_cols$nofood,
            col_sleep_hungry = hhs_cols$sleep,
            col_whole_day = hhs_cols$wholeday
          )
        }
        if (!is.null(hhs)) {
          sa_res$hhs <- hhs
          d$HHS_class <- hhs$class
          computed <- c(computed, if (isTRUE(hhs$precalc)) "HHS (pr\u00e9-calcul\u00e9)" else "HHS")
        } else {
          sa_diag[["HHS"]] <- paste0(n_hhs_mapped, "/3 questions mapp\u00e9es (min 2)")
          if (mode == "hhs") {
            skipped <- c(skipped, paste0("HHS : ", n_hhs_mapped, "/3 questions mapp\u00e9es (minimum 2 requises). ",
                         "S\u00e9lectionnez les colonnes de fr\u00e9quence de faim (0-2)"))
          }
        }
      }

      # LCSI
      if (mode %in% c("lcsi", "all")) {
        incProgress(0.1, detail = "LCSI...")
        lcsi_cols <- list(
          stress = nullify(input$sa_lcsi_stress),
          crisis = nullify(input$sa_lcsi_crisis),
          emerg  = nullify(input$sa_lcsi_emerg),
          class  = nullify(input$sa_lcsi_class)
        )
        n_lcsi_mapped <- sum(!sapply(lcsi_cols, is.null))
        if (n_lcsi_mapped >= 1) {
          lcsi <- calc_lcsi(d,
            col_stress = lcsi_cols$stress,
            col_crisis = lcsi_cols$crisis,
            col_emergency = lcsi_cols$emerg,
            col_lcsi_class = lcsi_cols$class
          )
          if (!is.null(lcsi$summary) && nrow(lcsi$summary) > 0) {
            sa_res$lcsi <- lcsi
            computed <- c(computed, "LCSI")
          } else {
            sa_diag[["LCSI"]] <- "colonnes LCSI invalides"
          }
        } else {
          sa_diag[["LCSI"]] <- "aucune colonne LCSI s\u00e9lectionn\u00e9e"
          if (mode == "lcsi") {
            skipped <- c(skipped, paste0("LCSI : s\u00e9lectionnez la classe pr\u00e9-calcul\u00e9e ",
                         "ou au moins une famille de strat\u00e9gies (stress/crise/urgence)"))
          }
        }
      }

      # FIES
      if (mode %in% c("fies", "all")) {
        incProgress(0.1, detail = "FIES...")
        col_fies <- nullify(input$sa_fies_col)
        if (!is.null(col_fies) && col_fies %in% names(d)) {
          fies <- calc_fies(d, col_fies)
          if (!is.null(fies$summary) && nrow(fies$summary) > 0 &&
              sum(fies$summary$N, na.rm = TRUE) > 0) {
            sa_res$fies <- fies
            computed <- c(computed, "FIES")
          } else {
            sa_diag[["FIES"]] <- "score FIES non valide (0-8 attendu)"
          }
        } else {
          sa_diag[["FIES"]] <- "aucune colonne FIES s\u00e9lectionn\u00e9e"
          if (mode == "fies") {
            skipped <- c(skipped, "FIES : s\u00e9lectionnez une colonne de score FIES (0-8)")
          }
        }
      }

      # MDD-W
      if (mode %in% c("mddw", "all")) {
        incProgress(0.1, detail = "MDD-W...")
        col_mddw <- nullify(input$sa_mddw_col)
        if (!is.null(col_mddw) && col_mddw %in% names(d)) {
          mddw <- calc_mddw(d, col_mddw)
          if (!is.na(mddw$pct) && mddw$n_valid > 0) {
            sa_res$mddw <- mddw
            computed <- c(computed, "MDD-W")
          } else {
            sa_diag[["MDD-W"]] <- "colonne MDD-W non exploitable (oui/non attendu)"
          }
        } else {
          sa_diag[["MDD-W"]] <- "aucune colonne MDD-W s\u00e9lectionn\u00e9e"
          if (mode == "mddw") {
            skipped <- c(skipped, "MDD-W : s\u00e9lectionnez la colonne binaire (oui/non) du seuil \u2265 5 groupes")
          }
        }
      }

      incProgress(0.15, detail = "Termin\u00e9")
    })

    # Feedback utilisateur
    if (length(computed) > 0) {
      showNotification(
        paste0("\u2705 Indicateurs calcul\u00e9s : ", paste(computed, collapse = ", ")),
        type = "message", duration = 6)
    }
    if (length(computed) == 0) {
      # Message détaillé selon le mode
      if (mode == "all") {
        # En mode "all", donner un diagnostic précis
        diag_msg <- paste(names(sa_diag), sa_diag, sep = " \u2192 ", collapse = " | ")
        showNotification(
          HTML(paste0(
            "<b>\u26a0 Aucun indicateur calcul\u00e9</b><br/>",
            "En mode \u00ab Tous les indicateurs \u00bb, aucune colonne de s\u00e9curit\u00e9 alimentaire ",
            "n'a \u00e9t\u00e9 d\u00e9tect\u00e9e automatiquement.<br/><br/>",
            "<b>Actions recommand\u00e9es :</b><br/>",
            "\u2022 S\u00e9lectionnez un mode sp\u00e9cifique (FCS, rCSI, HDDS ou HHS)<br/>",
            "\u2022 Mappez manuellement vos colonnes dans le panneau de gauche<br/>",
            "\u2022 V\u00e9rifiez que vos donn\u00e9es contiennent des variables de s\u00e9curit\u00e9 alimentaire"
          )),
          type = "warning", duration = 12)
      } else {
        showNotification(
          HTML(paste0(
            "<b>\u26a0 Aucun indicateur calcul\u00e9</b><br/>",
            "V\u00e9rifiez que les colonnes sont correctement s\u00e9lectionn\u00e9es ",
            "dans le panneau de gauche.<br/><br/>",
            if (length(skipped) > 0) paste0("<b>D\u00e9tail :</b> ", paste(skipped, collapse = "<br/>"))
            else "S\u00e9lectionnez les colonnes correspondant aux variables demand\u00e9es."
          )),
          type = "warning", duration = 10)
      }
    }
    if (length(computed) > 0 && length(skipped) > 0) {
      showNotification(
        paste0("Indicateurs ignor\u00e9s : ", paste(skipped, collapse = " | ")),
        type = "warning", duration = 8)
    }
  })

  # ── Rendu : Synthèse ──
  sa_severity_r <- reactive({
    calc_sa_severity_index(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs)
  })
  sa_verdict_r <- reactive({
    calc_sa_verdict(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs,
                    sev_res = sa_severity_r(), ipc_summary = calc_ipc_summary(.ipc_phases_r()))
  })

  output$sa_verdict <- renderUI({
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_res) return(NULL)
    v <- sa_verdict_r(); req(v)
    tagList(
      tags$div(style = paste0("background:", v$bg, ";border-left:5px solid ", v$color,
        ";border-radius:10px;padding:14px 18px;margin-bottom:12px;"),
        tags$div(style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
          tags$span(style = paste0("background:", v$color,
            ";color:white;font-size:11px;font-weight:700;padding:3px 10px;",
            "border-radius:20px;letter-spacing:.5px;text-transform:uppercase;"), v$badge),
          tags$span(style = "font-size:13px;color:#333;",
            tags$b(v$headline), " ", v$recommendation)
        ),
        if (length(v$bullets) > 0)
          tags$ul(style = "margin:8px 0 0 0;padding-left:18px;font-size:12px;color:#444;line-height:1.7;",
            lapply(v$bullets, function(b) tags$li(b)))
      )
    )
  })

  output$sa_summary_cards <- renderUI({
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_res) return(NULL)
    card <- function(icon_name, col, title, value, sub = NULL) {
      tags$div(class = "zs-kpi-card", style = "flex:1;min-width:150px;padding:14px 16px;",
        tags$div(class = "zs-header",
          tags$div(class = "zs-header-icon", style = paste0("background:", col, ";"),
                   icon(icon_name)),
          tags$div(class = "zs-title", title)),
        tags$div(class = "zs-value", style = paste0("color:", col, ";font-size:26px;"), value),
        if (!is.null(sub)) tags$div(class = "zs-subtitle", sub))
    }
    n_obs <- max(c(length(sa_res$fcs$score), length(sa_res$rcsi$score),
                   length(sa_res$hdds$score), length(sa_res$hhs$score)), 0)
    pct_worst <- function(r, level) {
      if (is.null(r)) return(NULL)
      round(sum(r$class == level, na.rm = TRUE) / sum(!is.na(r$class)) * 100, 1)
    }
    tags$div(style = "display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
      card("users", "#0B3954", "Ménages analysés", n_obs,
           paste0(sum(c(!is.null(sa_res$fcs), !is.null(sa_res$rcsi),
                        !is.null(sa_res$hdds), !is.null(sa_res$hhs))), " indicateur(s) core")),
      if (!is.null(sa_res$fcs))
        card("utensils", "#0077B6", "FCS moyen",
             round(mean(sa_res$fcs$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$fcs, "Pauvre"), " % pauvre")),
      if (!is.null(sa_res$rcsi))
        card("triangle-exclamation", "#E67E22", "rCSI moyen",
             round(mean(sa_res$rcsi$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$rcsi, "Coping \u00e9lev\u00e9"), " % coping \u00e9lev\u00e9")),
      if (!is.null(sa_res$hdds))
        card("seedling", "#27AE60", "HDDS moyen",
             round(mean(sa_res$hdds$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$hdds, "Faible diversit\u00e9"), " % faible diversit\u00e9")),
      if (!is.null(sa_res$hhs))
        card("heartbeat", "#C0392B", "HHS moyen",
             round(mean(sa_res$hhs$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$hhs, "Faim s\u00e9v\u00e8re"), " % faim s\u00e9v\u00e8re")),
      if (!is.null(sa_res$mddw))
        card("bowl-food", "#8E44AD", "MDD-W adéquat",
             paste0(sa_res$mddw$pct, " %"),
             paste0("IC 95 % [", sa_res$mddw$ci_lower, "\u2013", sa_res$mddw$ci_upper, "]")),
      if (!is.null(sa_res$fies))
        card("face-frown", "#b36b00", "FIES mod./sévère",
             paste0(sa_res$fies$pct_moderate_severe, " %"),
             paste0("moyenne ", sa_res$fies$mean_score, "/8"))
    )
  })

  output$sa_severity_plot <- renderPlotly({
    sev <- sa_severity_r()
    if (is.null(sev))
      return(plotly_empty() |> layout(title = list(
        text = "Calculez au moins un indicateur pour l'indice composite", x = .5)))
    plot_sa_donut(sev$summary |> rename(Catégorie = Classe),
                  "Indice composite de sévérité")
  })

  output$sa_synthesis_reading <- renderUI({
    txt <- sa_reading_synthesis(sa_verdict_r())
    txt_sev <- sa_reading_severity(sa_severity_r())
    if (is.null(txt) && is.null(txt_sev)) return(NULL)
    tagList(
      if (!is.null(txt)) pk_info(txt, "lightbulb", "#0077B6", "#e8f6fc"),
      if (!is.null(txt_sev)) pk_info(txt_sev, "chart-simple", "#8E44AD", "#f5eefa")
    )
  })

  output$sa_synthesis_details <- renderUI({
    any_result <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
                  !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_result) {
      sa_gs <- "padding:6px 8px;border:1px solid #e5e5e5;"
      sa_hs <- "padding:8px;border:1px solid #d0e8f5;"
      return(tags$div(style = "padding:15px;",
        tags$div(style = "background:#f0f8ff;border-left:4px solid #0077B6;padding:14px 16px;border-radius:0 8px 8px 0;margin-bottom:16px;",
          tags$div(style = "font-size:14px;font-weight:700;color:#0B3954;margin-bottom:8px;",
            icon("info-circle"), " Guide : Comment utiliser ce module"),
          tags$ol(style = "font-size:12px;color:#333;line-height:1.8;margin:0;padding-left:20px;",
            tags$li("Choisissez un ", tags$b("mode de calcul"), " dans le panneau de gauche."),
            tags$li("Pour chaque indicateur, ", tags$b("s\u00e9lectionnez manuellement"), " les colonnes correspondantes."),
            tags$li("Cliquez sur ", tags$b("\u00ab Calculer les indicateurs \u00bb"), ".")
          ),
          tags$div(style="color:#888;font-size:10px;margin-top:6px;",
            "Astuce : les colonnes aux noms standards (FCSStap, rCSILessQlty, HHSNoFood\u2026) sont pr\u00e9-s\u00e9lectionn\u00e9es automatiquement.")
        ),
        tags$table(style = "width:100%;border-collapse:collapse;font-size:11px;",
          tags$thead(tags$tr(style = "background:#e8f4fd;",
            tags$th(style = sa_hs, "Indicateur"),
            tags$th(style = sa_hs, "Description"),
            tags$th(style = sa_hs, "Format attendu"),
            tags$th(style = sa_hs, "Seuils")
          )),
          tags$tbody(
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "FCS"),
              tags$td(style = sa_gs, "Somme pond\u00e9r\u00e9e de 8 groupes alimentaires"),
              tags$td(style = sa_gs, "8 colonnes, 0-7 jours"),
              tags$td(style = sa_gs, "Pauvre <21 | Limite 21-35 | Acceptable >35")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "rCSI"),
              tags$td(style = sa_gs, "Somme pond\u00e9r\u00e9e de 5 strat\u00e9gies d'adaptation"),
              tags$td(style = sa_gs, "5 colonnes, 0-7 jours"),
              tags$td(style = sa_gs, "Faible 0-3 | Moyen 4-18 | \u00c9lev\u00e9 \u226519")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "HDDS"),
              tags$td(style = sa_gs, "Groupes alimentaires consomm\u00e9s en 24h"),
              tags$td(style = sa_gs, "Colonnes binaires 0/1"),
              tags$td(style = sa_gs, "Faible 0-3 | Moyen 4-5 | \u00c9lev\u00e9 \u22656")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "HHS"),
              tags$td(style = sa_gs, "3 questions d'exp\u00e9rience de faim"),
              tags$td(style = sa_gs, "3 colonnes, valeurs 0-2"),
              tags$td(style = sa_gs, "Aucune 0-1 | Mod\u00e9r\u00e9e 2-3 | S\u00e9v\u00e8re 4-6")
            )
          )
        )
      ))
    }
    # Tableau de synthèse des stats
    stats_all <- bind_rows(
      if (!is.null(sa_res$fcs))  sa_res$fcs$stats  |> mutate(Indicateur = "FCS"),
      if (!is.null(sa_res$rcsi)) sa_res$rcsi$stats |> mutate(Indicateur = "rCSI"),
      if (!is.null(sa_res$hdds)) sa_res$hdds$stats |> mutate(Indicateur = "HDDS"),
      if (!is.null(sa_res$hhs))  sa_res$hhs$stats  |> mutate(Indicateur = "HHS")
    )
    if (nrow(stats_all) == 0) return(NULL)
    tags$div(
      tags$div(class = "section-header", "Statistiques descriptives"),
      renderDT(
        datatable(stats_all |> pivot_wider(names_from = Indicateur, values_from = Valeur),
                  rownames = FALSE, options = list(dom = "t"), class = "compact stripe"),
        outputArgs = list()
      )
    )
  })

  # ── Rendu : FCS ──
  output$sa_fcs_donut <- renderPlotly({
    req(sa_res$fcs); plot_sa_donut(sa_res$fcs$summary, "FCS — Classification PAM")
  })
  output$sa_fcs_hist <- renderPlotly({
    req(sa_res$fcs)
    plot_sa_histogram(sa_res$fcs$score, "Distribution du FCS",
      breaks_lines = list(
        list(x = 21, color = "#e74c3c", label = "Pauvre"),
        list(x = 35, color = "#f39c12", label = "Limite")
      ))
  })
  output$sa_fcs_stats <- renderDT({
    req(sa_res$fcs)
    datatable(sa_add_ci(sa_res$fcs$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : rCSI ──
  output$sa_rcsi_donut <- renderPlotly({
    req(sa_res$rcsi); plot_sa_donut(sa_res$rcsi$summary, "rCSI — Stratégies d'adaptation")
  })
  output$sa_rcsi_hist <- renderPlotly({
    req(sa_res$rcsi)
    plot_sa_histogram(sa_res$rcsi$score, "Distribution du rCSI",
      breaks_lines = list(
        list(x = 4, color = "#f39c12", label = "Moyen"),
        list(x = 19, color = "#e74c3c", label = "Élevé")
      ))
  })
  output$sa_rcsi_stats <- renderDT({
    req(sa_res$rcsi)
    datatable(sa_add_ci(sa_res$rcsi$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : HDDS ──
  output$sa_hdds_donut <- renderPlotly({
    req(sa_res$hdds); plot_sa_donut(sa_res$hdds$summary, "HDDS — Diversité alimentaire")
  })
  output$sa_hdds_hist <- renderPlotly({
    req(sa_res$hdds)
    plot_sa_histogram(sa_res$hdds$score, "Distribution du HDDS",
      breaks_lines = list(
        list(x = 3.5, color = "#e74c3c", label = "Faible"),
        list(x = 5.5, color = "#f39c12", label = "Moyen")
      ))
  })
  output$sa_hdds_groups <- renderDT({
    req(sa_res$hdds)
    if (isTRUE(sa_res$hdds$precalc)) {
      datatable(sa_add_ci(sa_res$hdds$summary) |> select(-Couleur) |> rename(`N ménages` = N),
                rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
    } else {
      datatable(sa_res$hdds$group_freq, rownames = FALSE,
                options = list(dom = "t", pageLength = 15), class = "compact stripe") |>
        formatStyle("% consommé",
                    background = styleColorBar(c(0, 100), "#0077B6"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
    }
  })

  # ── Rendu : HHS ──
  output$sa_hhs_donut <- renderPlotly({
    req(sa_res$hhs); plot_sa_donut(sa_res$hhs$summary, "HHS — Échelle de la Faim")
  })
  output$sa_hhs_hist <- renderPlotly({
    req(sa_res$hhs)
    plot_sa_histogram(sa_res$hhs$score, "Distribution du HHS",
      breaks_lines = list(
        list(x = 1.5, color = "#f39c12", label = "Modérée"),
        list(x = 3.5, color = "#e74c3c", label = "Sévère")
      ))
  })
  output$sa_hhs_stats <- renderDT({
    req(sa_res$hhs)
    datatable(sa_add_ci(sa_res$hhs$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : Par strate ──
  output$sa_strat_plot <- renderPlotly({
    d <- df_f()
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    req(strat_var)

    # Utiliser le FCS comme indicateur principal pour la stratification
    if (!is.null(sa_res$fcs)) {
      d$FCS_class <- factor(sa_res$fcs$class,
                             levels = c("Pauvre", "Limite", "Acceptable"))
      plot_sa_by_group(d, "FCS_class", strat_var, "FCS par strate",
                        color_map = c("Pauvre" = "#e74c3c", "Limite" = "#f39c12",
                                      "Acceptable" = "#27ae60")) |> nutri_config()
    } else if (!is.null(sa_res$rcsi)) {
      d$rCSI_class <- factor(sa_res$rcsi$class,
                              levels = c("Pas/Peu de coping", "Coping moyen", "Coping élevé"))
      plot_sa_by_group(d, "rCSI_class", strat_var, "rCSI par strate",
                        color_map = c("Pas/Peu de coping" = "#27ae60",
                                      "Coping moyen" = "#f39c12",
                                      "Coping élevé" = "#e74c3c")) |> nutri_config()
    } else {
      plotly_empty() |> layout(title = list(text = "Calculez un indicateur d'abord", x = .5))
    }
  })

  output$sa_strat_table <- renderDT({
    d <- df_f()
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    req(strat_var)

    results <- list()
    if (!is.null(sa_res$fcs)) {
      d$FCS_class <- sa_res$fcs$class
      results$FCS <- d |>
        filter(!is.na(.data[[strat_var]])) |>
        group_by(Groupe = .data[[strat_var]]) |>
        summarise(
          `FCS moyen` = round(mean(sa_res$fcs$score[cur_group_rows()], na.rm = TRUE), 1),
          `% Pauvre` = round(sum(FCS_class == "Pauvre") / n() * 100, 1),
          `% Limite` = round(sum(FCS_class == "Limite") / n() * 100, 1),
          `% Acceptable` = round(sum(FCS_class == "Acceptable") / n() * 100, 1),
          N = n(), .groups = "drop"
        )
    }
    if (!is.null(sa_res$hhs)) {
      d$HHS_class <- sa_res$hhs$class
      results$HHS <- d |>
        filter(!is.na(.data[[strat_var]])) |>
        group_by(Groupe = .data[[strat_var]]) |>
        summarise(
          `HHS moyen` = round(mean(sa_res$hhs$score[cur_group_rows()], na.rm = TRUE), 1),
          `% Faim sévère` = round(sum(HHS_class == "Faim sévère") / n() * 100, 1),
          N = n(), .groups = "drop"
        )
    }

    if (length(results) > 0) {
      tbl <- results[[1]]
      if (length(results) > 1) {
        for (i in 2:length(results)) {
          tbl <- left_join(tbl, results[[i]] |> select(-N), by = "Groupe")
        }
      }
      datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 20),
                class = "compact stripe hover")
    } else {
      datatable(data.frame(Message = "Calculez les indicateurs d'abord"), rownames = FALSE)
    }
  })


  # ══════════════════════════════════════════════════════════════════
  # MODULE SA — NOUVEAUX OUTPUTS (FCS Composition · rCSI Détail · IPC · Rounds)
  # ══════════════════════════════════════════════════════════════════

  # ── FCS Composition ──
  output$sa_fcs_composition_bar <- renderPlotly({
    req(sa_res$fcs)
    tryCatch(
      plot_fcs_composition_bar(sa_res$fcs),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_radar <- renderPlotly({
    tryCatch(
      plot_sa_radar(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs),
      error = function(e) plotly_empty() |> layout(title = list(text = "Radar — calculez 2+ indicateurs", x=.5))
    )
  })

  output$sa_fcs_composition_table <- renderDT({
    req(sa_res$fcs)
    comp <- calc_fcs_composition(sa_res$fcs)
    req(comp)
    datatable(
      comp |> rename(
        `Groupe alimentaire` = Groupe, `Poids PAM` = Poids_PAM,
        `Consommation moy (j/sem)` = Consommation_moy,
        `Contribution moy (pts)` = Contribution_moy,
        `Contribution (%)` = Contribution_pct
      ),
      rownames = FALSE, options = list(dom = "t", pageLength = 12),
      class = "compact stripe"
    ) |>
      formatStyle(
        "Contribution (%)",
        background = styleColorBar(c(0, 100), "#0077B6"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ── rCSI Composantes ──
  output$sa_rcsi_components_plot <- renderPlotly({
    req(sa_res$rcsi)
    d_cur <- df_f()
    req(d_cur)
    cols_rcsi <- list(
      col_moins_prefere    = if (!is.null(input$sa_rcsi_prefere))    input$sa_rcsi_prefere    else NULL,
      col_emprunter        = if (!is.null(input$sa_rcsi_emprunter))   input$sa_rcsi_emprunter   else NULL,
      col_reduire_repas    = if (!is.null(input$sa_rcsi_repas))       input$sa_rcsi_repas       else NULL,
      col_reduire_portions = if (!is.null(input$sa_rcsi_portions))    input$sa_rcsi_portions    else NULL,
      col_reduire_adultes  = if (!is.null(input$sa_rcsi_adultes))     input$sa_rcsi_adultes     else NULL
    )
    tryCatch(
      do.call(plot_rcsi_components, c(list(d = d_cur), cols_rcsi)),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_rcsi_components_table <- renderDT({
    req(sa_res$rcsi)
    d_cur <- df_f(); req(d_cur)
    cols_rcsi <- list(
      col_moins_prefere    = if (!is.null(input$sa_rcsi_prefere))    input$sa_rcsi_prefere    else NULL,
      col_emprunter        = if (!is.null(input$sa_rcsi_emprunter))   input$sa_rcsi_emprunter   else NULL,
      col_reduire_repas    = if (!is.null(input$sa_rcsi_repas))       input$sa_rcsi_repas       else NULL,
      col_reduire_portions = if (!is.null(input$sa_rcsi_portions))    input$sa_rcsi_portions    else NULL,
      col_reduire_adultes  = if (!is.null(input$sa_rcsi_adultes))     input$sa_rcsi_adultes     else NULL
    )
    comp <- tryCatch(
      do.call(calc_rcsi_components, c(list(d = d_cur), cols_rcsi)),
      error = function(e) NULL
    )
    req(comp)
    datatable(
      comp |> rename(
        `Stratégie d'adaptation` = Stratégie, `Poids sévérité` = Poids,
        `Moy jours/sem` = Moy_jours, `% ménages recourant` = Pct_recours,
        `Contribution pondérée` = Contribution_moy
      ),
      rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
    ) |>
      formatStyle(
        "% ménages recourant",
        background = styleColorBar(c(0, 100), "#E67E22"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
      )
  })

  # ── Phases IPC/CH ──
  .ipc_phases_r <- reactive({
    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL
    d_cur  <- df_f()
    muac_s <- if (!is.null(d_cur) && ".muac" %in% names(d_cur)) d_cur$.muac else NULL
    if (is.null(fcs_s) && is.null(rcsi_s) && is.null(hhs_s)) return(NULL)
    calc_ipc_phase(fcs_s, rcsi_s, hhs_s, muac_s)
  })

  output$sa_ipc_kpi_cards <- renderUI({
    phases <- .ipc_phases_r(); req(phases)
    summ   <- calc_ipc_summary(phases); req(summ)
    phase3plus <- sum(summ$N[summ$Phase >= 3])
    pct3plus   <- round(phase3plus / sum(summ$N) * 100, 1)
    phase4plus <- sum(summ$N[summ$Phase >= 4])
    pct4plus   <- round(phase4plus / sum(summ$N) * 100, 1)

    col_main <- if (pct3plus >= 20) "#C80000" else if (pct3plus >= 10) "#E67800" else "#59A608"

    tags$div(
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;margin-bottom:10px;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Phase 3+ (Crise+)"),
        tags$div(style=paste0("font-size:32px;font-weight:800;color:", col_main, ";"), paste0(pct3plus, "%")),
        tags$div(style="font-size:11px;color:#555;", paste0(phase3plus, " / ", sum(summ$N), " ménages"))
      ),
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;margin-bottom:10px;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Phase 4-5 (Urgence+)"),
        tags$div(style="font-size:28px;font-weight:800;color:#C80000;", paste0(pct4plus, "%")),
        tags$div(style="font-size:11px;color:#555;", paste0(phase4plus, " ménages"))
      ),
      tags$div(style="background:white;border-radius:10px;padding:14px;border:1px solid #ddd;",
        tags$div(style="font-size:11px;text-transform:uppercase;color:#888;letter-spacing:1px;", "Répartition phases"),
        lapply(1:5, function(ph) {
          r <- summ[summ$Phase == ph, ]
          if (nrow(r) == 0 || r$N == 0) return(NULL)
          tags$div(style="display:flex;align-items:center;gap:6px;margin:3px 0;",
            tags$div(style=paste0("width:12px;height:12px;border-radius:2px;background:", r$Couleur, ";flex-shrink:0;")),
            tags$span(style="font-size:10px;color:#555;flex:1;", paste0("Ph", ph)),
            tags$span(style="font-size:11px;font-weight:700;", paste0(r$Pct, "%"))
          )
        })
      )
    )
  })

  output$sa_ipc_phases_plot <- renderPlotly({
    phases <- .ipc_phases_r(); req(phases)
    tryCatch(
      plot_ipc_phases(phases),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_ipc_by_group_plot <- renderPlotly({
    phases <- .ipc_phases_r(); req(phases)
    d_cur <- df_f(); req(d_cur)
    grp_var <- if (!is.null(input$sa_ipc_group_var) && input$sa_ipc_group_var != "")
      input$sa_ipc_group_var else NULL
    grp_vec <- if (!is.null(grp_var) && grp_var %in% names(d_cur)) d_cur[[grp_var]] else NULL
    tryCatch(
      plot_ipc_phases(phases, group_var = grp_vec),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_ipc_table <- renderDT({
    phases <- .ipc_phases_r(); req(phases)
    summ <- calc_ipc_summary(phases); req(summ)
    datatable(
      summ |> select(-Couleur) |> rename(`Phase IPC` = Phase, Libellé = Label,
                                          `N ménages` = N, `%` = Pct),
      rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
    ) |>
      formatStyle("Phase IPC",
        target = "row",
        backgroundColor = styleEqual(1:5, c("#2E7D52","#D4A843","#D98B14","#BF3636","#5C1A1A")),
        color = styleEqual(1:5, c("white","black","white","white","white"))
      )
  })

  # ── Comparaison inter-rounds ──
  output$sa_rounds_plot <- renderPlotly({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    req(length(round_col) > 0)
    round_col <- round_col[1]
    if (length(unique(d_cur[[round_col]])) < 2)
      return(plotly_empty() |> layout(title = list(
        text = "Au moins 2 rounds requis pour la comparaison", x = .5)))

    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hdds_s <- if (!is.null(sa_res$hdds)) sa_res$hdds$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL

    if (is.null(fcs_s) && is.null(rcsi_s) && is.null(hdds_s) && is.null(hhs_s))
      return(plotly_empty() |> layout(title = list(text = "Calculez les indicateurs SA d'abord", x = .5)))

    tryCatch(
      plot_sa_rounds(d_cur, round_col, fcs_s, rcsi_s, hdds_s, hhs_s),
      error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur:", e$message), x=.5))
    )
  })

  output$sa_rounds_table <- renderDT({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    req(length(round_col) > 0)
    round_col <- round_col[1]
    fcs_s  <- if (!is.null(sa_res$fcs))  sa_res$fcs$score  else NULL
    rcsi_s <- if (!is.null(sa_res$rcsi)) sa_res$rcsi$score else NULL
    hdds_s <- if (!is.null(sa_res$hdds)) sa_res$hdds$score else NULL
    hhs_s  <- if (!is.null(sa_res$hhs))  sa_res$hhs$score  else NULL

    tbl <- tryCatch(
      calc_sa_by_round(d_cur, round_col, fcs_s, rcsi_s, hdds_s, hhs_s),
      error = function(e) NULL
    )
    req(tbl)
    datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"),
              class = "compact stripe")
  })


  # ══════════════════════════════════════════════════════════════════
  # MODULE SA — COUCHE EXPERTE (lectures · OMS · ménages · export)
  # ══════════════════════════════════════════════════════════════════

  # ── Lectures expertes par indicateur ──
  output$sa_fcs_reading <- renderUI({
    txt <- sa_reading_fcs(sa_res$fcs); if (is.null(txt)) return(NULL)
    pk_info(txt, "utensils", "#0077B6", "#e8f6fc")
  })
  output$sa_rcsi_reading <- renderUI({
    txt <- sa_reading_rcsi(sa_res$rcsi); if (is.null(txt)) return(NULL)
    pk_info(txt, "triangle-exclamation", "#E67E22", "#fff3e0")
  })
  output$sa_hdds_reading <- renderUI({
    txt <- sa_reading_hdds(sa_res$hdds); if (is.null(txt)) return(NULL)
    pk_info(txt, "seedling", "#27AE60", "#eafaf1")
  })
  output$sa_hhs_reading <- renderUI({
    txt <- sa_reading_hhs(sa_res$hhs); if (is.null(txt)) return(NULL)
    pk_info(txt, "heartbeat", "#C0392B", "#fdf2f2")
  })

  # ── Lecture composition FCS ──
  output$sa_fcs_comp_reading <- renderUI({
    r <- sa_res$fcs; if (is.null(r)) return(NULL)
    comp <- calc_fcs_composition(r); if (is.null(comp) || nrow(comp) == 0) return(NULL)
    top <- comp[1, ]
    txt <- sprintf(paste0(
      "Lecture experte : le groupe %s contribue le plus au score moyen (%.1f pts, %.1f %% du FCS), ",
      "suivi de %s. Les c\u00e9r\u00e9ales et l\u00e9gumineuses p\u00e8sent structurellement lourd (poids PAM \u00d72 et \u00d73) : ",
      "un score acceptable peut masquer une alimentation peu diversifi\u00e9e si ces seuls groupes sont consomm\u00e9s."),
      top$Groupe, top$Contribution_moy, top$Contribution_pct,
      if (nrow(comp) > 1) comp$Groupe[2] else "\u2014")
    pk_info(txt, "chart-bar", "#0077B6", "#e8f6fc")
  })

  # ── Lecture rCSI détail ──
  output$sa_rcsi_comp_reading <- renderUI({
    if (is.null(sa_res$rcsi)) return(NULL)
    d_cur <- df_f(); req(d_cur)
    comp <- tryCatch(calc_rcsi_components(d_cur,
      col_moins_prefere    = if (is.null(input$sa_rcsi_prefere) || input$sa_rcsi_prefere == "") NULL else input$sa_rcsi_prefere,
      col_emprunter        = if (is.null(input$sa_rcsi_emprunter) || input$sa_rcsi_emprunter == "") NULL else input$sa_rcsi_emprunter,
      col_reduire_repas    = if (is.null(input$sa_rcsi_repas) || input$sa_rcsi_repas == "") NULL else input$sa_rcsi_repas,
      col_reduire_portions = if (is.null(input$sa_rcsi_portions) || input$sa_rcsi_portions == "") NULL else input$sa_rcsi_portions,
      col_reduire_adultes  = if (is.null(input$sa_rcsi_adultes) || input$sa_rcsi_adultes == "") NULL else input$sa_rcsi_adultes),
      error = function(e) NULL)
    if (is.null(comp)) return(NULL)
    top <- comp[1, ]
    txt <- sprintf(paste0(
      "Lecture experte : la strat\u00e9gie la plus fr\u00e9quente est \u00ab %s \u00bb (%.1f %% des m\u00e9nages, ",
      "%.2f j/sem en moyenne, contribution %.2f pts). ",
      "Les strat\u00e9gies de poids 3 (restriction des adultes) signalent les situations les plus s\u00e9v\u00e8res : ",
      "les suivre s\u00e9par\u00e9ment permet de distinguer stress \u00e9conomique et ins\u00e9curit\u00e9 alimentaire install\u00e9e."),
      top$Stratégie, top$Pct_recours, top$Moy_jours, top$Contribution_moy)
    pk_info(txt, "exclamation-circle", "#E67E22", "#fff3e0")
  })

  # ── Lecture stratification (χ²) ──
  output$sa_strat_reading <- renderUI({
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    if (is.null(strat_var)) return(NULL)
    d_cur <- df_f(); req(d_cur, strat_var %in% names(d_cur))
    class_vec <- if (!is.null(sa_res$fcs)) sa_res$fcs$class
                 else if (!is.null(sa_res$rcsi)) sa_res$rcsi$class
                 else NULL
    if (is.null(class_vec)) return(NULL)
    test <- calc_strat_test(class_vec, d_cur[[strat_var]])
    txt <- sa_reading_strat(test, strat_var)
    pk_info(txt, "chart-simple", "#0B3954", "#eef2f5")
  })

  # ── Lecture IPC ──
  output$sa_ipc_reading <- renderUI({
    phases <- .ipc_phases_r(); if (is.null(phases)) return(NULL)
    txt <- sa_reading_ipc(calc_ipc_summary(phases)); if (is.null(txt)) return(NULL)
    pk_info(txt, "globe", "#D4A843", "#fff8e1")
  })

  # ── Lecture rounds (tendance + test) ──
  output$sa_rounds_reading <- renderUI({
    d_cur <- df_f(); req(d_cur)
    round_col <- intersect(c("survey_round", "round", "vague", "collecte"), names(d_cur))
    if (length(round_col) == 0 || is.null(sa_res$fcs)) return(NULL)
    trend <- tryCatch(
      calc_rounds_trend(d_cur[[round_col[1]]], sa_res$fcs$class, "Pauvre",
                        "FCS pauvre"),
      error = function(e) NULL)
    txt <- sa_reading_rounds(trend); if (is.null(txt)) return(NULL)
    pk_info(txt, "calendar-alt", "#27AE60", "#eafaf1")
  })

  # ── Indicateurs OMS : LCSI · FIES · MDD-W ──
  sa_oms_any <- function() {
    !is.null(sa_res$lcsi) || !is.null(sa_res$fies) || !is.null(sa_res$mddw)
  }
  output$sa_lcsi_plot <- renderPlotly({
    if (is.null(sa_res$lcsi))
      return(plotly_empty() |> layout(title = list(
        text = "LCSI non calculé — mappez une classe ou les stratégies", x = .5)))
    tryCatch(plot_lcsi_plotly(sa_res$lcsi),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_lcsi_table <- renderDT({
    if (is.null(sa_res$lcsi) || is.null(sa_res$lcsi$summary))
      return(datatable(data.frame(Message = "LCSI non calculé"), rownames = FALSE, options = list(dom = "t")))
    datatable(sa_add_ci(sa_res$lcsi$summary) |> select(-Couleur) |>
                rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })
  output$sa_fies_plot <- renderPlotly({
    if (is.null(sa_res$fies))
      return(plotly_empty() |> layout(title = list(
        text = "FIES non calculé — sélectionnez une colonne de score (0-8)", x = .5)))
    tryCatch(plot_fies_plotly(sa_res$fies),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_fies_table <- renderDT({
    if (is.null(sa_res$fies) || is.null(sa_res$fies$summary))
      return(datatable(data.frame(Message = "FIES non calculé"), rownames = FALSE, options = list(dom = "t")))
    s <- sa_add_ci(sa_res$fies$summary) |> select(-Couleur)
    datatable(s, rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })
  output$sa_mddw_plot <- renderPlotly({
    if (is.null(sa_res$mddw))
      return(plotly_empty() |> layout(title = list(
        text = "MDD-W non calculé — sélectionnez la colonne binaire", x = .5)))
    tryCatch(plot_mddw_plotly(sa_res$mddw),
             error = function(e) plotly_empty() |> layout(title = list(text = paste("Erreur :", e$message), x = .5)))
  })
  output$sa_oms_reading <- renderUI({
    if (!sa_oms_any()) return(NULL)
    tagList(
      if (!is.null(sa_res$lcsi)) pk_info(sa_reading_lcsi(sa_res$lcsi),
        "briefcase", "#0077B6", "#e8f6fc"),
      if (!is.null(sa_res$fies)) pk_info(sa_reading_fies(sa_res$fies),
        "face-frown", "#E67E22", "#fff3e0"),
      if (!is.null(sa_res$mddw)) pk_info(sa_reading_mddw(sa_res$mddw),
        "bowl-food", "#27AE60", "#eafaf1")
    )
  })

  # ── Table ménages ──
  sa_household_r <- reactive({
    d_cur <- df_f(); req(d_cur)
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs) ||
               !is.null(sa_res$lcsi) || !is.null(sa_res$fies) || !is.null(sa_res$mddw)
    if (!any_res) return(NULL)
    build_sa_household_table(d_cur,
      fcs_res = sa_res$fcs, rcsi_res = sa_res$rcsi,
      hdds_res = sa_res$hdds, hhs_res = sa_res$hhs,
      lcsi_res = sa_res$lcsi, fies_res = sa_res$fies,
      mddw_res = sa_res$mddw, sev_res = sa_severity_r(),
      ipc_phases = .ipc_phases_r())
  })
  output$sa_household_table <- renderDT({
    tbl <- sa_household_r(); req(tbl)
    datatable(tbl, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
              class = "compact stripe hover", filter = "top")
  })

  # ── Exports ──
  sa_frames_r <- reactive({
    sa_export_frames(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs,
                     sa_res$lcsi, sa_res$fies, sa_res$mddw,
                     sa_severity_r(), calc_ipc_summary(.ipc_phases_r()),
                     sa_household_r(), sa_verdict_r())
  })
  output$sa_dl_xlsx <- downloadHandler(
    filename = function() paste0("securite_alimentaire_", Sys.Date(), ".xlsx"),
    content = function(file) {
      frames <- sa_frames_r()
      if (length(frames) == 0) { writeLines("Aucun résultat", file); return() }
      wb <- openxlsx::createWorkbook()
      for (nm in names(frames)) {
        openxlsx::addWorksheet(wb, substr(nm, 1, 31))
        openxlsx::writeData(wb, substr(nm, 1, 31), frames[[nm]])
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })
  sa_write_csv <- function(file) {
    hh <- sa_household_r()
    if (is.null(hh)) { writeLines("Aucun résultat", file); return(invisible()) }
    utils::write.csv2(hh, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
  output$sa_dl_csv_hh <- downloadHandler(
    filename = function() paste0("menages_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) sa_write_csv(file))
  output$sa_dl_csv_hh2 <- downloadHandler(
    filename = function() paste0("menages_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) sa_write_csv(file))
  output$sa_dl_csv_summary <- downloadHandler(
    filename = function() paste0("synthese_securite_alimentaire_", Sys.Date(), ".csv"),
    content = function(file) {
      frames <- sa_frames_r()
      frames$Menages <- NULL
      if (length(frames) == 0) { writeLines("Aucun résultat", file); return() }
      synth <- bind_rows(lapply(names(frames), function(nm) {
        s <- frames[[nm]]
        if (is.null(s) || nrow(s) == 0) return(NULL)
        s$Section <- nm
        s
      }))
      utils::write.csv2(synth, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output$sa_export_info <- renderUI({
    frames <- sa_frames_r()
    tags$div(style = "font-size:11px;color:#666;background:#f8f9fa;padding:8px 12px;border-radius:6px;",
      icon("circle-info"), " Contenu disponible : ",
      tags$b(paste(length(frames), "feuille(s)")), " — ",
      paste(setdiff(names(frames), "Menages"), collapse = ", "),
      if ("Menages" %in% names(frames))
        paste0(" + table ménages (", nrow(sa_household_r()), " lignes)")
    )
  })

  })
}
