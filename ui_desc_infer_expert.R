# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  ui_desc_infer_expert.R — UI expert pour descriptif + inférentiel          ║
# ║  Tabs additionnels intégrés dans les sections existantes                   ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── TABS EXPERT DESCRIPTIF (à insérer dans la section Descriptive OMS) ──────

desc_expert_tabs <- function(ns = shiny::NS(NULL)) {
  list(

    # ── Tab 1 : Bootstrap IC ────────────────────────────────────────────
    tabPanel(tags$span(icon("random"), " Bootstrap IC"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Paramètres Bootstrap"),
            uiOutput(ns("desc_boot_var_ui")),
            sliderInput(ns("desc_boot_B"), "Nombre de rééchantillonnages :",
                        min = 500, max = 5000, value = 2000, step = 500, width = "100%"),
            sliderInput(ns("desc_boot_conf"), "Niveau de confiance :",
                        min = 0.80, max = 0.99, value = 0.95, step = 0.01, width = "100%"),
            actionButton(ns("run_desc_boot"), "Lancer Bootstrap",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("Bootstrap non-paramétrique :"), tags$br(),
              "IC calculé par percentiles sur B rééchantillonnages. ",
              "Valide sans hypothèse de normalité."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "IC Bootstrap pour statistiques descriptives"),
            DTOutput(ns("desc_boot_table")),
            br(),
            tags$div(class = "section-header", "Comparaison : classique vs robuste"),
            uiOutput(ns("desc_boot_comparison"))
          )
        )
      )
    ),

    # ── Tab 2 : Statistiques robustes ───────────────────────────────────
    tabPanel(tags$span(icon("shield-alt"), " Stats robustes"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Variable à analyser"),
            uiOutput(ns("desc_robust_var_ui")),
            actionButton(ns("run_desc_robust"), "Calculer",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("Mesures robustes :"), tags$br(),
              "Moyenne tronquée, winsorisée, MAD, Huber M-estimateur. ",
              "Résistantes aux valeurs aberrantes."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Tableau comparatif : classique vs robuste"),
            DTOutput(ns("desc_robust_table"))
          )
        )
      )
    ),

    # ── Tab 3 : Batterie de normalité ───────────────────────────────────
    tabPanel(tags$span(icon("bell"), " Batterie normalité"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Paramètres"),
            uiOutput(ns("desc_norm_var_ui")),
            sliderInput(ns("desc_norm_alpha"), "Seuil α :",
                        min = 0.01, max = 0.10, value = 0.05, step = 0.01, width = "100%"),
            actionButton(ns("run_desc_norm"), "Lancer batterie",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            uiOutput(ns("desc_norm_verdict")),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("6 tests de normalité :"), tags$br(),
              "Shapiro-Wilk, Anderson-Darling, Lilliefors, ",
              "Cramér-von Mises, Jarque-Bera, D'Agostino-Pearson. ",
              "Verdict par vote majoritaire."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Résultats de la batterie de normalité"),
            DTOutput(ns("desc_norm_table")),
            br(),
            tags$div(class = "section-header", "QQ-plot diagnostique"),
            plotlyOutput(ns("desc_norm_qqplot"), height = "350px")
          )
        )
      )
    ),

    # ── Tab 4 : Détection outliers ──────────────────────────────────────
    tabPanel(tags$span(icon("exclamation-triangle"), " Outliers"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Variable"),
            uiOutput(ns("desc_outlier_var_ui")),
            actionButton(ns("run_desc_outlier"), "Détecter outliers",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("5 méthodes :"), tags$br(),
              "IQR (Tukey), Z-score, MAD (robuste), ",
              "Grubbs (max outlier), Percentiles extrêmes."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Synthèse multi-méthodes"),
            DTOutput(ns("desc_outlier_table")),
            br(),
            tags$div(class = "section-header", "Diagnostic visuel"),
            plotlyOutput(ns("desc_outlier_plot"), height = "380px")
          )
        )
      )
    ),

    # ── Tab 5 : Ajustement de distributions ─────────────────────────────
    tabPanel(tags$span(icon("chart-area"), " Distributions"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Variable"),
            uiOutput(ns("desc_distfit_var_ui")),
            actionButton(ns("run_desc_distfit"), "Ajuster distributions",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            uiOutput(ns("desc_distfit_best")),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("5 distributions testées :"), tags$br(),
              "Normale, Log-Normale, Gamma, Weibull, Exponentielle. ",
              "Classement par AIC/BIC + test KS d'adéquation."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Classement des distributions (AIC)"),
            DTOutput(ns("desc_distfit_table")),
            br(),
            tags$div(class = "section-header", "Superposition graphique"),
            plotlyOutput(ns("desc_distfit_plot"), height = "400px")
          )
        )
      )
    ),

    # ── Tab 6 : Profil par groupe ───────────────────────────────────────
    tabPanel(tags$span(icon("users"), " Profil groupes"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Paramètres"),
            uiOutput(ns("desc_profile_grp_ui")),
            uiOutput(ns("desc_profile_vars_ui")),
            actionButton(ns("run_desc_profile"), "Générer profil",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;")
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Tableau descriptif complet par groupe"),
            DTOutput(ns("desc_profile_table")),
            br(),
            tags$div(class = "section-header", "Heatmap des moyennes normalisées"),
            plotlyOutput(ns("desc_profile_heatmap"), height = "400px")
          )
        )
      )
    ),

    # ── Tab 7 : Corrélation avancée ─────────────────────────────────────
    tabPanel(tags$span(icon("project-diagram"), " Corrélation avancée"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Paramètres"),
            uiOutput(ns("desc_cor_adv_vars_ui")),
            selectInput(ns("desc_cor_method"), "Méthode :",
                        choices = c("Spearman" = "spearman", "Pearson" = "pearson", "Kendall" = "kendall"),
                        selected = "spearman", width = "100%"),
            selectInput(ns("desc_cor_padj"), "Correction multiple :",
                        choices = c("BH (Benjamini-Hochberg)" = "BH",
                                    "Bonferroni" = "bonferroni",
                                    "Holm" = "holm",
                                    "Aucune" = "none"),
                        selected = "BH", width = "100%"),
            actionButton(ns("run_desc_cor_adv"), "Calculer corrélations",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            uiOutput(ns("desc_cor_adv_summary"))
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Paires de corrélations (classées par |r|)"),
            DTOutput(ns("desc_cor_adv_table")),
            br(),
            tags$div(class = "section-header", "Matrice de corrélation avec significativité"),
            plotOutput(ns("desc_cor_adv_plot"), height = "450px")
          )
        )
      )
    )
  )
}

# ── TABS EXPERT INFÉRENTIEL (à insérer dans la section Inférentielle) ──────

infer_expert_tabs <- function(ns = shiny::NS(NULL)) {
  list(

    # ── Tab 1 : Tests appariés ──────────────────────────────────────────
    tabPanel(tags$span(icon("link"), " Tests appariés"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Variables appariées"),
            uiOutput(ns("infer_paired_x1_ui")),
            uiOutput(ns("infer_paired_x2_ui")),
            sliderInput(ns("infer_paired_alpha"), "Seuil α :",
                        min = 0.01, max = 0.10, value = 0.05, step = 0.01, width = "100%"),
            actionButton(ns("run_infer_paired"), "Lancer tests appariés",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("3 tests appariés :"), tags$br(),
              "t-test apparié, Wilcoxon signé, Test des signes. ",
              "Pour mesures avant/après ou données pairées."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Résultats des tests appariés"),
            uiOutput(ns("infer_paired_cards")),
            br(),
            tags$div(class = "section-header", "Distribution des différences"),
            plotlyOutput(ns("infer_paired_plot"), height = "350px")
          )
        )
      )
    ),

    # ── Tab 2 : Puissance statistique ───────────────────────────────────
    tabPanel(tags$span(icon("bolt"), " Puissance"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Paramètres de puissance"),
            selectInput(ns("infer_power_type"), "Type de test :",
                        choices = c("t-test 2 groupes" = "t2",
                                    "t-test apparié" = "t_paired",
                                    "ANOVA" = "anova",
                                    "Corrélation" = "cor",
                                    "Chi²" = "chi2"),
                        width = "100%"),
            numericInput(ns("infer_power_n"), "N par groupe :", value = 50, min = 5, max = 10000, width = "100%"),
            numericInput(ns("infer_power_effect"), "Taille d'effet :", value = 0.5, min = 0.01, max = 3, step = 0.05, width = "100%"),
            sliderInput(ns("infer_power_alpha"), "Alpha :",
                        min = 0.01, max = 0.10, value = 0.05, step = 0.01, width = "100%"),
            conditionalPanel(ns = ns, 
              condition = "input.infer_power_type == 'anova' || input.infer_power_type == 'chi2'",
              numericInput(ns("infer_power_k"), "k (groupes/ddl) :", value = 3, min = 2, max = 20, width = "100%")
            ),
            actionButton(ns("run_infer_power"), "Calculer puissance",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            uiOutput(ns("infer_power_result"))
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Courbe de puissance"),
            plotlyOutput(ns("infer_power_curve"), height = "420px"),
            br(),
            tags$div(class = "section-header", "Grille de puissance (N × effet)"),
            DTOutput(ns("infer_power_grid"))
          )
        )
      )
    ),

    # ── Tab 3 : Bayes Factor ───────────────────────────────────────────
    tabPanel(tags$span(icon("brain"), " Bayes Factor"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Variables"),
            uiOutput(ns("infer_bf_x_ui")),
            uiOutput(ns("infer_bf_y_ui")),
            actionButton(ns("run_infer_bf"), "Calculer Bayes Factor",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("Interprétation BF10 :"), tags$br(),
              "> 100 : Décisif | 30-100 : Très fort", tags$br(),
              "10-30 : Fort | 3-10 : Modéré", tags$br(),
              "1-3 : Anecdotique pour H1", tags$br(),
              "1/3-1 : Anecdotique pour H0", tags$br(),
              "< 1/3 : Modéré pour H0"
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Résultats bayésiens"),
            DTOutput(ns("infer_bf_table")),
            br(),
            uiOutput(ns("infer_bf_interpretation"))
          )
        )
      )
    ),

    # ── Tab 4 : Équivalence TOST ────────────────────────────────────────
    tabPanel(tags$span(icon("equals"), " Équivalence TOST"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Paramètres TOST"),
            uiOutput(ns("infer_tost_grp_ui")),
            uiOutput(ns("infer_tost_y_ui")),
            numericInput(ns("infer_tost_delta"), "Marge d'équivalence (Δ) :",
                         value = NA, min = 0, step = 0.1, width = "100%"),
            tags$div(style = "font-size:10px;color:#888;margin-top:-8px;margin-bottom:8px;",
                     "Laisser vide = 0.5 × SD poolé (auto)"),
            sliderInput(ns("infer_tost_alpha"), "Alpha :",
                        min = 0.01, max = 0.10, value = 0.05, step = 0.01, width = "100%"),
            actionButton(ns("run_infer_tost"), "Test d'équivalence",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("TOST :"), tags$br(),
              "Teste si la différence est DANS les bornes [-Δ, +Δ]. ",
              "Complément du test de différence classique."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Résultat du test d'équivalence"),
            uiOutput(ns("infer_tost_result")),
            br(),
            tags$div(class = "section-header", "Visualisation TOST"),
            plotlyOutput(ns("infer_tost_plot"), height = "300px")
          )
        )
      )
    ),

    # ── Tab 5 : Régression + Diagnostics ────────────────────────────────
    tabPanel(tags$span(icon("chart-line"), " Régression diagnostics"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Modèle de régression"),
            uiOutput(ns("infer_reg_outcome_ui")),
            uiOutput(ns("infer_reg_pred_ui")),
            actionButton(ns("run_infer_reg"), "Ajuster le modèle",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            uiOutput(ns("infer_reg_summary")),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("Diagnostics inclus :"), tags$br(),
              "VIF (multicolinéarité), Cook (influence), ",
              "Durbin-Watson (autocorrélation), ",
              "Breusch-Pagan (hétéroscédasticité), ",
              "Shapiro-Wilk (normalité résidus)."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tabsetPanel(
              tabPanel("Coefficients",
                tags$div(class = "section-header", "Coefficients avec IC 95%"),
                DTOutput(ns("infer_reg_coef_table")),
                br(),
                plotlyOutput(ns("infer_reg_forest"), height = "350px")
              ),
              tabPanel("Diagnostics",
                tags$div(class = "section-header", "Diagnostics du modèle"),
                DTOutput(ns("infer_reg_diag_table")),
                br(),
                tags$div(class = "section-header", "Graphiques diagnostiques"),
                plotOutput(ns("infer_reg_diag_plots"), height = "600px")
              ),
              tabPanel("VIF",
                tags$div(class = "section-header", "Facteur d'inflation de la variance (VIF)"),
                DTOutput(ns("infer_reg_vif_table")),
                br(),
                uiOutput(ns("infer_reg_vif_interp"))
              )
            )
          )
        )
      )
    ),

    # ── Tab 6 : Mesures répétées ────────────────────────────────────────
    tabPanel(tags$span(icon("redo"), " Mesures répétées"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Variables (mesures répétées)"),
            uiOutput(ns("infer_repeat_vars_ui")),
            sliderInput(ns("infer_repeat_alpha"), "Seuil α :",
                        min = 0.01, max = 0.10, value = 0.05, step = 0.01, width = "100%"),
            actionButton(ns("run_infer_repeat"), "Test de Friedman",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("Friedman :"), tags$br(),
              "Alternative non paramétrique à l'ANOVA à mesures répétées. ",
              "Post-hoc : Wilcoxon par paires avec correction BH."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Résultat du test de Friedman"),
            uiOutput(ns("infer_repeat_result")),
            br(),
            tags$div(class = "section-header", "Comparaisons post-hoc par paires"),
            DTOutput(ns("infer_repeat_posthoc")),
            br(),
            tags$div(class = "section-header", "Distribution par mesure"),
            plotlyOutput(ns("infer_repeat_plot"), height = "380px")
          )
        )
      )
    ),

    # ── Tab 7 : Tailles d'effet avec IC ─────────────────────────────────
    tabPanel(tags$span(icon("ruler"), " Tailles d'effet IC"),
      fluidRow(
        column(3,
          tags$div(style = "padding:8px;",
            tags$div(class = "section-header", "Variables"),
            uiOutput(ns("infer_es_x_ui")),
            uiOutput(ns("infer_es_y_ui")),
            sliderInput(ns("infer_es_B"), "Bootstrap B :",
                        min = 500, max = 5000, value = 2000, step = 500, width = "100%"),
            actionButton(ns("run_infer_es"), "Calculer tailles d'effet",
                         class = "btn btn-oms btn-block", width = "100%",
                         style = "margin-top:8px;font-size:13px;padding:8px;"),
            hr(),
            tags$div(style = "font-size:11px;color:#555;line-height:1.7;",
              tags$b("4 mesures :"), tags$br(),
              "Cohen d + IC bootstrap, Hedges g (corrigé), ",
              "Glass Δ, CLES (probabilité de supériorité)."
            )
          )
        ),
        column(9,
          tags$div(style = "padding:4px;",
            tags$div(class = "section-header", "Tailles d'effet avec intervalles de confiance"),
            DTOutput(ns("infer_es_table")),
            br(),
            uiOutput(ns("infer_es_interpretation"))
          )
        )
      )
    ),

    # ── Tab 8 : Synthèse expert ─────────────────────────────────────────
    tabPanel(tags$span(icon("clipboard-list"), " Synthèse expert"),
      fluidRow(
        column(12,
          tags$div(style = "padding:12px;",
            tags$div(
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                       border-radius: 10px; padding: 16px 20px; margin-bottom: 16px;",
              tags$h4(style = "color:white; margin:0;", icon("star"),
                      " Tableau de synthèse — Approches fréquentiste + bayésienne + puissance")
            ),
            tags$div(class = "section-header", "Vue unifiée de toutes les analyses"),
            DTOutput(ns("infer_expert_summary_table")),
            br(),
            tags$div(class = "section-header", "Corrections pour comparaisons multiples"),
            DTOutput(ns("infer_expert_corrections_table")),
            br(),
            div(style = "text-align:right;",
              downloadButton(ns("dl_expert_synthesis"), "Télécharger synthèse (CSV)",
                style = "background:#27ae60;color:white;border:none;border-radius:6px;padding:6px 14px;")
            )
          )
        )
      )
    )
  )
}

cat("ui_desc_infer_expert.R chargé — 7 tabs descriptif + 8 tabs inférentiel expert\n")
