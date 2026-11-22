# Extrait de app_parts/ui/ui_body.R — onglet rdim
ui_tab_rdim <- function() {
      tabItem(tabName="rdim",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("sitemap"), "Réduction de dimension"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Simplifiez des données multivariées complexes avec l'ACP (Analyse en Composantes Principales) ",
                 "ou l'AFC (Analyse Factorielle des Correspondances). ",
                 "Identifiez les axes principaux de variation et visualisez les individus et variables dans un espace réduit.")
        ),
        fluidRow(
          box(width=3, status="primary", solidHeader=TRUE,
              title=icon("sitemap"), " Paramètres",
              tags$div(class="section-header","Méthode"),
              radioButtons("rdim_method","Choisir automatiquement ou manuellement :",
                choices=c(
                  "Auto (selon le type de données)"="auto",
                  "ACP — Variables numériques"="pca",
                  "ACM — Variables catégorielles"="mca",
                  "FAMD — Données mixtes"="famd",
                  "AFC — Tableau de contingence"="ca"
                ), selected="auto"),
              uiOutput("rdim_vars_ui"),
              numericInput("rdim_ndim","Dimensions à garder :",5,2,15,1),
              checkboxInput("rdim_scale","Centrer-réduire (ACP)",TRUE),
              hr(),
              tags$div(class="section-header","Variable de groupage (biplot)"),
              uiOutput("rdim_grp_ui"),
              hr(),
              tags$div(class="section-header","Mode d'affichage"),
              radioButtons("rdim_render_mode","",
                choices=c(
                  "🖱️ Interactif (Plotly)"="plotly",
                  "📊 factoextra (Publication)"="factoextra"
                ), selected="plotly", inline=FALSE),
              hr(),
              tags$div(class="section-header","Résumé méthode"),
              verbatimTextOutput("rdim_method_info"),
              actionButton("run_rdim","🔬 Lancer l'analyse",
                           class="btn btn-oms btn-block", width="100%"),
              br(),
              actionButton("run_factoshiny","🌐 Ouvrir FactoShiny",
                style="width:100%;background:linear-gradient(135deg,#8E44AD,#6C3483);
                       color:white;border:none;border-radius:5px;padding:7px;
                       font-size:12px;font-weight:600;margin-top:4px;"),
              br(),
              uiOutput("rdim_kaiser_badge")
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Résultats — Réduction de dimension",
              tabsetPanel(
                tabPanel("📊 Éboulis des valeurs propres",
                  downloadButton("dl_rdim_scree","⬇ PNG", class="btn btn-xs",
                    style="float:right;margin:4px;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:6px;padding:3px 8px;font-size:11px;"),
                  uiOutput("rdim_scree_ui")),
                tabPanel("🎯 Sélection automatique des axes",
                         uiOutput("rdim_selec_auto")),
                tabPanel("🔵 Cercle des corrélations",
                         fluidRow(
                           column(3,
                             selectInput("rdim_axes_x","Axe X :",
                               choices=paste0("Dim.",1:10), selected="Dim.1", width="100%"),
                             selectInput("rdim_axes_y","Axe Y :",
                               choices=paste0("Dim.",1:10), selected="Dim.2", width="100%"),
                             radioButtons("rdim_circle_color","Colorer par :",
                               choices=c("Cos² (qualité)"="cos2","Contribution"="contrib","Uniforme"="none"),
                               selected="cos2"),
                             downloadButton("dl_rdim_circle","⬇ PNG",
                               style="width:100%;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;
                                      border-radius:4px;padding:4px;font-size:11px;margin-top:6px;")
                           ),
                           column(9, uiOutput("rdim_circle_ui"))
                         )),
                tabPanel("👥 Carte des individus",
                         fluidRow(
                           column(3,
                             uiOutput("rdim_biplot_grp_ui"),
                             radioButtons("rdim_ind_color","Colorer par :",
                               choices=c("Variable groupe"="grp","Cos²"="cos2","Densité"="density"),
                               selected="grp"),
                             downloadButton("dl_rdim_biplot","⬇ PNG",
                               style="width:100%;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;
                                      border-radius:4px;padding:4px;font-size:11px;margin-top:6px;")
                           ),
                           column(9, uiOutput("rdim_biplot_ui"))
                         )),
                tabPanel("📈 Contributions variables",
                         fluidRow(
                           column(3,
                             uiOutput("rdim_contrib_dim_ui"),
                             checkboxInput("rdim_contrib_thresh","Afficher seuil (1/p)×100",TRUE),
                             downloadButton("dl_rdim_contrib","⬇ PNG",
                               style="width:100%;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;
                                      border-radius:4px;padding:4px;font-size:11px;margin-top:6px;")
                           ),
                           column(9, uiOutput("rdim_contrib_ui"))
                         )),
                tabPanel("🎨 Cos² (qualité)",
                  downloadButton("dl_rdim_cos2","⬇ PNG", class="btn btn-xs",
                    style="float:right;margin:4px;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:6px;padding:3px 8px;font-size:11px;"),
                  uiOutput("rdim_cos2_ui")),
                tabPanel("🔥 Heatmap contributions",
                  downloadButton("dl_rdim_heat","⬇ PNG", class="btn btn-xs",
                    style="float:right;margin:4px;background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:6px;padding:3px 8px;font-size:11px;"),
                  uiOutput("rdim_heatmap_ui")),
                tabPanel("📋 Tableau des résultats",
                         DTOutput("rdim_table")),

                # ── ONGLETS EXPERT (module_rdim_expert.R) ────────────
                tabPanel("\U0001f9ec Analyse Parallèle (Horn)",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " L'analyse parallèle de Horn compare les valeurs propres réelles ",
                    "à celles obtenues sur données aléatoires. Plus rigoureux que Kaiser."),
                  fluidRow(
                    column(4, sliderInput("pa_nsim","Simulations :",
                      min=100, max=1000, value=500, step=100)),
                    column(4, br(), actionButton("run_parallel","▶ Lancer analyse parallèle",
                      class="btn btn-warning", style="margin-top:5px;font-weight:600;"))
                  ),
                  plotlyOutput("pa_plot", height="450px"), br(),
                  DTOutput("pa_table")
                ),

                tabPanel("\U0001f333 HCPC (Classification sur CP)",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " Classification Hiérarchique sur Composantes Principales. ",
                    "Combine la réduction de dimension avec le clustering."),
                  fluidRow(
                    column(4, numericInput("hcpc_nclust","Nombre de clusters (0=auto) :",
                      value=0, min=0, max=10, step=1)),
                    column(4, br(), actionButton("run_hcpc","▶ Lancer HCPC",
                      class="btn btn-success", style="margin-top:5px;font-weight:600;color:white;"))
                  ),
                  fluidRow(
                    column(6, plotlyOutput("hcpc_map", height="420px")),
                    column(6, plotlyOutput("hcpc_dendro", height="420px"))
                  ), br(),
                  DTOutput("hcpc_desc_table")
                ),

                tabPanel("\U0001f30c t-SNE",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " t-SNE projette les données de haute dimension en 2D en préservant ",
                    "les voisinages locaux. Idéal pour détecter des clusters non-linéaires."),
                  fluidRow(
                    column(3, sliderInput("tsne_perp","Perplexité :",
                      min=5, max=100, value=30, step=5)),
                    column(3, sliderInput("tsne_iter","Itérations :",
                      min=300, max=2000, value=1000, step=100)),
                    column(3, uiOutput("tsne_grp_ui")),
                    column(3, br(), actionButton("run_tsne","▶ Lancer t-SNE",
                      class="btn btn-info", style="margin-top:5px;font-weight:600;color:white;"))
                  ),
                  plotlyOutput("tsne_plot", height="480px")
                ),

                tabPanel("\U0001f52d UMAP",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " UMAP préserve mieux la structure globale que t-SNE et est plus rapide. ",
                    "Paramètres : n_neighbors (structure locale) et min_dist (compacité)."),
                  fluidRow(
                    column(3, sliderInput("umap_nn","Voisins (n_neighbors) :",
                      min=3, max=50, value=15, step=2)),
                    column(3, sliderInput("umap_md","Distance min (min_dist) :",
                      min=0.01, max=0.99, value=0.1, step=0.05)),
                    column(3, uiOutput("umap_grp_ui")),
                    column(3, br(), actionButton("run_umap","▶ Lancer UMAP",
                      class="btn btn-info", style="margin-top:5px;font-weight:600;color:white;"))
                  ),
                  plotlyOutput("umap_plot", height="480px")
                ),

                tabPanel("\U0001f504 Stabilité Bootstrap",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " Évalue la stabilité des loadings ACP par rééchantillonnage bootstrap. ",
                    "Un IC 95% qui inclut 0 indique un loading instable."),
                  fluidRow(
                    column(4, sliderInput("boot_nsim","Rééchantillonnages :",
                      min=100, max=1000, value=500, step=100)),
                    column(4, uiOutput("boot_dim_ui")),
                    column(4, br(), actionButton("run_boot","▶ Lancer bootstrap",
                      class="btn btn-warning", style="margin-top:5px;font-weight:600;"))
                  ),
                  plotlyOutput("boot_plot", height="480px"), br(),
                  DTOutput("boot_table")
                ),

                tabPanel("\U0001f504 Comparaison méthodes",
                  tags$div(style="margin:10px 0;font-size:12px;color:#666;",
                    icon("info-circle"),
                    " Compare ACP (linéaire) vs t-SNE vs UMAP (non-linéaires) ",
                    "sur les mêmes données."),
                  fluidRow(
                    column(4, uiOutput("cmp_grp_ui")),
                    column(4, br(), actionButton("run_cmp_methods","▶ Comparer",
                      class="btn btn-primary", style="margin-top:5px;font-weight:600;"))
                  ),
                  plotlyOutput("cmp_methods_plot", height="450px")
                )
              )
          )
        )
      )
}
