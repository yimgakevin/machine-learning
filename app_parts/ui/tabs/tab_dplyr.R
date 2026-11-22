# Extrait de app_parts/ui/ui_body.R — onglet dplyr
ui_tab_dplyr <- function() {
      tabItem(tabName="dplyr",
        tags$div(
          style=paste0("background:linear-gradient(135deg,#0B3954,#00838F);",
                       "color:white;padding:14px 20px;border-radius:10px;margin-bottom:16px;"),
          tags$h4(style="margin:0;font-weight:700;",
                  icon("filter")," Manipulation des données — Interface dplyr"),
          tags$p(style="margin:4px 0 0;font-size:12px;opacity:.85;",
                 "Filtrer · Sélectionner · Mutater · Grouper · Résumer · Arranger · Joindre")
        ),
        fluidRow(
          box(width=4, status="primary", solidHeader=TRUE,
              title=tags$span(icon("filter")," Opérations dplyr"),
              tabsetPanel(
                tabPanel("Filtrer",
                  br(),
                  uiOutput("dplyr_filter_ui"),
                  actionButton("dplyr_apply_filter","✅ Appliquer le filtre",
                    class="btn btn-oms btn-block",width="100%",style="margin-top:8px;")
                ),
                tabPanel("Sélectionner",
                  br(),
                  uiOutput("dplyr_select_ui"),
                  actionButton("dplyr_apply_select","✅ Sélectionner",
                    class="btn btn-oms btn-block",width="100%",style="margin-top:8px;")
                ),
                tabPanel("Mutater",
                  br(),
                  textInput("dplyr_new_col","Nom nouvelle colonne :","nouvelle_var",width="100%"),
                  textAreaInput("dplyr_expr","Expression R :",
                    "col1 + col2  # ou: ifelse(col1>0, 'Positif', 'Négatif')",
                    rows=3, width="100%"),
                  actionButton("dplyr_apply_mutate","✅ Créer la colonne",
                    class="btn btn-oms btn-block",width="100%")
                ),
                tabPanel("Arranger",
                  br(),
                  uiOutput("dplyr_arrange_ui"),
                  radioButtons("dplyr_order","Ordre :",
                    choices=c("Croissant"="asc","Décroissant"="desc"),
                    inline=TRUE),
                  actionButton("dplyr_apply_arrange","✅ Trier",
                    class="btn btn-oms btn-block",width="100%")
                ),
                tabPanel("Grouper & Résumer",
                  br(),
                  uiOutput("dplyr_group_ui"),
                  uiOutput("dplyr_summarise_ui"),
                  actionButton("dplyr_apply_summarise","✅ Résumer",
                    class="btn btn-oms btn-block",width="100%",style="margin-top:8px;")
                ),
                tabPanel("Renommer",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Renommer une ou plusieurs colonnes avec rename()."),
                  uiOutput("dplyr_rename_ui"),
                  actionButton("dplyr_apply_rename","✅ Renommer",
                    class="btn btn-oms btn-block", width="100%", style="margin-top:8px;")
                ),
                tabPanel("Dédoublonner",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Supprimer les doublons avec distinct()."),
                  uiOutput("dplyr_distinct_ui"),
                  checkboxInput("dplyr_keep_all","Garder toutes les colonnes (.keep_all=TRUE)",
                                value=TRUE),
                  actionButton("dplyr_apply_distinct","✅ Dédoublonner",
                    class="btn btn-oms btn-block", width="100%")
                ),
                tabPanel("Découper",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Extraire un sous-ensemble de lignes avec slice_*()."),
                  selectInput("dplyr_slice_type","Méthode :",
                    choices=c("slice_head (premières N)"="head",
                              "slice_tail (dernières N)"="tail",
                              "slice_sample (N aléatoires)"="sample",
                              "slice_min (minimum d'une var)"="min",
                              "slice_max (maximum d'une var)"="max"),
                    width="100%"),
                  numericInput("dplyr_slice_n","N :", value=50, min=1, width="100%"),
                  uiOutput("dplyr_slice_col_ui"),
                  checkboxInput("dplyr_slice_with_ties","with_ties (inclure ex-aequo)", value=TRUE),
                  actionButton("dplyr_apply_slice","✅ Découper",
                    class="btn btn-oms btn-block", width="100%")
                ),
                tabPanel("Compter",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Compter les occurrences avec count() / add_count()."),
                  uiOutput("dplyr_count_ui"),
                  uiOutput("dplyr_count_wt_ui"),
                  checkboxInput("dplyr_count_sort","Trier par effectif décroissant", value=TRUE),
                  radioButtons("dplyr_count_mode","Mode :",
                    choices=c("count (résumé)"="count","add_count (ajouter colonne n)"="add"),
                    inline=TRUE),
                  actionButton("dplyr_apply_count","✅ Compter",
                    class="btn btn-oms btn-block", width="100%")
                ),
                tabPanel("Pivoter",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Transformer format long ↔ large avec pivot_wider / pivot_longer."),
                  radioButtons("dplyr_pivot_dir","Direction :",
                    choices=c("pivot_wider (long → large)"="wider",
                              "pivot_longer (large → long)"="longer"),
                    inline=TRUE),
                  conditionalPanel("input.dplyr_pivot_dir == 'wider'",
                    uiOutput("dplyr_pivot_names_from_ui"),
                    uiOutput("dplyr_pivot_values_from_ui")
                  ),
                  conditionalPanel("input.dplyr_pivot_dir == 'longer'",
                    uiOutput("dplyr_pivot_longer_cols_ui"),
                    textInput("dplyr_pivot_names_to","Nom colonne 'noms' :","variable",width="100%"),
                    textInput("dplyr_pivot_values_to","Nom colonne 'valeurs' :","valeur",width="100%")
                  ),
                  actionButton("dplyr_apply_pivot","✅ Pivoter",
                    class="btn btn-oms btn-block", width="100%")
                ),
                tabPanel("Transmuter",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Créer de nouvelles colonnes et ne garder qu'elles (transmute())."),
                  uiOutput("dplyr_transmute_ui"),
                  actionButton("dplyr_add_transmute_row","➕ Ajouter une expression",
                    style="width:100%;background:rgba(0,119,182,.1);border:1px solid #0077B6;
                           border-radius:4px;font-size:11px;margin:4px 0;"),
                  actionButton("dplyr_apply_transmute","✅ Transmuter",
                    class="btn btn-oms btn-block", width="100%", style="margin-top:4px;")
                ),
                tabPanel("Gérer NA",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Traiter les valeurs manquantes avec drop_na / replace_na / fill."),
                  radioButtons("dplyr_na_op","Opération :",
                    choices=c("drop_na (supprimer lignes NA)"="drop",
                              "replace_na (remplacer par valeur)"="replace",
                              "fill (propager valeur précédente/suivante)"="fill"),
                    selected="drop"),
                  uiOutput("dplyr_na_cols_ui"),
                  conditionalPanel("input.dplyr_na_op == 'replace'",
                    textInput("dplyr_na_val","Valeur de remplacement :","0",width="100%")
                  ),
                  conditionalPanel("input.dplyr_na_op == 'fill'",
                    radioButtons("dplyr_fill_dir","Direction :",
                      choices=c("Bas (valeur précédente)"="down","Haut (valeur suivante)"="up"),
                      inline=TRUE)
                  ),
                  actionButton("dplyr_apply_na","✅ Appliquer",
                    class="btn btn-oms btn-block", width="100%")
                ),
                tabPanel("Across",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Appliquer une fonction à plusieurs colonnes avec across()."),
                  radioButtons("dplyr_across_verb","Verbe :",
                    choices=c("mutate + across"="mutate","summarise + across"="summarise"),
                    inline=TRUE),
                  uiOutput("dplyr_across_cols_ui"),
                  selectInput("dplyr_across_fn","Fonction :",
                    choices=c("mean (moyenne)"="mean","median (médiane)"="median",
                              "sd (écart-type)"="sd","sum (somme)"="sum",
                              "min"="min","max"="max",
                              "n_distinct (nb valeurs uniques)"="n_distinct",
                              "~round(.,2) (arrondir)"="~round(.,2)",
                              "~scale(.) (centrer-réduire)"="~as.numeric(scale(.))"),
                    width="100%"),
                  uiOutput("dplyr_across_grp_ui"),
                  actionButton("dplyr_apply_across","✅ Appliquer across()",
                    class="btn btn-oms btn-block", width="100%")
                ),
                tabPanel("Code R",
                  br(),
                  tags$p(style="font-size:12px;color:#555;",
                    "Écrivez directement du code dplyr. df_f() = données filtrées actuelles."),
                  textAreaInput("dplyr_code",NULL,
                    'df_f() |>
  filter(GAM == TRUE) |>
  group_by(region) |>
  summarise(n=n(), GAM_pct=mean(GAM)*100)',
                    rows=6, width="100%"),
                  actionButton("dplyr_run_code","▶ Exécuter",
                    class="btn btn-oms btn-block",width="100%"),
                  br(),
                  verbatimTextOutput("dplyr_code_result")
                )
              )
          ),
          box(width=8, status="primary", solidHeader=TRUE,
              title=tags$span(icon("table")," Résultat de la manipulation"),
              fluidRow(
                column(6, uiOutput("dplyr_nrow_ui")),
                column(6,
                  downloadButton("dplyr_dl","📥 Télécharger le résultat",
                    style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;
                           padding:6px 12px;float:right;margin-top:5px;")
                )
              ),
              br(),
              DTOutput("dplyr_result_tbl"),
              br(),
              uiOutput("dplyr_stats_quick_ui")
          )
        ),
        fluidRow(
          box(width=12, status="info", solidHeader=TRUE,
              title=tags$span(icon("chart-bar")," Visualisation rapide du résultat"),
              fluidRow(
                column(3,
                  uiOutput("dplyr_plot_x_ui"),
                  uiOutput("dplyr_plot_y_ui"),
                  selectInput("dplyr_plot_type","Type :",
                    choices=c("Barplot"="bar","Boxplot"="box","Scatter"="scatter",
                              "Histogramme"="hist","Densité"="density"),
                    width="100%"),
                  actionButton("dplyr_plot_btn","📊 Tracer",
                    class="btn btn-oms btn-block",width="100%")
                ),
                column(9, plotlyOutput("dplyr_plot_out",height="350px"))
              )
          )
        )
      )
}
