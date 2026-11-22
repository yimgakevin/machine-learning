# Extrait de app_parts/ui/ui_body.R — onglet uni
ui_tab_uni <- function() {
      tabItem(tabName="uni",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("chart-bar"), "Analyse univariée"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Analysez une variable à la fois : histogrammes, diagrammes en barres, boîtes à moustaches ",
                 "et statistiques descriptives. Utile pour comprendre la distribution de chaque indicateur ",
                 "avant de passer aux analyses croisées.")
        ),
        fluidRow(
          box(width=3, status="primary", solidHeader=TRUE,
              title=icon("sliders"), " Paramètres",
              selectInput("uni_type","Type de graphique :",
                choices=c(
                  "— Qualitatif —"="",
                  "Barplot (effectifs)"="barplot",
                  "Barplot (proportions %)"="barplot_pct",
                  "Camembert / Donut"="pie",
                  "— Quantitatif —"="",
                  "Histogramme + Gauss"="histo",
                  "Boxplot annoté"="boxplot",
                  "Violin + Boxplot"="violin",
                  "Courbe de densité"="density",
                  "Courbe de densité ridgeline"="ridgeline",
                  "QQ-plot (normalité)"="qqplot",
                  "Densité vs OMS N(0,1)"="oms_ref",
                  "— Combiné —"="",
                  "Histogramme + densité"="histo_dens",
                  "Boxplot + points (jitter)"="boxjitter"
                ), width="100%"),
              uiOutput("uni_var_ui"),
              selectInput("uni_pal","Palette :",
                choices=c("Auto (Global)" = "Auto (Global)",
                          PALETTE_CHOICES,
                          "Gradient bleu/rouge","RdYlGn"),
                width="100%"),
              checkboxInput("uni_lbl","Afficher effectifs/valeurs",TRUE),
              checkboxInput("uni_interactive","Mode ggiraph (hover+)",FALSE),
              hr(),
              tags$div(class="section-header","📐 Statistiques complètes"),
              div(style="max-height:350px;overflow-y:auto;font-size:12px;",
                  tableOutput("uni_stats_tbl"))
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Graphique — sélection instantanée",
              div(style="text-align:right;margin-bottom:8px;",
                downloadButton("uni_dl_plot","📥 Télécharger HD",
                  style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:5px 12px;font-size:12px;")),
              uiOutput("uni_plot_ui", style="height:500px;")
          )
        )
      )
}
