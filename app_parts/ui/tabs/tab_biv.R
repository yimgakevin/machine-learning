# Extrait de app_parts/ui/ui_body.R — onglet biv
ui_tab_biv <- function() {
      tabItem(tabName="biv",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("chart-line"), "Analyse bivariée"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Étudiez la relation entre deux variables : tableaux croisés, nuages de points, ",
                 "corrélations et tests d'association. Permet de détecter des liens ",
                 "entre facteurs (quali × quali, quali × quanti, quanti × quanti).")
        ),
        fluidRow(
          box(width=3, status="primary", solidHeader=TRUE,
              title=icon("sliders"), " Paramètres bivariés",
              selectInput("biv_type","Type :",
                choices=c(
                  "— Quali × Quali —"="",
                  "Barplot groupé (dodge)"="dodge",
                  "Barplot empilé (%)"="stack",
                  "Heatmap fréquences"="heatfreq",
                  "Mosaïque (proportions)"="mosaic",
                  "— Quali × Quanti —"="",
                  "Boxplot par groupe"="boxgrp",
                  "Violin par groupe"="violgrp",
                  "Densités superposées"="densgrp",
                  "Histogrammes facettes"="facethist",
                  "ggdist (halfeye)"="halfeye",
                  "— Quanti × Quanti —"="",
                  "Nuage de points + LOESS"="scatter",
                  "Hexbin (densité 2D)"="hexbin",
                  "Heatmap corrélations"="cormap"
                ), width="100%"),
              uiOutput("biv_x_ui"),
              uiOutput("biv_y_ui"),
              selectInput("biv_pal","Palette :",
                choices=c("Auto (Global)" = "Auto (Global)",
                          PALETTE_CHOICES,
                          "Set2","RdYlGn"),
                width="100%"),
              hr(),
              tags$div(class="section-header","🧪 Test automatique"),
              verbatimTextOutput("biv_test_result"),
              uiOutput("biv_interpretation")
          ),
          box(width=9, status="primary", solidHeader=TRUE,
              title="Graphique bivarié",
              div(style="text-align:right;margin-bottom:8px;",
                downloadButton("biv_dl_plot","📥 Télécharger HD",
                  style="background:linear-gradient(135deg,#0077B6,#0C4A6E);color:white;border:none;border-radius:8px;padding:5px 12px;font-size:12px;")),
              plotlyOutput("biv_plot", height="500px")
          )
        )
      )
}
