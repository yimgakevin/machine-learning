# Extrait de app_parts/ui/ui_body.R — onglet clust
ui_tab_clust <- function() {
      tabItem(tabName="clust",
        tags$div(
          style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
                   border-left: 4px solid #0077B6; border-radius: 8px;
                   padding: 15px 20px; margin-bottom: 15px;",
          tags$h4(style = "margin:0 0 8px 0; color:#0077B6;",
                  icon("object-group"), "Clustering — Classification non supervisée"),
          tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
                 "Regroupez automatiquement vos observations en profils similaires. ",
                 "Trois méthodes disponibles : K-means, CAH (Classification Ascendante Hiérarchique) et DBSCAN. ",
                 "Utile pour identifier des sous-populations ou des typologies dans vos données.")
        ),
        fluidRow(
          box(width=3, status="info", solidHeader=TRUE,
              title=icon("object-group"), " Paramètres",
              radioButtons("clust_method","Méthode :",
                choices=c("K-means"="kmeans","CAH (Ward.D2)"="hclust","DBSCAN"="dbscan"),
                selected="kmeans"),
              uiOutput("clust_vars_ui"),
              uiOutput("clust_k_ui"),
              checkboxInput("clust_auto_k","Nombre optimal automatique (silhouette)",TRUE),
              uiOutput("clust_eps_ui"),
              actionButton("run_clust","🔗 Lancer le clustering",
                           class="btn btn-oms btn-block", width="100%"),
              hr(),
              verbatimTextOutput("clust_summary")
          ),
          box(width=9, status="info", solidHeader=TRUE,
              title="Résultats — Clustering",
              tabsetPanel(
                tabPanel("Visualisation 2D (PCA)",
                         plotlyOutput("clust_plot2d", height="420px")),
                tabPanel("Nombre optimal de clusters",
                         plotlyOutput("clust_optimal", height="350px")),
                tabPanel("Dendrogramme (CAH)",
                         plotOutput("clust_dendro", height="420px")),
                tabPanel("Profil des clusters",
                         plotlyOutput("clust_profile", height="380px")),
                tabPanel("Tableau des clusters",
                         DTOutput("clust_table"))
              )
          )
        )
      )
}
