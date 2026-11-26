# ui_carto_pro.R - UI Cartographie PROFESSIONNELLE
# v3 : scroll + plein écran + téléchargement par onglet

# Helper : barre d'actions (plein écran + download) réutilisable
carto_action_bar <- function(dl_id, fs_id = NULL, dl_label = "PNG") {
  btns <- list()
  if (!is.null(fs_id)) {
    btns[[length(btns) + 1]] <- actionButton(fs_id, tagList(icon("expand"), " Plein \u00e9cran"),
      class = "btn-default btn-sm", style = "font-weight:600;margin-right:6px;")
  }
  btns[[length(btns) + 1]] <- downloadButton(dl_id, paste0(icon("download"), " ", dl_label),
    style = "background:#008DC9;color:white;border:none;border-radius:6px;padding:6px 14px;font-weight:600;font-size:12px;")
  tags$div(style = "display:flex; justify-content:flex-end; align-items:center; margin:8px 0;", btns)
}

ui_carto_pro <- tabItem(tabName = "carto",

  tags$head(tags$style(HTML("
    .leaflet-popup-content-wrapper { padding: 0 !important; border-radius: 12px !important; overflow: hidden; }
    .leaflet-popup-content { margin: 0 !important; width: auto !important; }
    .leaflet-popup-tip { display: none; }
    .carto-section-header {
      font-size: 12px; font-weight: 700; color: #00426A;
      text-transform: uppercase; letter-spacing: 0.5px;
      margin: 12px 0 6px; padding-bottom: 4px;
      border-bottom: 2px solid #008DC9;
    }
    .carto-scroll-container {
      max-height: 80vh; overflow-y: auto;
      border: 1px solid #e0e0e0; border-radius: 8px;
      padding: 8px; background: white;
    }
  "))),

  # Bandeau titre
  tags$div(
    style = "background:linear-gradient(135deg,#00426A 0%,#005A8B 50%,#008DC9 100%);color:white;padding:16px 24px;border-radius:12px;margin-bottom:12px;box-shadow:0 4px 20px rgba(0,66,106,0.3);",
    tags$div(style = "display:flex;justify-content:space-between;align-items:center;",
      tags$div(
        tags$h4(style = "margin:0;font-weight:800;font-size:18px;",
                icon("globe-africa"), " Cartographie Nutritionnelle Pro"),
        tags$p(style = "margin:4px 0 0;font-size:12px;opacity:0.85;",
               "Regions | Departements | Arrondissements | Indicateurs OMS | Bivariate")
      ),
      tags$div(style = "text-align:right;opacity:0.7;font-size:10px;",
        tags$div("GADM v4.1 | Normes OMS 2006"))
    )
  ),

  # Guide rapide
  tags$div(
    style = "background:#f0f8ff; border-left:4px solid #008DC9; border-radius:6px;
             padding:14px 18px; margin-bottom:16px; font-size:12px; color:#444; line-height:1.7;",
    tags$p(style = "margin:0 0 8px 0; font-weight:700; color:#00426A; font-size:13px;",
           icon("info-circle"), " Guide rapide des onglets cartographiques"),
    tags$table(style = "width:100%; border-collapse:collapse;",
      tags$tr(
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9; white-space:nowrap;", "Carte interactive"),
        tags$td(style = "padding:3px 8px;", "Carte Leaflet zoomable avec choropl\u00e8the, cercles ou heatmap. Cliquez sur une zone pour voir les d\u00e9tails.")),
      tags$tr(style = "background:#f8fbff;",
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9;", "Carte bivariate"),
        tags$td(style = "padding:3px 8px;", "Croise 2 indicateurs sur une m\u00eame carte pour identifier les zones de double charge nutritionnelle.")),
      tags$tr(
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9;", "Multi-indicateurs"),
        tags$td(style = "padding:3px 8px;", "Cartes c\u00f4te \u00e0 c\u00f4te pour comparer la r\u00e9partition de diff\u00e9rents indicateurs.")),
      tags$tr(style = "background:#f8fbff;",
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9;", "Carte statique"),
        tags$td(style = "padding:3px 8px;", "Version ggplot2 haute r\u00e9solution pour publication ou rapport.")),
      tags$tr(
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9;", "Carte GPS"),
        tags$td(style = "padding:3px 8px;", "Chaque point d\u2019enqu\u00eate sur la carte (n\u00e9cessite latitude/longitude).")),
      tags$tr(style = "background:#f8fbff;",
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9;", "Hotspots LISA"),
        tags$td(style = "padding:3px 8px;",
          "D\u00e9tecte les ", tags$b("clusters spatiaux significatifs"), " (hotspots/coldspots). Bas\u00e9 sur l\u2019indice de Moran local.")),
      tags$tr(
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9;", "Moran I"),
        tags$td(style = "padding:3px 8px;", "Teste l\u2019autocorr\u00e9lation spatiale. Un Moran I significatif justifie l\u2019analyse LISA.")),
      tags$tr(style = "background:#f8fbff;",
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9;", "Priorisation"),
        tags$td(style = "padding:3px 8px;", "Classe les zones par priorit\u00e9 d\u2019intervention en combinant plusieurs indicateurs.")),
      tags$tr(
        tags$td(style = "padding:3px 8px; font-weight:600; color:#008DC9;", "Comparaison temporelle"),
        tags$td(style = "padding:3px 8px;", "Compare les indicateurs entre vagues d\u2019enqu\u00eate."))
    )
  ),

  fluidRow(
    # PANNEAU DE CONTROLE
    box(width = 3, status = "primary", solidHeader = TRUE,
        title = tags$span(icon("sliders-h"), " Parametres"),
        style = "max-height:85vh;overflow-y:auto;",

        tags$div(class = "carto-section-header", "Niveau geographique"),
        radioButtons("carto_niveau", NULL,
          choices  = c("Region"="Region", "Departement"="Departement", "Arrondissement"="Arrondissement"),
          selected = "Region", inline = FALSE),
        uiOutput("carto_filtre_region_ui"),
        uiOutput("carto_filtre_dept_ui"),
        hr(),

        tags$div(class = "carto-section-header", "Indicateur principal"),
        uiOutput("carto_indicateur_ui"),
        hr(),

        tags$div(class = "carto-section-header", "Symbologie (choropl\u00e8the)"),
        selectInput("carto_class_method", "Classification :",
          choices  = c("Seuils OMS (fixes)"="oms", "Quantiles"="quantile",
                       "Jenks (ruptures naturelles)"="jenks",
                       "Intervalles \u00e9gaux"="equal", "\u00c9cart-type"="sd"),
          selected = "oms", width = "100%"),
        conditionalPanel(condition = "input.carto_class_method != 'oms'",
          sliderInput("carto_n_classes", "Nombre de classes :",
                      min = 2, max = 9, value = 5, step = 1, width = "100%"),
          selectInput("carto_palette", "Palette :",
            choices  = c("Rouge-orang\u00e9 (risque)"="ylorrd", "OMS (accident\u00e9e)"="oms",
                         "Bleu"="blues", "Viridis"="viridis", "S\u00e9pia"="sepia"),
            selected = "ylorrd", width = "100%")),
        sliderInput("carto_opacity", "Opacit\u00e9 du remplissage :",
                    min = 0.3, max = 1, value = 0.78, step = 0.05, width = "100%"),
        hr(),

        tags$div(class = "carto-section-header", "Recherche de zone"),
        uiOutput("carto_search_ui"),
        hr(),

        tags$div(class = "carto-section-header", "Habillage"),
        checkboxInput("carto_habillage", "Nord + cartouche (titre / source)", TRUE),
        checkboxInput("carto_coords", "Coordonn\u00e9es au survol", TRUE),
        hr(),

        tags$div(class = "carto-section-header", "Type de visualisation"),
        selectInput("carto_type", NULL,
          choices  = c("Choroplethe"="Choroplethe", "Cercles proportionnels"="Cercles proportionnels",
                       "Bivariate (2 indicateurs)"="Bivariate", "Multi-indicateurs"="Multi",
                       "Choroplethe + Cercles"="Tous"),
          selected = "Choroplethe", width = "100%"),

        conditionalPanel(condition = "input.carto_type == \"Bivariate\"",
          tags$div(class = "carto-section-header", "2e indicateur (bivariate)"),
          selectInput("carto_ind2", NULL,
            choices = c("Stunting","SAM","Underweight","MUAC_GAM"),
            selected = "Stunting", width = "100%")),

        tags$div(class = "carto-section-header", "Options"),
        checkboxInput("carto_pts", "Points GPS individuels", FALSE),
        checkboxInput("carto_heat", "Heatmap de densite", FALSE),
        checkboxInput("carto_labels", "Etiquettes sur la carte", TRUE),
        checkboxInput("carto_kpi_overlay", "KPIs sur la carte", TRUE),
        hr(),

        tags$div(class = "carto-section-header", "Fond de carte"),
        selectInput("carto_fond", NULL,
          choices  = c("Clair (Esri)"="Esri.WorldGrayCanvas", "OpenStreetMap"="OpenStreetMap",
                       "Satellite"="Esri.WorldImagery", "Relief"="Esri.WorldTopoMap"),
          selected = "Esri.WorldGrayCanvas", width = "100%")
    ),

    # ZONE PRINCIPALE
    box(width = 9, status = "primary", solidHeader = TRUE,
        title = uiOutput("carto_titre_pro_ui"),

        tabsetPanel(id = "carto_tabs",

          # ── TAB 1 : Carte interactive Leaflet ──────────────────────────
          tabPanel(title = tagList(icon("map-marked-alt"), " Carte interactive"),
            br(),
            fluidRow(
              column(6, carto_action_bar("dl_carte_png", dl_label = "PNG")),
              column(6, style = "text-align:right;",
                tags$span(downloadButton("dl_carte_hd", paste0(icon("download"), " PNG 300 dpi"),
                  style = "background:#00426A;color:white;border:none;border-radius:6px;padding:6px 14px;font-weight:600;font-size:12px;margin-right:6px;"),
                downloadButton("dl_carte_pdf", paste0(icon("file-pdf"), " PDF"),
                  style = "background:#c9161d;color:white;border:none;border-radius:6px;padding:6px 14px;font-weight:600;font-size:12px;"))
              )
            ),
            uiOutput("carto_anim_ui"),
            uiOutput("carto_kpi_panel"),
            leafletOutput("carto_leaflet_main", height = "720px"),
            br(),
            tags$h6(style = "font-weight:700;color:#00426A;",
              icon("table-list"), " Table attributaire — cliquez une ligne pour zoomer sur la zone"),
            DT::DTOutput("carto_table_attrib", height = "320px")
          ),

          # ── TAB 2 : Carte bivariate ────────────────────────────────────
          tabPanel(title = tagList(icon("layer-group"), " Carte bivariate"),
            br(),
            fluidRow(
              column(4,
                tags$div(style = "background:#f8fbff;padding:14px;border-radius:10px;border:1px solid #e0e0e0;",
                  tags$h6(style = "font-weight:700;color:#00426A;margin-top:0;", "Carte bivariate"),
                  tags$p(style = "font-size:11px;color:#666;",
                    "Croise 2 indicateurs pour identifier les zones de double charge."),
                  selectInput("carto_bivar_x", "Indicateur X :",
                    choices = c("GAM","SAM","MAM","MUAC_GAM"), selected = "GAM", width = "100%"),
                  selectInput("carto_bivar_y", "Indicateur Y :",
                    choices = c("Stunting","Underweight","Sev_Stunting"), selected = "Stunting", width = "100%"),
                  actionButton("run_bivariate", "G\u00e9n\u00e9rer carte bivariate",
                    class = "btn-primary", style = "width:100%;font-weight:600;margin-top:8px;"),
                  br(), br(),
                  downloadButton("dl_carto_bivar", tagList(icon("download"), " T\u00e9l\u00e9charger PNG"),
                    style = "width:100%;background:#008DC9;color:white;border:none;border-radius:6px;padding:8px;font-weight:600;")
                )
              ),
              column(8,
                tags$div(class = "carto-scroll-container",
                  leafletOutput("carto_bivariate_map", height = "650px")
                )
              )
            )
          ),

          # ── TAB 3 : Multi-indicateurs ──────────────────────────────────
          tabPanel(title = tagList(icon("th"), " Multi-indicateurs"),
            br(),
            fluidRow(
              column(3,
                tags$div(style = "background:#f8fbff;padding:14px;border-radius:10px;border:1px solid #e0e0e0;",
                  tags$h6(style = "font-weight:700;color:#00426A;margin-top:0;", "Comparaison multi-indicateurs"),
                  tags$p(style = "font-size:11px;color:#666; line-height:1.4;",
                    "S\u00e9lectionnez plusieurs indicateurs. Scrollez pour voir toutes les cartes."),
                  checkboxGroupInput("carto_multi_inds", "Indicateurs :",
                    choices = c("GAM","SAM","MAM","Stunting","Underweight","MUAC_GAM"),
                    selected = c("GAM","SAM","Stunting"), inline = FALSE),
                  selectInput("carto_multi_ncol", "Colonnes :",
                    choices = c("1 (grande)" = "1", "2 (moyen)" = "2", "3 (compact)" = "3"),
                    selected = "2", width = "100%"),
                  actionButton("run_multi_carte", "G\u00e9n\u00e9rer les cartes",
                    class = "btn-info", style = "width:100%;font-weight:600;margin-top:8px;"),
                  br(), br(),
                  actionButton("carto_multi_fullscreen", tagList(icon("expand"), " Plein \u00e9cran"),
                    class = "btn-default", style = "width:100%;font-weight:600;"),
                  br(), br(),
                  downloadButton("dl_carto_multi", tagList(icon("download"), " T\u00e9l\u00e9charger PNG"),
                    style = "width:100%;background:#008DC9;color:white;border:none;border-radius:6px;padding:8px;font-weight:600;")
                )
              ),
              column(9,
                tags$div(class = "carto-scroll-container",
                  uiOutput("carto_multi_plot_ui")
                )
              )
            )
          ),

          # ── TAB 4 : Carte statique ggplot2 ─────────────────────────────
          tabPanel(title = tagList(icon("file-image"), " Carte statique"),
            br(),
            fluidRow(
              column(3,
                tags$div(style = "background:#f8fbff;padding:14px;border-radius:10px;border:1px solid #e0e0e0;",
                  selectInput("carto_pal_static", "Palette :",
                    choices = c("OMS"="oms_sequentiel","UNICEF"="UNICEF","FAO"="FAO",
                                "PAM / WFP"="PAM","Chaleur"="chaleur","Bleu"="bleu_marine",
                                "Urgence"="urgence","Viridis"="Viridis","Spectral"="Spectral",
                                "Terre"="Terre"),
                    selected = "oms_sequentiel", width = "100%"),
                  checkboxInput("carto_static_labels", "Etiquettes", TRUE),
                  hr(),
                  actionButton("carto_static_fullscreen", tagList(icon("expand"), " Plein \u00e9cran"),
                    class = "btn-default", style = "width:100%;font-weight:600;"),
                  br(), br(),
                  downloadButton("dl_carto_static_png", tagList(icon("download"), " PNG haute r\u00e9solution"),
                    style = "width:100%;background:#008DC9;color:white;border:none;border-radius:6px;padding:8px;font-weight:600;margin-bottom:6px;"),
                  downloadButton("dl_carto_static_pdf", tagList(icon("download"), " PDF (publication)"),
                    style = "width:100%;background:#c9161d;color:white;border:none;border-radius:6px;padding:8px;font-weight:600;")
                )
              ),
              column(9,
                tags$div(class = "carto-scroll-container",
                  plotOutput("carto_ggplot_main", height = "700px")
                )
              )
            )
          ),

          # ── TAB 5 : Afrique centrale ───────────────────────────────────
          tabPanel(title = tagList(icon("globe-africa"), " Afrique centrale"),
            br(),
            fluidRow(
              column(3,
                tags$div(style = "background:#f8fbff;padding:14px;border-radius:10px;border:1px solid #e0e0e0;",
                  selectInput("carto_af_zoom", "Vue :",
                    choices = c("Afrique centrale","Afrique","Monde entier"), selected = "Afrique centrale", width = "100%"),
                  selectInput("carto_af_ind", "Indicateur :",
                    choices = c("GAM","SAM","Stunting"), selected = "GAM", width = "100%"),
                  hr(),
                  actionButton("carto_af_fullscreen", tagList(icon("expand"), " Plein \u00e9cran"),
                    class = "btn-default", style = "width:100%;font-weight:600;")
                )
              ),
              column(9,
                tags$div(class = "carto-scroll-container",
                  leafletOutput("carto_afrique", height = "650px")
                )
              )
            )
          ),

          # ── TAB 6 : Tableau prévalences ─────────────────────────────────
          tabPanel(title = tagList(icon("table"), " Tableau prevalences"),
            br(),
            carto_action_bar("dl_carto_tableau", dl_label = "CSV"),
            DTOutput("carto_tableau_prev"),
            br(), fluidRow(
              column(6, plotlyOutput("carto_bar_prev", height = "350px")),
              column(6, plotlyOutput("carto_forest_prev", height = "350px")))
          ),

          # ── TAB 7 : GPS ─────────────────────────────────────────────────
          tabPanel(title = tagList(icon("map-pin"), " Carte GPS"),
            br(), fluidRow(
              column(3,
                uiOutput("carto_gps_color_ui"),
                checkboxInput("carto_gps_cluster", "Cluster", TRUE),
                sliderInput("carto_gps_size", "Taille :", 3, 12, 5, 1)
              ),
              column(9,
                tags$div(class = "carto-scroll-container",
                  leafletOutput("carto_gps_map", height = "680px")
                )
              )
            )
          ),

          # ── TAB 8 : LISA ────────────────────────────────────────────────
          tabPanel(title = tagList(icon("fire"), " Hotspots LISA"),
            br(), fluidRow(
              column(3, tags$div(style = "background:#f8fbff;padding:14px;border-radius:10px;border:1px solid #e0e0e0;",
                tags$h6(style = "font-weight:700;color:#00426A;margin-top:0;", "Analyse LISA"),
                tags$p(style = "font-size:11px;color:#666; line-height:1.5;",
                  "LISA identifie les ",
                  tags$b("hotspots"), " (valeur \u00e9lev\u00e9e, voisins \u00e9lev\u00e9s) et ",
                  tags$b("coldspots"), " (valeur basse, voisins bas). ",
                  "Les ", tags$span(style="color:#e88400;font-weight:600;", "outliers"),
                  " diff\u00e8rent fortement de leurs voisins."),
                tags$p(style = "font-size:10px;color:#999; margin-top:6px;",
                  "N\u00e9cessite le package ", tags$code("spdep"), "."),
                hr(),
                selectInput("carto_lisa_ind", "Indicateur :",
                  choices = c("GAM","SAM","Stunting","Underweight"), selected = "GAM", width = "100%"),
                actionButton("run_carto_lisa", "Executer LISA",
                  class = "btn-danger", style = "width:100%;font-weight:600;margin-top:8px;"),
                br(), br(),
                downloadButton("dl_carto_lisa", tagList(icon("download"), " T\u00e9l\u00e9charger CSV"),
                  style = "width:100%;background:#008DC9;color:white;border:none;border-radius:6px;padding:8px;font-weight:600;")
              )),
              column(9,
                tags$div(class = "carto-scroll-container",
                  leafletOutput("carto_lisa_map", height = "650px")
                )
              )),
            br(), DTOutput("carto_lisa_table")
          ),

          # ── TAB 9 : Moran ──────────────────────────────────────────────
          tabPanel(title = tagList(icon("chart-line"), " Moran I"),
            br(), fluidRow(
              column(3, tags$div(style = "background:#f8fbff;padding:14px;border-radius:10px;border:1px solid #e0e0e0;",
                selectInput("carto_moran_ind", "Indicateur :",
                  choices = c("GAM","SAM","Stunting","Underweight"), selected = "GAM", width = "100%"),
                actionButton("run_carto_moran", "Calculer Moran I",
                  class = "btn-info", style = "width:100%;font-weight:600;margin-top:8px;"))),
              column(9,
                verbatimTextOutput("carto_moran_results"),
                tags$div(class = "carto-scroll-container",
                  plotlyOutput("carto_moran_plot", height = "450px")
                )
              ))
          ),

          # ── TAB 10 : Priorisation ──────────────────────────────────────
          tabPanel(title = tagList(icon("bullseye"), " Priorisation"),
            br(), fluidRow(
              column(3, tags$div(style = "background:#f8fbff;padding:14px;border-radius:10px;border:1px solid #e0e0e0;",
                actionButton("run_carto_priority", "Calculer priorites",
                  class = "btn-warning", style = "width:100%;font-weight:600;"),
                br(), br(),
                downloadButton("dl_carto_priority", tagList(icon("download"), " T\u00e9l\u00e9charger CSV"),
                  style = "width:100%;background:#008DC9;color:white;border:none;border-radius:6px;padding:8px;font-weight:600;")
              )),
              column(9,
                DTOutput("carto_priority_table"),
                br(),
                tags$div(class = "carto-scroll-container",
                  plotlyOutput("carto_priority_scatter", height = "450px")
                )
              ))
          ),

          # ── TAB 11 : Comparaison temporelle ────────────────────────────
          tabPanel(title = tagList(icon("clock"), " Comparaison temporelle"),
            br(),
            uiOutput("carto_temporal_available_ui"),
            fluidRow(
              column(3,
                tags$div(style = "background:#f8fbff;padding:14px;border-radius:10px;border:1px solid #e0e0e0;",
                  tags$h6(style = "font-weight:700;color:#00426A;margin-top:0;", "\u00c9volution temporelle"),
                  tags$p(style = "font-size:11px;color:#666;",
                    "Compare les indicateurs entre vagues d\u2019enqu\u00eate."),
                  uiOutput("carto_temporal_round_ui"),
                  selectInput("carto_temporal_ind", "Indicateur :",
                    choices = c("GAM","SAM","MAM","Stunting","Underweight","MUAC_GAM"),
                    selected = "GAM", width = "100%"),
                  selectInput("carto_temporal_viz", "Visualisation :",
                    choices = c("Cartes c\u00f4te \u00e0 c\u00f4te" = "side_by_side",
                                "Carte de variation" = "variation",
                                "Graphique d\u2019\u00e9volution" = "trend"),
                    selected = "side_by_side", width = "100%"),
                  actionButton("run_temporal", "Analyser l\u2019\u00e9volution",
                    class = "btn-primary", style = "width:100%;font-weight:600;margin-top:8px;"),
                  br(), br(),
                  actionButton("carto_temporal_fullscreen", tagList(icon("expand"), " Plein \u00e9cran"),
                    class = "btn-default", style = "width:100%;font-weight:600;"),
                  br(), br(),
                  downloadButton("dl_carto_temporal", tagList(icon("download"), " T\u00e9l\u00e9charger PNG"),
                    style = "width:100%;background:#008DC9;color:white;border:none;border-radius:6px;padding:8px;font-weight:600;")
                )
              ),
              column(9,
                tags$div(class = "carto-scroll-container",
                  conditionalPanel(condition = "input.carto_temporal_viz == 'side_by_side'",
                    plotOutput("carto_temporal_sidebyside", height = "700px")),
                  conditionalPanel(condition = "input.carto_temporal_viz == 'variation'",
                    leafletOutput("carto_temporal_variation", height = "700px")),
                  conditionalPanel(condition = "input.carto_temporal_viz == 'trend'",
                    plotlyOutput("carto_temporal_trend", height = "450px"),
                    br(),
                    DTOutput("carto_temporal_table"))
                )
              )
            )
          ),

          # ══════════════════════════════════════════════════════════
          # ONGLETS EXPERT (module_carto_expert.R)
          # ══════════════════════════════════════════════════════════

          # ── TAB 12 : Getis-Ord Gi* Hotspots ──────────────────────
          tabPanel(title = tagList(icon("fire-alt"), " Getis-Ord Gi*"),
            br(),
            tags$div(style="margin-bottom:10px;font-size:12px;color:#666;",
              icon("info-circle"),
              " Le Gi* de Getis-Ord d\u00e9tecte les concentrations spatiales ",
              "de valeurs \u00e9lev\u00e9es (hotspots) ou basses (coldspots). ",
              "Compl\u00e9mentaire au LISA : Gi* teste les niveaux, LISA les corr\u00e9lations."),
            fluidRow(
              column(4, actionButton("run_gistar", "\u25b6 Calculer Gi*",
                class="btn btn-danger", style="font-weight:600;width:100%;")),
              column(4, uiOutput("gistar_info"))
            ), br(),
            leafletOutput("gistar_map", height="550px"), br(),
            DTOutput("gistar_table")
          ),

          # ── TAB 13 : Lissage Bay\u00e9sien (EB) ─────────────────────
          tabPanel(title = tagList(icon("wave-square"), " Lissage EB"),
            br(),
            tags$div(style="margin-bottom:10px;font-size:12px;color:#666;",
              icon("info-circle"),
              " Le lissage Bay\u00e9sien Empirique stabilise les pr\u00e9valences des petites ",
              "zones en les tirant vers la moyenne globale (shrinkage). ",
              "Essential pour les enquetes SMART avec peu d'observations par zone."),
            fluidRow(
              column(4, actionButton("run_eb", "\u25b6 Calculer lissage EB",
                class="btn btn-info", style="font-weight:600;width:100%;color:white;"))
            ), br(),
            plotlyOutput("eb_plot", height="450px"), br(),
            DTOutput("eb_table")
          ),

          # ── TAB 14 : Carte IPC/CH ────────────────────────────────
          tabPanel(title = tagList(icon("exclamation-triangle"), " Carte IPC/CH"),
            br(),
            tags$div(style="margin-bottom:10px;font-size:12px;color:#666;",
              icon("info-circle"),
              " Classification IPC/Cadre Harmonis\u00e9 par zone g\u00e9ographique. ",
              "Phases : 1 (Minimale) \u2192 2 (Stress\u00e9e) \u2192 3 (Crise) \u2192 ",
              "4 (Urgence) \u2192 5 (Famine). Couleurs officielles IPC."),
            fluidRow(
              column(4, actionButton("run_ipc_map", "\u25b6 G\u00e9n\u00e9rer carte IPC",
                class="btn btn-warning", style="font-weight:600;width:100%;"))
            ), br(),
            leafletOutput("ipc_phase_map", height="550px"), br(),
            DTOutput("ipc_phase_table")
          ),

          # ── TAB 15 : Forest Plot Pr\u00e9valences ─────────────────────
          tabPanel(title = tagList(icon("project-diagram"), " Forest Plot"),
            br(),
            tags$div(style="margin-bottom:10px;font-size:12px;color:#666;",
              icon("info-circle"),
              " Forest plot des pr\u00e9valences par zone avec IC 95% Wilson. ",
              "Standard SMART/OMS pour les rapports de nutrition. ",
              "Taille des points proportionnelle \u00e0 l'effectif."),
            fluidRow(
              column(4, selectInput("forest_indicator", "Indicateur :",
                choices=c("GAM (WHZ<-2)"="GAM","SAM (WHZ<-3)"="SAM",
                          "MUAC GAM (<125)"="MUAC_GAM","MUAC SAM (<115)"="MUAC_SAM",
                          "Stunting (HAZ<-2)"="Stunting"),
                selected="GAM", width="100%")),
              column(4, actionButton("run_forest", "\u25b6 G\u00e9n\u00e9rer forest plot",
                class="btn btn-primary", style="font-weight:600;width:100%;"))
            ), br(),
            plotlyOutput("forest_plot", height="480px"), br(),
            DTOutput("forest_table")
          ),

          # ── TAB 16 : In\u00e9galit\u00e9 spatiale ──────────────────────────
          tabPanel(title = tagList(icon("balance-scale-left"), " In\u00e9galit\u00e9"),
            br(),
            tags$div(style="margin-bottom:10px;font-size:12px;color:#666;",
              icon("info-circle"),
              " Indices d'in\u00e9galit\u00e9 spatiale : Gini, Theil, CV, Ratio max/min. ",
              "Mesurent l'h\u00e9t\u00e9rog\u00e9n\u00e9it\u00e9 entre zones g\u00e9ographiques."),
            fluidRow(
              column(4, actionButton("run_inequality", "\u25b6 Calculer indices",
                class="btn btn-secondary", style="font-weight:600;width:100%;"))
            ), br(),
            DTOutput("inequality_table"), br(),
            uiOutput("inequality_interpretation")
          ),

          # ── TAB 17 : Profil r\u00e9gional ──────────────────────────────
          tabPanel(title = tagList(icon("chart-pie"), " Profil r\u00e9gional"),
            br(),
            tags$div(style="margin-bottom:10px;font-size:12px;color:#666;",
              icon("info-circle"),
              " Radar multi-indicateurs : compare le profil d'une zone ",
              "\u00e0 la moyenne nationale."),
            fluidRow(
              column(4, uiOutput("profile_zone_ui")),
              column(4, actionButton("run_profile", "\u25b6 G\u00e9n\u00e9rer profil",
                class="btn btn-info", style="font-weight:600;width:100%;color:white;"))
            ), br(),
            plotlyOutput("profile_radar", height="480px")
          )

        ) # fin tabsetPanel
    ) # fin box principale
  ) # fin fluidRow
) # fin tabItem

cat("ui_carto_pro.R charge\n")
