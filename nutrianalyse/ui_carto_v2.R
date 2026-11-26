# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  ui_carto_v2.R — UI et Server du nouvel onglet Cartographie                ║
# ║  Remplace tabItem(tabName="carto",...) dans app_v2.R                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── UI ────────────────────────────────────────────────────────────────────────
ui_carto_v2 <- tabItem(tabName = "carto",

  # Bandeau titre
  tags$div(
    style = paste0(
      "background:linear-gradient(135deg,#00426A,#008DC9);",
      "color:white;padding:14px 20px;border-radius:10px;",
      "margin-bottom:16px;"
    ),
    tags$h4(style = "margin:0;font-weight:700;",
            icon("map"), " Cartographie nutritionnelle — Cameroun & Afrique centrale"),
    tags$p(style = "margin:4px 0 0;font-size:12px;opacity:.85;",
           "GADM L1 (Régions) · L2 (Départements) · L3 (Arrondissements) | Tous indicateurs OMS/UNICEF")
  ),

  fluidRow(
    # ── Panneau de contrôle ────────────────────────────────────────────────
    box(width = 3, status = "primary", solidHeader = TRUE,
        title = tags$span(icon("sliders"), " Paramètres"),

        # Niveau géographique
        tags$div(class = "section-header", "Niveau géographique"),
        radioButtons("carto_niveau", NULL,
          choices  = c("Région"="Région",
                       "Département"="Département",
                       "Arrondissement"="Arrondissement"),
          selected = "Région",
          inline   = FALSE
        ),

        # Filtre région
        uiOutput("carto_filtre_region_ui"),
        # Filtre département (apparaît si Département/Arrondissement sélectionné)
        uiOutput("carto_filtre_dept_ui"),

        hr(),

        # Indicateur
        tags$div(class = "section-header", "Indicateur OMS/PAM"),
        uiOutput("carto_indicateur_ui"),

        hr(),

        # Type de carte
        tags$div(class = "section-header", "Type de carte"),
        selectInput("carto_type", NULL,
          choices  = c("Choroplèthe"     = "Choroplèthe",
                       "Cercles proportionnels" = "Cercles proportionnels",
                       "Choroplèthe + Cercles"  = "Tous"),
          selected = "Choroplèthe",
          width    = "100%"
        ),

        # Options
        checkboxInput("carto_pts",    "Points GPS individuels", TRUE),
        checkboxInput("carto_heat",   "Heatmap de densité",     FALSE),
        checkboxInput("carto_labels", "Étiquettes sur la carte",TRUE),

        hr(),

        # Fond de carte
        tags$div(class = "section-header", "Fond de carte"),
        selectInput("carto_fond", NULL,
          choices  = c(
            "CartoDB Positron (blanc)"   = "CartoDB.Positron",
            "OpenStreetMap"              = "OpenStreetMap",
            "Satellite (Esri)"           = "Esri.WorldImagery",
            "Sombre (CartoDB)"           = "CartoDB.DarkMatter",
            "NatGeo (Esri)"              = "Esri.NatGeoWorldMap"
          ),
          selected = "CartoDB.Positron",
          width    = "100%"
        ),

        hr(),

        # Export
        tags$div(class = "section-header", "Exporter"),
        downloadButton("dl_carte_png",
          label = "📥 PNG haute résolution",
          style = "width:100%;background:#008DC9;color:white;border:none;
                   border-radius:6px;padding:7px;font-weight:600;margin-bottom:5px;"
        ),
        downloadButton("dl_carte_pdf",
          label = "📥 PDF (publication)",
          style = "width:100%;background:#c9161d;color:white;border:none;
                   border-radius:6px;padding:7px;font-weight:600;"
        )
    ),

    # ── Zone principale ────────────────────────────────────────────────────
    box(width = 9, status = "primary", solidHeader = TRUE,
        title = uiOutput("carto_titre_ui"),

        tabsetPanel(
          id = "carto_tabs",
          tabPanel("🗺️ Carte interactive (Leaflet)",
            br(),
            fluidRow(
              column(12, align = "right",
                actionButton("carto_fullscreen_btn", "🔍 Plein écran",
                  class = "btn-sm btn-primary",
                  style = "margin-bottom:8px;font-weight:600;")
              )
            ),
            leafletOutput("carto_leaflet_main", height = "700px")
          ),

          tabPanel("📊 Carte statique (ggplot2 / export)",
            br(),
            fluidRow(
              column(4,
                selectInput("carto_pal_static", "Palette :",
                  choices  = c("OMS standard"="OMS",
                               "UNICEF"="UNICEF","FAO"="FAO","PAM / WFP"="PAM",
                               "YlOrRd","Blues","Viridis","Inferno","Plasma",
                               "Spectral","Terre (Nature)"="Terre"),
                  selected = "OMS",
                  width    = "100%"
                ),
                radioButtons("carto_geom_static", "Géométrie :",
                  choices  = c("Choroplèthe"="choroplèthe",
                               "Points proportionnels"="points"),
                  selected = "choroplèthe"
                )
              ),
              column(8,
                plotlyOutput("carto_ggplot_main", height = "620px")
              )
            )
          ),

          tabPanel("🌍 Afrique centrale",
            br(),
            fluidRow(
              column(3,
                selectInput("carto_af_zoom", "Vue :",
                  choices  = c("Afrique centrale","Afrique","Monde entier"),
                  selected = "Afrique centrale",
                  width    = "100%"
                ),
                selectInput("carto_af_ind", "Indicateur contextuel :",
                  choices  = c("GAM","SAM","Stunting"),
                  selected = "GAM",
                  width    = "100%"
                )
              ),
              column(9,
                plotlyOutput("carto_afrique", height = "650px")
              )
            )
          ),

          tabPanel("📋 Tableau des prévalences par zone",
            br(),
            DTOutput("carto_tableau_prev"),
            br(),
            fluidRow(
              column(6,
                plotlyOutput("carto_bar_prev", height = "350px")
              ),
              column(6,
                plotlyOutput("carto_forest_prev", height = "350px")
              )
            )
          ),

          tabPanel("📍 Carte GPS détaillée",
            br(),
            fluidRow(
              column(3,
                uiOutput("carto_gps_color_ui"),
                checkboxInput("carto_gps_cluster", "Regrouper (cluster)", TRUE),
                sliderInput("carto_gps_size", "Taille :", 3, 12, 5, 1),
                selectInput("carto_gps_fond", "Fond :",
                  choices  = c("CartoDB.Positron","OpenStreetMap",
                               "Esri.WorldImagery"),
                  selected = "CartoDB.Positron",
                  width    = "100%"
                )
              ),
              column(9,
                leafletOutput("carto_gps_map", height = "680px")
              )
            )
          ),

          # ── TIER 2-4 : Analyses spatiales avancées ────────────────────────────
          tabPanel("🔴 Hotspots LISA",
            br(),
            fluidRow(
              column(3,
                tags$h5("Analyse LISA", style = "font-weight:700;"),
                tags$p("Local Indicators of Spatial Association — Détection de clusters hotspots/coldspots.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                actionButton("run_carto_lisa", "▶️ Exécuter analyse LISA",
                  class = "btn-primary", style = "width:100%;"),
                br(), br(),
                selectInput("carto_lisa_ind", "Indicateur :",
                  choices  = c("GAM","SAM","Stunting","Underweight"),
                  selected = "GAM",
                  width    = "100%"
                )
              ),
              column(9,
                leafletOutput("carto_lisa_map", height = "650px")
              )
            ),
            br(),
            tags$h5("Détails LISA"),
            DTOutput("carto_lisa_table")
          ),

          tabPanel("📈 Moran's I & Autocorrélation",
            br(),
            fluidRow(
              column(3,
                tags$h5("Test d'autocorrélation spatiale", style = "font-weight:700;"),
                tags$p("Moran's I mesure le clustering global des valeurs similaires.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                actionButton("run_carto_moran", "▶️ Calculer Moran's I",
                  class = "btn-info", style = "width:100%;"),
                br(), br(),
                selectInput("carto_moran_ind", "Indicateur :",
                  choices  = c("GAM","SAM","Stunting","Underweight"),
                  selected = "GAM",
                  width    = "100%"
                )
              ),
              column(9,
                tags$div(
                  tags$h6("Résultats Moran's I", style = "font-weight:700;"),
                  verbatimTextOutput("carto_moran_results"),
                  br(),
                  plotlyOutput("carto_moran_plot", height = "400px")
                )
              )
            )
          ),

          tabPanel("🎯 Matrice de priorisation",
            br(),
            fluidRow(
              column(3,
                tags$h5("Priorisation interventions", style = "font-weight:700;"),
                tags$p("Score de 0-100 basé sur : prévalence (35%), urgence OMS (30%), population (20%), accessibilité (15%).",
                       style = "font-size:12px;color:#666;"),
                hr(),
                actionButton("run_carto_priority", "▶️ Calculer priorités",
                  class = "btn-warning", style = "width:100%;"),
                br(), br(),
                checkboxInput("carto_show_all_zones", "Afficher toutes les zones", TRUE)
              ),
              column(9,
                DTOutput("carto_priority_table")
              )
            ),
            br(),
            tags$h5("Visualisation des priorités"),
            plotlyOutput("carto_priority_scatter", height = "400px")
          ),

          tabPanel("🔮 Prédiction tendances",
            br(),
            fluidRow(
              column(3,
                tags$h5("Modèle prédictif", style = "font-weight:700;"),
                tags$p("Machine learning : prédiction des zones à risque croissant de malnutrition.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                actionButton("run_carto_predict", "▶️ Lancer prédictions",
                  class = "btn-success", style = "width:100%;"),
                br(), br(),
                sliderInput("carto_predict_horizon", "Horizon (mois) :",
                  min = 1, max = 12, value = 3, step = 1)
              ),
              column(9,
                plotlyOutput("carto_predict_trend", height = "450px")
              )
            ),
            br(),
            tags$h5("Zones à surveiller"),
            DTOutput("carto_predict_alerts")
          ),

          tabPanel("📊 Comparaison régionale",
            br(),
            fluidRow(
              column(3,
                tags$h5("Benchmark régional", style = "font-weight:700;"),
                tags$p("Comparaison avec : moyenne nationale, régions similaires, seuils OMS.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                selectInput("carto_bench_region", "Région cible :",
                  choices  = c("Littoral","Centre","Ouest"),
                  selected = "Littoral",
                  width    = "100%"
                ),
                selectInput("carto_bench_ind", "Indicateur :",
                  choices  = c("GAM","SAM","Stunting"),
                  selected = "GAM",
                  width    = "100%"
                )
              ),
              column(9,
                plotlyOutput("carto_bench_plot", height = "450px")
              )
            )
          ),

          tabPanel("🗺️ Clusters spatiaux",
            br(),
            fluidRow(
              column(3,
                tags$h5("K-means spatial", style = "font-weight:700;"),
                tags$p("Segmentation en clusters des zones similaires géographiquement.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                sliderInput("carto_kclust_k", "Nombre de clusters :",
                  min = 2, max = 8, value = 4, step = 1),
                br(),
                actionButton("run_carto_cluster", "▶️ Exécuter clustering",
                  class = "btn-primary", style = "width:100%;")
              ),
              column(9,
                leafletOutput("carto_cluster_map", height = "620px")
              )
            ),
            br(),
            tags$h5("Profils de clusters"),
            DTOutput("carto_cluster_profiles")
          ),

          tabPanel("🛣️ Accessibilité géographique",
            br(),
            fluidRow(
              column(3,
                tags$h5("Score d'accessibilité", style = "font-weight:700;"),
                tags$p("Évalue : distance aux routes, relief, densité de population.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                checkboxInput("carto_access_show_roads", "Afficher routes", TRUE),
                checkboxInput("carto_access_show_terrain", "Afficher relief", TRUE),
                br(),
                actionButton("run_carto_access", "▶️ Calculer accessibilité",
                  class = "btn-info", style = "width:100%;")
              ),
              column(9,
                leafletOutput("carto_access_map", height = "620px")
              )
            )
          ),

          tabPanel("📦 Allocation ressources",
            br(),
            fluidRow(
              column(3,
                tags$h5("Optimisation allocation", style = "font-weight:700;"),
                tags$p("Algorithme : distribution optimale des ressources sanitaires basée sur besoins.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                numericInput("carto_res_budget", "Budget (USD) :",
                  value = 100000, min = 10000, step = 10000),
                selectInput("carto_res_type", "Type :",
                  choices  = c("Staff","Equipement","Médicaments"),
                  selected = "Staff",
                  width    = "100%"
                ),
                br(),
                actionButton("run_carto_allocate", "▶️ Optimiser allocation",
                  class = "btn-success", style = "width:100%;")
              ),
              column(9,
                DTOutput("carto_allocation_table")
              )
            ),
            br(),
            plotlyOutput("carto_allocation_plot", height = "350px")
          ),

          tabPanel("⚠️ Drapeaux qualité",
            br(),
            fluidRow(
              column(12,
                tags$h5("Anomalies géographiques détectées", style = "font-weight:700;"),
                tags$p("Contrôle de qualité automatique : données manquantes, outliers spatiaux, incohérences.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                DTOutput("carto_quality_flags")
              )
            ),
            br(),
            fluidRow(
              column(6,
                plotlyOutput("carto_quality_coverage", height = "350px")
              ),
              column(6,
                plotlyOutput("carto_quality_outliers", height = "350px")
              )
            )
          ),

          tabPanel("📄 Rapport expert",
            br(),
            fluidRow(
              column(3,
                tags$h5("Export rapport", style = "font-weight:700;"),
                tags$p("Rapport complet avec analyses spatiales, recommandations, annexes.",
                       style = "font-size:12px;color:#666;"),
                hr(),
                actionButton("run_carto_report", "▶️ Générer rapport",
                  class = "btn-primary", style = "width:100%;"),
                br(), br(),
                downloadButton("dl_carto_report_md", "📥 Markdown",
                  style = "width:100%;background:#008DC9;color:white;border:none;border-radius:6px;padding:7px;font-weight:600;margin-bottom:5px;"),
                downloadButton("dl_carto_report_html", "📥 HTML",
                  style = "width:100%;background:#4dac26;color:white;border:none;border-radius:6px;padding:7px;font-weight:600;margin-bottom:5px;")
              ),
              column(9,
                tags$div(
                  style = "background:#f9f9f9;border:1px solid #ddd;border-radius:8px;padding:15px;max-height:600px;overflow-y:auto;",
                  htmlOutput("carto_report_text")
                )
              )
            )
          )
        )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server_carto_v2 <- function(input, output, session, df_f, map_cols) {

  m <- reactive(map_cols())

  # ── Titre dynamique ─────────────────────────────────────────────────────────
  output$carto_titre_ui <- renderUI({
    tags$span(icon("map"),
              paste0(" Carte — ", input$carto_niveau, " | ",
                     if(!is.null(LABEL_INDICATEURS[input$carto_indicateur]) &&
                        !is.na(LABEL_INDICATEURS[input$carto_indicateur]))
                       LABEL_INDICATEURS[input$carto_indicateur]
                     else input$carto_indicateur))
  })

  # ── Filtres dynamiques ───────────────────────────────────────────────────────
  output$carto_filtre_region_ui <- renderUI({
    d <- df_f(); mc <- m()
    if (is.null(mc$region) || !mc$region %in% names(d)) return(NULL)
    vals <- sort(unique(na.omit(as.character(d[[mc$region]]))))
    selectInput("carto_f_region", "Filtrer par région :",
                choices  = c("Toutes", vals),
                selected = "Toutes",
                width    = "100%")
  })

  output$carto_filtre_dept_ui <- renderUI({
    req(input$carto_niveau %in% c("Département","Arrondissement"))
    d <- df_f(); mc <- m()
    if (is.null(mc$dept) || !mc$dept %in% names(d)) return(NULL)
    d_f <- d
    if (!is.null(input$carto_f_region) && input$carto_f_region != "Toutes")
      d_f <- filter(d_f, .data[[mc$region]] == input$carto_f_region)
    vals <- sort(unique(na.omit(as.character(d_f[[mc$dept]]))))
    selectInput("carto_f_dept", "Filtrer par département :",
                choices  = c("Tous", vals),
                selected = "Tous",
                width    = "100%")
  })

  # ── Indicateurs disponibles ──────────────────────────────────────────────────
  output$carto_indicateur_ui <- renderUI({
    d <- df_f()
    # Indicateurs binaires calculés
    inds_bin <- intersect(c("GAM","SAM","MAM","Stunting","Sev_Stunting",
                             "Underweight","MUAC_GAM","MUAC_SAM"), names(d))
    choices_bin <- if(length(inds_bin)>0)
      setNames(inds_bin,
               sapply(inds_bin, function(x)
                 if(!is.na(LABEL_INDICATEURS[x])) LABEL_INDICATEURS[x] else x))
    else c()

    # Variables continues
    choices_num <- c(
      "WHZ moyen"      = "whz_moy",
      "HAZ moyen"      = "haz_moy",
      "WAZ moyen"      = "waz_moy",
      "MUAC moyen (mm)"= "muac_moy",
      "ICV moyen"      = "ICV_moy",
      "FCS moyen"      = "fcs_moy",
      "N enquêtés"     = "n"
    )

    choices_all <- c(
      `── Malnutrition aiguë ──`   = "",
      choices_bin[grepl("GAM|SAM|MAM|MUAC", names(choices_bin))],
      `── Retard de croissance ──` = "",
      choices_bin[grepl("Stunt", names(choices_bin))],
      `── Insuffisance pondérale ──` = "",
      choices_bin[grepl("Underweight|pondér", names(choices_bin))],
      `── Variables continues ──`  = "",
      choices_num
    )

    selectInput("carto_indicateur", NULL,
                choices  = c(choices_bin, choices_num),
                selected = if("GAM"%in%inds_bin)"GAM" else "n",
                width    = "100%")
  })

  # ── Carte Leaflet principale ──────────────────────────────────────────────
  output$carto_leaflet_main <- renderLeaflet({
    d  <- df_f(); mc <- m()
    withProgress(message = "Chargement de la carte...", {
      carte_cameroun_leaflet(
        d            = d,
        m            = mc,
        niveau       = input$carto_niveau,
        indicateur   = input$carto_indicateur,
        type_carte   = input$carto_type,
        fond         = input$carto_fond,
        show_pts     = input$carto_pts,
        show_heat    = input$carto_heat,
        show_labels  = input$carto_labels,
        filtre_region = input$carto_f_region,
        filtre_dept   = input$carto_f_dept
      )
    })
  })

  # ── Carte ggplot2 statique ───────────────────────────────────────────────
  output$carto_ggplot_main <- renderPlotly({
    d  <- df_f(); mc <- m()
    p  <- carte_statique_ggplot(
      d          = d,
      m          = mc,
      niveau     = input$carto_niveau,
      indicateur = input$carto_indicateur,
      palette    = input$carto_pal_static,
      type_geom  = input$carto_geom_static
    )
    ggplotly(p, tooltip = "text")
  })

  # ── Carte Afrique centrale ───────────────────────────────────────────────
  output$carto_afrique <- renderPlotly({
    carte_afrique_centrale(
      d            = df_f(),
      indicateur   = input$carto_af_ind,
      zoom_region  = input$carto_af_zoom
    )
  })

  # ── Tableau des prévalences ──────────────────────────────────────────────
  output$carto_tableau_prev <- renderDT({
    d <- df_f(); mc <- m()
    ind <- input$carto_indicateur
    col_geo <- switch(input$carto_niveau,
      "Région"         = mc$region,
      "Département"    = mc$dept,
      "Arrondissement" = detect_col(names(d), c("arrondissement","district","commune")),
      mc$region
    )
    req(!is.null(col_geo), col_geo %in% names(d))

    agg <- agreger_geo(d, col_geo, mc)
    inds_dispo <- intersect(c("GAM","SAM","MAM","Stunting","Underweight",
                               "MUAC_GAM","n"), names(agg))

    agg |>
      select(Zone, n, any_of(inds_dispo)) |>
      arrange(if(ind %in% names(agg)) desc(.data[[ind]]) else desc(n)) |>
      mutate(
        Sévérité = if("GAM" %in% names(agg)) {
          case_when(
            GAM >= 30 ~ "🟥 Urgence max",
            GAM >= 15 ~ "🔴 Critique",
            GAM >= 10 ~ "🟠 Sérieux",
            GAM >= 5  ~ "🟡 Alerte",
            TRUE      ~ "🟢 Acceptable"
          )
        } else "—"
      ) |>
      datatable(
        rownames = FALSE,
        filter   = "top",
        options  = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
        class    = "compact stripe hover"
      ) |>
      formatStyle(
        intersect(c("GAM","SAM","Stunting"), names(agg)),
        color = styleInterval(c(5, 10, 15, 30),
          c("#4dac26","#f9c734","#e88400","#c9161d","#640000")),
        fontWeight = "bold"
      ) |>
      formatStyle("n",
        background = styleColorBar(c(0, max(agg$n)), "#e3f2fd"),
        backgroundSize = "100% 70%"
      )
  })

  # ── Barplot prévalences ──────────────────────────────────────────────────
  output$carto_bar_prev <- renderPlotly({
    d <- df_f(); mc <- m(); ind <- input$carto_indicateur
    col_geo <- switch(input$carto_niveau,
      "Région"="region", "Département"="dept",
      "Arrondissement"="arrondissement", "region")
    col_geo <- mc[[col_geo]]
    req(!is.null(col_geo), col_geo %in% names(d), ind %in% names(d))

    agg <- agreger_geo(d, col_geo, mc)
    req(ind %in% names(agg))

    lbl <- if(!is.na(LABEL_INDICATEURS[ind])) LABEL_INDICATEURS[ind] else ind

    p <- ggplot(agg, aes(
      x    = fct_reorder(Zone, .data[[ind]]),
      y    = .data[[ind]],
      fill = .data[[ind]],
      text = paste0(Zone, "\n", lbl, " = ",
                    round(.data[[ind]], 1),
                    if(grepl("%",lbl))" %" else "")
    )) +
      geom_col(alpha = 0.88, width = 0.7) +
      geom_hline(
        yintercept = 15,
        linetype   = "dashed",
        color      = "#c9161d",
        linewidth  = 0.8
      ) +
      scale_fill_gradientn(
        colors = c("#4dac26","#f9c734","#e88400","#c9161d"),
        guide  = "none"
      ) +
      coord_flip() +
      labs(x = "", y = lbl,
           title = paste0(lbl, " par ", input$carto_niveau)) +
      theme_minimal(base_size = 10)
    ggplotly(p, tooltip = "text")
  })

  # ── Forest plot IC95% ────────────────────────────────────────────────────
  output$carto_forest_prev <- renderPlotly({
    d <- df_f(); mc <- m(); ind <- input$carto_indicateur
    col_geo <- switch(input$carto_niveau,
      "Région"="region","Département"="dept","arrondissement")
    col_geo <- if(!is.null(mc[[col_geo]])) mc[[col_geo]] else mc$region
    req(!is.null(col_geo), col_geo %in% names(d))

    # Seulement pour les indicateurs binaires
    inds_bin <- intersect(c("GAM","SAM","Stunting","Underweight","MUAC_GAM"),
                           names(d))
    req(ind %in% inds_bin)

    res <- d |>
      group_by(Zone = .data[[col_geo]]) |>
      group_modify(~{
        x  <- sum(.x[[ind]], na.rm = TRUE)
        n  <- sum(!is.na(.x[[ind]]))
        if(n == 0) return(NULL)
        ic <- binom.confint(x, n, methods = "wilson")
        tibble(prev = round(ic$mean*100,1),
               lo   = round(ic$lower*100,1),
               hi   = round(ic$upper*100,1),
               n    = n)
      }) |>
      ungroup() |>
      filter(!is.na(Zone))

    lbl <- if(!is.na(LABEL_INDICATEURS[ind])) LABEL_INDICATEURS[ind] else ind
    p <- ggplot(res, aes(
      x    = prev,
      y    = fct_reorder(Zone, prev),
      text = paste0(Zone, "\n", round(prev,1), "% [",lo,"–",hi,"%]\nN=",n)
    )) +
      geom_vline(xintercept = 15, linetype = "dashed",
                 color = "#c9161d", linewidth = 0.7) +
      geom_errorbarh(aes(xmin = lo, xmax = hi),
                     height = 0.35, linewidth = 0.9,
                     color = "#008DC9") +
      geom_point(aes(color = prev), size = 3.5) +
      scale_color_gradientn(
        colors = c("#4dac26","#f9c734","#e88400","#c9161d"),
        guide  = "none"
      ) +
      labs(x = paste0(lbl, " (%)"), y = "",
           title = paste0("Forest plot IC 95% — ", lbl)) +
      theme_minimal(base_size = 10)
    ggplotly(p, tooltip = "text")
  })

  # ── Carte GPS détaillée ──────────────────────────────────────────────────
  output$carto_gps_color_ui <- renderUI({
    d  <- df_f()
    vc <- names(d)[sapply(names(d), function(n) {
      x <- d[[n]]
      !startsWith(n,".") && (is.factor(x)||is.character(x)) &&
      length(unique(na.omit(x))) >= 2 &&
      length(unique(na.omit(x))) <= 20
    })]
    choices <- c()
    if("nutr_cat" %in% names(d)) choices <- c("Statut nutritionnel"="nutr_cat")
    if("muac_cat" %in% names(d)) choices <- c(choices,"Catégorie MUAC"="muac_cat")
    choices <- c(choices, setNames(head(vc,8), head(vc,8)))
    selectInput("carto_gps_color","Couleur par :",choices=choices,width="100%")
  })

  output$carto_gps_map <- renderLeaflet({
    d  <- df_f(); mc <- m()
    req(!is.null(mc$lat), !is.null(mc$lon),
        mc$lat %in% names(d), mc$lon %in% names(d))

    # Filtre GPS avec indexation base R (rapide pour gros data)
    valid_idx <- !is.na(d[[mc$lat]]) & !is.na(d[[mc$lon]])
    d_pts <- d[valid_idx, , drop = FALSE]
    req(nrow(d_pts) > 0)

    # Échantillonner si trop de points
    n_total <- nrow(d_pts)
    if (n_total > MAX_GPS_POINTS) {
      set.seed(42L)
      d_pts <- d_pts[sample.int(n_total, MAX_GPS_POINTS), , drop = FALSE]
      showNotification(
        paste0("📍 ", format(MAX_GPS_POINTS, big.mark = " "),
               " points affichés sur ", format(n_total, big.mark = " "),
               " (échantillon représentatif)"),
        type = "warning", duration = 8
      )
    }

    col_var  <- input$carto_gps_color
    col_vals <- if(!is.null(col_var) && col_var %in% names(d_pts))
      d_pts[[col_var]] else rep("Données", nrow(d_pts))
    niv      <- length(unique(na.omit(col_vals)))

    pal_def <- c("#4dac26","#e88400","#c9161d","#008DC9","#7b2d8b")
    pal_pt  <- colorFactor(
      palette = if(niv <= 5) pal_def[1:niv] else RColorBrewer::brewer.pal(min(niv,8),"Set2"),
      domain  = col_vals
    )

    # Préparer colonnes pour leaflet
    d_pts$.lat_gps  <- d_pts[[mc$lat]]
    d_pts$.lon_gps  <- d_pts[[mc$lon]]
    d_pts$.fill_gps <- pal_pt(col_vals)

    # Popups VECTORISÉS (pas de boucle — O(1) au lieu de O(n))
    popup_html <- "<div style='font-size:12px;'>"
    if (!is.null(col_var) && col_var %in% names(d_pts)) {
      popup_html <- paste0(popup_html,
        "<b>", col_var, " :</b> ", d_pts[[col_var]], "<br>")
    }
    if (".whz" %in% names(d_pts)) {
      popup_html <- paste0(popup_html,
        "<b>WHZ :</b> ", round(d_pts[[".whz"]], 2), "<br>")
    }
    if (".muac" %in% names(d_pts)) {
      popup_html <- paste0(popup_html,
        "<b>MUAC :</b> ", d_pts[[".muac"]], " mm")
    }
    d_pts$.popup_gps <- paste0(popup_html, "</div>")

    m_gps <- leaflet(d_pts) |>
      addProviderTiles(input$carto_gps_fond) |>
      addScaleBar(position = "bottomleft") |>
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) |>
      addFullscreenControl(position = "topleft")

    if (input$carto_gps_cluster) {
      m_gps <- m_gps |>
        addCircleMarkers(
          lat            = ~.lat_gps,
          lng            = ~.lon_gps,
          radius         = input$carto_gps_size,
          fillColor      = ~.fill_gps,
          fillOpacity    = 0.85,
          color          = "white",
          weight         = 0.8,
          clusterOptions = markerClusterOptions(
            disableClusteringAtZoom = 12
          ),
          popup = ~.popup_gps
        )
    } else {
      m_gps <- m_gps |>
        addCircleMarkers(
          lat         = ~.lat_gps,
          lng         = ~.lon_gps,
          radius      = input$carto_gps_size,
          fillColor   = ~.fill_gps,
          fillOpacity = 0.85,
          color       = "white",
          weight      = 0.8
        )
    }

    m_gps |>
      addLegend(
        pal      = pal_pt,
        values   = col_vals,
        title    = col_var,
        position = "bottomright"
      ) |>
      addControl(
        html = paste0(
          "<div style='background:white;padding:6px 10px;border-radius:6px;",
          "font-size:11px;color:#555;'>",
          "<b>N points :</b> ", format(nrow(d_pts), big.mark = " "),
          if (n_total > MAX_GPS_POINTS)
            paste0(" / ", format(n_total, big.mark = " "), " total")
          else "",
          "</div>"
        ),
        position = "topright"
      )
  })

  # ── Exports ──────────────────────────────────────────────────────────────
  output$dl_carte_png <- downloadHandler(
    filename = function()
      paste0("carte_cameroun_", input$carto_niveau, "_",
             input$carto_indicateur, "_", Sys.Date(), ".png"),
    content = function(file) {
      withProgress(message = "Génération PNG...", {
        p <- carte_statique_ggplot(
          d          = df_f(),
          m          = m(),
          niveau     = input$carto_niveau,
          indicateur = input$carto_indicateur,
          palette    = input$carto_pal_static,
          type_geom  = input$carto_geom_static
        )
        ggsave(file, plot = p, width = 20, height = 16,
               dpi = 300, units = "cm", bg = "white")
      })
    }
  )

  output$dl_carte_pdf <- downloadHandler(
    filename = function()
      paste0("carte_cameroun_", input$carto_niveau, "_",
             input$carto_indicateur, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      withProgress(message = "Génération PDF...", {
        p <- carte_statique_ggplot(
          d          = df_f(),
          m          = m(),
          niveau     = input$carto_niveau,
          indicateur = input$carto_indicateur,
          palette    = input$carto_pal_static,
          type_geom  = input$carto_geom_static
        )
        ggsave(file, plot = p, width = 20, height = 16,
               units = "cm", device = "pdf", bg = "white")
      })
    }
  )

  # ── Modal plein écran pour la carte ──────────────────────────────────────
  observeEvent(input$carto_fullscreen_btn, {
    showModal(modalDialog(
      title = tags$span(
        icon("map"), " Carte nutritionnelle — Vue plein écran",
        style = "color:#00426A;font-weight:700;"
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Fermer"),
      tags$div(
        style = "width:100%;",
        leafletOutput("carto_leaflet_fullscreen", height = "80vh")
      )
    ))
  })

  output$carto_leaflet_fullscreen <- renderLeaflet({
    d  <- df_f(); mc <- m()
    carte_cameroun_leaflet(
      d            = d,
      m            = mc,
      niveau       = input$carto_niveau,
      indicateur   = input$carto_indicateur,
      type_carte   = input$carto_type,
      fond         = input$carto_fond,
      show_pts     = input$carto_pts,
      show_heat    = input$carto_heat,
      show_labels  = input$carto_labels,
      filtre_region = input$carto_f_region,
      filtre_dept   = input$carto_f_dept
    )
  })
}
