# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  mod_advanced_geolocation.R — Géolocalisation & Analyse Spatiale           ║
# ║  Détection dynamique colonnes | Clustering spatial | Heat maps             ║
# ║  Niveau: Doctoral Research Grade — v2 Dynamic                             ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(plotly)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

advancedGeolocationUI <- function(id, lang = "fr") {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "background: linear-gradient(135deg, #f0f8ff 0%, #e8f4f8 100%);
               border-left: 4px solid #008DC9; border-radius: 6px;
               padding: 15px 20px; margin-bottom: 15px;",
      tags$h4(style = "margin:0 0 8px 0; color:#008DC9;",
              icon("map-location-dot"), tr("geo_title", lang)),
      tags$p(style = "margin:0; color:#555; font-size:13px; line-height:1.5;",
             tr("geo_desc", lang),
             )
    ),
    fluidRow(
      box(width = 3, status = "info", solidHeader = TRUE, title = tr("geo_config", lang),
          # --- Source de localisation ---
          tags$div(class = "section-header", tr("geo_loc_source_hdr", lang)),
          selectInput(ns("geo_location_source"), NULL,
                      choices = setNames(c("explicit","address","admin"), c(tr("geo_loc_explicit",lang),tr("geo_loc_address",lang),tr("geo_loc_admin",lang))),
                      width = "100%"),
          uiOutput(ns("geo_col_selection")),
          hr(),

          # --- Indicateur de coloration (dynamique) ---
          tags$div(class = "section-header", tr("geo_color_hdr", lang)),
          uiOutput(ns("geo_indicator_ui")),
          hr(),

          # --- Clustering ---
          tags$div(class = "section-header", tr("geo_cluster_hdr", lang)),
          numericInput(ns("geo_n_clusters"), tr("geo_n_clusters", lang),
                       value = 4, min = 2, max = 20, width = "100%"),
          hr(),

          # --- Google API (optionnel) ---
          textInput(ns("geo_api_key"), tr("geo_api_key", lang),
                    placeholder = "AIzaSy...", width = "100%"),
          tags$small(style = "display:block;color:#666;margin:6px 0;",
                     tr("geo_api_note", lang)),
          hr(),
          actionButton(ns("geo_geocode"), tr("geo_btn_run", lang),
                       class = "btn btn-oms btn-block", width = "100%",
                       style = "font-size:13px;padding:10px;")
      ),
      box(width = 9, status = "primary", solidHeader = TRUE,
          title = tr("geo_map_title", lang),
          leafletOutput(ns("geo_map"), height = "500px")
      )
    ),
    fluidRow(
      box(width = 6, status = "info", solidHeader = TRUE,
          title = tr("geo_kmeans_title", lang),
          plotlyOutput(ns("geo_clustering_plot"), height = "350px")
      ),
      box(width = 6, status = "info", solidHeader = TRUE,
          title = tr("geo_heatmap_title", lang),
          leafletOutput(ns("geo_heatmap"), height = "350px")
      )
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = TRUE,
          title = tr("geo_distance_title", lang),
          fluidRow(
            column(4,
              numericInput(ns("geo_radius_km"), tr("geo_radius", lang), value = 50, min = 5, width = "100%"),
              actionButton(ns("geo_distance_matrix"), tr("geo_btn_distance", lang),
                          class = "btn btn-info btn-block", width = "100%")
            ),
            column(8,
              plotlyOutput(ns("geo_distance_heatmap"), height = "300px")
            )
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = tags$span(icon("download"), " Exports géospatiaux"),
          fluidRow(
            column(3, downloadButton(ns("geo_dl_geojson"), "📍 GeoJSON",
                                     style = "background:#008DC9;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("geo_dl_shapefile"), "🗺️ Shapefile (ZIP)",
                                     style = "background:#4dac26;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("geo_dl_kml"), "🧭 KML (Google Earth)",
                                     style = "background:#e88400;color:white;border:none;width:100%;")),
            column(3, downloadButton(ns("geo_dl_clusters"), "📊 Clusters CSV",
                                     style = "background:#9b59b6;color:white;border:none;width:100%;"))
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = "Rapport d'analyse spatiale",
          verbatimTextOutput(ns("geo_spatial_report"))
      )
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

advancedGeolocationServer <- function(id, data_reactive, lang = reactive("fr")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Helpers : détecter colonnes par type / regex ─────────────────────────
    detect_cols <- function(d, pattern) {
      grep(pattern, names(d), ignore.case = TRUE, value = TRUE)
    }

    numeric_cols <- reactive({
      d <- data_reactive(); req(d)
      names(d)[sapply(d, is.numeric)]
    })

    # ── UI dynamique : sélection colonnes selon source ───────────────────────
    output$geo_col_selection <- renderUI({
      req(input$geo_location_source)
      switch(input$geo_location_source,
        "explicit" = tagList(uiOutput(ns("geo_lat_ui")), uiOutput(ns("geo_lon_ui"))),
        "address"  = uiOutput(ns("geo_address_ui")),
        "admin"    = tagList(uiOutput(ns("geo_region_ui")), uiOutput(ns("geo_district_ui")))
      )
    })

    output$geo_lat_ui <- renderUI({
      d <- data_reactive(); req(d)
      vn <- names(d)
      lat_guess <- detect_cols(d, "lat|latitude|y_coord|gps.*lat")
      selectInput(ns("geo_lat_col"), "Colonne latitude :",
                  choices = c("— Sélectionner —" = "", vn),
                  selected = if (length(lat_guess) > 0) lat_guess[1] else "",
                  width = "100%")
    })

    output$geo_lon_ui <- renderUI({
      d <- data_reactive(); req(d)
      vn <- names(d)
      lon_guess <- detect_cols(d, "lon|longitude|x_coord|gps.*lon")
      selectInput(ns("geo_lon_col"), "Colonne longitude :",
                  choices = c("— Sélectionner —" = "", vn),
                  selected = if (length(lon_guess) > 0) lon_guess[1] else "",
                  width = "100%")
    })

    output$geo_address_ui <- renderUI({
      d <- data_reactive(); req(d)
      vn <- names(d)
      addr_guess <- detect_cols(d, "address|adresse|lieu|location|village|localite")
      selectInput(ns("geo_address_col"), "Colonne adresse :",
                  choices = c("— Sélectionner —" = "", vn),
                  selected = if (length(addr_guess) > 0) addr_guess[1] else "",
                  width = "100%")
    })

    output$geo_region_ui <- renderUI({
      d <- data_reactive(); req(d)
      vn <- names(d)
      reg_guess <- detect_cols(d, "region|prov|state|division|gouvernorat")
      selectInput(ns("geo_region_col"), "Colonne région/province :",
                  choices = c("— Sélectionner —" = "", vn),
                  selected = if (length(reg_guess) > 0) reg_guess[1] else "",
                  width = "100%")
    })

    output$geo_district_ui <- renderUI({
      d <- data_reactive(); req(d)
      vn <- names(d)
      dist_guess <- detect_cols(d, "district|department|dept|arrond|commune")
      selectInput(ns("geo_district_col"), "Colonne district/département :",
                  choices = c("— Sélectionner —" = "", vn),
                  selected = if (length(dist_guess) > 0) dist_guess[1] else "",
                  width = "100%")
    })

    # ── UI dynamique : indicateur de coloration (toute colonne numérique) ────
    output$geo_indicator_ui <- renderUI({
      req(numeric_cols())
      nc <- numeric_cols()
      selectInput(ns("geo_indicator"), "Colorier par (variable numérique) :",
                  choices = c("Aucun (taille cluster)" = "__cluster_size__", nc),
                  width = "100%")
    })

    # ── Données géocodées ────────────────────────────────────────────────────
    geocoded_data <- reactive({
      req(data_reactive(), input$geo_location_source, input$geo_geocode)

      d <- data_reactive()

      tryCatch({
        if (input$geo_location_source == "explicit") {
          req(input$geo_lat_col, input$geo_lon_col)
          req(input$geo_lat_col != "", input$geo_lon_col != "")
          d <- d |>
            mutate(
              .geo_lat = as.numeric(.data[[input$geo_lat_col]]),
              .geo_lon = as.numeric(.data[[input$geo_lon_col]])
            ) |>
            filter(!is.na(.geo_lat), !is.na(.geo_lon))

        } else if (input$geo_location_source == "address") {
          req(input$geo_address_col, input$geo_address_col != "")
          # Géocodage réel nécessiterait une API ; ici on prévient l'utilisateur
          showNotification(
            "Le géocodage d'adresses nécessite une clé Google API valide. Mode démo activé.",
            type = "warning", duration = 8
          )
          d <- d |>
            mutate(
              .geo_lat = runif(n(), -5, 15),
              .geo_lon = runif(n(), -15, 50),
              .geo_address = as.character(.data[[input$geo_address_col]])
            )

        } else {
          req(input$geo_region_col, input$geo_region_col != "")
          showNotification(
            "Centroïdes admin simulés. Connectez une API ou un fichier shapefile pour la vraie géolocalisation.",
            type = "warning", duration = 8
          )
          d <- d |>
            mutate(
              .geo_region = as.character(.data[[input$geo_region_col]]),
              .geo_lat = runif(n(), -5, 15),
              .geo_lon = runif(n(), -15, 50)
            )
        }

        validate(need(nrow(d) >= 3, "Moins de 3 observations géolocalisables trouvées."))
        d
      }, error = function(e) {
        showNotification(paste("Erreur géocodage :", e$message), type = "error")
        NULL
      })
    })

    # ── K-means clustering ───────────────────────────────────────────────────
    spatial_clusters <- reactive({
      req(geocoded_data())
      d <- geocoded_data()
      k <- min(input$geo_n_clusters %||% 4, floor(nrow(d) / 2))
      k <- max(k, 2)

      coords <- d |> select(.geo_lat, .geo_lon) |> drop_na()
      if (nrow(coords) < k) return(NULL)

      set.seed(3742)
      km <- kmeans(coords, centers = k, nstart = 10)

      d |>
        mutate(.geo_cluster = km$cluster) |>
        group_by(.geo_cluster) |>
        mutate(
          .geo_cluster_lat  = mean(.geo_lat),
          .geo_cluster_lon  = mean(.geo_lon),
          .geo_cluster_size = n()
        ) |>
        ungroup()
    })

    # ── Valeur de coloration (depuis les vraies données) ─────────────────────
    color_values <- reactive({
      req(spatial_clusters(), input$geo_indicator)
      d <- spatial_clusters()

      if (input$geo_indicator == "__cluster_size__") {
        d$.geo_color_val <- d$.geo_cluster_size
      } else if (input$geo_indicator %in% names(d)) {
        d$.geo_color_val <- as.numeric(d[[input$geo_indicator]])
      } else {
        d$.geo_color_val <- d$.geo_cluster_size
      }
      d
    })

    # ── Carte principale ─────────────────────────────────────────────────────
    output$geo_map <- renderLeaflet({
      req(color_values())
      d <- color_values()
      req(sum(!is.na(d$.geo_color_val)) > 0)

      indicator_label <- if (input$geo_indicator == "__cluster_size__") {
        "Taille cluster"
      } else {
        input$geo_indicator
      }

      pal <- colorBin("RdYlGn", domain = d$.geo_color_val, reverse = TRUE,
                       bins = min(5, length(unique(na.omit(d$.geo_color_val)))))

      leaflet(d) |>
        addTiles() |>
        addCircleMarkers(
          ~.geo_lon, ~.geo_lat,
          radius = ~sqrt(.geo_cluster_size) * 3,
          color = ~pal(.geo_color_val),
          opacity = 0.7,
          fillOpacity = 0.5,
          popup = ~paste0(
            "<b>Cluster ", .geo_cluster, "</b><br/>",
            "N: ", .geo_cluster_size, "<br/>",
            indicator_label, ": ", round(.geo_color_val, 2)
          )
        ) |>
        addLegend(position = "bottomright", pal = pal,
                  values = ~.geo_color_val, title = indicator_label, opacity = 0.7)
    })

    # ── Heat map ─────────────────────────────────────────────────────────────
    output$geo_heatmap <- renderLeaflet({
      req(spatial_clusters())
      d <- spatial_clusters()

      leaflet(d) |>
        addTiles() |>
        addCircleMarkers(
          ~.geo_lon, ~.geo_lat,
          radius = ~sqrt(.geo_cluster_size) * 2,
          color = "red", fillColor = "red", fillOpacity = 0.3,
          popup = ~paste("Cluster", .geo_cluster, ":", .geo_cluster_size, "obs")
        )
    })

    # ── Clustering plot ──────────────────────────────────────────────────────
    output$geo_clustering_plot <- renderPlotly({
      req(spatial_clusters())
      d <- spatial_clusters()

      p <- ggplot(d, aes(x = .geo_lon, y = .geo_lat,
                         color = factor(.geo_cluster),
                         size = .geo_cluster_size)) +
        geom_point(alpha = 0.6) +
        geom_point(aes(x = .geo_cluster_lon, y = .geo_cluster_lat),
                   color = "black", shape = 4, size = 8) +
        scale_color_brewer(palette = "Set1") +
        labs(title = "K-means clustering spatial",
             x = "Longitude", y = "Latitude",
             color = "Cluster", size = "Effectif") +
        theme_minimal()

      ggplotly(p)
    })

    # ── Matrice de distances ─────────────────────────────────────────────────
    output$geo_distance_heatmap <- renderPlotly({
      req(input$geo_distance_matrix, spatial_clusters())

      centroids <- spatial_clusters() |>
        select(.geo_cluster, .geo_cluster_lat, .geo_cluster_lon) |>
        distinct()

      if (nrow(centroids) < 2) return(plotly_empty())

      # Haversine
      haversine_km <- function(lon1, lat1, lon2, lat2) {
        R <- 6371
        dLat <- (lat2 - lat1) * pi / 180
        dLon <- (lon2 - lon1) * pi / 180
        a <- sin(dLat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon / 2)^2
        R * 2 * atan2(sqrt(a), sqrt(1 - a))
      }

      n <- nrow(centroids)
      dist_mat <- matrix(0, n, n)
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          dist_mat[i, j] <- haversine_km(
            centroids$.geo_cluster_lon[i], centroids$.geo_cluster_lat[i],
            centroids$.geo_cluster_lon[j], centroids$.geo_cluster_lat[j]
          )
        }
      }
      rownames(dist_mat) <- colnames(dist_mat) <- paste0("C", centroids$.geo_cluster)

      plot_ly(z = dist_mat, x = colnames(dist_mat), y = rownames(dist_mat),
              type = "heatmap", colorscale = "Viridis") |>
        layout(title = "Distances inter-clusters (km)",
               xaxis = list(title = ""), yaxis = list(title = ""))
    })

    # ── Rapport spatial ──────────────────────────────────────────────────────
    output$geo_spatial_report <- renderPrint({
      req(spatial_clusters())
      d <- spatial_clusters()

      cat("═══════════════════════════════════════════════════════════\n")
      cat("RAPPORT D'ANALYSE SPATIALE\n")
      cat("═══════════════════════════════════════════════════════════\n\n")

      cat("SOURCE:", switch(input$geo_location_source,
        "explicit" = "Colonnes latitude / longitude",
        "address"  = "Adresses géocodées (mode démo)",
        "admin"    = "Divisions admin (mode démo)"
      ), "\n")

      ind_label <- if (input$geo_indicator == "__cluster_size__") "Taille cluster" else input$geo_indicator
      cat("INDICATEUR COLORATION:", ind_label, "\n\n")

      cat("STATISTIQUES CLUSTERS\n")
      cat("Nombre de clusters:", max(d$.geo_cluster), "\n")
      cat("Total observations:", nrow(d), "\n\n")

      cat("DISTRIBUTION PAR CLUSTER\n")
      cluster_stats <- d |>
        group_by(.geo_cluster) |>
        summarise(
          N = n(),
          Lat_mean = round(mean(.geo_lat), 4),
          Lon_mean = round(mean(.geo_lon), 4),
          .groups = "drop"
        )
      print(as.data.frame(cluster_stats))

      cat("\nBOUNDING BOX\n")
      cat("Latitude  : [", round(min(d$.geo_lat), 4), ",", round(max(d$.geo_lat), 4), "]\n")
      cat("Longitude : [", round(min(d$.geo_lon), 4), ",", round(max(d$.geo_lon), 4), "]\n\n")

      # Indicateur résumé si numérique réel
      if (input$geo_indicator != "__cluster_size__" && input$geo_indicator %in% names(d)) {
        vals <- as.numeric(d[[input$geo_indicator]])
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0) {
          cat("RÉSUMÉ INDICATEUR :", input$geo_indicator, "\n")
          cat("  Moyenne :", round(mean(vals), 2), "\n")
          cat("  Médiane :", round(median(vals), 2), "\n")
          cat("  Min-Max : [", round(min(vals), 2), ",", round(max(vals), 2), "]\n")
        }
      }

      haversine_km <- function(lon1, lat1, lon2, lat2) {
        R <- 6371
        dLat <- (lat2 - lat1) * pi / 180
        dLon <- (lon2 - lon1) * pi / 180
        a <- sin(dLat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon / 2)^2
        R * 2 * atan2(sqrt(a), sqrt(1 - a))
      }
      dx <- haversine_km(min(d$.geo_lon), min(d$.geo_lat), max(d$.geo_lon), min(d$.geo_lat))
      dy <- haversine_km(min(d$.geo_lon), min(d$.geo_lat), min(d$.geo_lon), max(d$.geo_lat))
      area_km2 <- max(dx * dy, 0.01)
      cat("\nDENSITÉ (obs/km²):", round(nrow(d) / area_km2, 4), "\n")
    })

    # ── Downloads ────────────────────────────────────────────────────────────
    output$geo_dl_geojson <- downloadHandler(
      filename = function() paste0("geo_enquetes_", Sys.Date(), ".geojson"),
      content = function(file) {
        req(spatial_clusters())
        d <- spatial_clusters() |>
          st_as_sf(coords = c(".geo_lon", ".geo_lat"), crs = 4326)
        sf::write_sf(d, file)
      }
    )

    output$geo_dl_shapefile <- downloadHandler(
      filename = function() paste0("geo_enquetes_", Sys.Date(), ".zip"),
      content = function(file) {
        req(spatial_clusters())
        d <- spatial_clusters() |>
          st_as_sf(coords = c(".geo_lon", ".geo_lat"), crs = 4326)
        temp_dir <- tempdir()
        shp_base <- file.path(temp_dir, "enquetes")
        sf::write_sf(d, paste0(shp_base, ".shp"))
        zip_files <- list.files(temp_dir, pattern = "^enquetes\\.", full.names = TRUE)
        utils::zip(file, zip_files, flags = "-j")
      }
    )

    output$geo_dl_kml <- downloadHandler(
      filename = function() paste0("geo_enquetes_", Sys.Date(), ".kml"),
      content = function(file) {
        req(spatial_clusters())
        d <- spatial_clusters() |>
          st_as_sf(coords = c(".geo_lon", ".geo_lat"), crs = 4326)
        sf::write_sf(d, file, driver = "KML")
      }
    )

    output$geo_dl_clusters <- downloadHandler(
      filename = function() paste0("geo_clusters_", Sys.Date(), ".csv"),
      content = function(file) {
        req(spatial_clusters())
        d <- spatial_clusters() |>
          select(.geo_cluster, .geo_cluster_lat, .geo_cluster_lon, .geo_cluster_size) |>
          distinct() |>
          rename(cluster = .geo_cluster, lat = .geo_cluster_lat,
                 lon = .geo_cluster_lon, size = .geo_cluster_size)
        write.csv(d, file, row.names = FALSE)
      }
    )
  })
}
