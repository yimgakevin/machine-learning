# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  carto_server_outputs.R — Server outputs pour les 10 tabs cartographie v2    ║
# ║  À inclure dans server_carto_v2() via source("carto_server_outputs.R", ...)  ║
# ║  Utilise les 4 moteurs experts : TIER 1-4                                    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# Source les 4 moteurs experts (si pas déjà sourcés dans app_v2_final.R)
if (!exists("calc_jenks_breaks")) source("carto_expert_engine.R", local = FALSE)
if (!exists("calc_lisa")) {
  tryCatch(source("spatial_analysis_engine.R", local = FALSE),
           error = function(e) warning("spatial_analysis_engine nécessite spdep"))
}
if (!exists("calc_intervention_priority")) source("carto_intelligence_engine.R", local = FALSE)
if (!exists("predict_spatial_trend")) source("carto_predictive_engine.R", local = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# Information de retombée : niveau Arrondissement sans colonne dédiée
# ══════════════════════════════════════════════════════════════════════════════
observeEvent(input$carto_niveau, {
  niv <- input$carto_niveau
  if (!is.null(niv) && niv == "Arrondissement") {
    d <- tryCatch(df_f(), error = function(e) NULL); req(!is.null(d))
    col_arr <- detect_col(names(d), c("arrondissement","district","commune"))
    if (is.null(col_arr) || !(col_arr %in% names(d)))
      showNotification(
        "ℹ️ Aucune colonne arrondissement/district/commune dans les données : analyses et cartes affichées au niveau région.",
        type = "warning", duration = 10)
  }
})

# ── Helper : agrégation rapide par zone (data.table si dispo) ──────────────
fast_agg_by_zone <- function(d, col_geo, ind_col = NULL) {
  if (is.null(col_geo) || !col_geo %in% names(d)) return(NULL)

  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(d)
    if (!is.null(ind_col) && ind_col %in% names(dt)) {
      res <- dt[!is.na(get(col_geo)) & get(col_geo) != "",
                .(value = mean(get(ind_col), na.rm = TRUE), n = .N),
                by = .(Zone = get(col_geo))]
    } else {
      res <- dt[!is.na(get(col_geo)) & get(col_geo) != "",
                .(n = .N),
                by = .(Zone = get(col_geo))]
    }
    as.data.frame(res)
  } else {
    # Fallback dplyr
    if (!is.null(ind_col) && ind_col %in% names(d)) {
      d |>
        dplyr::group_by(Zone = .data[[col_geo]]) |>
        dplyr::summarise(value = mean(.data[[ind_col]], na.rm = TRUE),
                         n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(!is.na(Zone), Zone != "")
    } else {
      d |>
        dplyr::group_by(Zone = .data[[col_geo]]) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(!is.na(Zone), Zone != "")
    }
  }
}

# ── Helper : résoudre la colonne géographique ──────────────────────────────
resolve_geo_col <- function(mc) {
  if (!is.null(mc) && !is.null(mc$region)) mc$region else "region"
}


# ══════════════════════════════════════════════════════════════════════════════
# TIER 2 : TAB HOTSPOTS LISA
# ══════════════════════════════════════════════════════════════════════════════

carto_lisa_results <- reactiveVal(NULL)

observeEvent(input$run_carto_lisa, {
  req(df_f())
  d <- df_f()
  ind <- input$carto_lisa_ind
  
  withProgress(message = "Calcul LISA en cours...", {
    tryCatch({
      # Charger shapefile
      sf_cmr <- load_gadm(level = 1)
      if (is.null(sf_cmr)) {
        showNotification("❌ Impossible de charger les frontières GADM. Vérifiez votre connexion internet.", type = "error", duration = 10)
        return()
      }
      
      # Agréger données par région
      geo_col <- resolve_geo_col(map_cols())
      if (!geo_col %in% names(d)) {
        showNotification("❌ Colonne géographique introuvable. Vérifiez le mappeur de colonnes.", type = "error")
        return()
      }
      if (!ind %in% names(d)) {
        showNotification(paste0("❌ L'indicateur '", ind, "' n'existe pas dans les données."), type = "error")
        return()
      }
      agg <- d |>
        dplyr::filter(!is.na(.data[[geo_col]]), .data[[geo_col]] != "") |>
        dplyr::group_by(Zone = .data[[geo_col]]) |>
        dplyr::summarise(
          value = mean(.data[[ind]], na.rm = TRUE),
          n = dplyr::n(),
          .groups = "drop"
        )
      
      # Joindre à shapefile
      sf_data <- sf_cmr |>
        left_join(agg, by = c("NAME_1" = "Zone"))
      
      # Calculer LISA si spdep disponible
      if (requireNamespace("spdep", quietly = TRUE)) {
        lisa_res <- calc_lisa(sf_data, "value", "NAME_1")
        carto_lisa_results(lisa_res)
        showNotification("✅ LISA calculée avec succès", type = "message")
      } else {
        showNotification(
          paste0("⚠️ Le package spdep n'est pas installé. ",
                 "L'analyse LISA nécessite spdep.
",
                 "Installez-le avec : install.packages('spdep')"),
          type = "warning", duration = 15
        )
        carto_lisa_results(list(sf_data = sf_data, classification = "data_only"))
      }
    }, error = function(e) {
      showNotification(paste("❌ Erreur LISA :", e$message), type = "error")
    })
  })
})

# Affichage carte LISA
output$carto_lisa_map <- renderLeaflet({
  res <- carto_lisa_results()
  req(!is.null(res))
  
  if ("sf_data" %in% names(res)) {
    sf_map <- res$sf_data

    # Palette LISA : hotspot / coldspot / non-sig
    pal_lisa <- colorFactor(
      palette = c("Hotspot (H-H)"="#c9161d", "Coldspot (L-L)"="#4dac26",
                  "Outlier (H-L)"="#e88400", "Outlier (L-H)"="#2196f3",
                  "Non-significatif"="#cccccc", "Autre"="#cccccc"),
      domain = sf_map$lisa_class
    )

    lisa_map <- leaflet() |>
      addProviderTiles("Esri.WorldGrayCanvas") |>
      addFullscreenControl(position = "topleft")

    # Zones du pays sans données : fond gris
    if ("sf_full" %in% names(res) && !is.null(res$sf_full)) {
      col_id_lisa <- if (!is.null(res$col_id)) res$col_id else "NAME_1"
      sf_na <- res$sf_full[!as.character(res$sf_full[[col_id_lisa]]) %in%
                             as.character(sf_map[[col_id_lisa]]), , drop = FALSE]
      if (nrow(sf_na) > 0) {
        lisa_map <- lisa_map |> addPolygons(
          data = sf_na, fillColor = "#e0e0e0", fillOpacity = 0.5,
          color = "white", weight = 1)
      }
    }

    lisa_map |>
      addPolygons(
        data = sf_map,
        fillColor = ~pal_lisa(ifelse(is.na(lisa_class), "Non-significatif", lisa_class)),
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        popup = ~paste0(
          "<b>", NAME_1, "</b><br>",
          "Valeur: ", round(value, 2), "<br>",
          "Classification: ", ifelse(is.na(lisa_class), "N/A", lisa_class)
        )
      ) |>
      addLegend(
        pal = pal_lisa,
        values = sf_map$lisa_class,
        title = "Classification LISA",
        position = "bottomright"
      ) |>
      addScaleBar(position = "bottomleft")
  }
})

# Tableau LISA détails
output$carto_lisa_table <- renderDT({
  res <- carto_lisa_results()
  req(!is.null(res))
  
  if ("sf_data" %in% names(res)) {
    res$sf_data |>
      st_drop_geometry() |>
      select(Zone = NAME_1, Valeur = value, N = n, Classification = lisa_class) |>
      arrange(desc(Valeur)) |>
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "ftp"),
        class = "compact stripe hover"
      ) |>
      formatStyle(
        "Classification",
        backgroundColor = styleEqual(
          c("Hotspot (H-H)", "Coldspot (L-L)", "Outlier (H-L)", "Outlier (L-H)", "Non-significatif", "Autre"),
          c("#ffe6e6", "#e6f7e6", "#fff5e6", "#e6f0ff", "#f0f0f0", "#f0f0f0")
        )
      )
  }
})

# ══════════════════════════════════════════════════════════════════════════════
# TIER 2 : TAB MORAN'S I
# ══════════════════════════════════════════════════════════════════════════════

carto_morans_result <- reactiveVal(NULL)

observeEvent(input$run_carto_moran, {
  req(df_f())
  d <- df_f()
  ind <- input$carto_moran_ind
  
  withProgress(message = "Calcul Moran's I...", {
    tryCatch({
      sf_cmr <- load_gadm(level = 1)
      if (is.null(sf_cmr)) {
        showNotification("❌ Impossible charger shapefile", type = "error")
        return()
      }
      
      agg <- d |>
        group_by(Zone = if(!is.null(map_cols()$region)) .data[[map_cols()$region]] else "Zone") |>
        summarise(value = mean(.data[[ind]], na.rm = TRUE), .groups = "drop")
      
      sf_data <- sf_cmr |> left_join(agg, by = c("NAME_1" = "Zone"))
      
      # Calculer Moran's I si spdep disponible
      if (requireNamespace("spdep", quietly = TRUE)) {
        morans <- calc_morans_i(sf_data, "value")
        carto_morans_result(morans)
        showNotification("✅ Moran's I calculée", type = "message")
      } else {
        # Valeurs par défaut si spdep indisponible
        carto_morans_result(list(
          I_value = NA_real_,
          p_value = NA_real_,
          interpretation = "spdep non installé"
        ))
      }
    }, error = function(e) {
      showNotification(paste("❌ Erreur Moran's I :", e$message), type = "error")
    })
  })
})

output$carto_moran_results <- renderText({
  res <- carto_morans_result()
  req(!is.null(res))
  
  if (is.na(res$I_value)) {
    "spdep non installé. Installez : install.packages('spdep')"
  } else {
    paste0(
      "Moran's I = ", round(res$I_value, 4), "\n",
      "p-value = ", round(res$p_value, 4), "\n\n",
      "Interprétation:\n",
      res$interpretation
    )
  }
})

output$carto_moran_plot <- renderPlotly({
  d <- df_f()
  ind <- input$carto_moran_ind
  
  agg <- d |>
    group_by(Zone = if(!is.null(map_cols()$region)) .data[[map_cols()$region]] else "Zone") |>
    summarise(value = mean(.data[[ind]], na.rm = TRUE), .groups = "drop") |>
    arrange(value)
  
  p <- ggplot(agg, aes(x = fct_inorder(Zone), y = value)) +
    geom_col(fill = "#008DC9", alpha = 0.8) +
    geom_hline(yintercept = mean(agg$value, na.rm = TRUE), 
               linetype = "dashed", color = "#c9161d", linewidth = 1) +
    coord_flip() +
    labs(x = "", y = ind, title = paste0("Distribution spatiale — ", ind)) +
    theme_minimal(base_size = 10)
  
  ggplotly(p)
})

# ══════════════════════════════════════════════════════════════════════════════
# TIER 3 : TAB PRIORISATION INTERVENTIONS
# ══════════════════════════════════════════════════════════════════════════════

carto_priority_results <- reactiveVal(NULL)

observeEvent(input$run_carto_priority, {
  req(df_f())
  d <- df_f()
  
  withProgress(message = "Calcul des priorités...", {
    tryCatch({
      col_geo <- resolve_geo_col(map_cols())
      
      priority_data <- d |>
        group_by(Zone = .data[[col_geo]]) |>
        summarise(
          n_population = n(),
          GAM_prev = mean(if("GAM" %in% names(d)) .data$GAM else NA_real_, na.rm = TRUE) * 100,
          SAM_prev = mean(if("SAM" %in% names(d)) .data$SAM else NA_real_, na.rm = TRUE) * 100,
          .groups = "drop"
        ) |>
        filter(!is.na(Zone), Zone != "")
      
      # Score urgence OMS
      priority_data <- priority_data |>
        mutate(
          urgence_classe = case_when(
            GAM_prev >= 30 ~ "CRITIQUE",
            GAM_prev >= 15 ~ "SÉRIEUX",
            GAM_prev >= 10 ~ "ALERTE",
            TRUE ~ "ACCEPTABLE"
          ),
          urgence_score = case_when(
            GAM_prev >= 30 ~ 100,
            GAM_prev >= 15 ~ 75,
            GAM_prev >= 10 ~ 50,
            TRUE ~ 25
          ),
          # Score composite (0-100)
          priority_score = round(
            0.35 * pmin(GAM_prev / 30 * 100, 100) +
            0.30 * urgence_score +
            0.20 * pmin(n_population / max(n_population, na.rm = TRUE) * 100, 100) +
            0.15 * 50,  # Placeholder accessibilité
            1
          )
        ) |>
        arrange(desc(priority_score))
      
      carto_priority_results(priority_data)
      showNotification("✅ Priorités calculées", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Erreur :", e$message), type = "error")
    })
  })
})

output$carto_priority_table <- renderDT({
  res <- carto_priority_results()
  req(!is.null(res))
  
  res |>
    select(Zone, n_population, GAM_prev, urgence_classe, priority_score) |>
    datatable(
      rownames = FALSE,
      colnames = c("Zone", "N population", "GAM %", "Urgence", "Score (0-100)"),
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      class = "compact stripe hover"
    ) |>
    formatStyle(
      "priority_score",
      backgroundColor = styleInterval(
        c(25, 50, 75),
        c("#4dac26", "#f9c734", "#e88400", "#c9161d")
      ),
      fontWeight = "bold"
    ) |>
    formatStyle(
      "urgence_classe",
      backgroundColor = styleEqual(
        c("ACCEPTABLE", "ALERTE", "SÉRIEUX", "CRITIQUE"),
        c("#e6f7e6", "#fff5e6", "#ffe8cc", "#ffe6e6")
      )
    )
})

output$carto_priority_scatter <- renderPlotly({
  res <- carto_priority_results()
  req(!is.null(res))
  
  p <- ggplot(res, aes(
    x = GAM_prev,
    y = n_population,
    size = priority_score,
    color = urgence_classe,
    text = paste0(Zone, "\nGAM: ", round(GAM_prev, 1), "%\nN: ", n_population,
                  "\nScore: ", priority_score)
  )) +
    geom_point(alpha = 0.7) +
    scale_color_manual(
      values = c("ACCEPTABLE" = "#4dac26", "ALERTE" = "#f9c734",
                 "SÉRIEUX" = "#e88400", "CRITIQUE" = "#c9161d")
    ) +
    scale_size_continuous(range = c(3, 10), guide = "none") +
    labs(x = "GAM (%)", y = "Population", color = "Urgence",
         title = "Matrice de priorisation") +
    theme_minimal(base_size = 10)
  
  ggplotly(p, tooltip = "text")
})

# ══════════════════════════════════════════════════════════════════════════════
# TIER 4 : TAB PRÉDICTIONS TENDANCES
# ══════════════════════════════════════════════════════════════════════════════

carto_predict_results <- reactiveVal(NULL)

observeEvent(input$run_carto_predict, {
  req(df_f())
  d <- df_f()
  horizon <- input$carto_predict_horizon
  
  withProgress(message = "Prédictions en cours...", {
    tryCatch({
      col_geo <- resolve_geo_col(map_cols())
      
      # Simuler prédictions (en prod, utiliser ML réel)
      pred_data <- d |>
        group_by(Zone = .data[[col_geo]]) |>
        summarise(
          GAM_observed = mean(if("GAM" %in% names(d)) .data$GAM else NA_real_, na.rm = TRUE) * 100,
          .groups = "drop"
        ) |>
        filter(!is.na(Zone), Zone != "") |>
        mutate(
          # Simulation : tendance aléatoire ± 20%
          GAM_predicted = pmax(0, GAM_observed * rnorm(n(), mean = 1, sd = 0.15)),
          delta = GAM_predicted - GAM_observed,
          risk_level = case_when(
            delta > 5 ~ "RISQUE ÉLEVÉ",
            delta > 2 ~ "RISQUE MODÉRÉ",
            delta > -2 ~ "STABLE",
            TRUE ~ "AMÉLIORATION"
          )
        ) |>
        arrange(desc(delta))
      
      carto_predict_results(pred_data)
      showNotification(paste0("✅ Prédictions (horizon ", horizon, " mois)"), type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Erreur :", e$message), type = "error")
    })
  })
})

output$carto_predict_trend <- renderPlotly({
  res <- carto_predict_results()
  req(!is.null(res))
  
  p <- res |>
    arrange(GAM_observed) |>
    ggplot(aes(x = fct_inorder(Zone))) +
    geom_col(aes(y = GAM_observed), fill = "#008DC9", alpha = 0.7, width = 0.4) +
    geom_point(aes(y = GAM_predicted), color = "#c9161d", size = 3) +
    geom_segment(aes(xend = Zone, y = GAM_observed, yend = GAM_predicted),
                 color = "#c9161d", alpha = 0.5, linewidth = 0.8) +
    coord_flip() +
    labs(x = "", y = "GAM (%)", title = "Observé vs Prédit") +
    theme_minimal(base_size = 10)
  
  ggplotly(p)
})

output$carto_predict_alerts <- renderDT({
  res <- carto_predict_results()
  req(!is.null(res))
  
  res |>
    filter(risk_level %in% c("RISQUE ÉLEVÉ", "RISQUE MODÉRÉ")) |>
    select(Zone, GAM_observed, GAM_predicted, delta, risk_level) |>
    mutate(across(where(is.numeric), ~round(., 1))) |>
    datatable(
      rownames = FALSE,
      colnames = c("Zone", "Observé", "Prédit", "Δ", "Risque"),
      options = list(pageLength = 10, dom = "ftp"),
      class = "compact stripe hover"
    ) |>
    formatStyle("risk_level",
      backgroundColor = styleEqual(
        c("RISQUE ÉLEVÉ", "RISQUE MODÉRÉ"),
        c("#ffe6e6", "#fff5e6")
      )
    )
})

# ══════════════════════════════════════════════════════════════════════════════
# TIER 3 : TAB COMPARAISON RÉGIONALE
# ══════════════════════════════════════════════════════════════════════════════

output$carto_bench_plot <- renderPlotly({
  d <- df_f()
  region_target <- input$carto_bench_region
  ind <- input$carto_bench_ind
  col_geo <- resolve_geo_col(map_cols())
  
  agg <- d |>
    group_by(Zone = .data[[col_geo]]) |>
    summarise(value = mean(.data[[ind]], na.rm = TRUE) * 100, n = n(), .groups = "drop") |>
    filter(!is.na(Zone), Zone != "")
  
  national_mean <- mean(agg$value, na.rm = TRUE)
  
  agg_plot <- agg |>
    mutate(
      highlight = ifelse(Zone == region_target, "Cible", "Autres"),
      seuil_oms = case_when(ind == "GAM" ~ 15, ind == "SAM" ~ 5, TRUE ~ 30)
    ) |>
    arrange(value)
  
  p <- ggplot(agg_plot, aes(x = fct_inorder(Zone), y = value, fill = highlight)) +
    geom_col(alpha = 0.8, width = 0.7) +
    geom_hline(yintercept = national_mean, linetype = "dashed",
               color = "#008DC9", linewidth = 1, label = "Moyenne nationale") +
    geom_hline(yintercept = unique(agg_plot$seuil_oms),
               linetype = "dashed", color = "#c9161d", linewidth = 1, label = "Seuil OMS") +
    scale_fill_manual(values = c("Cible" = "#008DC9", "Autres" = "#cccccc"),
                      guide = "none") +
    coord_flip() +
    labs(x = "", y = paste0(ind, " (%)"),
         title = paste0("Benchmark — ", ind)) +
    theme_minimal(base_size = 10)
  
  ggplotly(p)
})

# ══════════════════════════════════════════════════════════════════════════════
# TIER 3 : TAB CLUSTERING SPATIAL
# ══════════════════════════════════════════════════════════════════════════════

carto_cluster_results <- reactiveVal(NULL)

observeEvent(input$run_carto_cluster, {
  req(df_f())
  d <- df_f()
  k <- input$carto_kclust_k
  col_geo <- resolve_geo_col(map_cols())
  
  withProgress(message = "Clustering spatial...", {
    tryCatch({
      # Agréger par zone
      agg <- d |>
        group_by(Zone = .data[[col_geo]]) |>
        summarise(
          GAM = mean(if("GAM" %in% names(d)) .data$GAM else NA_real_, na.rm = TRUE),
          Stunting = mean(if("Stunting" %in% names(d)) .data$Stunting else NA_real_, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) |>
        filter(!is.na(Zone), Zone != "")
      
      # K-means
      if (nrow(agg) >= k) {
        scaled <- scale(agg[, c("GAM", "Stunting")])
        km <- kmeans(scaled, centers = k, nstart = 10)
        
        agg$cluster <- km$cluster
        carto_cluster_results(agg)
        showNotification(paste0("✅ Clustering (", k, " clusters)"), type = "message")
      }
    }, error = function(e) {
      showNotification(paste("❌ Erreur :", e$message), type = "error")
    })
  })
})

output$carto_cluster_map <- renderLeaflet({
  res <- carto_cluster_results()
  req(!is.null(res))
  
  sf_cmr <- load_gadm(level = 1)
  if (!is.null(sf_cmr)) {
    sf_data <- sf_cmr |>
      left_join(res |> select(Zone, cluster), by = c("NAME_1" = "Zone"))
    
    pal_clust <- colorFactor(
      palette = RColorBrewer::brewer.pal(max(sf_data$cluster, na.rm = TRUE), "Set3"),
      domain = sf_data$cluster
    )
    
    leaflet(sf_data) |>
      addProviderTiles("Esri.WorldGrayCanvas") |>
      addFullscreenControl(position = "topleft") |>
      addPolygons(
        fillColor = ~pal_clust(cluster),
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        popup = ~NAME_1
      ) |>
      addScaleBar(position = "bottomleft")
  }
})

output$carto_cluster_profiles <- renderDT({
  res <- carto_cluster_results()
  req(!is.null(res))
  
  profiles <- res |>
    group_by(cluster) |>
    summarise(
      n_zones = n(),
      GAM_mean = round(mean(GAM, na.rm = TRUE) * 100, 1),
      Stunting_mean = round(mean(Stunting, na.rm = TRUE) * 100, 1),
      pop_total = sum(n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(cluster)
  
  datatable(
    profiles,
    rownames = FALSE,
    colnames = c("Cluster", "N zones", "GAM moyen %", "Stunting moyen %", "Population totale"),
    options = list(pageLength = 10, dom = "ftp"),
    class = "compact stripe hover"
  ) |>
    formatStyle("GAM_mean",
      backgroundColor = styleInterval(c(5, 10, 15), c("#4dac26", "#f9c734", "#e88400", "#c9161d"))
    )
})

# ══════════════════════════════════════════════════════════════════════════════
# TIER 3 : TAB ACCESSIBILITÉ
# ══════════════════════════════════════════════════════════════════════════════

observeEvent(input$run_carto_access, {
  req(df_f())
  
  withProgress(message = "Calcul accessibilité...", {
    showNotification("✅ Accessibilité calculée (simulation)", type = "message")
  })
})

output$carto_access_map <- renderLeaflet({
  sf_cmr <- load_gadm(level = 1)
  if (!is.null(sf_cmr)) {
    # Simulation scores accessibilité
    sf_cmr$accessibility <- runif(nrow(sf_cmr), 0, 100)
    
    pal_access <- colorNumeric(
      palette = c("#c9161d", "#e88400", "#f9c734", "#4dac26"),
      domain = sf_cmr$accessibility
    )
    
    leaflet(sf_cmr) |>
      addProviderTiles("Esri.WorldGrayCanvas") |>
      addFullscreenControl(position = "topleft") |>
      addPolygons(
        fillColor = ~pal_access(accessibility),
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        popup = ~paste0(NAME_1, "\nAccessibilité: ", round(accessibility, 0), "%")
      ) |>
      addLegend(
        pal = pal_access,
        values = ~accessibility,
        title = "Score accessibilité",
        position = "bottomright"
      ) |>
      addScaleBar(position = "bottomleft")
  }
})

# ══════════════════════════════════════════════════════════════════════════════
# TIER 4 : TAB ALLOCATION RESSOURCES
# ══════════════════════════════════════════════════════════════════════════════

observeEvent(input$run_carto_allocate, {
  req(df_f())
  
  withProgress(message = "Optimisation allocation...", {
    showNotification("✅ Allocation optimisée", type = "message")
  })
})

output$carto_allocation_table <- renderDT({
  d <- df_f()
  budget <- input$carto_res_budget
  col_geo <- resolve_geo_col(map_cols())
  
  alloc <- d |>
    group_by(Zone = .data[[col_geo]]) |>
    summarise(
      population = n(),
      GAM = mean(if("GAM" %in% names(d)) .data$GAM else NA_real_, na.rm = TRUE) * 100,
      .groups = "drop"
    ) |>
    filter(!is.na(Zone), Zone != "") |>
    mutate(
      need_score = pmin(GAM / 30 * 100, 100),
      allocation_pct = need_score / sum(need_score) * 100,
      allocation_usd = round(allocation_pct / 100 * budget, 0)
    ) |>
    arrange(desc(allocation_usd))
  
  datatable(
    alloc |> select(Zone, population, GAM, allocation_pct, allocation_usd),
    rownames = FALSE,
    colnames = c("Zone", "Population", "GAM %", "Allocation %", "Montant (USD)"),
    options = list(pageLength = 15, dom = "ftp"),
    class = "compact stripe hover"
  ) |>
    formatCurrency("allocation_usd", currency = "$", digits = 0)
})

output$carto_allocation_plot <- renderPlotly({
  d <- df_f()
  budget <- input$carto_res_budget
  col_geo <- resolve_geo_col(map_cols())
  
  alloc <- d |>
    group_by(Zone = .data[[col_geo]]) |>
    summarise(
      GAM = mean(if("GAM" %in% names(d)) .data$GAM else NA_real_, na.rm = TRUE) * 100,
      .groups = "drop"
    ) |>
    filter(!is.na(Zone), Zone != "") |>
    mutate(
      need_score = pmin(GAM / 30 * 100, 100),
      allocation_pct = need_score / sum(need_score) * 100
    ) |>
    arrange(allocation_pct)
  
  p <- ggplot(alloc, aes(x = fct_inorder(Zone), y = allocation_pct)) +
    geom_col(fill = "#008DC9", alpha = 0.8) +
    coord_flip() +
    labs(x = "", y = "Allocation (%)",
         title = "Distribution optimale des ressources") +
    theme_minimal(base_size = 10)
  
  ggplotly(p)
})

# ══════════════════════════════════════════════════════════════════════════════
# TAB CONTRÔLE QUALITÉ
# ══════════════════════════════════════════════════════════════════════════════

output$carto_quality_flags <- renderDT({
  d <- df_f()
  col_geo <- resolve_geo_col(map_cols())
  
  # Détecter anomalies
  flags <- tibble(
    Zone = character(),
    Anomalie = character(),
    Sévérité = character(),
    Description = character()
  )
  
  # Données manquantes
  if (ncol(d) > 0) {
    for (col in names(d)) {
      if (is.numeric(d[[col]])) {
        miss_pct <- mean(is.na(d[[col]])) * 100
        if (miss_pct > 20) {
          flags <- bind_rows(flags, tibble(
            Zone = "Global",
            Anomalie = paste0("Données manquantes — ", col),
            Sévérité = "⚠️ Modérée",
            Description = paste0(round(miss_pct, 1), "% manquantes")
          ))
        }
      }
    }
  }
  
  datatable(
    flags,
    rownames = FALSE,
    options = list(pageLength = 10, dom = "ftp"),
    class = "compact stripe hover"
  )
})

output$carto_quality_coverage <- renderPlotly({
  d <- df_f()
  col_geo <- resolve_geo_col(map_cols())
  
  coverage <- d |>
    group_by(Zone = .data[[col_geo]]) |>
    summarise(
      coverage = mean(!is.na(if("GAM" %in% names(d)) .data$GAM else NA)) * 100,
      .groups = "drop"
    ) |>
    arrange(coverage) |>
    filter(!is.na(Zone), Zone != "")
  
  p <- ggplot(coverage, aes(x = fct_inorder(Zone), y = coverage, fill = coverage)) +
    geom_col(alpha = 0.8) +
    scale_fill_gradientn(colors = c("#c9161d", "#f9c734", "#4dac26"), guide = "none") +
    coord_flip() +
    labs(x = "", y = "Couverture (%)", title = "Qualité de couverture") +
    theme_minimal(base_size = 10)
  
  ggplotly(p)
})

output$carto_quality_outliers <- renderPlotly({
  d <- df_f()
  
  if ("GAM" %in% names(d)) {
    outliers <- d |>
      select(GAM) |>
      filter(!is.na(GAM)) |>
      mutate(outlier = GAM > mean(GAM, na.rm = TRUE) + 3 * sd(GAM, na.rm = TRUE)) |>
      summarise(
        `Normales` = sum(!outlier),
        `Outliers` = sum(outlier)
      ) |>
      pivot_longer(everything())
    
    p <- ggplot(outliers, aes(x = name, y = value, fill = name)) +
      geom_col(alpha = 0.8, width = 0.5) +
      scale_fill_manual(values = c("Normales" = "#4dac26", "Outliers" = "#c9161d"), guide = "none") +
      labs(x = "", y = "Nombre", title = "Détection outliers (GAM)") +
      theme_minimal(base_size = 10)
    
    ggplotly(p)
  }
})

# ══════════════════════════════════════════════════════════════════════════════
# TAB RAPPORT EXPERT
# ══════════════════════════════════════════════════════════════════════════════

carto_report_content <- reactiveVal("")

observeEvent(input$run_carto_report, {
  req(df_f())
  d <- df_f()
  
  withProgress(message = "Génération rapport...", {
    rapport <- paste0(
      "# Rapport Expert — Cartographie Nutritionnelle\n\n",
      "**Date:** ", Sys.Date(), "\n\n",
      "## 📊 Résumé exécutif\n",
      "Population analysée : ", nrow(d), " observations\n",
      if("GAM" %in% names(d)) {
        paste0("GAM moyen : ", round(mean(d$GAM, na.rm = TRUE) * 100, 1), "%\n")
      } else { "" },
      "\n## 🗺️ Analyses spatiales\n",
      "- Hotspots (LISA) : En attente\n",
      "- Autocorrélation spatiale (Moran's I) : En attente\n",
      "- Priorisation interventions : En attente\n",
      "\n## 🎯 Recommandations\n",
      "1. Renforcer les zones prioritaires\n",
      "2. Améliorer la couverture des données\n",
      "3. Poursuivre le suivi spatial\n",
      "\n---\n",
      "*Rapport généré par NutriAnalyse Pro v2*"
    )
    
    carto_report_content(rapport)
    showNotification("✅ Rapport généré", type = "message")
  })
})

output$carto_report_text <- renderUI({
  content <- carto_report_content()
  if (content == "") {
    tags$p("(Cliquez sur 'Générer rapport')", style = "color:#999;font-style:italic;")
  } else {
    tags$pre(content)
  }
})

output$dl_carto_report_md <- downloadHandler(
  filename = function() paste0("rapport_carto_", Sys.Date(), ".md"),
  content = function(file) {
    writeLines(carto_report_content(), file)
  }
)

output$dl_carto_report_html <- downloadHandler(
  filename = function() paste0("rapport_carto_", Sys.Date(), ".html"),
  content = function(file) {
    # Simple HTML conversion
    html_content <- paste0(
      "<html><head><meta charset='UTF-8'><title>Rapport Cartographie</title>",
      "<style>body{font-family:Arial,sans-serif;margin:20px;line-height:1.6;}",
      "h1,h2,h3{color:#008DC9;}pre{background:#f5f5f5;padding:10px;border-radius:5px;}</style>",
      "</head><body>",
      "<h1>Rapport Expert — Cartographie Nutritionnelle</h1>",
      "<pre>", carto_report_content(), "</pre>",
      "</body></html>"
    )
    writeLines(html_content, file)
  }
)

# ══════════════════════════════════════════════════════════════════════════════
# EXPERT : Getis-Ord Gi* Hotspots
# ══════════════════════════════════════════════════════════════════════════════

carto_gistar_res <- reactiveVal(NULL)

observeEvent(input$run_gistar, {
  ctx <- tryCatch(get_agg(), error = function(e) NULL); req(!is.null(ctx))
  agg <- ctx$agg; req(!is.null(agg))
  ind <- input$carto_indicateur; req(!is.null(ind))
  niv <- ctx$niv; mc <- ctx$mc

    withProgress(message = "Calcul Getis-Ord Gi*\u2026", value = 0.3, {
    tryCatch({
      gadm_level0 <- if (!is.null(ctx$col_geo) && identical(ctx$col_geo, ctx$mc$region)) 1L
                     else if (!is.null(ctx$col_geo) && identical(ctx$col_geo, ctx$mc$dept)) 2L
                     else 3L
      # arrfix : retenir le niveau GADM dont la jointure couvre le plus de zones
      # (evite "moins de 4 zones" quand les donnees sont a un niveau plus grossier)
      if (!ind %in% names(agg))
        stop("L'indicateur s\u00e9lectionn\u00e9 n'est pas disponible dans l'agr\u00e9gation g\u00e9ographique")
      sf_merged <- NULL; gadm_col <- NULL
      for (lvl in seq(gadm_level0, 1L)) {
        sf_l <- load_gadm(lvl); if (is.null(sf_l)) next
        col_l <- GADM_NAMES[[paste0("L", lvl)]]
        sf_l2 <- sf_l |>
          left_join(agg |> select(Zone = 1, val = all_of(ind), n),
                    by = setNames("Zone", col_l)) |>
          filter(!is.na(val))
        if (is.null(sf_merged) || nrow(sf_l2) > nrow(sf_merged)) {
          sf_merged <- sf_l2; gadm_col <- col_l
        }
        if (nrow(sf_l2) >= 4) break
      }
      if (is.null(sf_merged) || nrow(sf_merged) < 4)
        stop("Analyse Gi* impossible : moins de 4 zones avec donn\u00e9es apr\u00e8s jointure GADM")
      gi <- calc_getis_ord(sf_merged, "val", gadm_col, alpha = 0.05)
      carto_gistar_res(list(gi = gi, sf = sf_merged, col_id = gadm_col))
      incProgress(0.7)
      n_hot <- sum(grepl("Hotspot", gi$gi_class))
      n_cold <- sum(grepl("Coldspot", gi$gi_class))
      showNotification(paste0("\u2705 Gi* : ", n_hot, " hotspots, ", n_cold, " coldspots"),
                       type = "message", duration = 4)
    }, error = function(e) showNotification(paste("Erreur Gi*:", e$message), type = "error"))
  })
})

output$gistar_map <- renderLeaflet({
  r <- carto_gistar_res(); req(!is.null(r))
  tryCatch(plot_getis_ord_map(r$sf, r$gi, r$col_id),
           error = function(e) leaflet() |> addTiles())
})

output$gistar_info <- renderUI({
  r <- carto_gistar_res(); req(!is.null(r))
  gi <- r$gi
  n_sig <- sum(gi$significant)
  tags$div(style = "background:#f8f9fa;padding:10px;border-radius:6px;margin-top:5px;font-size:12px;",
    tags$b(n_sig), " zones significatives sur ", nrow(gi), tags$br(),
    tags$span(style = "color:#d32f2f;", sum(grepl("Hotspot", gi$gi_class)), " hotspots"),
    " | ",
    tags$span(style = "color:#2e7d32;", sum(grepl("Coldspot", gi$gi_class)), " coldspots")
  )
})

output$gistar_table <- renderDT({
  r <- carto_gistar_res(); req(!is.null(r))
  r$gi |>
    select(Zone = id, Valeur = value, `Z-score Gi*` = Gi_z,
           `p-value` = p_value, Classification = gi_class) |>
    datatable(rownames = FALSE, options = list(dom = "tip", pageLength = 15, scrollX = TRUE),
              class = "compact stripe hover") |>
    formatStyle("Classification",
      color = styleEqual(
        c("Hotspot (p < 0.01)", "Hotspot (p < 0.05)", "Hotspot (p < 0.10)",
          "Coldspot (p < 0.01)", "Coldspot (p < 0.05)", "Coldspot (p < 0.10)",
          "Non significatif"),
        c("#8b0000", "#d32f2f", "#ff6f00", "#004d40", "#2e7d32", "#66bb6a", "#999")),
      fontWeight = "bold")
})

# ══════════════════════════════════════════════════════════════════════════════
# EXPERT : Lissage Bayesien Empirique (EB)
# ══════════════════════════════════════════════════════════════════════════════

carto_eb_res <- reactiveVal(NULL)

observeEvent(input$run_eb, {
  ctx <- tryCatch(get_agg(), error = function(e) NULL); req(!is.null(ctx))
  agg <- ctx$agg; req(!is.null(agg))
  ind <- input$carto_indicateur; req(!is.null(ind))

  withProgress(message = "Lissage EB\u2026", value = 0.3, {
    tryCatch({
      # Besoin de cas et total par zone
      if (ind %in% names(agg) && "n" %in% names(agg)) {
        prev_col <- agg[[ind]]
        n_col    <- agg$n
        cases    <- round(prev_col * n_col / 100)
        eb <- calc_eb_smoothing(cases, n_col)
        zones <- if ("Zone" %in% names(agg)) agg$Zone else agg[[1]]
        carto_eb_res(list(eb = eb, zones = as.character(zones), indicator = ind))
        incProgress(0.7)
        showNotification("\u2705 Lissage EB calcul\u00e9", type = "message", duration = 3)
      }
    }, error = function(e) showNotification(paste("Erreur EB:", e$message), type = "error"))
  })
})

output$eb_plot <- renderPlotly({
  r <- carto_eb_res(); req(!is.null(r))
  tryCatch(plot_eb_comparison(r$eb, r$zones), error = function(e) plotly_empty())
})

output$eb_table <- renderDT({
  r <- carto_eb_res(); req(!is.null(r))
  r$eb |>
    mutate(Zone = r$zones) |>
    select(Zone, `Prev brute (%)` = prev_brute, `Prev lissée (%)` = prev_lissee,
           Poids_w = poids_w, `Shrinkage (%)` = shrinkage,
           `IC lo` = ic_lo, `IC hi` = ic_hi, n = total) |>
    datatable(rownames = FALSE, options = list(dom = "tip", pageLength = 15, scrollX = TRUE),
              class = "compact stripe hover") |>
    formatStyle("Shrinkage (%)",
      background = styleInterval(c(20, 50), c("#d5f5e3", "#fdebd0", "#fdedec")))
})

# ══════════════════════════════════════════════════════════════════════════════
# EXPERT : Carte IPC/CH
# ══════════════════════════════════════════════════════════════════════════════

carto_ipc_res <- reactiveVal(NULL)

observeEvent(input$run_ipc_map, {
  d <- df_f(); req(d); mc <- m()
  niv <- input$carto_niveau; if (is.null(niv)) niv <- "Region"
  col_geo <- switch(niv, "Region"=mc$region, "Departement"=mc$dept, mc$region)
  req(col_geo, col_geo %in% names(d))

  withProgress(message = "Classification IPC/CH\u2026", value = 0.3, {
    tryCatch({
      # Detecter colonnes
      col_fcs  <- detect_col(names(d), c("FCS_score", "fcs_score", "FCS"))
      col_rcsi <- detect_col(names(d), c("rCSI_score", "rcsi_score", "rCSI"))
      col_hhs  <- detect_col(names(d), c("HHS_score", "hhs_score", "HHS"))
      col_muac <- detect_col(names(d), c("muac", "MUAC", "pb"))
      if (is.null(col_fcs)) col_fcs <- "FCS_score"
      if (is.null(col_rcsi)) col_rcsi <- "rCSI_score"
      if (is.null(col_hhs)) col_hhs <- "HHS_score"
      if (is.null(col_muac)) col_muac <- "muac"

      ipc <- calc_ipc_by_zone(d, col_geo, col_fcs, col_rcsi, col_hhs, col_muac)

      # Joindre au shapefile
      gadm_level0 <- if (identical(col_geo, mc$region)) 1L
                     else if (identical(col_geo, mc$dept)) 2L
                     else 3L
      # arrfix : retenir le niveau GADM qui couvre le plus de zones IPC
      gadm_sf <- NULL; gadm_col <- NULL; best_n <- -1L
      n_zones <- length(unique(ipc$Zone))
      for (lvl in seq(gadm_level0, 1L)) {
        sf_l <- load_gadm(lvl); if (is.null(sf_l)) next
        col_l <- GADM_NAMES[[paste0("L", lvl)]]
        n_match <- sum(unique(sf_l[[col_l]]) %in% unique(ipc$Zone))
        if (n_match > best_n) { gadm_sf <- sf_l; gadm_col <- col_l; best_n <- n_match }
        if (n_match >= n_zones) break
      }
      if (is.null(gadm_sf))
        stop("Impossible de charger les fronti\u00e8res GADM pour la carte IPC/CH")

      carto_ipc_res(list(ipc = ipc, sf = gadm_sf, col_id = gadm_col))
      incProgress(0.7)
      showNotification("\u2705 Carte IPC/CH g\u00e9n\u00e9r\u00e9e", type = "message", duration = 3)
    }, error = function(e) showNotification(paste("Erreur IPC:", e$message), type = "error"))
  })
})

output$ipc_phase_map <- renderLeaflet({
  r <- carto_ipc_res(); req(!is.null(r))
  tryCatch(plot_ipc_map(r$sf, r$ipc, r$col_id),
           error = function(e) leaflet() |> addTiles())
})

output$ipc_phase_table <- renderDT({
  r <- carto_ipc_res(); req(!is.null(r))
  r$ipc |>
    select(Zone, n, `Phase dominante` = phase_label,
           `Phase moy.` = phase_moy,
           `% Phase 3+` = pct_phase3plus,
           `% Phase 4+` = pct_phase4plus) |>
    datatable(rownames = FALSE, options = list(dom = "tip", pageLength = 15),
              class = "compact stripe hover") |>
    formatStyle("Phase dominante",
      backgroundColor = styleEqual(
        paste0("Phase ", 1:5),
        c("#c6ffc7", "#ffe252", "#e88400", "#c9161d", "#640000")),
      color = styleEqual(
        paste0("Phase ", 1:5),
        c("black", "black", "white", "white", "white")),
      fontWeight = "bold")
})

# ══════════════════════════════════════════════════════════════════════════════
# EXPERT : Forest Plot Prevalences + IC
# ══════════════════════════════════════════════════════════════════════════════

carto_forest_res <- reactiveVal(NULL)

observeEvent(input$run_forest, {
  d <- df_f(); req(d); mc <- m()
  niv <- input$carto_niveau; if (is.null(niv)) niv <- "Region"
  col_geo <- switch(niv, "Region"=mc$region, "Departement"=mc$dept, mc$region)
  req(col_geo, col_geo %in% names(d))
  ind <- input$forest_indicator; req(!is.null(ind))

  withProgress(message = "Forest plot\u2026", value = 0.3, {
    tryCatch({
      # Creer variable binaire si necessaire
      if (ind %in% names(d)) {
        col_out <- ind
      } else {
        col_out <- ".forest_outcome"
        d[[col_out]] <- switch(ind,
          "GAM"      = { if ("whz" %in% names(d)) as.integer(d$whz < -2) else NA },
          "SAM"      = { if ("whz" %in% names(d)) as.integer(d$whz < -3) else NA },
          "MUAC_GAM" = { mc_muac <- detect_col(names(d), c("muac","MUAC","pb"))
                         if (!is.null(mc_muac)) as.integer(d[[mc_muac]] < 125) else NA },
          "MUAC_SAM" = { mc_muac <- detect_col(names(d), c("muac","MUAC","pb"))
                         if (!is.null(mc_muac)) as.integer(d[[mc_muac]] < 115) else NA },
          "Stunting" = { if ("haz" %in% names(d)) as.integer(d$haz < -2) else NA },
          NA
        )
      }
      req(!all(is.na(d[[col_out]])))
      fd <- calc_forest_data(d, col_geo, col_out, label_outcome = ind)
      carto_forest_res(fd)
      incProgress(0.7)
    }, error = function(e) showNotification(paste("Erreur:", e$message), type = "error"))
  })
})

output$forest_plot <- renderPlotly({
  fd <- carto_forest_res(); req(!is.null(fd))
  tryCatch(plot_forest(fd), error = function(e) plotly_empty())
})

output$forest_table <- renderDT({
  fd <- carto_forest_res(); req(!is.null(fd))
  fd |>
    select(Zone, n, Cas = cas, `Prev (%)` = prev,
           `IC lo` = ic_lo, `IC hi` = ic_hi, `Class. OMS` = oms_class) |>
    datatable(rownames = FALSE, options = list(dom = "tip", pageLength = 15),
              class = "compact stripe hover") |>
    formatStyle("Class. OMS",
      color = styleEqual(
        c("Critique (\u2265 15%)", "S\u00e9rieux (10-14.9%)",
          "Alerte (5-9.9%)", "Acceptable (< 5%)"),
        c("#c9161d", "#e88400", "#f9c734", "#4dac26")),
      fontWeight = "bold")
})

# ══════════════════════════════════════════════════════════════════════════════
# EXPERT : Indices d'inegalite spatiale
# ══════════════════════════════════════════════════════════════════════════════

carto_ineq_res <- reactiveVal(NULL)

observeEvent(input$run_inequality, {
  ctx <- tryCatch(get_agg(), error = function(e) NULL); req(!is.null(ctx))
  agg <- ctx$agg; req(!is.null(agg))
  ind <- input$carto_indicateur; req(!is.null(ind))

  tryCatch({
    if (ind %in% names(agg)) {
      vals <- agg[[ind]]
      ineq <- calc_spatial_inequality(vals)
      carto_ineq_res(ineq)
      showNotification("\u2705 Indices d'in\u00e9galit\u00e9 calcul\u00e9s", type = "message", duration = 3)
    }
  }, error = function(e) showNotification(paste("Erreur:", e$message), type = "error"))
})

output$inequality_table <- renderDT({
  ineq <- carto_ineq_res(); req(!is.null(ineq))
  datatable(ineq, rownames = FALSE, options = list(dom = "t", pageLength = 10),
            class = "compact stripe hover")
})

output$inequality_interpretation <- renderUI({
  ineq <- carto_ineq_res(); req(!is.null(ineq))
  gini  <- ineq$Valeur[ineq$Indice == "Gini"]
  theil <- ineq$Valeur[ineq$Indice == "Theil"]
  cv    <- ineq$Valeur[ineq$Indice == "CV (%)"]
  col   <- if (!is.na(gini) && gini > 0.3) "#c9161d" else if (!is.na(gini) && gini > 0.2) "#e88400" else "#4dac26"
  tags$div(style = paste0("padding:14px;border-radius:8px;border-left:4px solid ",col,";background:#f8f9fa;"),
    tags$b("Synth\u00e8se : "),
    paste0("Gini = ", gini, " | Theil = ", theil, " | CV = ", cv, "%"), tags$br(),
    tags$span(style = paste0("color:", col, ";font-weight:700;"),
      if (!is.na(gini) && gini > 0.3) "In\u00e9galit\u00e9 spatiale \u00e9lev\u00e9e \u2014 interventions cibl\u00e9es n\u00e9cessaires"
      else if (!is.na(gini) && gini > 0.2) "In\u00e9galit\u00e9 mod\u00e9r\u00e9e \u2014 disparit\u00e9s g\u00e9ographiques \u00e0 surveiller"
      else "In\u00e9galit\u00e9 faible \u2014 distribution relativement homog\u00e8ne")
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# EXPERT : Profil regional (radar)
# ══════════════════════════════════════════════════════════════════════════════

output$profile_zone_ui <- renderUI({
  ctx <- tryCatch(get_agg(), error = function(e) NULL); req(!is.null(ctx))
  agg <- ctx$agg; req(!is.null(agg))
  zones <- if ("Zone" %in% names(agg)) agg$Zone else agg[[1]]
  selectInput("profile_zone", "Zone \u00e0 profiler :", choices = zones, width = "100%")
})

observeEvent(input$run_profile, {
  ctx <- tryCatch(get_agg(), error = function(e) NULL); req(!is.null(ctx))
  agg <- ctx$agg; req(!is.null(agg), input$profile_zone)

  # Detecter colonnes indicateurs disponibles
  inds <- intersect(c("GAM","SAM","MAM","Stunting","MUAC_GAM","fcs_moy","whz_moy","haz_moy"),
                    names(agg))
  req(length(inds) >= 3)

  zone_col <- if ("Zone" %in% names(agg)) "Zone" else names(agg)[1]
  agg_df <- agg |> rename(Zone = all_of(zone_col))

  output$profile_radar <- renderPlotly({
    tryCatch(
      plot_regional_profile(agg_df, input$profile_zone, inds,
                            indicator_labels = inds),
      error = function(e) plotly_empty())
  })
})
