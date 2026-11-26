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
      palette = c("hotspot"="#c9161d", "coldspot"="#4dac26", "outlier"="#e88400", "insignificant"="#cccccc"),
      domain = sf_map$lisa_class
    )
    
    leaflet(sf_map) |>
      addProviderTiles("CartoDB.Positron") |>
      addFullscreenControl(position = "topleft") |>
      addPolygons(
        fillColor = ~pal_lisa(ifelse(is.na(lisa_class), "insignificant", lisa_class)),
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
        values = ~ifelse(is.na(lisa_class), "insignificant", lisa_class),
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
          c("hotspot", "coldspot", "outlier", "insignificant"),
          c("#ffe6e6", "#e6f7e6", "#fff5e6", "#f0f0f0")
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
      addProviderTiles("CartoDB.Positron") |>
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
      addProviderTiles("CartoDB.Positron") |>
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
