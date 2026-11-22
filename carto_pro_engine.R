
# carto_pro_engine.R - Moteur cartographique PROFESSIONNEL
# Qualite superieure a QGIS/ArcGIS - Bivariate - Multi-indicateurs

library(sf)
library(leaflet)
tryCatch(library(leaflet.extras), error = function(e) message("⚠️ leaflet.extras non disponible"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(classInt)
library(htmltools)

# -- PALETTES PROFESSIONNELLES
PAL_OMS <- list(acceptable="#2d8e47", precaire="#a8c834", alerte="#f5c242",
                serieux="#e8822a", critique="#c9161d", urgence="#640000")

PALETTES_PRO <- list(
  oms_sequentiel = c("#e8f5e9","#a5d6a7","#f9c734","#e88400","#c9161d","#640000"),
  oms_divergent  = c("#2d8e47","#a8c834","#fffde7","#e8822a","#c9161d"),
  bleu_marine    = c("#e3f2fd","#90caf9","#42a5f5","#1565c0","#0d2137"),
  chaleur        = c("#fff8e1","#ffcc02","#ff8f00","#d84315","#4a0000"),
  urgence        = c("#c8e6c9","#fff9c4","#ffe0b2","#ef9a9a","#b71c1c")
)

BIVAR_MATRIX <- matrix(
  c("#e8e8e8","#b5c0da","#6c83b5",
    "#b8d6be","#90b2b3","#567994",
    "#73ae80","#5a9178","#2a5a5b"),
  nrow = 3, ncol = 3, byrow = TRUE)

# -- SEUILS OMS PAR INDICATEUR
SEUILS_OMS_PRO <- list(
  GAM = c(5, 10, 15, 30), SAM = c(1, 2, 5, 10), MAM = c(3, 8, 12, 20),
  Stunting = c(20, 30, 40, 60), Sev_Stunting = c(10, 15, 25, 40),
  Underweight = c(10, 20, 30, 40), MUAC_GAM = c(5, 10, 15, 20),
  MUAC_SAM = c(1, 2, 5, 10))

LABELS_FR_PRO <- c(
  GAM          = "MAG (Malnutrition Aigue Globale)",
  SAM          = "MAS (Malnutrition Aigue Severe)",
  MAM          = "MAM (Malnutrition Aigue Moderee)",
  Stunting     = "Retard de croissance (Stunting)",
  Sev_Stunting = "Stunting Severe",
  Underweight  = "Insuffisance ponderale",
  MUAC_GAM     = "MAG par PB (<125mm)",
  MUAC_SAM     = "MAS par PB (<115mm)",
  whz_moy      = "Z-score WHZ moyen",
  haz_moy      = "Z-score HAZ moyen",
  waz_moy      = "Z-score WAZ moyen",
  muac_moy     = "PB moyen (mm)",
  n            = "Effectif enquete")

# -- CLASSIFICATION OMS
classify_oms_pro <- function(val, indicateur = "GAM") {
  s <- SEUILS_OMS_PRO[[indicateur]]
  if (is.null(s)) s <- c(5, 10, 15, 30)
  if (is.na(val)) return(list(classe = "N/A", couleur = "#cccccc", niveau = 0))
  if (val < s[1]) return(list(classe = "Acceptable", couleur = "#2d8e47", niveau = 1))
  if (val < s[2]) return(list(classe = "Precaire", couleur = "#a8c834", niveau = 2))
  if (val < s[3]) return(list(classe = "Serieux", couleur = "#e8822a", niveau = 3))
  if (val < s[4]) return(list(classe = "Critique", couleur = "#c9161d", niveau = 4))
  return(list(classe = "Urgence", couleur = "#640000", niveau = 5))
}

# -- JENKS OPTIMISEE
calc_jenks_pro <- function(vals, n_classes = 5, style = "jenks") {
  vals_clean <- vals[!is.na(vals) & is.finite(vals)]
  if (length(vals_clean) < n_classes) {
    return(list(
      breaks = quantile(vals_clean, probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE),
      style = "quantile_fallback"))
  }
  ci <- tryCatch(
    classIntervals(vals_clean, n = n_classes, style = style),
    error = function(e) classIntervals(vals_clean, n = n_classes, style = "quantile"))
  list(breaks = ci$brks, style = ci$style)
}


# -- POPUP PROFESSIONNEL AVEC MINI-DASHBOARD
make_popup_pro <- function(zone_name, row_data, all_indicators) {
  inds_prev <- c("GAM","SAM","MAM","Stunting","Sev_Stunting","Underweight","MUAC_GAM","MUAC_SAM")
  inds_avail <- intersect(inds_prev, names(row_data))

  html <- paste0(
    "<div style='font-family:Segoe UI,system-ui,sans-serif;width:340px;border-radius:12px;overflow:hidden;box-shadow:0 4px 20px rgba(0,0,0,0.15);'>",
    "<div style='background:linear-gradient(135deg,#00426A,#008DC9);padding:14px 18px;color:white;'>",
    "<div style='font-size:16px;font-weight:700;'>", zone_name, "</div>",
    "<div style='font-size:12px;opacity:0.85;margin-top:2px;'>N = <b>",
    format(row_data$n, big.mark = " "), "</b> enfants</div></div>",
    "<div style='padding:12px 18px 16px;background:white;'>"
  )

  for (ind in inds_avail) {
    val <- row_data[[ind]]
    if (is.na(val)) next
    cls <- classify_oms_pro(val, ind)
    seuils <- SEUILS_OMS_PRO[[ind]]
    if (is.null(seuils)) seuils <- c(5, 10, 15, 30)
    max_bar <- max(seuils[4] * 1.3, val * 1.1, 40)
    pct_bar <- min(val / max_bar * 100, 100)
    label_short <- gsub(" \\(.*", "", LABELS_FR_PRO[ind])
    seuil_crit <- seuils[3]
    pct_seuil <- min(seuil_crit / max_bar * 100, 100)

    html <- paste0(html,
      "<div style='margin:8px 0;'>",
      "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:3px;'>",
      "<span style='font-size:11px;color:#555;font-weight:500;'>", label_short, "</span>",
      "<span style='font-size:13px;font-weight:700;color:", cls$couleur, ";'>", val, " %</span></div>",
      "<div style='position:relative;height:8px;background:#f0f0f0;border-radius:4px;overflow:visible;'>",
      "<div style='height:100%;width:", round(pct_bar, 1), "%;background:", cls$couleur,
      ";border-radius:4px;'></div>",
      "<div style='position:absolute;top:-3px;left:", round(pct_seuil, 1),
      "%;width:2px;height:14px;background:#333;opacity:0.5;' title='Seuil OMS: ", seuil_crit, "%'></div></div>",
      if (val >= seuil_crit) paste0(
        "<div style='font-size:9px;color:", cls$couleur,
        ";font-weight:600;margin-top:1px;'>Seuil OMS depasse (", seuil_crit, "%)</div>") else "",
      "</div>"
    )
  }

  # Scores Z continus
  inds_cont <- intersect(c(".whz_moy",".haz_moy",".waz_moy",".muac_moy",
                            "whz_moy","haz_moy","waz_moy","muac_moy"), names(row_data))
  if (length(inds_cont) > 0) {
    html <- paste0(html,
      "<div style='border-top:1px solid #eee;margin-top:10px;padding-top:8px;'>",
      "<div style='font-size:10px;color:#888;font-weight:600;text-transform:uppercase;'>Scores moyens</div>")
    for (v in inds_cont) {
      val2 <- row_data[[v]]
      if (is.na(val2)) next
      label <- LABELS_FR_PRO[gsub("^\\.","",v)]
      if (is.na(label)) label <- v
      col_z <- if (grepl("whz|haz|waz", v)) {
        if (val2 < -2) "#c9161d" else if (val2 < -1) "#e8822a" else "#2d8e47"
      } else "#008DC9"
      html <- paste0(html,
        "<div style='display:flex;justify-content:space-between;font-size:11px;padding:2px 0;'>",
        "<span style='color:#666;'>", label, "</span>",
        "<span style='font-weight:600;color:", col_z, ";'>", round(val2, 2), "</span></div>")
    }
    html <- paste0(html, "</div>")
  }

  html <- paste0(html,
    "<div style='border-top:1px solid #eee;margin-top:8px;padding-top:6px;font-size:9px;color:#aaa;text-align:center;'>",
    "Normes OMS 2006 | Enquete SMART</div></div></div>")
  html
}

# -- LEGENDE HTML PROFESSIONNELLE
legend_html_pro <- function(indicateur = "GAM", title = NULL) {
  s <- SEUILS_OMS_PRO[[indicateur]]
  if (is.null(s)) s <- c(5, 10, 15, 30)
  if (is.null(title)) { title <- LABELS_FR_PRO[indicateur]; if (is.na(title)) title <- indicateur }

  items <- list(
    list(c = "#2d8e47", l = paste0("< ", s[1], "% - Acceptable")),
    list(c = "#a8c834", l = paste0(s[1], "-", s[2]-0.1, "% - Precaire")),
    list(c = "#e8822a", l = paste0(s[2], "-", s[3]-0.1, "% - Serieux")),
    list(c = "#c9161d", l = paste0(s[3], "-", s[4]-0.1, "% - Critique")),
    list(c = "#640000", l = paste0(">= ", s[4], "% - Urgence")))

  items_html <- paste0(vapply(items, function(it) {
    paste0("<div style='display:flex;align-items:center;margin:3px 0;'>",
      "<div style='width:18px;height:12px;background:", it$c,
      ";border-radius:2px;margin-right:8px;'></div>",
      "<span>", it$l, "</span></div>")
  }, character(1)), collapse = "")

  paste0(
    "<div style='background:rgba(255,255,255,0.95);padding:12px 16px;border-radius:10px;",
    "font-family:Segoe UI,sans-serif;font-size:11px;line-height:1.5;",
    "box-shadow:0 2px 12px rgba(0,0,0,0.1);border:1px solid rgba(0,0,0,0.06);min-width:180px;'>",
    "<div style='font-weight:700;font-size:12px;color:#00426A;margin-bottom:6px;",
    "border-bottom:2px solid #008DC9;padding-bottom:4px;'>", title, "</div>",
    items_html,
    "<div style='margin-top:6px;padding-top:4px;border-top:1px solid #eee;",
    "font-size:9px;color:#999;'>Source : Normes OMS/UNICEF</div></div>")
}

# -- KPI OVERLAY SUR LA CARTE
make_kpi_overlay <- function(agg_data, indicateur = "GAM") {
  val <- if (indicateur %in% names(agg_data)) round(mean(agg_data[[indicateur]], na.rm=TRUE), 1) else NA
  n_total <- sum(agg_data$n, na.rm = TRUE)
  n_zones <- nrow(agg_data)
  cls <- classify_oms_pro(val, indicateur)
  seuils <- SEUILS_OMS_PRO[[indicateur]]
  if (is.null(seuils)) seuils <- c(5,10,15,30)
  n_alerte <- if (indicateur %in% names(agg_data)) sum(agg_data[[indicateur]] >= seuils[3], na.rm=TRUE) else 0
  label_ind <- gsub(" \\(.*","", LABELS_FR_PRO[indicateur])
  if (is.na(label_ind)) label_ind <- indicateur

  paste0(
    "<div style='display:flex;gap:10px;font-family:Segoe UI,sans-serif;'>",
    "<div style='background:rgba(255,255,255,0.95);padding:10px 14px;border-radius:10px;border-left:4px solid ", cls$couleur,
    ";box-shadow:0 2px 8px rgba(0,0,0,0.08);min-width:100px;'>",
    "<div style='font-size:9px;color:#888;text-transform:uppercase;font-weight:600;'>", label_ind, "</div>",
    "<div style='font-size:24px;font-weight:800;color:", cls$couleur, ";line-height:1.1;'>",
    if (!is.na(val)) paste0(val, "%") else "N/A", "</div>",
    "<div style='font-size:9px;color:#aaa;'>", cls$classe, "</div></div>",
    "<div style='background:rgba(255,255,255,0.95);padding:10px 14px;border-radius:10px;border-left:4px solid #008DC9;",
    "box-shadow:0 2px 8px rgba(0,0,0,0.08);'>",
    "<div style='font-size:9px;color:#888;text-transform:uppercase;font-weight:600;'>Effectif</div>",
    "<div style='font-size:24px;font-weight:800;color:#00426A;line-height:1.1;'>", format(n_total, big.mark=" "), "</div>",
    "<div style='font-size:9px;color:#aaa;'>", n_zones, " zones</div></div>",
    "<div style='background:rgba(255,255,255,0.95);padding:10px 14px;border-radius:10px;border-left:4px solid ",
    if (n_alerte > 0) "#c9161d" else "#2d8e47", ";box-shadow:0 2px 8px rgba(0,0,0,0.08);'>",
    "<div style='font-size:9px;color:#888;text-transform:uppercase;font-weight:600;'>Zones en alerte</div>",
    "<div style='font-size:24px;font-weight:800;color:", if (n_alerte > 0) "#c9161d" else "#2d8e47",
    ";line-height:1.1;'>", n_alerte, "/", n_zones, "</div>",
    "<div style='font-size:9px;color:#aaa;'>Seuil >= ", seuils[3], "%</div></div></div>")
}

# -- CLASSIFICATION BIVARIATE
classify_bivariate <- function(vals_x, vals_y) {
  qx <- quantile(vals_x, probs = c(1/3, 2/3), na.rm = TRUE)
  qy <- quantile(vals_y, probs = c(1/3, 2/3), na.rm = TRUE)
  class_x <- ifelse(vals_x <= qx[1], 1, ifelse(vals_x <= qx[2], 2, 3))
  class_y <- ifelse(vals_y <= qy[1], 1, ifelse(vals_y <= qy[2], 2, 3))
  colors <- mapply(function(cx, cy) {
    if (is.na(cx) || is.na(cy)) return("#cccccc")
    BIVAR_MATRIX[cy, cx]
  }, class_x, class_y)
  list(colors = colors, class_x = class_x, class_y = class_y, breaks_x = qx, breaks_y = qy)
}

legend_bivariate_html <- function(label_x = "GAM", label_y = "Stunting") {
  sz <- 22
  cells <- ""
  for (row in 3:1) for (col in 1:3)
    cells <- paste0(cells, "<div style='width:", sz, "px;height:", sz, "px;background:", BIVAR_MATRIX[row, col], ";'></div>")
  paste0(
    "<div style='background:rgba(255,255,255,0.95);padding:14px;border-radius:10px;font-family:Segoe UI,sans-serif;font-size:10px;box-shadow:0 2px 12px rgba(0,0,0,0.1);'>",
    "<div style='font-weight:700;font-size:11px;color:#00426A;margin-bottom:8px;'>Carte bivariate</div>",
    "<div style='display:grid;grid-template-columns:repeat(3,", sz, "px);grid-template-rows:repeat(3,", sz, "px);gap:0;'>", cells, "</div>",
    "<div style='margin-top:4px;font-weight:600;'><span style='color:#6c83b5;'>", label_x, " -></span></div>",
    "<div style='margin-top:2px;font-weight:600;'><span style='color:#73ae80;'>^ ", label_y, "</span></div></div>")
}

# -- PALETTE LEAFLET OMS
pal_oms_pro <- function(vals, indicateur = "GAM") {
  s <- SEUILS_OMS_PRO[[indicateur]]
  if (is.null(s)) s <- c(5, 10, 15, 30)
  domain_max <- max(c(vals, s[4] * 1.2), na.rm = TRUE)
  colorBin(
    palette = c("#2d8e47","#a8c834","#f5c242","#e8822a","#c9161d","#640000"),
    domain = c(0, domain_max),
    bins = c(0, s[1], s[2], s[3], s[4], domain_max),
    na.color = "#e0e0e0")
}

# -- CARTOUCHE PROFESSIONNEL
make_cartouche <- function(titre, sous_titre = NULL, source_txt = NULL,
                            date = format(Sys.Date(), "%B %Y")) {
  paste0(
    "<div style='background:linear-gradient(135deg,#00426A 0%,#008DC9 100%);color:white;padding:10px 18px;",
    "border-radius:0 0 12px 12px;font-family:Segoe UI,sans-serif;box-shadow:0 3px 15px rgba(0,0,0,0.15);'>",
    "<div style='font-size:15px;font-weight:700;'>", titre, "</div>",
    if (!is.null(sous_titre)) paste0("<div style='font-size:11px;opacity:0.85;margin-top:2px;'>", sous_titre, "</div>") else "",
    "<div style='display:flex;justify-content:space-between;margin-top:4px;font-size:9px;opacity:0.7;'>",
    "<span>", if (!is.null(source_txt)) source_txt else "Enquete SMART", "</span>",
    "<span>", date, "</span></div></div>")
}

# -- CARTE STATIQUE PROFESSIONNELLE (ggplot2)
carte_statique_pro <- function(sf_data, ind_col, niveau = "Region",
                                indicateur = "GAM", n_total = 0,
                                palette = "oms_sequentiel",
                                show_labels = TRUE, title_prefix = "CAMEROUN") {
  label_ind <- LABELS_FR_PRO[indicateur]
  if (is.na(label_ind)) label_ind <- indicateur
  seuils <- SEUILS_OMS_PRO[[indicateur]]
  if (is.null(seuils)) seuils <- c(5, 10, 15, 30)
  pal_colors <- PALETTES_PRO[[palette]]
  if (is.null(pal_colors)) pal_colors <- PALETTES_PRO$oms_sequentiel

  centroids <- sf_data |>
    filter(!is.na(.data[[ind_col]])) |>
    st_centroid() |>
    mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2],
           val = .data[[ind_col]], label = paste0(Zone, "
", round(val, 1), "%")) |>
    st_drop_geometry()

  fill_limits <- c(0, max(sf_data[[ind_col]] * 1.1, seuils[4], na.rm = TRUE))

  p <- ggplot() +
    geom_sf(data = sf_data, fill = "#f5f5f5", color = "white", linewidth = 0.4) +
    geom_sf(data = sf_data |> filter(!is.na(.data[[ind_col]])),
            aes(fill = .data[[ind_col]]), color = "white", linewidth = 0.6) +
    geom_sf(data = sf_data, fill = NA, color = "#00426A", linewidth = 0.15)

  if (show_labels && nrow(centroids) > 0) {
    p <- p +
      geom_text(data = centroids, aes(x = x, y = y, label = label),
                size = 3, fontface = "bold", color = "white",
                lineheight = 0.85, check_overlap = TRUE, nudge_y = 0.02) +
      geom_text(data = centroids, aes(x = x, y = y, label = label),
                size = 2.8, fontface = "bold", color = "#00426A",
                lineheight = 0.85, check_overlap = TRUE)
  }

  p + scale_fill_stepsn(
      colours = pal_colors, breaks = seuils, limits = fill_limits,
      na.value = "#f5f5f5", name = label_ind,
      labels = paste0(seuils, "%"),
      guide = guide_colorsteps(barwidth = 15, barheight = 0.8,
                                title.position = "top", title.hjust = 0.5)) +
    labs(title = paste0(title_prefix, " - ", label_ind),
         subtitle = paste0("Niveau : ", niveau, " | N = ", format(n_total, big.mark = " "), " enfants"),
         caption = paste0("Source : Enquete SMART | GADM v4.1 | OMS/UNICEF | ", format(Sys.Date(), "%B %Y"))) +
    theme_void(base_family = "sans") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#00426A", margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#555", margin = margin(b = 12)),
      plot.caption = element_text(hjust = 0.5, size = 8, color = "#999", margin = margin(t = 12)),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold", color = "#333"),
      legend.text = element_text(size = 8, color = "#555"),
      plot.margin = margin(15, 15, 15, 15),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#f8fbff", color = NA))
}

# -- CARTE MULTI-INDICATEURS (small multiples)
carte_multi_indicateurs <- function(sf_data, indicateurs, niveau = "Region",
                                    n_total = 0, ncol_maps = 2) {
  inds_avail <- intersect(indicateurs, names(sf_data |> st_drop_geometry()))
  if (length(inds_avail) == 0) return(NULL)

  sf_long <- sf_data |>
    select(Zone, geometry, all_of(inds_avail)) |>
    pivot_longer(cols = all_of(inds_avail), names_to = "indicateur", values_to = "valeur") |>
    mutate(label = factor(indicateur, levels = inds_avail,
      labels = vapply(inds_avail, function(x) {
        l <- LABELS_FR_PRO[x]; if (is.na(l)) x else gsub(" \\(.*","",l)
      }, character(1))))

  # Ajouter les étiquettes de valeur sur les centroïdes
  centroids <- sf_long |>
    filter(!is.na(valeur)) |>
    sf::st_centroid() |>
    mutate(
      x = sf::st_coordinates(geometry)[, 1],
      y = sf::st_coordinates(geometry)[, 2],
      lbl = paste0(Zone, "\n", round(valeur, 1), "%")
    ) |>
    sf::st_drop_geometry()

  ggplot(sf_long) +
    geom_sf(fill = "#f5f5f5", color = "white", linewidth = 0.3) +
    geom_sf(data = sf_long |> filter(!is.na(valeur)),
            aes(fill = valeur), color = "white", linewidth = 0.4) +
    geom_text(data = centroids, aes(x = x, y = y, label = lbl),
              size = 2.8, color = "#222", lineheight = 0.85, fontface = "bold") +
    scale_fill_gradientn(
      colours = c("#2d8e47","#a8c834","#f5c242","#e8822a","#c9161d","#640000"),
      na.value = "#f5f5f5", name = "Pr\u00e9valence (%)") +
    facet_wrap(~label, ncol = ncol_maps) +
    labs(title = paste0("CAMEROUN \u2014 Indicateurs de malnutrition par ", niveau),
         subtitle = paste0("N = ", format(n_total, big.mark = " "),
                           " enfants | Normes OMS 2006 | ", length(inds_avail), " indicateurs"),
         caption = paste0("Source : Enqu\u00eate SMART | GADM v4.1 | ",
                          format(Sys.Date(), "%B %Y"))) +
    theme_void(base_family = "sans") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#00426A"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#555"),
      plot.caption = element_text(hjust = 0.5, size = 8, color = "#999"),
      strip.text = element_text(face = "bold", size = 12, color = "#00426A",
                                margin = margin(t = 6, b = 6)),
      legend.position = "bottom", legend.title = element_text(face = "bold", size = 9),
      legend.key.width = unit(2, "cm"),
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = "white", color = NA))
}

cat("carto_pro_engine.R charge avec succes
")

# -- CARTES TEMPORELLES (comparaison entre vagues) --

carte_temporal_sidebyside <- function(sf_data_list, ind_col, round_names,
                                       niveau = "Region", n_totals = NULL) {
  label_ind <- LABELS_FR_PRO[ind_col]
  if (is.na(label_ind)) label_ind <- ind_col
  seuils <- SEUILS_OMS_PRO[[ind_col]]
  if (is.null(seuils)) seuils <- c(5, 10, 15, 30)
  sf_all <- do.call(rbind, lapply(seq_along(sf_data_list), function(i) {
    sf_i <- sf_data_list[[i]]
    sf_i$Vague <- round_names[i]
    sf_i
  }))
  sf_all$Vague <- factor(sf_all$Vague, levels = round_names)
  centroids <- sf_all |>
    filter(!is.na(.data[[ind_col]])) |>
    st_centroid() |>
    mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2],
           val = .data[[ind_col]], label = paste0(Zone, "
", round(val,1), "%")) |>
    st_drop_geometry()
  all_vals <- sf_all[[ind_col]][!is.na(sf_all[[ind_col]])]
  fill_limits <- c(0, max(c(all_vals, seuils[4]), na.rm = TRUE))
  ggplot() +
    geom_sf(data = sf_all, fill = "#f5f5f5", color = "white", linewidth = 0.3) +
    geom_sf(data = sf_all |> filter(!is.na(.data[[ind_col]])),
            aes(fill = .data[[ind_col]]), color = "white", linewidth = 0.5) +
    geom_sf(data = sf_all, fill = NA, color = "#00426A", linewidth = 0.1) +
    geom_text(data = centroids, aes(x = x, y = y, label = label),
              size = 2.5, fontface = "bold", color = "white",
              lineheight = 0.85, check_overlap = TRUE, nudge_y = 0.02) +
    geom_text(data = centroids, aes(x = x, y = y, label = label),
              size = 2.3, fontface = "bold", color = "#00426A",
              lineheight = 0.85, check_overlap = TRUE) +
    scale_fill_stepsn(
      colours = PALETTES_PRO$oms_sequentiel, breaks = seuils,
      limits = fill_limits, na.value = "#f5f5f5", name = label_ind,
      labels = paste0(seuils, "%"),
      guide = guide_colorsteps(barwidth = 12, barheight = 0.7,
                                title.position = "top", title.hjust = 0.5)) +
    facet_wrap(~Vague, ncol = length(round_names)) +
    labs(
      title = paste0("CAMEROUN - Evolution de ", label_ind, " par ", niveau),
      subtitle = "Comparaison entre vagues | Classification OMS",
      caption = paste0("Source : Enquete SMART | GADM v4.1 | ", format(Sys.Date(), "%B %Y"))) +
    theme_void(base_family = "sans") +
    theme(
      plot.title = element_text(hjust=0.5, face="bold", size=16, color="#00426A", margin=margin(b=4)),
      plot.subtitle = element_text(hjust=0.5, size=11, color="#555", margin=margin(b=12)),
      plot.caption = element_text(hjust=0.5, size=8, color="#999", margin=margin(t=12)),
      strip.text = element_text(face="bold", size=12, color="#00426A", margin=margin(b=6)),
      legend.position = "bottom",
      legend.title = element_text(size=10, face="bold", color="#333"),
      plot.margin = margin(10,10,10,10),
      plot.background = element_rect(fill="white", color=NA))
}
# Popup temporel pour carte de variation leaflet
make_popup_temporal <- function(zone, r1_val, r2_val, delta, ind_name, r1_name, r2_name, n1, n2) {
  cls1 <- classify_oms_pro(r1_val, ind_name)
  cls2 <- classify_oms_pro(r2_val, ind_name)
  arr_col <- if (!is.na(delta) && delta > 0) "#c9161d" else if (!is.na(delta) && delta < 0) "#2d8e47" else "#888"
  arr_sym <- if (!is.na(delta) && delta > 2) "&#9650;" else if (!is.na(delta) && delta > 0) "&#9652;" else if (!is.na(delta) && delta < -2) "&#9660;" else if (!is.na(delta) && delta < 0) "&#9662;" else "&#9644;"
  paste0(
    "<div style=\"font-family:Segoe UI,sans-serif;width:300px;border-radius:12px;overflow:hidden;box-shadow:0 4px 20px rgba(0,0,0,0.15);\">",
    "<div style=\"background:linear-gradient(135deg,#00426A,#008DC9);padding:12px 16px;color:white;\">",
    "<div style=\"font-size:15px;font-weight:700;\">", zone, "</div>",
    "<div style=\"font-size:11px;opacity:0.85;\">Evolution temporelle</div></div>",
    "<div style=\"padding:12px 16px;background:white;\">",
    "<div style=\"display:flex;justify-content:space-around;text-align:center;margin:8px 0;\">",
    "<div><div style=\"font-size:9px;color:#888;\">", r1_name, "</div>",
    "<div style=\"font-size:20px;font-weight:800;color:", cls1$couleur, ";\">",
    if(!is.na(r1_val)) paste0(round(r1_val,1),"%") else "N/A", "</div>",
    "<div style=\"font-size:9px;color:#aaa;\">n=", if(!is.na(n1)) n1 else "?", "</div></div>",
    "<div style=\"font-size:28px;color:", arr_col, ";padding:0 10px;\">", arr_sym, "</div>",
    "<div><div style=\"font-size:9px;color:#888;\">", r2_name, "</div>",
    "<div style=\"font-size:20px;font-weight:800;color:", cls2$couleur, ";\">",
    if(!is.na(r2_val)) paste0(round(r2_val,1),"%") else "N/A", "</div>",
    "<div style=\"font-size:9px;color:#aaa;\">n=", if(!is.na(n2)) n2 else "?", "</div></div>",
    "</div>",
    "<div style=\"text-align:center;padding:8px 0;border-top:1px solid #eee;\">",
    "<span style=\"font-size:14px;font-weight:700;color:", arr_col, ";\">",
    if(!is.na(delta)) paste0(ifelse(delta>0,"+",""), round(delta,1), " pp") else "N/A",
    "</span>",
    "<div style=\"font-size:9px;color:#aaa;\">points de pourcentage</div>",
    "</div></div></div>")
}

# ═══════════════════════════════════════════════════════════════════════════════
#  HABILLAGE EXPERT — classification configurable · légende dynamique · nord
# ═══════════════════════════════════════════════════════════════════════════════

PALETTES_CARTE_PRO <- list(
  oms     = c("#2d8e47","#a8c834","#f5c242","#e8822a","#c9161d","#640000"),
  ylorrd  = c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026"),
  blues   = c("#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"),
  viridis = c("#440154","#482878","#3e4989","#31688e","#26828e","#1f9e89","#35b779","#6ece58","#b5de2b","#fde725"),
  sepia   = c("#fbf2e0","#ecdcc0","#dcc4a0","#c9a878","#b48a54","#9c6b33","#7f4f1e")
)

CLASS_METHODS_PRO <- c(
  oms      = "Seuils OMS (fixes)",
  quantile = "Quantiles",
  jenks    = "Jenks (ruptures naturelles)",
  equal    = "Intervalles \u00e9gaux",
  sd       = "\u00c9cart-type"
)

#' Classification configurable des valeurs pour choroplethe expert
#' @return list(brks, pal, cols, method_label, k_eff) ou NULL
classify_zones_pro <- function(vals, method = "oms", k = 5,
                               indicateur = "GAM", palette = "ylorrd") {
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NULL)
  if (is.null(k)) k <- 5L
  k <- max(2L, min(9L, as.integer(k)))

  if (method == "oms") {
    s <- SEUILS_OMS_PRO[[indicateur]]; if (is.null(s)) s <- c(5, 10, 15, 30)
    brks <- unique(c(0, s, max(c(vals, s[4] * 1.2), na.rm = TRUE) * 1.01))
    pvec <- PALETTES_CARTE_PRO$oms
  } else {
    if (is.null(palette)) palette <- "ylorrd"
    pvec <- PALETTES_CARTE_PRO[[palette]]
    if (is.null(pvec)) pvec <- PALETTES_CARTE_PRO$ylorrd
  }
  if (method == "quantile") {
    brks <- unique(as.numeric(quantile(vals, probs = seq(0, 1, length.out = k + 1))))
  } else if (method == "jenks") {
    brks <- NULL
    if (requireNamespace("classInt", quietly = TRUE)) {
      brks <- tryCatch(as.numeric(
        classInt::classIntervals(vals, n = k, style = "jenks")$brks),
        error = function(e) NULL)
      if (!is.null(brks)) brks <- unique(as.numeric(round(brks, 6)))
    }
    if (is.null(brks) || length(brks) < 3)
      brks <- unique(as.numeric(quantile(vals, probs = seq(0, 1, length.out = k + 1))))
  } else if (method == "equal") {
    brks <- seq(min(vals), max(vals), length.out = k + 1)
    if (isTRUE(all.equal(brks[1], brks[length(brks)])))
      brks <- c(brks[1] - 1, brks[-1] + 1)
    brks <- unique(as.numeric(round(brks, 3)))
  } else if (method == "sd") {
    m <- mean(vals); s0 <- sd(vals)
    if (!is.finite(s0) || s0 == 0) s0 <- max(abs(m) * 0.1, 1)
    brks <- sort(unique(as.numeric(round(
      c(m - 2 * s0, m - s0, m, m + s0, m + 2 * s0), 3))))
  }

  brks <- brks[is.finite(brks)]
  brks <- unique(brks)
  if (length(brks) < 3) return(NULL)
  k_eff <- length(brks) - 1L

  pal <- colorBin(palette = pvec, domain = c(brks[1], brks[length(brks)]),
                  bins = brks, na.color = "#e0e0e0", pretty = FALSE)
  # Couleurs exactes par intervalle (m\u00eame source que le rendu Leaflet)
  cols <- vapply(seq_len(k_eff), function(i) {
    pal((brks[i] + brks[i + 1]) / 2)
  }, character(1))

  list(brks = brks, pal = pal, cols = cols, k_eff = k_eff,
       method_label = if (!is.null(CLASS_METHODS_PRO[method])) unname(CLASS_METHODS_PRO[method]) else method)
}

#' Légende dynamique générée d'après les classes réellement calculées
legend_dynamic_html_pro <- function(ind, cls, unit = "%") {
  if (is.null(cls)) return(NULL)
  brks <- cls$brks; cols <- cls$cols; meth <- cls$method_label
  title <- LABELS_FR_PRO[ind]; if (is.na(title) || is.null(title)) title <- ind

  n <- length(brks) - 1L
  lbls_oms <- c("Acceptable", "Pr\u00e9caire", "S\u00e9rieux", "Critique", "Urgence")
  use_oms <- identical(meth, "Seuils OMS (fixes)") && n <= 5

  items <- vapply(seq_len(n), function(i) {
    lbl <- paste0(format(brks[i], trim = TRUE), " \u2013 ", format(brks[i + 1], trim = TRUE), unit)
    if (use_oms) lbl <- paste0(lbl, " \u2014 ", lbls_oms[i])
    paste0("<div style='display:flex;align-items:center;margin:3px 0;'>",
           "<div style='width:20px;height:13px;background:", cols[i],
           ";border-radius:2px;margin-right:8px;border:1px solid rgba(0,0,0,.15);'></div>",
           "<span>", lbl, "</span></div>")
  }, character(1))

  paste0(
    "<div style='background:rgba(255,255,255,0.95);padding:12px 16px;border-radius:10px;",
    "font-family:Segoe UI,sans-serif;font-size:11px;line-height:1.5;",
    "box-shadow:0 2px 12px rgba(0,0,0,0.1);border:1px solid rgba(0,0,0,0.06);min-width:190px;'>",
    "<div style='font-weight:700;font-size:12px;color:#00426A;margin-bottom:6px;",
    "border-bottom:2px solid #008DC9;padding-bottom:4px;'>", title, "</div>",
    paste(items, collapse = ""),
    "<div style='display:flex;align-items:center;margin:3px 0;'>",
    "<div style='width:20px;height:13px;background:#e0e0e0;border-radius:2px;margin-right:8px;",
    "border:1px dashed #999;'></div><span style='color:#777;'>Pas de donn\u00e9es</span></div>",
    "<div style='margin-top:6px;padding-top:4px;border-top:1px solid #eee;",
    "font-size:9px;color:#999;'>M\u00e9thode : ", meth, " (", length(cols), " classes)",
    " \u00b7 Source : GADM / enqu\u00eate</div></div>")
}

#' Normalisation robuste des noms de zones (accents, casse, ponctuation)
#' Pour joindre les noms GADM (ex. "Extr\u00eame-Nord") aux noms des donn\u00e9es (ex. "Extreme-Nord").
norm_zone_name <- function(x) {
  x <- tolower(as.character(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]", "", x)
  x[!nzchar(x)] <- NA_character_
  x
}

#' Flèche du Nord (habillage cartographique)
north_arrow_html <- function() {
  paste0(
    "<div style='background:rgba(255,255,255,0.95);border-radius:8px;padding:5px 9px;",
    "box-shadow:0 2px 8px rgba(0,0,0,0.12);text-align:center;font-family:Segoe UI,sans-serif;'>",
    "<div style='font-size:18px;line-height:1;color:#00426A;'>&#9650;</div>",
    "<div style='font-size:9px;font-weight:700;color:#00426A;letter-spacing:1px;'>N</div></div>")
}

#' Carte statique qualité publication (ggplot + sf) — export PNG 300 dpi / PDF
#' Reproduit la symbologie interactive : même classification, même palette.
carte_statique_pro <- function(sf_join, ind,
                               titre = NULL, sous_titre = NULL,
                               source_txt = "Fronti\u00e8res : GADM v4.1",
                               method = "oms", k = 5, palette = "ylorrd") {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE))
    return(NULL)
  if (is.null(titre)) titre <- ind
  val <- sf_join[[ind]]
  if (is.null(val)) return(NULL)
  vals_ok <- val[is.finite(val)]
  if (length(vals_ok) == 0) return(NULL)

  cls <- classify_zones_pro(vals_ok, method, k, ind, palette)
  if (is.null(cls)) return(NULL)
  brks <- round(cls$brks, 2); cols <- cls$cols
  n <- length(brks) - 1L

  sf_join$.classe <- cut(val, breaks = cls$brks, include.lowest = TRUE, labels = FALSE)
  lbls <- paste0(format(brks[-length(brks)], trim = TRUE), " \u2013 ",
                 format(brks[-1], trim = TRUE), "%")
  sf_join$.lbl <- ifelse(is.na(sf_join$.classe), "Pas de donn\u00e9es",
                         lbls[pmax(1, sf_join$.classe)])
  lvls <- c(lbls, "Pas de donn\u00e9es")
  sf_join$.lbl <- factor(sf_join$.lbl, levels = lvls)
  pal_cols <- c(setNames(cols, lbls), setNames("#e0e0e0", "Pas de donn\u00e9es"))

  bb <- sf::st_bbox(sf_join)
  span_x <- bb[["xmax"]] - bb[["xmin"]]; span_y <- bb[["ymax"]] - bb[["ymin"]]

  p <- ggplot(sf_join) +
    ggplot2::geom_sf(aes(fill = .lbl), color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_manual(values = pal_cols, name = NULL,
                               drop = FALSE, na.translate = FALSE) +
    ggplot2::annotate("text", x = bb[["xmax"]] - 0.04 * span_x,
                      y = bb[["ymax"]] - 0.07 * span_y,
                      label = "\u25b2\nN", size = 3.4, fontface = "bold",
                      color = "#00426A", lineheight = 0.9) +
    ggplot2::labs(title = titre, subtitle = sous_titre, caption = source_txt) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", color = "#00426A", size = 15),
      plot.subtitle    = ggplot2::element_text(color = "#008DC9", size = 11),
      plot.caption     = ggplot2::element_text(color = "#888", size = 8, hjust = 0),
      legend.title     = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 9),
      legend.key.height = ggplot2::unit(0.5, "cm"),
      panel.grid       = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA))
  p
}
#' Carte monde/Afrique interactive (Leaflet + Natural Earth)
#' Remplace le ggplotly statique : zoom/pan reels, labels, popups, habillage.
#' vals_pays : liste nommee (nom pays dans les donnees -> valeur indicateur %)
#' pour les enquetes multi-pays ; les pays identifies sont colores selon
#' les seuils OMS et leur popup affiche la valeur.
carte_monde_interactive_pro <- function(d = NULL, indicateur = "GAM",
                                        zoom_region = "Afrique centrale",
                                        pays_donnees_en = NULL,
                                        pays_donnees_fr = NULL,
                                        vals_pays = NULL) {
  world <- ne_world_cached()
  if (is.null(world) || !requireNamespace("sf", quietly = TRUE)) return(NULL)

  val_ind <- if (!is.null(d) && !is.null(indicateur) && indicateur %in% names(d))
    round(mean(d[[indicateur]], na.rm = TRUE) * 100, 1) else NA

  pays_hl <- c("Cameroon", "Chad", "Central African Republic", "Niger", "Nigeria",
               "Gabon", "Congo", "Equatorial Guinea",
               "Democratic Republic of the Congo")


  has_d <- !is.null(pays_donnees_en) && nzchar(pays_donnees_en) &&
    !grepl("^Inconnu", pays_donnees_en)
  if (!has_d) pays_donnees_en <- "__none__"
  fr_lbl <- if (!is.null(pays_donnees_fr) && nzchar(pays_donnees_fr)) pays_donnees_fr
            else pays_donnees_en

  world$.cat <- dplyr::case_when(
    world$name == pays_donnees_en ~ paste0(fr_lbl, " (donn\u00e9es)"),
    world$name %in% pays_hl ~ "Afrique centrale",
    world$continent == "Africa" ~ "Autre Afrique",
    TRUE ~ "Monde")
  cat_cols <- c("Afrique centrale" = "#b3d9ef", "Autre Afrique" = "#dce8f0",
                "Monde" = "#f0f0f0")
  if (has_d) cat_cols <- c(setNames("#c9161d", paste0(fr_lbl, " (donn\u00e9es)")), cat_cols)

  # Valeurs par pays (enquetes multi-pays) : match normalise des noms
  world$.val <- NA_real_
  n_matched <- 0L
  if (!is.null(vals_pays)) {
    wm <- norm_zone_name(world$name)
    for (nm in names(vals_pays)) {
      z <- norm_zone_name(nm)
      i <- which(wm == z & !is.na(wm))
      if (length(i) == 1) {
        world$.val[i] <- suppressWarnings(as.numeric(vals_pays[[nm]]))
        n_matched <- n_matched + 1L
      }
    }
  }
  ind_lbl <- LABELS_FR_PRO[indicateur]
  if (is.na(ind_lbl) || is.null(ind_lbl)) ind_lbl <- indicateur

  # Remplissage : choroplethe OMS sur les pays de donnees identifies
  fill_vec <- unname(cat_cols[world$.cat])
  ok <- !is.na(world$.val)
  cls <- NULL
  if (sum(ok) >= 2) {
    cls <- classify_zones_pro(world$.val[ok], "oms", 5, indicateur, "ylorrd")
    if (!is.null(cls)) {
      idx <- cut(world$.val[ok], breaks = cls$brks, include.lowest = TRUE, labels = FALSE)
      fill_vec[ok] <- cls$cols[idx]
    }
  } else if (sum(ok) == 1) {
    fill_vec[ok] <- "#c9161d"
  }

  world$popup_txt <- vapply(seq_len(nrow(world)), function(i) {
    nm <- world$name[i]; cat <- world$.cat[i]
    extra <- ""
    if (!is.na(world$.val[i])) {
      extra <- paste0("<br><b>", ind_lbl, " :</b> ", world$.val[i], "%")
    } else if (world$name[i] == pays_donnees_en && !is.na(val_ind)) {
      extra <- paste0("<br><b>", ind_lbl, " :</b> ", val_ind, "%")
    }
    paste0("<b>", nm, "</b><br>", cat, extra)
  }, character(1))

  bb <- switch(zoom_region,
    "Afrique centrale" = c(xmin = 8, ymin = -5, xmax = 30, ymax = 25),
    "Afrique"          = c(xmin = -20, ymin = -35, xmax = 55, ymax = 38),
    "Monde entier"     = c(xmin = -180, ymin = -58, xmax = 180, ymax = 84),
    c(xmin = 8, ymin = -5, xmax = 30, ymax = 25))

  cmap <- leaflet(world, options = leafletOptions(minZoom = 2, worldCopyJump = TRUE,
                                                  zoomControl = TRUE)) |>
    addProviderTiles("Esri.WorldGrayCanvas") |>
    addPolygons(fillColor = fill_vec, fillOpacity = 0.92,
                color = "white", weight = 0.6,
                label = ~paste0(name, ifelse(is.na(.val), "",
                                             paste0(" : ", .val, "%"))),
                popup = ~popup_txt, layerId = ~name,
                highlightOptions = highlightOptions(weight = 2, color = "#008DC9",
                                                    fillOpacity = 1, bringToFront = TRUE))
  if (!is.null(cls) && n_matched >= 2) {
    cmap <- cmap |> addControl(html = legend_dynamic_html_pro(indicateur, cls),
                               position = "bottomright")
  } else {
    cmap <- cmap |> addLegend(colors = unname(cat_cols), labels = names(cat_cols),
                              title = "Contexte", position = "bottomright",
                              opacity = 0.95)
  }
  cmap <- cmap |>
    addScaleBar(position = "bottomleft") |>
    addFullscreenControl(position = "topleft") |>
    addControl(html = make_cartouche(
      titre = "Contexte r\u00e9gional et mondial",
      sous_titre = if (n_matched >= 2) paste0(n_matched, " pays avec donn\u00e9es")
                   else if (has_d) fr_lbl else "Vue r\u00e9gionale",
      source_txt = "Fronti\u00e8res : Natural Earth \u00b7 Donn\u00e9es : enqu\u00eate SMART",
      date = format(Sys.Date(), "%d/%m/%Y")), position = "topleft") |>
    addControl(html = north_arrow_html(), position = "topright") |>
    fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  cmap
}

#' Natural Earth medium en cache RDS (premier rendu ~30 s sinon)
ne_world_cached <- function(cache_dir = "gadm_cache") {
  if (!dir.exists(cache_dir)) cache_dir <- tempdir()
  f <- file.path(cache_dir, "ne_world_medium.rds")
  if (file.exists(f)) {
    w <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(w)) return(w)
  }
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) return(NULL)
  w <- tryCatch(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
                error = function(e) NULL)
  if (!is.null(w)) tryCatch(saveRDS(w, f), error = function(e) NULL)
  w
}
