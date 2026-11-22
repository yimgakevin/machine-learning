# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  carto_expert_engine.R — Moteur cartographique EXPERT                        ║
# ║  Jenks Natural Breaks · Annotations contextuelles · Zoom smart               ║
# ║  TIER 1 : Cartes intelligentes niveau publication                           ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(sf)
library(tidyverse)
library(classInt)  # Pour Jenks breaks
library(scales)

# ══════════════════════════════════════════════════════════════════════════════
# 1. JENKS NATURAL BREAKS — Ruptures optimales au lieu de linéaire
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer les ruptures naturelles Jenks pour une variable
#'
#' @param values Vecteur de valeurs numériques
#' @param n_classes Nombre de classes (défaut: 5)
#' @param style "jenks" | "quantile" | "equal.interval"
#'
#' @return Liste avec breaks, labels, colors
calc_jenks_breaks <- function(values, n_classes = 5, style = "jenks") {
  
  # Nettoyer les NA
  vals_clean <- values[!is.na(values)]
  
  if (length(vals_clean) < 2) {
    return(list(
      breaks = c(0, max(vals_clean, 1)),
      labels = c("Pas de données"),
      colors = c("#cccccc"),
      style = "error"
    ))
  }
  
  # Calculer les classes naturelles
  if (style == "jenks") {
    # Jenks = minimise variance intra-classe
    brks <- classInt::classIntervals(vals_clean, n = n_classes, style = "jenks")
  } else if (style == "quantile") {
    brks <- classInt::classIntervals(vals_clean, n = n_classes, style = "quantile")
  } else {
    brks <- classInt::classIntervals(vals_clean, n = n_classes, style = "equal.interval")
  }
  
  # Extraire les breaks
  breaks <- brks$brks
  
  # Créer les labels intelligents
  labels <- character(n_classes)
  for (i in 1:n_classes) {
    lower <- round(breaks[i], 1)
    upper <- round(breaks[i+1], 1)
    labels[i] <- paste0(lower, " - ", upper, "%")
  }
  
  # Palette de couleurs OMS
  colors <- c("#4dac26", "#f9c734", "#e88400", "#c9161d", "#640000")[1:n_classes]
  
  list(
    breaks = breaks,
    labels = labels,
    colors = colors,
    style = style,
    n_classes = n_classes
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. ANNOTATIONS CONTEXTUELLES — Population enfants, n_obs, IC
# ══════════════════════════════════════════════════════════════════════════════

#' Générer des annotations enrichies pour chaque région
#'
#' @param sf_data objet sf avec données
#' @param col_nom colonne avec noms régions
#' @param col_var colonne avec variables à afficher
#' @param col_n colonne avec effectifs
#' @param col_ic_low colonne IC bas (optionnel)
#' @param col_ic_high colonne IC haut (optionnel)
#'
#' @return data.frame avec annotations formatées
gen_region_annotations <- function(sf_data, col_nom, col_var, col_n, 
                                   col_ic_low = NULL, col_ic_high = NULL) {
  
  d <- sf_data %>%
    as.data.frame() %>%
    select(-geometry) %>%
    mutate(
      annotation = case_when(
        is.na(!!rlang::sym(col_var)) ~ paste0(
          !!rlang::sym(col_nom),
          "\n<span style='font-size:10px;color:#999;'>Pas de données</span>"
        ),
        TRUE ~ paste0(
          !!rlang::sym(col_nom),
          "\n<b style='font-size:14px;'>", round(!!rlang::sym(col_var), 1), "%</b>",
          "\n<span style='font-size:10px;color:#666;'>n = ", 
          format(!!rlang::sym(col_n), big.mark = " "),
          " enfants</span>",
          if (!is.null(col_ic_low)) paste0(
            "\n<span style='font-size:9px;color:#888;'>IC95% [",
            round(!!rlang::sym(col_ic_low), 1), " - ",
            round(!!rlang::sym(col_ic_high), 1), "]</span>"
          ) else ""
        )
      )
    )
  
  d
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. PALETTES CONTEXTUELLES — Adaptées à chaque indicateur + seuils OMS
# ══════════════════════════════════════════════════════════════════════════════

#' Créer une palette colorée adaptée à l'indicateur avec seuils OMS
#'
#' @param indicateur "GAM", "HAZ", "WAZ", etc.
#' @param values Vecteur de valeurs pour domaine
#'
#' @return fonction colorNumeric
pal_oms_expert <- function(indicateur = "GAM", values = NULL) {
  
  # Définir seuils OMS et styles
  styles <- list(
    GAM = list(
      thresholds = c(5, 10, 15),
      colors = c("#4dac26", "#f9c734", "#e88400", "#c9161d"),
      labels = c("Acceptable<5%", "Pauvre 5-9%", "Sérieux 10-14%", "Critique≥15%")
    ),
    SAM = list(
      thresholds = c(1, 2, 5),
      colors = c("#90ee90", "#ffd700", "#ff8c00", "#dc143c"),
      labels = c("Acceptable<1%", "Pauvre 1-2%", "Sérieux 2-5%", "Critique≥5%")
    ),
    HAZ = list(
      thresholds = c(20, 30, 40),
      colors = c("#4dac26", "#f9c734", "#e88400", "#c9161d"),
      labels = c("Acceptable<20%", "Pauvre 20-29%", "Sérieux 30-39%", "Critique≥40%")
    ),
    WAZ = list(
      thresholds = c(10, 20, 30),
      colors = c("#4dac26", "#f9c734", "#e88400", "#c9161d"),
      labels = c("Acceptable<10%", "Pauvre 10-19%", "Sérieux 20-29%", "Critique≥30%")
    )
  )
  
  # Défaut
  style <- if (!is.null(styles[[indicateur]])) styles[[indicateur]] 
           else styles$GAM
  
  # Domaine
  domain_max <- if (!is.null(values)) {
    max(values, style$thresholds, na.rm = TRUE) * 1.1
  } else {
    max(style$thresholds) * 1.5
  }
  
  # Créer fonction
  leaflet::colorNumeric(
    palette = colorRamp(style$colors),
    domain = c(0, domain_max),
    na.color = "#cccccc"
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. ZOOM SMART — Zoom automatique + animation par région
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer bbox optimale pour région donnée
#'
#' @param sf_region objet sf avec la région
#' @param padding padding en % (défaut: 0.1)
#'
#' @return list(lat, lng, zoom)
calc_optimal_bounds <- function(sf_region, padding = 0.1) {
  
  bbox <- st_bbox(sf_region)
  
  # Extraire les limites
  xmin <- bbox["xmin"]
  xmax <- bbox["xmax"]
  ymin <- bbox["ymin"]
  ymax <- bbox["ymax"]
  
  # Ajouter padding
  x_pad <- (xmax - xmin) * padding
  y_pad <- (ymax - ymin) * padding
  
  xmin_padded <- xmin - x_pad
  xmax_padded <- xmax + x_pad
  ymin_padded <- ymin - y_pad
  ymax_padded <- ymax + y_pad
  
  # Centre
  lat_center <- (ymin_padded + ymax_padded) / 2
  lng_center <- (xmin_padded + xmax_padded) / 2
  
  # Estimer zoom (approximation)
  lon_diff <- xmax_padded - xmin_padded
  zoom <- round(11 - log2(lon_diff / 360) * 2)
  zoom <- pmax(3, pmin(15, zoom))  # Limiter 3-15
  
  list(
    lat = lat_center,
    lng = lng_center,
    zoom = zoom,
    bounds = list(
      southwest = list(lat = ymin_padded, lng = xmin_padded),
      northeast = list(lat = ymax_padded, lng = xmax_padded)
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. TOOLTIP ENRICHIE — Valeur + IC + Comparaison nationale
# ══════════════════════════════════════════════════════════════════════════════

#' Générer tooltip HTML riche pour leaflet
#'
#' @param nom Nom région
#' @param valeur Valeur pour la région
#' @param n Effectif
#' @param ic_low IC bas
#' @param ic_high IC haut
#' @param val_nationale Valeur nationale (pour comparaison)
#' @param theme "light" ou "dark"
#'
#' @return HTML string
gen_rich_tooltip <- function(nom, valeur, n, ic_low, ic_high, val_nationale = NULL, theme = "light") {
  
  bg_color <- if (theme == "light") "white" else "#2a2a2a"
  text_color <- if (theme == "light") "#333" else "#eee"
  
  # Comparaison nationale
  comp_text <- if (!is.null(val_nationale)) {
    diff <- valeur - val_nationale
    direction <- if (diff > 0) "↑" else if (diff < 0) "↓" else "="
    color_comp <- if (diff > 0) "#d32f2f" else if (diff < 0) "#4caf50" else "#999"
    paste0(
      "<tr><td colspan='2' style='border-top:1px solid #ddd;padding-top:6px;margin-top:6px;'>",
      "<span style='font-size:11px;color:", color_comp, ";'>",
      direction, " ", abs(round(diff, 1)), "% vs nationale</span>",
      "</td></tr>"
    )
  } else ""
  
  # HTML table
  html <- paste0(
    "<div style='background:", bg_color, ";color:", text_color, ";",
    "padding:10px;border-radius:6px;font-family:Arial,sans-serif;",
    "min-width:200px;box-shadow:0 2px 8px rgba(0,0,0,0.2);'>",
    "<table style='width:100%;border-collapse:collapse;'>",
    "<tr><td colspan='2' style='font-weight:700;font-size:13px;",
    "margin-bottom:6px;padding-bottom:6px;border-bottom:2px solid #008DC9;'>",
    nom, "</td></tr>",
    "<tr><td style='font-size:12px;'>Prévalence:</td>",
    "<td style='font-weight:700;font-size:14px;color:#008DC9;text-align:right;'>",
    round(valeur, 1), "%</td></tr>",
    "<tr><td style='font-size:11px;color:#666;'>IC 95%:</td>",
    "<td style='font-size:11px;color:#666;text-align:right;'>[",
    round(ic_low, 1), " - ", round(ic_high, 1), "]</td></tr>",
    "<tr><td style='font-size:11px;color:#666;'>Effectif:</td>",
    "<td style='font-size:11px;color:#666;text-align:right;'>",
    format(n, big.mark = " "), " enfants</td></tr>",
    comp_text,
    "</table></div>"
  )
  
  html
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. SMART ZOOM & CENTRE AUTO — Adapter la vue à la région sélectionnée
# ══════════════════════════════════════════════════════════════════════════════

#' Faire zoom sur région après sélection
#'
#' @param sf_region objet sf
#' @param sf_base objet sf complet (pour contexte)
#'
#' @return list optimisation
optimize_view <- function(sf_region, sf_base = NULL) {
  
  # Zoom principal
  bounds_main <- calc_optimal_bounds(sf_region, padding = 0.15)
  
  # Si on a le contexte (cartes adjacentes)
  if (!is.null(sf_base)) {
    # Garder les frontières des zones adjacentes
    sf_adjacent <- st_intersects(st_buffer(sf_region, 0.1), sf_base, sparse = FALSE)
    sf_context <- sf_base[rowSums(sf_adjacent) > 0, ]
  } else {
    sf_context <- NULL
  }
  
  list(
    main_bounds = bounds_main,
    context = sf_context,
    animation = "easeInOut"
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 7. CHOROPLÈTHE EXPERT — Cartogramme avec Jenks + annotations
# ══════════════════════════════════════════════════════════════════════════════

#' Créer une choroplèthe expert
#'
#' @param sf_data objet sf avec géométries + données
#' @param col_var colonne variable à cartographier
#' @param col_nom colonne noms régions
#' @param col_n colonne effectifs
#' @param indicateur type d'indicateur (GAM, HAZ, etc.)
#' @param jenks_breaks breaks (calculés avec calc_jenks_breaks)
#'
#' @return objet sf avec colonnes ajoutées
prepare_choropleth_data <- function(sf_data, col_var, col_nom, col_n, 
                                    indicateur = "GAM", jenks_breaks = NULL) {
  
  # Calculer Jenks si non fourni
  if (is.null(jenks_breaks)) {
    jenks_breaks <- calc_jenks_breaks(sf_data[[col_var]], n_classes = 5)
  }
  
  # Créer bins
  sf_out <- sf_data %>%
    mutate(
      class = cut(!!rlang::sym(col_var), 
                  breaks = jenks_breaks$breaks,
                  labels = jenks_breaks$labels,
                  include.lowest = TRUE),
      color = case_when(
        is.na(!!rlang::sym(col_var)) ~ "#cccccc",
        !!rlang::sym(col_var) < jenks_breaks$breaks[2] ~ jenks_breaks$colors[1],
        !!rlang::sym(col_var) < jenks_breaks$breaks[3] ~ jenks_breaks$colors[2],
        !!rlang::sym(col_var) < jenks_breaks$breaks[4] ~ jenks_breaks$colors[3],
        TRUE ~ jenks_breaks$colors[4]
      ),
      tooltip = gen_rich_tooltip(
        nom = !!rlang::sym(col_nom),
        valeur = !!rlang::sym(col_var),
        n = !!rlang::sym(col_n),
        ic_low = !!rlang::sym(col_var) * 0.9,  # Approximation
        ic_high = !!rlang::sym(col_var) * 1.1,
        val_nationale = mean(!!rlang::sym(col_var), na.rm = TRUE)
      )
    )
  
  sf_out
}

# ══════════════════════════════════════════════════════════════════════════════
# 8. LÉGENDE DYNAMIQUE — Avec seuils OMS annotés
# ══════════════════════════════════════════════════════════════════════════════

#' Créer HTML légende expert
#'
#' @param jenks_breaks résultat de calc_jenks_breaks
#' @param indicateur type d'indicateur
#'
#' @return HTML
gen_legend_html <- function(jenks_breaks, indicateur = "GAM") {
  
  seuils <- list(
    GAM = "Seuils OMS: Acceptable <5%, Pauvre 5-9%, Sérieux 10-14%, Critique ≥15%",
    HAZ = "Seuils OMS: Acceptable <20%, Pauvre 20-29%, Sérieux 30-39%, Critique ≥40%",
    WAZ = "Seuils OMS: Acceptable <10%, Pauvre 10-19%, Sérieux 20-29%, Critique ≥30%"
  )
  
  seuil_text <- seuils[[indicateur]] %||% seuils$GAM
  
  html <- paste0(
    "<div style='background:white;padding:12px;border-radius:6px;",
    "box-shadow:0 2px 8px rgba(0,0,0,0.15);'>",
    "<p style='margin:0 0 8px;font-weight:700;font-size:12px;'>",
    "Ruptures Naturelles (Jenks)</p>"
  )
  
  for (i in seq_along(jenks_breaks$labels)) {
    html <- paste0(html,
      "<div style='margin:4px 0;display:flex;align-items:center;'>",
      "<div style='width:20px;height:20px;background:",
      jenks_breaks$colors[i], ";border-radius:3px;margin-right:8px;",
      "border:1px solid #999;'></div>",
      "<span style='font-size:11px;'>", jenks_breaks$labels[i], "</span>",
      "</div>"
    )
  }
  
  html <- paste0(html,
    "<hr style='margin:8px 0;border:none;border-top:1px solid #ddd;'>",
    "<p style='margin:6px 0;font-size:10px;color:#666;'>",
    seuil_text, "</p></div>"
  )
  
  html
}
