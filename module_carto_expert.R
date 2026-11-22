# module_carto_expert.R -- Extensions expert cartographie nutritionnelle
# ═══════════════════════════════════════════════════════════════════════════════
# Getis-Ord Gi* hotspots | Lissage Bayesien empirique (EB)
# Carte IPC/CH officielle | Forest plot prevalences + IC
# Indices d'inegalite spatiale (Gini, Theil) | Profil regional radar
# ═══════════════════════════════════════════════════════════════════════════════

# [C2] Chargement robuste des packages
for (.pkg in c("sf", "tidyverse", "binom")) {
  if (!requireNamespace(.pkg, quietly = TRUE)) {
    message("\u26a0\ufe0f module_carto_expert.R : package '", .pkg, "' non disponible")
  } else {
    suppressPackageStartupMessages(library(.pkg, character.only = TRUE))
  }
}
if (requireNamespace("spdep", quietly = TRUE)) library(spdep)

# ═══════════════════════════════════════════════════════════════════════════════
# 1. GETIS-ORD Gi* — Hotspots statistiquement significatifs
# ═══════════════════════════════════════════════════════════════════════════════

#' Calcule le Gi* de Getis-Ord pour chaque unite spatiale.
#' Contrairement a LISA (autocorrelation locale), Gi* detecte
#' les zones de concentration de valeurs elevees/basses.
#'
#' @param sf_data objet sf avec geometries
#' @param col_var nom de la colonne numerique
#' @param col_id nom de la colonne identifiant
#' @param alpha seuil de significativite
#' @return tibble avec z-scores Gi*, p-values et classification
calc_getis_ord <- function(sf_data, col_var, col_id, alpha = 0.05) {
  if (!requireNamespace("spdep", quietly = TRUE))
    stop("Le package spdep est requis pour Getis-Ord Gi*")

  # Exclure les zones sans donnees (NA)
  vals_all <- sf_data[[col_var]]
  if (any(is.na(vals_all))) {
    sf_data <- sf_data[!is.na(vals_all), , drop = FALSE]
    message(sprintf("[GISTAR] %d zones sans données exclues de l'analyse",
                    sum(is.na(vals_all))))
  }
  if (nrow(sf_data) < 4)
    stop("Analyse Gi* impossible : moins de 4 zones avec données")

  vals <- sf_data[[col_var]]
  ids  <- sf_data[[col_id]]

  # Matrice de voisinage (include self pour Gi*)
  nb  <- spdep::poly2nb(sf_data, queen = TRUE)
  # Inclure le point lui-meme (Gi* vs Gi)
  nb_self <- spdep::include.self(nb)
  lw  <- spdep::nb2listw(nb_self, style = "B", zero.policy = TRUE)

  # Gi* = z-score standardise
  gi <- spdep::localG(vals, lw, zero.policy = TRUE)
  gi_z <- as.numeric(gi)
  # p-value bilaterale
  gi_p <- 2 * pnorm(abs(gi_z), lower.tail = FALSE)

  tibble(
    id        = ids,
    value     = vals,
    Gi_z      = round(gi_z, 4),
    p_value   = round(gi_p, 6),
    significant = gi_p < alpha,
    gi_class  = case_when(
      gi_p >= alpha            ~ "Non significatif",
      gi_z > 2.58              ~ "Hotspot (p < 0.01)",
      gi_z > 1.96              ~ "Hotspot (p < 0.05)",
      gi_z > 1.645             ~ "Hotspot (p < 0.10)",
      gi_z < -2.58             ~ "Coldspot (p < 0.01)",
      gi_z < -1.96             ~ "Coldspot (p < 0.05)",
      gi_z < -1.645            ~ "Coldspot (p < 0.10)",
      TRUE                     ~ "Non significatif"
    ),
    gi_color  = case_when(
      gi_z > 2.58  & gi_p < alpha ~ "#8b0000",
      gi_z > 1.96  & gi_p < alpha ~ "#d32f2f",
      gi_z > 1.645 & gi_p < alpha ~ "#ff6f00",
      gi_z < -2.58 & gi_p < alpha ~ "#004d40",
      gi_z < -1.96 & gi_p < alpha ~ "#2e7d32",
      gi_z < -1.645& gi_p < alpha ~ "#66bb6a",
      TRUE                        ~ "#cccccc"
    )
  )
}

#' Carte Leaflet Gi* hotspots
plot_getis_ord_map <- function(sf_data, gi_results, col_id) {
  sf_joined <- sf_data |>
    left_join(gi_results, by = setNames("id", col_id))

  pal <- colorFactor(
    palette = c("#8b0000", "#d32f2f", "#ff6f00", "#66bb6a", "#2e7d32",
                "#004d40", "#cccccc"),
    levels  = c("Hotspot (p < 0.01)", "Hotspot (p < 0.05)", "Hotspot (p < 0.10)",
                "Coldspot (p < 0.10)", "Coldspot (p < 0.05)", "Coldspot (p < 0.01)",
                "Non significatif"),
    na.color = "#cccccc"
  )

  popup_html <- paste0(
    "<div style='padding:10px;min-width:180px;'>",
    "<b>", sf_joined[[col_id]], "</b><br/>",
    ifelse(is.na(sf_joined$Gi_z),
      "<i>Zone sans données</i>",
      paste0(
      "Valeur : <b>", round(sf_joined$value, 1), "%</b><br/>",
      "Gi* z-score : <b>", round(sf_joined$Gi_z, 3), "</b><br/>",
      "p-value : ", format(sf_joined$p_value, digits = 4), "<br/>",
      "<span style='color:", sf_joined$gi_color, ";font-weight:700;'>",
      sf_joined$gi_class, "</span>")),
    "</div>"
  )

  leaflet(sf_joined) |>
    addProviderTiles("Esri.WorldGrayCanvas") |>
    addPolygons(
      fillColor   = ~pal(gi_class),
      fillOpacity = 0.75,
      weight      = 1.5, color = "white", opacity = 1,
      popup       = popup_html,
      highlightOptions = highlightOptions(weight = 3, color = "#333",
                                           bringToFront = TRUE)
    ) |>
    addLegend("bottomright", pal = pal,
              values = sf_joined$gi_class,
              title  = "Getis-Ord Gi*",
              opacity = 0.9) |>
    addScaleBar(position = "bottomleft")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 2. LISSAGE BAYESIEN EMPIRIQUE (EB) — Small Area Estimation
# ═══════════════════════════════════════════════════════════════════════════════

#' Lissage bayesien empirique des prevalences.
#' Stabilise les estimations des petites zones en les tirant
#' vers la moyenne globale (shrinkage).
#'
#' @param cases vecteur : nombre de cas par zone
#' @param totals vecteur : effectif total par zone
#' @return tibble avec prevalences brutes et lissees
calc_eb_smoothing <- function(cases, totals) {
  n <- length(cases)
  if (n < 2) stop("Au moins 2 zones requises")

  # Prevalences brutes
  p_raw <- cases / totals
  p_raw[totals == 0] <- NA

  # Moyenne globale ponderee
  p_global <- sum(cases, na.rm = TRUE) / sum(totals, na.rm = TRUE)

  # Variance inter-zones (methode des moments)
  s2_raw <- var(p_raw, na.rm = TRUE)
  # Variance d'echantillonnage moyenne
  v_bar  <- mean(p_global * (1 - p_global) / totals, na.rm = TRUE)
  # Variance "vraie" entre zones
  tau2   <- max(0, s2_raw - v_bar)

  # Poids de shrinkage (specifique a chaque zone)
  vi <- p_global * (1 - p_global) / totals
  wi <- tau2 / (tau2 + vi)
  wi[is.na(wi) | totals == 0] <- 0

  # Estimation EB (lissee)
  p_eb <- wi * p_raw + (1 - wi) * p_global
  p_eb[is.na(p_raw)] <- p_global

  # IC approximatif (variance EB)
  var_eb <- wi * vi
  se_eb  <- sqrt(var_eb)

  tibble(
    cas         = cases,
    total       = totals,
    prev_brute  = round(p_raw * 100, 2),
    prev_lissee = round(p_eb * 100, 2),
    poids_w     = round(wi, 3),
    shrinkage   = round((1 - wi) * 100, 1),
    ic_lo       = round(pmax(0, (p_eb - 1.96 * se_eb)) * 100, 2),
    ic_hi       = round(pmin(1, (p_eb + 1.96 * se_eb)) * 100, 2),
    p_global    = round(p_global * 100, 2)
  )
}

#' Graphique comparatif brut vs lisse
plot_eb_comparison <- function(eb_res, zone_names) {
  if (is.null(eb_res) || length(zone_names) != nrow(eb_res)) return(plotly_empty())
  df <- eb_res |>
    mutate(Zone = zone_names) |>
    tidyr::pivot_longer(c(prev_brute, prev_lissee),
                        names_to = "Type", values_to = "Prevalence") |>
    mutate(Type = dplyr::recode(Type,
      "prev_brute"  = "Brute",
      "prev_lissee" = "Liss\u00e9e (EB)"
    ))
  p <- ggplot(df, aes(y = fct_reorder(Zone, Prevalence), x = Prevalence,
                      color = Type, shape = Type)) +
    geom_point(size = 3.5, alpha = 0.85) +
    geom_vline(xintercept = eb_res$p_global[1], linetype = "dashed",
               color = "gray50") +
    annotate("text", x = eb_res$p_global[1] + 0.5, y = 1,
             label = paste0("Moy. ", eb_res$p_global[1], "%"),
             color = "gray40", size = 3, hjust = 0) +
    scale_color_manual(values = c("Brute" = "#c9161d", "Liss\u00e9e (EB)" = "#008DC9")) +
    labs(y = "", x = "Pr\u00e9valence (%)",
         title = "Lissage Bay\u00e9sien Empirique",
         subtitle = "Les petites zones sont tir\u00e9es vers la moyenne globale",
         color = "", shape = "") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 3. CARTE IPC/CH — Phases officielles
# ═══════════════════════════════════════════════════════════════════════════════

# Couleurs officielles IPC/CH
IPC_COLORS <- c(
  "Phase 1" = "#c6ffc7",  # Minimale (vert clair)
  "Phase 2" = "#ffe252",  # Stress (jaune)
  "Phase 3" = "#e88400",  # Crise (orange)
  "Phase 4" = "#c9161d",  # Urgence (rouge)
  "Phase 5" = "#640000"   # Famine (marron fonce)
)

IPC_LABELS <- c(
  "Phase 1" = "Phase 1 \u2014 Minimale",
  "Phase 2" = "Phase 2 \u2014 Stress\u00e9e",
  "Phase 3" = "Phase 3 \u2014 Crise",
  "Phase 4" = "Phase 4 \u2014 Urgence",
  "Phase 5" = "Phase 5 \u2014 Famine"
)

#' Determine la phase IPC dominante par zone geographique.
#' Utilise : FCS (seuil 28/42), rCSI (seuil 4/19), HHS (seuil 1/3), prevalence MUAC GAM.
#'
#' @param d data.frame avec indicateurs par individu
#' @param col_geo colonne de regroupement geographique
#' @param col_fcs colonne FCS score
#' @param col_rcsi colonne rCSI score
#' @param col_hhs colonne HHS score
#' @param col_muac colonne MUAC (mm)
calc_ipc_by_zone <- function(d, col_geo, col_fcs = "FCS_score",
                              col_rcsi = "rCSI_score", col_hhs = "HHS_score",
                              col_muac = "muac") {
  # [C1] Vérification : avertir si aucune colonne indicateur trouvée
  found <- c(col_fcs %in% names(d), col_rcsi %in% names(d),
             col_hhs %in% names(d), col_muac %in% names(d))
  if (sum(found) == 0) {
    warning("Aucune colonne indicateur IPC trouv\u00e9e (", col_fcs, ", ", col_rcsi,
            ", ", col_hhs, ", ", col_muac, "). V\u00e9rifiez les noms de colonnes. ",
            "Toutes les phases seront class\u00e9es Phase 1 par d\u00e9faut.")
  }

  # Phase individuelle
  d_ipc <- d |>
    mutate(
      .fcs  = if (col_fcs %in% names(d))  .data[[col_fcs]]  else NA_real_,
      .rcsi = if (col_rcsi %in% names(d)) .data[[col_rcsi]] else NA_real_,
      .hhs  = if (col_hhs %in% names(d))  .data[[col_hhs]]  else NA_real_,
      .muac = if (col_muac %in% names(d)) .data[[col_muac]] else NA_real_,
      ipc_ind = case_when(
        .hhs >= 4 | .muac < 115                     ~ 5L,
        .hhs >= 3 | .fcs < 21 | .rcsi >= 19         ~ 4L,
        .hhs >= 2 | .fcs < 28 | .rcsi >= 10 | .muac < 125 ~ 3L,
        .fcs < 42 | .rcsi >= 4                       ~ 2L,
        TRUE                                         ~ 1L
      )
    )

  # Phase dominante par zone (mode)
  d_ipc |>
    filter(!is.na(.data[[col_geo]])) |>
    group_by(Zone = .data[[col_geo]]) |>
    summarise(
      n = n(),
      phase_moy     = round(mean(ipc_ind, na.rm = TRUE), 2),
      phase_dominante = {
        tbl <- table(ipc_ind)
        as.integer(names(tbl)[which.max(tbl)])
      },
      pct_phase3plus = round(mean(ipc_ind >= 3, na.rm = TRUE) * 100, 1),
      pct_phase4plus = round(mean(ipc_ind >= 4, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) |>
    mutate(
      phase_label = paste0("Phase ", phase_dominante),
      phase_color = IPC_COLORS[phase_label]
    )
}

#' Carte Leaflet IPC/CH
plot_ipc_map <- function(sf_data, ipc_zones, col_id) {
  sf_joined <- sf_data |>
    left_join(ipc_zones, by = setNames("Zone", col_id))

  pal <- colorFactor(
    palette = unname(IPC_COLORS),
    levels  = names(IPC_COLORS),
    na.color = "#cccccc"
  )

  popup_html <- paste0(
    "<div style='padding:10px;min-width:200px;'>",
    "<b style='font-size:14px;'>", sf_joined[[col_id]], "</b><br/>",
    "<span style='background:", sf_joined$phase_color,
    ";color:", ifelse(sf_joined$phase_dominante >= 3, "white", "black"),
    ";padding:3px 8px;border-radius:4px;font-weight:700;'>",
    IPC_LABELS[sf_joined$phase_label], "</span><br/><br/>",
    "Phase moyenne : <b>", sf_joined$phase_moy, "</b><br/>",
    "% Phase 3+ : <b style='color:#e88400;'>", sf_joined$pct_phase3plus, "%</b><br/>",
    "% Phase 4+ : <b style='color:#c9161d;'>", sf_joined$pct_phase4plus, "%</b><br/>",
    "n = ", sf_joined$n, "</div>"
  )

  leaflet(sf_joined) |>
    addProviderTiles("Esri.WorldGrayCanvas") |>
    addPolygons(
      fillColor   = ~pal(phase_label),
      fillOpacity = 0.8,
      weight      = 2, color = "white", opacity = 1,
      popup       = popup_html,
      highlightOptions = highlightOptions(weight = 3, color = "#333",
                                           bringToFront = TRUE)
    ) |>
    addLegend("bottomright", pal = pal,
              values  = sf_joined$phase_label,
              title   = "Classification IPC/CH",
              opacity = 0.9,
              labFormat = labelFormat(transform = function(x) IPC_LABELS[x])) |>
    addScaleBar(position = "bottomleft")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 4. FOREST PLOT PREVALENCES + IC PAR REGION
# ═══════════════════════════════════════════════════════════════════════════════

#' Forest plot des prevalences par region avec IC 95% (Wilson)
#' Standard SMART pour les rapports de prevalence
#'
#' @param d data.frame individuel
#' @param col_geo colonne geographique
#' @param col_outcome colonne binaire (0/1)
#' @param label_outcome libelle de l'indicateur
calc_forest_data <- function(d, col_geo, col_outcome, label_outcome = "Pr\u00e9valence") {
  d |>
    filter(!is.na(.data[[col_geo]]), !is.na(.data[[col_outcome]])) |>
    group_by(Zone = .data[[col_geo]]) |>
    summarise(
      n   = n(),
      cas = sum(.data[[col_outcome]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      prev = round(cas / n * 100, 2),
      # Wilson IC 95%
      ic_lo = round(binom::binom.confint(cas, n, method = "wilson")$lower * 100, 2),
      ic_hi = round(binom::binom.confint(cas, n, method = "wilson")$upper * 100, 2),
      # Seuils OMS
      oms_class = case_when(
        prev >= 15 ~ "Critique (\u2265 15%)",
        prev >= 10 ~ "S\u00e9rieux (10-14.9%)",
        prev >= 5  ~ "Alerte (5-9.9%)",
        TRUE       ~ "Acceptable (< 5%)"
      ),
      oms_color = case_when(
        prev >= 15 ~ "#c9161d",
        prev >= 10 ~ "#e88400",
        prev >= 5  ~ "#f9c734",
        TRUE       ~ "#4dac26"
      ),
      indicateur = label_outcome
    )
}

plot_forest <- function(forest_data) {
  if (is.null(forest_data) || nrow(forest_data) == 0) return(plotly_empty())

  # Moyenne nationale ponderee
  nat <- with(forest_data, round(sum(cas) / sum(n) * 100, 2))

  p <- ggplot(forest_data, aes(y = fct_reorder(Zone, prev), x = prev)) +
    geom_vline(xintercept = nat, linetype = "dashed", color = "gray50") +
    # IC
    geom_errorbarh(aes(xmin = ic_lo, xmax = ic_hi), height = 0.3,
                   color = "gray40", linewidth = 0.6) +
    # Points
    geom_point(aes(color = oms_class, size = n), alpha = 0.85) +
    scale_color_manual(values = c(
      "Critique (\u2265 15%)" = "#c9161d",
      "S\u00e9rieux (10-14.9%)"  = "#e88400",
      "Alerte (5-9.9%)"       = "#f9c734",
      "Acceptable (< 5%)"     = "#4dac26"
    )) +
    scale_size_continuous(range = c(3, 8), name = "n") +
    annotate("text", x = nat + 0.5, y = 0.7,
             label = paste0("National : ", nat, "%"),
             color = "gray40", size = 3, hjust = 0) +
    labs(y = "", x = "Pr\u00e9valence (%) avec IC 95% Wilson",
         title = paste0("Forest plot \u2014 ", forest_data$indicateur[1]),
         color = "Classification OMS") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          legend.box = "vertical")
  ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.2))
}

# ═══════════════════════════════════════════════════════════════════════════════
# 5. INDICES D'INEGALITE SPATIALE
# ═══════════════════════════════════════════════════════════════════════════════

#' Calcul du coefficient de Gini spatial (inegalite entre zones)
calc_spatial_gini <- function(values) {
  v <- sort(values[!is.na(values)])
  n <- length(v)
  if (n < 2) return(NA_real_)
  numerator   <- 2 * sum(seq_along(v) * v)
  denominator <- n * sum(v)
  gini <- (numerator / denominator) - (n + 1) / n
  round(gini, 4)
}

#' Indice de Theil (decomposable en intra/inter)
calc_theil_index <- function(values) {
  v <- values[!is.na(values)]
  n <- length(v)
  if (n < 2) return(NA_real_)
  mu <- mean(v)
  if (mu <= 0) return(NA_real_)
  theil <- mean((v / mu) * log(v / mu), na.rm = TRUE)
  round(theil, 4)
}

#' Coefficient de variation (CV)
calc_spatial_cv <- function(values) {
  v <- values[!is.na(values)]
  if (length(v) < 2 || mean(v) == 0) return(NA_real_)
  round(sd(v) / mean(v) * 100, 2)
}

#' Ratio max/min
calc_range_ratio <- function(values) {
  v <- values[!is.na(values) & values > 0]
  if (length(v) < 2) return(NA_real_)
  round(max(v) / min(v), 2)
}

#' Tableau complet des indices d'inegalite spatiale
calc_spatial_inequality <- function(prev_by_zone) {
  vals <- prev_by_zone[!is.na(prev_by_zone)]
  tibble(
    Indice   = c("Gini", "Theil", "CV (%)", "Ratio Max/Min",
                 "Ecart absolu max", "Moyenne", "Mediane", "Ecart-type"),
    Valeur   = c(
      calc_spatial_gini(vals),
      calc_theil_index(vals),
      calc_spatial_cv(vals),
      calc_range_ratio(vals),
      round(max(vals) - min(vals), 2),
      round(mean(vals), 2),
      round(median(vals), 2),
      round(sd(vals), 2)
    ),
    Interpretation = c(
      case_when(
        calc_spatial_gini(vals) < 0.2  ~ "Faible in\u00e9galit\u00e9",
        calc_spatial_gini(vals) < 0.35 ~ "In\u00e9galit\u00e9 mod\u00e9r\u00e9e",
        TRUE                           ~ "In\u00e9galit\u00e9 \u00e9lev\u00e9e"
      ),
      case_when(
        calc_theil_index(vals) < 0.1 ~ "Homog\u00e8ne",
        calc_theil_index(vals) < 0.3 ~ "H\u00e9t\u00e9rog\u00e8ne",
        TRUE                         ~ "Tr\u00e8s h\u00e9t\u00e9rog\u00e8ne"
      ),
      paste0(round(calc_spatial_cv(vals), 0), "% de variation"),
      paste0("Zone max = ", round(calc_range_ratio(vals), 1), "x zone min"),
      paste0(round(max(vals) - min(vals), 1), " points de %"),
      "", "", ""
    )
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# 6. PROFIL REGIONAL (Radar multi-indicateurs)
# ═══════════════════════════════════════════════════════════════════════════════

#' Compare un profil regional a la moyenne nationale
#' @param agg_data data.frame agrege par zone avec colonnes indicateurs
#' @param zone_name nom de la zone a profiler
#' @param indicators vecteur de noms de colonnes indicateurs
#' @param indicator_labels libelles (meme longueur que indicators)
plot_regional_profile <- function(agg_data, zone_name, indicators,
                                  indicator_labels = NULL) {
  if (is.null(indicator_labels)) indicator_labels <- indicators
  if (!zone_name %in% agg_data$Zone) return(plotly_empty())

  region_vals  <- as.numeric(agg_data[agg_data$Zone == zone_name, indicators])
  national_vals <- colMeans(agg_data[, indicators, drop = FALSE], na.rm = TRUE)

  # Normaliser 0-100 pour le radar
  maxv <- pmax(region_vals, national_vals, na.rm = TRUE) * 1.2
  maxv[maxv == 0] <- 1
  region_norm  <- round(region_vals / maxv * 100, 1)
  national_norm <- round(national_vals / maxv * 100, 1)

  fig <- plot_ly(type = "scatterpolar", fill = "toself") |>
    add_trace(
      r = c(region_norm, region_norm[1]),
      theta = c(indicator_labels, indicator_labels[1]),
      name = zone_name,
      line = list(color = "#c9161d", width = 2),
      fillcolor = "rgba(201,22,29,0.15)"
    ) |>
    add_trace(
      r = c(national_norm, national_norm[1]),
      theta = c(indicator_labels, indicator_labels[1]),
      name = "Moyenne nationale",
      line = list(color = "#008DC9", width = 2, dash = "dash"),
      fillcolor = "rgba(0,141,201,0.08)"
    ) |>
    layout(
      title = list(text = paste0("Profil nutritionnel \u2014 ", zone_name),
                   font = list(size = 14)),
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
      legend = list(orientation = "h", y = -0.15),
      showlegend = TRUE
    )
  fig
}

# ═══════════════════════════════════════════════════════════════════════════════
# 7. COMPARAISON TEMPORELLE (multi-rounds)
# ═══════════════════════════════════════════════════════════════════════════════

#' Compare les prevalences entre rounds d'enquete
#' @param d data.frame avec col_round, col_geo, col_outcome
calc_temporal_comparison <- function(d, col_geo, col_outcome, col_round) {
  d |>
    filter(!is.na(.data[[col_geo]]),
           !is.na(.data[[col_outcome]]),
           !is.na(.data[[col_round]])) |>
    group_by(Zone = .data[[col_geo]], Round = .data[[col_round]]) |>
    summarise(
      n    = n(),
      cas  = sum(.data[[col_outcome]], na.rm = TRUE),
      prev = round(cas / n * 100, 2),
      .groups = "drop"
    ) |>
    group_by(Zone) |>
    arrange(Round) |>
    mutate(
      variation     = prev - lag(prev),
      variation_pct = round(variation / lag(prev) * 100, 1),
      tendance      = case_when(
        is.na(variation) ~ "Baseline",
        variation > 3    ~ "\u2191 D\u00e9t\u00e9rioration",
        variation < -3   ~ "\u2193 Am\u00e9lioration",
        TRUE             ~ "\u2194 Stable"
      )
    ) |>
    ungroup()
}

plot_temporal_heatmap <- function(temp_data) {
  if (is.null(temp_data) || nrow(temp_data) == 0) return(plotly_empty())
  p <- ggplot(temp_data, aes(x = factor(Round), y = fct_reorder(Zone, prev),
                              fill = prev)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = paste0(prev, "%")), size = 3, fontface = "bold") +
    scale_fill_gradientn(
      colors = c("#4dac26", "#f9c734", "#e88400", "#c9161d", "#640000"),
      name = "Pr\u00e9valence (%)"
    ) +
    labs(x = "Round d'enqu\u00eate", y = "",
         title = "\u00c9volution temporelle par zone") +
    theme_minimal(base_size = 11)
  ggplotly(p)
}

cat("module_carto_expert.R charg\u00e9 (Gi* | EB Smoothing | IPC Map | Forest | In\u00e9galit\u00e9 | Profil | Temporel)\n")
