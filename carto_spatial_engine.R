# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  carto_spatial_engine.R — Moteur de Cartographie Spatiale Expert             ║
# ║  Moran's I · LISA · Hotspot Detection · Risk Stratification                 ║
# ║  Dépasse VAM/ENA/NutriDash en intelligence épidémiologique spatiale        ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(sf)
if (requireNamespace("spdep", quietly = TRUE)) library(spdep)
library(tidyverse)
library(scales)

# ══════════════════════════════════════════════════════════════════════════════
# 1. SPATIAL AUTOCORRELATION — Moran's I Global
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer l'autocorrélation spatiale globale (Moran's I)
#'
#' Mesure le degré de clustering spatial dans les données
#' I > 0.3 = clustering fort (hotspots/coldspots)
#' I ≈ 0 = aléatoire
#' I < 0 = dispersé (alternating)
#'
#' @param sf_data Objet sf avec géométrie + indicateur
#' @param col_indicator Nom de la colonne indicateur (GAM, HAZ, etc.)
#' @return Liste avec I, p-value, interprétation
calc_morans_i <- function(sf_data, col_indicator = "GAM") {

  # Vérifier colonnes
  if (!col_indicator %in% names(sf_data)) {
    return(list(I = NA, p.value = NA, error = "Indicateur non trouvé"))
  }

  # Extraire les valeurs (nettoyer NA)
  vals <- sf_data[[col_indicator]]
  valid_idx <- !is.na(vals)

  if (sum(valid_idx) < 3) {
    return(list(I = NA, p.value = NA, error = "Données insuffisantes"))
  }

  sf_clean <- sf_data[valid_idx, ]
  vals_clean <- vals[valid_idx]

  # Créer la matrice de poids (rook adjacency = bord partagé)
  suppressWarnings({
    neighbors <- st_touches(sf_clean, sparse = TRUE)
    weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
  })

  # Calculer Moran's I
  tryCatch({
    morans_result <- moran.test(vals_clean, weights, zero.policy = TRUE)

    I <- morans_result$estimate[1]
    p_value <- morans_result$p.value
    expected_I <- morans_result$estimate[2]

    # Interprétation
    if (p_value < 0.05) {
      if (I > 0.3) {
        interpretation <- "Clustering FORT — Hotspots/Coldspots significatifs"
        severity <- "Haute"
      } else if (I > 0.1) {
        interpretation <- "Clustering MODÉRÉ — Zones similaires adjacentes"
        severity <- "Modérée"
      } else if (I > 0) {
        interpretation <- "Clustering LÉGER — Pattern faiblement clustérisé"
        severity <- "Basse"
      } else if (I > -0.1) {
        interpretation <- "Aléatoire — Pas d'autocorrélation spatiale"
        severity <- "Aucune"
      } else {
        interpretation <- "DISPERSED — Données alternantes (zones différentes adjacentes)"
        severity <- "Inversée"
      }
    } else {
      interpretation <- "Non-significatif — Pas de pattern spatial détecté"
      severity <- "Aucune"
    }

    list(
      I = I,
      p.value = p_value,
      expected_I = expected_I,
      interpretation = interpretation,
      severity = severity,
      significant = p_value < 0.05,
      n_zones = nrow(sf_clean),
      n_neighbors_avg = mean(sapply(neighbors, length))
    )
  }, error = function(e) {
    list(I = NA, p.value = NA, error = as.character(e))
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. LOCAL INDICATOR OF SPATIAL ASSOCIATION — LISA
# ══════════════════════════════════════════════════════════════════════════════

#' Calculer l'indicateur spatial local (LISA)
#'
#' Classifie chaque zone comme HH, LL, HL, LH ou non-significatif
#' HH = Hotspot (zone haute + voisins hauts)
#' LL = Coldspot (zone basse + voisins bas)
#' HL = Anomalie (zone haute isolée)
#' LH = Anomalie (zone basse isolée)
#'
#' @param sf_data Objet sf
#' @param col_indicator Nom de la colonne
#' @param alpha Seuil de significativité (défaut 0.05)
#' @return sf_data avec colonnes LISA_class et LISA_pvalue
calc_lisa <- function(sf_data, col_indicator = "GAM", alpha = 0.05) {

  if (!col_indicator %in% names(sf_data)) {
    sf_data$LISA_class <- NA
    sf_data$LISA_pvalue <- NA
    return(sf_data)
  }

  vals <- sf_data[[col_indicator]]
  valid_idx <- !is.na(vals)

  if (sum(valid_idx) < 3) {
    sf_data$LISA_class <- NA
    sf_data$LISA_pvalue <- NA
    return(sf_data)
  }

  sf_clean <- sf_data[valid_idx, ]
  vals_clean <- vals[valid_idx]

  # Standardiser
  vals_std <- (vals_clean - mean(vals_clean, na.rm = TRUE)) / sd(vals_clean, na.rm = TRUE)

  # Matrice de poids
  suppressWarnings({
    neighbors <- st_touches(sf_clean, sparse = TRUE)
    weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
  })

  # Calculer LISA
  tryCatch({
    lisa_result <- localmoran(vals_std, weights, zero.policy = TRUE)

    # Extraire Ii (local indicator) et p-value
    lisa_i <- lisa_result[, 1]
    lisa_pval <- lisa_result[, 5]

    # Classer
    lisa_class <- rep("Non-sig", length(lisa_i))

    significant <- lisa_pval < alpha

    # HH : valeur haute, moyenne voisins positive
    lisa_class[significant & vals_std > 0 & lisa_i > 0] <- "HH (Hotspot)"

    # LL : valeur basse, moyenne voisins négative
    lisa_class[significant & vals_std < 0 & lisa_i > 0] <- "LL (Coldspot)"

    # HL : valeur haute, moyenne voisins négative (anomalie)
    lisa_class[significant & vals_std > 0 & lisa_i < 0] <- "HL (Anomalie haute)"

    # LH : valeur basse, moyenne voisins positive (anomalie)
    lisa_class[significant & vals_std < 0 & lisa_i < 0] <- "LH (Anomalie basse)"

    # Ajouter au sf_data original
    result_df <- tibble(
      LISA_class = NA_character_,
      LISA_pvalue = NA_real_,
      LISA_i = NA_real_
    )

    result_df$LISA_class[valid_idx] <- lisa_class
    result_df$LISA_pvalue[valid_idx] <- lisa_pval
    result_df$LISA_i[valid_idx] <- lisa_i

    sf_data <- cbind(sf_data, result_df)

    sf_data
  }, error = function(e) {
    sf_data$LISA_class <- NA
    sf_data$LISA_pvalue <- NA
    sf_data$LISA_i <- NA
    sf_data
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. HOTSPOT & COLDSPOT DETECTION
# ══════════════════════════════════════════════════════════════════════════════

#' Détecter les hotspots et coldspots
#'
#' @param sf_data sf avec LISA_class (résultat calc_lisa)
#' @return Liste avec hotspots, coldspots, anomalies
detect_hotspots_coldspots <- function(sf_data) {

  if (!"LISA_class" %in% names(sf_data)) {
    return(list(
      hotspots = character(0),
      coldspots = character(0),
      anomalies = character(0),
      summary = NULL
    ))
  }

  sf_data <- sf_data |> mutate(
    LISA_class = replace_na(LISA_class, "Non-sig")
  )

  hotspots <- sf_data |> filter(LISA_class == "HH (Hotspot)") |> pull(NAME_2) |> unique()
  coldspots <- sf_data |> filter(LISA_class == "LL (Coldspot)") |> pull(NAME_2) |> unique()
  anomalies_hh <- sf_data |> filter(LISA_class == "HL (Anomalie haute)") |> pull(NAME_2) |> unique()
  anomalies_lh <- sf_data |> filter(LISA_class == "LH (Anomalie basse)") |> pull(NAME_2) |> unique()

  list(
    hotspots = hotspots,
    coldspots = coldspots,
    anomalies_high = anomalies_hh,
    anomalies_low = anomalies_lh,
    summary = tibble(
      Type = c("Hotspots", "Coldspots", "Anomalies Hautes", "Anomalies Basses"),
      Count = c(length(hotspots), length(coldspots), length(anomalies_hh), length(anomalies_lh))
    ),
    total = length(hotspots) + length(coldspots) + length(anomalies_hh) + length(anomalies_lh)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. SPATIAL RISK STRATIFICATION
# ══════════════════════════════════════════════════════════════════════════════

#' Stratifier les zones par risque d'intervention
#'
#' Combine : magnitude du problème + clustering spatial + accessibilité
#'
#' @param sf_data sf avec indicateur et LISA_class
#' @param col_indicator Nom de l'indicateur
#' @param col_capital Nom colonne distance to capital (optionnel)
#' @param threshold_seuil OMS threshold (ex: 15 pour GAM)
#' @return sf_data avec colonne risk_level
stratify_spatial_risk <- function(sf_data, col_indicator = "GAM",
                                   col_capital = NULL, threshold_seuil = 15) {

  if (!col_indicator %in% names(sf_data)) {
    sf_data$risk_level <- NA
    sf_data$risk_score <- NA
    return(sf_data)
  }

  sf_data <- sf_data |> mutate(
    LISA_class = replace_na(LISA_class, "Non-sig"),
    indicateur = !!rlang::sym(col_indicator)
  )

  # Composante 1 : Magnitude (0-40 points)
  magnitude_score <- (sf_data$indicateur / threshold_seuil) * 40
  magnitude_score <- pmin(magnitude_score, 40) # Cap à 40

  # Composante 2 : Clustering spatial (0-40 points)
  clustering_score <- case_when(
    sf_data$LISA_class %in% c("HH (Hotspot)", "LL (Coldspot)") ~ 40,
    sf_data$LISA_class %in% c("HL (Anomalie haute)", "LH (Anomalie basse)") ~ 20,
    TRUE ~ 0
  )

  # Composante 3 : Accessibilité (0-20 points, optionnel)
  accessibility_score <- rep(20, nrow(sf_data)) # Par défaut: max points
  if (!is.null(col_capital) && col_capital %in% names(sf_data)) {
    # Plus loin de la capitale = moins accessible = plus de points
    dist <- sf_data[[col_capital]]
    accessibility_score <- (dist / max(dist, na.rm = TRUE)) * 20
  }

  # Score total (0-100)
  sf_data$risk_score <- magnitude_score + clustering_score + accessibility_score

  # Classification
  sf_data$risk_level <- case_when(
    sf_data$risk_score >= 70 ~ "HAUTE",
    sf_data$risk_score >= 40 ~ "MOYENNE",
    TRUE ~ "BASSE"
  )

  sf_data
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. SPATIAL NARRATIVES
# ══════════════════════════════════════════════════════════════════════════════

#' Générer narratif clinique spatial pour une zone
#'
#' @param zone_name Nom de la zone
#' @param zone_value Valeur de l'indicateur
#' @param zone_lisa LISA_class
#' @param threshold OMS threshold
#' @param risk_level Risk stratification
#' @param hotspot_neighbors Zones hotspot voisines
#' @return Texte narratif spatial
gen_spatial_narrative <- function(zone_name, zone_value, zone_lisa, threshold = 15,
                                  risk_level = "MOYENNE", hotspot_neighbors = NULL) {

  # Sévérité
  if (zone_value >= threshold * 2) {
    sev_text <- "CRITIQUE"
    sev_color <- "🔴"
  } else if (zone_value >= threshold * 1.5) {
    sev_text <- "SÉRIEUX"
    sev_color <- "🟠"
  } else if (zone_value >= threshold) {
    sev_text <- "ALERTE"
    sev_color <- "🟡"
  } else {
    sev_text <- "ACCEPTABLE"
    sev_color <- "🟢"
  }

  # Type spatial
  spatial_text <- case_when(
    zone_lisa == "HH (Hotspot)" ~ "fait partie d'un **HOTSPOT** (voisins aussi affectés)",
    zone_lisa == "LL (Coldspot)" ~ "fait partie d'un **COLDSPOT** (situation contrôlée)",
    zone_lisa == "HL (Anomalie haute)" ~ "est une **ANOMALIE ISOLÉE** (voisins non-affectés)",
    zone_lisa == "LH (Anomalie basse)" ~ "est une **ANOMALIE INVERSÉE** (voisins affectés)",
    TRUE ~ "a une situation spatiale non-significative"
  )

  # Risque
  risk_text <- case_when(
    risk_level == "HAUTE" ~ "PRIORITÉ MAXIMALE : Intervention urgente + coordination régionale",
    risk_level == "MOYENNE" ~ "PRIORITÉ MODÉRÉE : Intervention requise, peut coordonner avec voisins",
    TRUE ~ "PRIORITÉ BASSE : Surveillance suffisante"
  )

  # Recommandation
  rec_text <- case_when(
    zone_lisa == "HH (Hotspot)" ~ {
      if (!is.null(hotspot_neighbors)) {
        paste0("Approche régionale coordonnée avec : ", paste(hotspot_neighbors, collapse = ", "))
      } else {
        "Approche régionale coordonnée avec zones adjacentes"
      }
    },
    zone_lisa == "HL (Anomalie haute)" ~ "Valider données. Investigation rapide recommandée.",
    TRUE ~ "Continuer surveillance standardisée"
  )

  paste0(
    sev_color, " **", zone_name, "** : ", zone_value, "% (",  sev_text, ")\n\n",
    "**Contexte spatial** : Cette zone ", spatial_text, ".\n\n",
    "**Classification de risque** : ", risk_level, "\n\n",
    "**Stratégie** : ", risk_text, "\n\n",
    "**Action recommandée** : ", rec_text
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. SPATIAL ANOMALY DETECTION
# ══════════════════════════════════════════════════════════════════════════════

#' Détecter les anomalies spatiales (zones aberrantes)
#'
#' @param sf_data sf_data avec LISA
#' @param col_indicator Nom indicateur
#' @return Tibble des anomalies avec explication
detect_spatial_anomalies <- function(sf_data, col_indicator = "GAM") {

  anomalies <- sf_data |>
    filter(LISA_class %in% c("HL (Anomalie haute)", "LH (Anomalie basse)")) |>
    select(NAME_2, !!rlang::sym(col_indicator), LISA_class, LISA_pvalue)

  if (nrow(anomalies) == 0) {
    return(tibble(
      Zone = character(0),
      Valeur = numeric(0),
      Type = character(0),
      Explication = character(0)
    ))
  }

  anomalies |>
    mutate(
      Explication = case_when(
        LISA_class == "HL (Anomalie haute)" ~ 
          paste0("Zone haute isolée (", round(!!rlang::sym(col_indicator), 1), 
                 "%) mais voisins non-affectés. Vérifier données."),
        LISA_class == "LH (Anomalie basse)" ~
          paste0("Zone basse (", round(!!rlang::sym(col_indicator), 1),
                 "%) mais voisins affectés. Investigate transmission.")
      ),
      Priorité = "VERIFICATION RAPIDE"
    ) |>
    select(Zone = NAME_2, Valeur = !!rlang::sym(col_indicator),
           Type = LISA_class, Explication, Priorité)
}

# ══════════════════════════════════════════════════════════════════════════════
# 7. SPATIAL COMPARISON
# ══════════════════════════════════════════════════════════════════════════════

#' Comparer deux régions spatiallement
#'
#' @param sf_data1 Région 1
#' @param sf_data2 Région 2
#' @param col_indicator Indicateur à comparer
#' @param name1 Nom région 1
#' @param name2 Nom région 2
#' @return Tableau comparatif
compare_regions_spatial <- function(sf_data1, sf_data2, col_indicator = "GAM",
                                     name1 = "Région 1", name2 = "Région 2") {

  morans1 <- calc_morans_i(sf_data1, col_indicator)
  morans2 <- calc_morans_i(sf_data2, col_indicator)

  hotspots1 <- detect_hotspots_coldspots(sf_data1)$summary
  hotspots2 <- detect_hotspots_coldspots(sf_data2)$summary

  tibble(
    Métrique = c("Moran's I", "P-value", "Clustering", "Hotspots", "Coldspots"),
    !!name1 := c(
      round(morans1$I, 3),
      round(morans1$p.value, 4),
      morans1$interpretation,
      hotspots1$Count[1],
      hotspots1$Count[2]
    ),
    !!name2 := c(
      round(morans2$I, 3),
      round(morans2$p.value, 4),
      morans2$interpretation,
      hotspots2$Count[1],
      hotspots2$Count[2]
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 8. SPATIAL REPORT GENERATION
# ══════════════════════════════════════════════════════════════════════════════

#' Générer rapport spatial complet
#'
#' @param sf_data sf_data avec tous les analyses
#' @param col_indicator Indicateur
#' @return Rapport Markdown complet
gen_spatial_report <- function(sf_data, col_indicator = "GAM") {

  morans <- calc_morans_i(sf_data, col_indicator)
  hotspots <- detect_hotspots_coldspots(sf_data)
  anomalies <- detect_spatial_anomalies(sf_data, col_indicator)

  rapport <- paste0(
    "# Rapport d'Analyse Spatiale\n\n",
    "**Date:** ", Sys.Date(), "\n",
    "**Indicateur:** ", col_indicator, "\n\n",

    "## 1. Autocorrélation Spatiale Globale (Moran's I)\n\n",
    "**I =", round(morans$I, 3), "**\n",
    "**p-value =", round(morans$p.value, 4), "**\n\n",
    morans$interpretation, "\n\n",
    "**Interprétation:** ",
    if (morans$I > 0.3) {
      "Clustering FORT : Les zones similaires sont groupées géographiquement. "
    } else if (morans$I > 0.1) {
      "Clustering MODÉRÉ : Pattern spatial significatif. "
    } else {
      "Pattern spatial faible ou aléatoire. "
    },
    if (!morans$significant) {
      "Cependant, le pattern n'est pas statistiquement significatif (p > 0.05)."
    } else {
      "Le pattern est statistiquement significatif (p < 0.05)."
    },
    "\n\n",

    "## 2. Hotspots & Coldspots Détectés\n\n",
    "**Hotspots (HH):** ", length(hotspots$hotspots), " zones\n",
    if (length(hotspots$hotspots) > 0) {
      paste0("  - ", paste(hotspots$hotspots, collapse = "\n  - "), "\n")
    },
    "\n**Coldspots (LL):** ", length(hotspots$coldspots), " zones\n",
    if (length(hotspots$coldspots) > 0) {
      paste0("  - ", paste(hotspots$coldspots, collapse = "\n  - "), "\n")
    },
    "\n**Anomalies Détectées:** ", length(hotspots$anomalies_high) + length(hotspots$anomalies_low), " zones\n",
    if (length(hotspots$anomalies_high) > 0) {
      paste0("  - Hautes isolées : ", paste(hotspots$anomalies_high, collapse = ", "), "\n")
    },
    if (length(hotspots$anomalies_low) > 0) {
      paste0("  - Basses isolées : ", paste(hotspots$anomalies_low, collapse = ", "), "\n")
    },
    "\n\n",

    "## 3. Anomalies Spatiales\n\n",
    if (nrow(anomalies) > 0) {
      paste0(
        "**ATTENTION:** ", nrow(anomalies), " zone(s) aberrante(s) détectée(s).\n",
        "Vérifier qualité des données pour ces zones.\n\n"
      )
    } else {
      "Aucune anomalie spatiale majeure détectée.\n\n"
    },

    "## 4. Recommandations d'Intervention\n\n",
    "1. **Hotspots** : Approche régionale coordonnée (ressources partagées)\n",
    "2. **Coldspots** : Maintenir surveillance, peut servir de comparateur\n",
    "3. **Anomalies** : Validation données + investigation urgente\n",
    "4. **Zones Non-Significatives** : Monitoring standard\n\n",

    "---\n",
    "**Rapport généré par analyse spatiale OMS**\n"
  )

  rapport
}
