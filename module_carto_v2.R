# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_carto_v2.R — Cartographie complète Cameroun + Afrique centrale     ║
# ║  GADM L1/L2/L3 · Choroplèthe · Cercles · GPS · Heatmap · Indicateurs OMS  ║
# ║  + MOTEURS EXPERTS : Jenks, LISA, Prédiction, Priorisation                 ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# Source les 4 moteurs experts
source("carto_expert_engine.R")
tryCatch(source("spatial_analysis_engine.R"), error = function(e) message("spdep non disponible"))
source("carto_intelligence_engine.R")
source("carto_predictive_engine.R")
source("module_carto_expert.R")

library(sf)
library(leaflet)
tryCatch(library(leaflet.extras), error = function(e) message("⚠️ leaflet.extras non disponible"))
library(plotly)
library(ggplot2)
library(dplyr)
library(purrr)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)
if (requireNamespace("spdep", quietly = TRUE)) library(spdep)
library(classInt)
library(binom)
tryCatch(
  if (requireNamespace("data.table", quietly = TRUE)) library(data.table),
  error = function(e) message("⚠️ data.table non chargé : ", e$message)
)
tryCatch(library(caret), error = function(e) message("⚠️ caret non disponible"))

# ── URLs GADM Cameroun ────────────────────────────────────────────────────────
GADM_URLS <- list(
  CMR_L1 = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_1.json",
  CMR_L2 = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_2.json",
  CMR_L3 = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_3.json"
)

# Noms standardisés des colonnes GADM
GADM_NAMES <- list(
  L1 = "NAME_1",   # Région
  L2 = "NAME_2",   # Département
  L3 = "NAME_3"    # Arrondissement
)

# ── Cache des shapefiles (évite de re-télécharger) ────────────────────────────
.carto_cache <- new.env(parent = emptyenv())

# ── Pays courant de la cartographie (mis à jour par le sélecteur géographique
#    et par la détection automatique depuis les données) ────────────────────────
.carto_country <- "CMR"

set_carto_country <- function(code) {
  code <- toupper(as.character(code)[1])
  if (is.na(code) || !nzchar(code)) code <- "CMR"
  if (!identical(code, .carto_country)) {
    .carto_country <<- code
    # Invalider le cache des niveaux du pays précédent
    keys <- ls(.carto_cache)
    for (k in keys) if (grepl("^[A-Z]{3}_L[0-9]+$", k)) rm(list = k, envir = .carto_cache)
    message(sprintf("[CARTO] Pays cartographique : %s", code))
  }
  invisible(.carto_country)
}

get_carto_country <- function() .carto_country

# Détecte automatiquement le pays dont le GADM L1 correspond le mieux aux
# noms de zones présents dans les données d'enquête.
auto_detect_country <- function(regions, cache_dir = "gadm_cache") {
  regs <- unique(regions[!is.na(regions) & nzchar(regions)])
  if (length(regs) < 2) return(NULL)
  norm <- function(x) {
    x <- tolower(as.character(x))
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    x <- gsub("[^a-z0-9]", "", x)
    x
  }
  regs_n <- norm(regs)
  files <- list.files(cache_dir, pattern = "_L1\\.rds$", full.names = TRUE)
  best <- NULL; best_ratio <- 0
  for (f in files) {
    code <- toupper(sub("_L1\\.rds$", "", basename(f)))
    sf1 <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(sf1)) next
    col <- if (exists("gadm_name_col")) gadm_name_col(sf1, 1) else "NAME_1"
    if (is.null(col) || !col %in% names(sf1)) next
    nms_n <- norm(unique(as.character(sf1[[col]])))
    nms_n <- nms_n[nzchar(nms_n)]
    if (!length(nms_n)) next
    ratio <- mean(vapply(regs_n, function(r) any(nms_n == r), logical(1)))
    if (ratio > best_ratio) { best_ratio <- ratio; best <- code }
  }
  if (!is.null(best) && best_ratio >= 0.5)
    list(country = best, ratio = round(best_ratio, 2))
  else NULL
}

load_gadm <- function(level = 1, timeout_sec = 30, max_retries = 2) {
  key <- paste0(.carto_country, "_L", level)

  # Cache hit
  if (!is.null(.carto_cache[[key]])) return(.carto_cache[[key]])

  # 1) Loader mondial avec cache local (gadm_cache/<ISO>_L<n>.rds) + téléchargement
  if (exists("load_gadm_level") && exists("is_country_supported") &&
      is_country_supported(.carto_country)) {
    sf_obj <- tryCatch(
      load_gadm_level(.carto_country, level, cache_dir = "gadm_cache"),
      error = function(e) {
        message(sprintf("[CARTO] load_gadm_level(%s, L%d) échoué : %s",
                        .carto_country, level, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(sf_obj)) {
      .carto_cache[[key]] <- sf_obj
      return(sf_obj)
    }
  }

  # 2) Ancienne config d'URLs (compat arrière CMR)
  url <- GADM_URLS[[key]]
  if (is.null(url)) {
    warning("Pas d'URL GADM pour ", key, " (niveau ", level, ")")
    return(NULL)
  }

  sf_obj <- NULL
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout_sec)

  for (attempt in seq_len(max_retries)) {
    sf_obj <- tryCatch({
      message(sprintf("Téléchargement GADM niveau %d (tentative %d/%d)...",
                      level, attempt, max_retries))
      st_read(url, quiet = TRUE)
    }, error = function(e) {
      if (attempt < max_retries) {
        message(sprintf("Tentative %d échouée : %s. Nouvel essai...",
                        attempt, conditionMessage(e)))
        Sys.sleep(1)
      } else {
        warning(sprintf(
          "GADM L%d non disponible après %d tentatives. Vérifiez votre connexion internet.\nURL: %s\nErreur: %s",
          level, max_retries, url, conditionMessage(e)
        ))
      }
      NULL
    })
    if (!is.null(sf_obj)) break
  }

  if (!is.null(sf_obj)) .carto_cache[[key]] <- sf_obj
  sf_obj
}

load_afrique_centrale <- function() {
  if (!is.null(.carto_cache$afrique)) return(.carto_cache$afrique)
  pays <- c("Cameroon","Chad","Central African Republic",
            "Niger","Nigeria","Gabon","Congo","Equatorial Guinea",
            "Democratic Republic of the Congo","Republic of Congo")
  world <- ne_countries(scale = "medium", returnclass = "sf")
  af <- world |> filter(name %in% pays | admin %in% pays)
  .carto_cache$afrique <- af
  af
}

# ── Palette OMS selon le niveau d'alerte ─────────────────────────────────────
pal_oms_leaflet <- function(vals, indicateur = "GAM") {
  seuils <- list(
    GAM        = c(5, 10, 15, 30),
    SAM        = c(1,  2,  5, 10),
    MAM        = c(3,  8, 12, 20),
    Stunting   = c(20, 30, 40, 60),
    Underweight= c(10, 20, 30, 40),
    MUAC_GAM   = c(5, 10, 15, 20),
    default    = c(5, 10, 15, 30)
  )
  s <- if (!is.null(seuils[[indicateur]])) seuils[[indicateur]] else seuils$default
  # Palette continue calée sur les seuils OMS
  colorNumeric(
    palette = c("#4dac26","#f9c734","#e88400","#c9161d","#640000"),
    domain  = c(0, max(vals, s[4] * 1.2, na.rm = TRUE)),
    na.color = "#cccccc"
  )
}

# ── Libellés OMS complets ─────────────────────────────────────────────────────
LABEL_INDICATEURS <- c(
  GAM         = "MAG — Malnutrition Aiguë Globale (%)",
  SAM         = "MAS — Malnutrition Aiguë Sévère (%)",
  MAM         = "MAM — Malnutrition Aiguë Modérée (%)",
  Stunting    = "Retard de croissance — Stunting (%)",
  Sev_Stunting= "Stunting Sévère (HAZ < -3 DS) (%)",
  Underweight = "Insuffisance Pondérale (WAZ < -2 DS) (%)",
  MUAC_GAM    = "MAG par MUAC (< 125 mm) (%)",
  MUAC_SAM    = "MAS par MUAC (< 115 mm) (%)",
  whz_moy     = "Score WHZ moyen",
  haz_moy     = "Score HAZ moyen",
  waz_moy     = "Score WAZ moyen",
  muac_moy    = "MUAC moyen (mm)",
  ICV_moy     = "Indice de Vulnérabilité (ICV/100)",
  fcs_moy     = "FCS moyen (Score Conso. Alimentaire)",
  n           = "Effectif enquêté"
)

# ── Agrégation des données par niveau géographique ────────────────────────────
# ── Constante : limite max de points GPS pour leaflet ──────────────────────
MAX_GPS_POINTS   <- if (exists("PERF_LIMITS")) PERF_LIMITS$max_gps_points else 50000L
MAX_HEAT_POINTS  <- if (exists("PERF_LIMITS")) PERF_LIMITS$max_heat_points else 100000L

agreger_geo <- function(d, col_geo, m = NULL) {
  # Utiliser dplyr systématiquement — le chemin data.table avec eval(as.call(...))
  # cause des erreurs de portée ("objet 'GAM' introuvable") quand la fonction
  # est chargée depuis un namespace de package (pkgload / golem).
  agreger_geo_dplyr(d, col_geo, m)
}

# Fallback dplyr (pour petits jeux de données ou si data.table absent)
agreger_geo_dplyr <- function(d, col_geo, m = NULL) {
  inds_bin <- intersect(c("GAM","SAM","MAM","Stunting","Sev_Stunting",
                           "Underweight","MUAC_GAM","MUAC_SAM"), names(d))
  inds_num <- intersect(c(".whz",".haz",".waz",".muac","ICV"), names(d))
  lat_col <- if (!is.null(m) && !is.null(m$lat) && m$lat %in% names(d)) m$lat else NULL
  lon_col <- if (!is.null(m) && !is.null(m$lon) && m$lon %in% names(d)) m$lon else NULL

  agg <- d |>
    group_by(Zone = .data[[col_geo]]) |>
    summarise(
      n = n(),
      across(all_of(inds_bin),
             ~round(mean(., na.rm = TRUE) * 100, 1),
             .names = "{.col}"),
      across(all_of(inds_num),
             ~round(mean(., na.rm = TRUE), 2),
             .names = "{.col}_moy"),
      .groups = "drop"
    ) |>
    filter(!is.na(Zone), Zone != "")

  if (!is.null(lat_col))
    agg$lat <- tapply(d[[lat_col]], d[[col_geo]], mean, na.rm=TRUE)[agg$Zone]
  if (!is.null(lon_col))
    agg$lon <- tapply(d[[lon_col]], d[[col_geo]], mean, na.rm=TRUE)[agg$Zone]

  agg
}

# ── Popup HTML riche (style OMS) ──────────────────────────────────────────────
make_popup <- function(nom, row_data, indicateurs_dispo) {
  # En-tête
  html <- paste0(
    "<div style='font-family:Segoe UI,Arial;font-size:13px;",
    "min-width:260px;max-width:320px;line-height:1.9;'>",
    "<b style='color:#00426A;font-size:15px;'>", nom, "</b>",
    "<hr style='margin:5px 0;border-color:#e0e0e0;'>",
    "<div style='font-size:11px;color:#888;margin-bottom:6px;'>",
    "N = <b>", row_data$n, "</b> enfants enquêtés</div>"
  )

  # Indicateurs OMS avec codes couleur
  inds_afficher <- c("GAM","SAM","MAM","Stunting","Underweight","MUAC_GAM")
  seuils_crit   <- c(GAM=15, SAM=2, MAM=20, Stunting=30, Underweight=20, MUAC_GAM=15)
  icons_inds    <- c(GAM="🔴",SAM="🟥",MAM="🟠",Stunting="🔵",
                     Underweight="🟡",MUAC_GAM="🟠")

  for (ind in inds_afficher) {
    if (ind %in% names(row_data) && !is.na(row_data[[ind]])) {
      val <- row_data[[ind]]
      seuil <- seuils_crit[ind]
      col   <- if (val >= seuil) "#c9161d" else if (val >= seuil*0.7) "#e88400" else "#4dac26"
      ico   <- icons_inds[ind]
      lbl   <- gsub(" \\(.*","", LABEL_INDICATEURS[ind])
      html  <- paste0(html,
        "<div>", ico, " <span style='color:#555;'>", lbl, " :</span> ",
        "<b style='color:", col, ";'>", val, " %</b>",
        if (val >= seuil) paste0(" <span style='color:#c9161d;font-size:10px;'>⚠ Seuil OMS !", "</span>") else "",
        "</div>")
    }
  }

  # Variables continues
  for (v in c("whz_moy","haz_moy","muac_moy","ICV_moy","fcs_moy")) {
    col_v <- paste0(gsub("\\.","",v))
    if (col_v %in% names(row_data) && !is.na(row_data[[col_v]])) {
      lbl <- gsub(" \\(.*","", LABEL_INDICATEURS[v])
      html <- paste0(html,
        "<div style='font-size:12px;color:#555;'>📊 ", lbl, " : <b>",
        row_data[[col_v]], "</b></div>")
    }
  }

  html <- paste0(html, "</div>")
  html
}

# ════════════════════════════════════════════════════════════════════════════
# CARTE PRINCIPALE — Cameroun multi-niveaux
# ════════════════════════════════════════════════════════════════════════════
carte_cameroun_leaflet <- function(d, m,
                                    niveau    = "Région",      # Région / Département / Arrondissement
                                    indicateur= "GAM",
                                    type_carte= "Choroplèthe",
                                    fond      = "Esri.WorldGrayCanvas",
                                    show_pts  = TRUE,
                                    show_heat = FALSE,
                                    show_labels = TRUE,
                                    filtre_region = NULL,
                                    filtre_dept   = NULL) {

  # ── Déterminer la colonne géo dans les données ────────────────────────────
  col_map <- list(
    "Région"        = m$region,
    "Département"   = m$dept,
    "Arrondissement"= detect_col(names(d), c("arrondissement","arrond","district",
                                              "sous_prefecture","health_district",
                                              "district_sanitaire","commune")),
    "District"      = detect_col(names(d), c("district","health_district",
                                              "district_sanitaire"))
  )
  col_geo <- col_map[[niveau]]

  # ── Appliquer filtres ──────────────────────────────────────────────────────
  d_f <- d
  if (!is.null(filtre_region) && !is.null(m$region) && m$region %in% names(d))
    if (filtre_region != "Toutes")
      d_f <- filter(d_f, .data[[m$region]] == filtre_region)
  if (!is.null(filtre_dept) && !is.null(m$dept) && m$dept %in% names(d))
    if (filtre_dept != "Tous")
      d_f <- filter(d_f, .data[[m$dept]] == filtre_dept)

  # ── Agrégation ──────────────────────────────────────────────────────────────
  agg <- if (!is.null(col_geo) && col_geo %in% names(d_f)) {
    agreger_geo(d_f, col_geo, m)
  } else {
    # Fallback : agréger par région si la colonne n'existe pas
    if (!is.null(m$region) && m$region %in% names(d_f))
      agreger_geo(d_f, m$region, m)
    else return(leaflet() |> addTiles() |>
                  addControl("<b>Colonne géographique non trouvée</b>", position="topright"))
  }

  # ── Charger le shapefile GADM ─────────────────────────────────────────────
  gadm_level <- switch(niveau, "Région"=1, "Département"=2,
                        "Arrondissement"=3, "District"=3, 1)
  gadm_sf    <- load_gadm(gadm_level)
  gadm_col   <- GADM_NAMES[[paste0("L", gadm_level)]]

  # ── Filtrer le shapefile GADM selon la sélection ──────────────────────────
  # → Si on choisit un département, afficher UNIQUEMENT les polygones de ce département
  # → Si on choisit un arrondissement, afficher UNIQUEMENT les arrondissements du département
  if (!is.null(gadm_sf)) {
    if (niveau == "Département" &&
        !is.null(filtre_region) && filtre_region != "Toutes" && filtre_region != "") {
      gadm_sf <- gadm_sf |> filter(NAME_1 == filtre_region)
    }
    if (niveau == "Arrondissement") {
      if (!is.null(filtre_dept) && filtre_dept != "Tous" && filtre_dept != "") {
        gadm_sf <- gadm_sf |> filter(NAME_2 == filtre_dept)
      } else if (!is.null(filtre_region) && filtre_region != "Toutes" && filtre_region != "") {
        gadm_sf <- gadm_sf |> filter(NAME_1 == filtre_region)
      }
    }
    # Sécurité : si filtre vide, ne pas afficher une carte vide
    if (nrow(gadm_sf) == 0) gadm_sf <- NULL
  }

  # ── Carte de base ─────────────────────────────────────────────────────────
  map_leaf <- leaflet() |>
    addProviderTiles(fond) |>
    addScaleBar(position = "bottomleft") |>
    addMiniMap(toggleDisplay = TRUE, minimized = TRUE,
               tiles = "Esri.WorldGrayCanvas") |>
    addFullscreenControl(position = "topleft")

  # ── Indicateur à cartographier ────────────────────────────────────────────
  ind_col  <- indicateur
  has_ind  <- ind_col %in% names(agg)
  vals     <- if (has_ind) agg[[ind_col]] else agg$n
  pal_l    <- pal_oms_leaflet(vals, indicateur)
  lbl_ind  <- if (!is.null(LABEL_INDICATEURS[indicateur]) &&
                   !is.na(LABEL_INDICATEURS[indicateur]))
               LABEL_INDICATEURS[indicateur] else indicateur

  # ── CHOROPLÈTHE avec GADM ─────────────────────────────────────────────────
  if (!is.null(gadm_sf) && type_carte %in% c("Choroplèthe","Tous")) {
    # Jointure données + shapefile
    sf_join <- gadm_sf |>
      rename(Zone = all_of(gadm_col)) |>
      left_join(agg, by = "Zone") |>
      mutate(
        val_carte = if (has_ind) .data[[ind_col]] else n,
        popup_txt = map2_chr(Zone, row_number(), function(nom, i) {
          row <- agg[agg$Zone == nom, ]
          if (nrow(row) == 0) return(paste0("<b>", nom, "</b><br>Pas de données"))
          make_popup(nom, row, names(agg))
        })
      )

    pal_poly <- colorNumeric(
      palette  = c("#4dac26","#f9c734","#e88400","#c9161d","#640000"),
      domain   = sf_join$val_carte,
      na.color = "#f0f0f0"
    )

    map_leaf <- map_leaf |>
      addPolygons(
        data        = sf_join,
        fillColor   = ~pal_poly(val_carte),
        fillOpacity = 0.72,
        color       = "white",
        weight      = 1.5,
        popup       = ~popup_txt,
        label       = ~paste0(Zone, ": ",
                              ifelse(is.na(val_carte), "N/A",
                                     paste0(round(val_carte, 1),
                                            if(has_ind && grepl("%",lbl_ind))"%" else ""))),
        highlightOptions = highlightOptions(
          weight      = 3,
          color       = "#008DC9",
          fillOpacity = 0.88,
          bringToFront = TRUE
        ),
        group = "Choroplèthe"
      ) |>
      addLegend(
        pal      = pal_poly,
        values   = sf_join$val_carte,
        title    = lbl_ind,
        position = "bottomright",
        labFormat = labelFormat(
          suffix = if(has_ind && grepl("%",lbl_ind))" %" else "",
          digits = 1
        ),
        na.label = "Pas de données"
      )
  }

  # ── CERCLES PROPORTIONNELS ────────────────────────────────────────────────
  if (type_carte %in% c("Cercles proportionnels","Tous")) {
    # Utiliser coordonnées GPS agrégées ou centroïdes GADM
    agg_pts <- agg |> filter(!is.na(lat), !is.na(lon), is.finite(lat), is.finite(lon))

    if (nrow(agg_pts) == 0 && !is.null(gadm_sf)) {
      # Calculer centroïdes si pas de GPS
      centroides <- gadm_sf |>
        rename(Zone = all_of(gadm_col)) |>
        st_centroid() |>
        mutate(
          lon = st_coordinates(geometry)[,1],
          lat = st_coordinates(geometry)[,2]
        ) |>
        st_drop_geometry() |>
        select(Zone, lat, lon)
      agg_pts <- agg |> left_join(centroides, by = "Zone") |>
        filter(!is.na(lat), !is.na(lon))
    }

    if (nrow(agg_pts) > 0) {
      vals_pts <- if (has_ind) agg_pts[[ind_col]] else agg_pts$n
      agg_pts$val_pts <- vals_pts

      map_leaf <- map_leaf |>
        addCircleMarkers(
          data        = agg_pts,
          lat         = ~lat,
          lng         = ~lon,
          radius      = ~rescale(sqrt(n + 1), to = c(8, 30)),
          fillColor   = ~pal_l(val_pts),
          fillOpacity = 0.85,
          color       = "white",
          weight      = 2,
          popup       = ~{
            map_chr(seq_len(nrow(agg_pts)), function(i) {
              make_popup(agg_pts$Zone[i], agg_pts[i,], names(agg_pts))
            })
          },
          label = ~paste0(Zone, ": ",
                          round(val_pts, 1),
                          if(has_ind && grepl("%",lbl_ind))"%" else ""),
          group = "Cercles"
        )
    }
  }

  # ── HEATMAP de densité GPS (sous-échantillonnée pour gros volumes) ──────
  if (show_heat && !is.null(m$lat) && !is.null(m$lon) &&
      m$lat %in% names(d_f) && m$lon %in% names(d_f)) {
    d_pts_heat <- d_f[!is.na(d_f[[m$lat]]) & !is.na(d_f[[m$lon]]),
                       c(m$lat, m$lon), drop = FALSE]
    names(d_pts_heat) <- c("lat", "lon")

    # Sous-échantillonner si trop de points (heatmap n'a pas besoin de tout)
    if (nrow(d_pts_heat) > MAX_HEAT_POINTS) {
      set.seed(42L)
      d_pts_heat <- d_pts_heat[sample.int(nrow(d_pts_heat), MAX_HEAT_POINTS), ]
    }

    if (nrow(d_pts_heat) > 0) {
      map_leaf <- map_leaf |>
        addHeatmap(
          data      = d_pts_heat,
          lat       = ~lat,
          lng       = ~lon,
          blur      = 20,
          max       = 0.8,
          radius    = 18,
          gradient  = c("0" = "#4dac26", "0.5" = "#f9c734",
                        "0.7" = "#e88400", "1" = "#c9161d"),
          group     = "Heatmap"
        )
    }
  }

  # ── POINTS GPS individuels (échantillonnés + popups vectorisés) ─────────
  if (show_pts && !is.null(m$lat) && !is.null(m$lon) &&
      m$lat %in% names(d_f) && m$lon %in% names(d_f)) {

    # Filtrer NAs avec indexation base R (plus rapide que filter pour gros data)
    valid_gps <- !is.na(d_f[[m$lat]]) & !is.na(d_f[[m$lon]])
    d_pts <- d_f[valid_gps, , drop = FALSE]

    # Échantillonner si trop de points (leaflet ne gère pas > 50k markers)
    n_pts_total <- nrow(d_pts)
    if (n_pts_total > MAX_GPS_POINTS) {
      set.seed(42L)
      idx_sample <- sample.int(n_pts_total, MAX_GPS_POINTS)
      d_pts <- d_pts[idx_sample, , drop = FALSE]
      message(sprintf("GPS : %s points échantillonnés sur %s (max %s)",
                      format(MAX_GPS_POINTS, big.mark = " "),
                      format(n_pts_total, big.mark = " "),
                      MAX_GPS_POINTS))
    }

    if (nrow(d_pts) > 0) {
      pal_pt <- colorFactor(
        c("#4dac26","#e88400","#c9161d"),
        domain = c("Normal","MAM","SAM")
      )

      col_nutr <- if ("nutr_cat" %in% names(d_pts)) "nutr_cat" else NULL

      # Préparer colonnes pour leaflet (~ formulas ne supportent pas .data)
      d_pts$.lat_lf  <- d_pts[[m$lat]]
      d_pts$.lon_lf  <- d_pts[[m$lon]]
      d_pts$.fill_lf <- if (!is.null(col_nutr)) {
        pal_pt(d_pts[[col_nutr]])
      } else {
        rep("#008DC9", nrow(d_pts))
      }

      # Popups VECTORISÉS (pas de boucle vapply — ~100x plus rapide)
      popup_parts <- "<div style='font-family:Arial;font-size:12px;'>"
      if (!is.null(col_nutr)) {
        nutr_vals <- d_pts[[col_nutr]]
        nutr_colors <- ifelse(nutr_vals == "SAM", "#c9161d",
                        ifelse(nutr_vals == "MAM", "#e88400", "#4dac26"))
        popup_parts <- paste0(popup_parts,
          "<b>Statut :</b> <b style='color:", nutr_colors, "'>",
          nutr_vals, "</b><br>")
      }
      if (".whz" %in% names(d_pts)) {
        popup_parts <- paste0(popup_parts,
          "WHZ = ", round(d_pts[[".whz"]], 2), "<br>")
      }
      if (".muac" %in% names(d_pts)) {
        popup_parts <- paste0(popup_parts,
          "MUAC = ", d_pts[[".muac"]], " mm")
      }
      d_pts$.popup_lf <- paste0(popup_parts, "</div>")

      map_leaf <- map_leaf |>
        addCircleMarkers(
          data        = d_pts,
          lat         = ~.lat_lf,
          lng         = ~.lon_lf,
          radius      = 4,
          fillColor   = ~.fill_lf,
          fillOpacity = 0.8,
          color       = "white",
          weight      = 0.5,
          clusterOptions = markerClusterOptions(
            disableClusteringAtZoom = 12,
            iconCreateFunction = JS("
              function(cluster) {
                var count = cluster.getChildCount();
                var color = count > 50 ? '#c9161d' : count > 20 ? '#e88400' : '#4dac26';
                return L.divIcon({
                  html: '<div style=\"background:' + color + ';color:white;' +
                        'border-radius:50%;width:32px;height:32px;' +
                        'display:flex;align-items:center;justify-content:center;' +
                        'font-weight:700;font-size:12px;border:2px solid white;\">' +
                        count + '</div>',
                  className: '',
                  iconSize: [32, 32]
                });
              }
            ")
          ),
          popup = ~.popup_lf,
          group = "Points GPS"
        )

      if (!is.null(col_nutr)) {
        map_leaf <- map_leaf |>
          addLegend(
            pal      = pal_pt,
            values   = d_pts[[col_nutr]],
            title    = "Statut nutritionnel",
            position = "topleft"
          )
      }
    }
  }

  # ── Étiquettes texte sur la carte ─────────────────────────────────────────
  if (show_labels && !is.null(gadm_sf)) {
    centroides_lbl <- tryCatch({
      gadm_sf |>
        rename(Zone = all_of(gadm_col)) |>
        left_join(agg |> select(Zone, n,
                                 any_of(ind_col)), by = "Zone") |>
        st_centroid() |>
        mutate(
          lon2 = st_coordinates(geometry)[,1],
          lat2 = st_coordinates(geometry)[,2],
          label_txt = paste0(Zone,
                             if (has_ind && ind_col %in% names(agg))
                               paste0("\n", round(.data[[ind_col]], 1), "%")
                             else "")
        ) |>
        st_drop_geometry() |>
        filter(!is.na(lon2))
    }, error = function(e) NULL)

    if (!is.null(centroides_lbl) && nrow(centroides_lbl) > 0) {
      map_leaf <- map_leaf |>
        addLabelOnlyMarkers(
          data  = centroides_lbl,
          lat   = ~lat2,
          lng   = ~lon2,
          label = ~label_txt,
          labelOptions = labelOptions(
            noHide    = TRUE,
            direction = "center",
            textOnly  = TRUE,
            style     = list(
              "color"       = "#00426A",
              "font-weight" = "600",
              "font-size"   = "10px",
              "font-family" = "Segoe UI, Arial",
              "text-shadow" = "1px 1px 2px white,-1px -1px 2px white",
              "white-space" = "nowrap"
            )
          ),
          group = "Étiquettes"
        )
    }
  }

  # ── Contrôle des couches ──────────────────────────────────────────────────
  couches_overlay <- c()
  if (type_carte %in% c("Choroplèthe","Tous"))         couches_overlay <- c(couches_overlay, "Choroplèthe")
  if (type_carte %in% c("Cercles proportionnels","Tous")) couches_overlay <- c(couches_overlay, "Cercles")
  if (show_heat)  couches_overlay <- c(couches_overlay, "Heatmap")
  if (show_pts)   couches_overlay <- c(couches_overlay, "Points GPS")
  if (show_labels) couches_overlay <- c(couches_overlay, "Étiquettes")

  if (length(couches_overlay) > 1) {
    map_leaf <- map_leaf |>
      addLayersControl(
        overlayGroups = couches_overlay,
        options       = layersControlOptions(collapsed = FALSE)
      )
  }

  # ── Seuils OMS ────────────────────────────────────────────────────────────
  map_leaf <- map_leaf |>
    addControl(
      html = paste0(
        "<div style='background:white;padding:8px 12px;border-radius:8px;",
        "border:0.5px solid #e0e0e0;font-family:Arial;font-size:11px;'>",
        "<b style='color:#00426A;'>Seuils OMS</b><br>",
        "<span style='color:#4dac26;'>●</span> &lt; 5% Acceptable<br>",
        "<span style='color:#f9c734;'>●</span> 5-9.9% Alerte<br>",
        "<span style='color:#e88400;'>●</span> 10-14.9% Sérieux<br>",
        "<span style='color:#c9161d;'>●</span> ≥ 15% Critique<br>",
        "<span style='color:#640000;'>●</span> ≥ 30% Urgence max",
        "</div>"
      ),
      position = "topright"
    )

  # ── Auto-zoom sur la zone sélectionnée ────────────────────────────────────
  # Calculer la bounding box du shapefile filtré pour zoomer automatiquement
  zoom_sf <- tryCatch({
    if (!is.null(gadm_sf) && nrow(gadm_sf) > 0) gadm_sf
    else NULL
  }, error = function(e) NULL)

  if (!is.null(zoom_sf)) {
    bbox <- tryCatch(sf::st_bbox(zoom_sf), error = function(e) NULL)
    if (!is.null(bbox) && all(is.finite(as.numeric(bbox)))) {
      # Ajouter un petit padding autour de la zone
      pad_x <- (bbox["xmax"] - bbox["xmin"]) * 0.05
      pad_y <- (bbox["ymax"] - bbox["ymin"]) * 0.05
      map_leaf <- map_leaf |>
        fitBounds(
          lng1 = as.numeric(bbox["xmin"]) - pad_x,
          lat1 = as.numeric(bbox["ymin"]) - pad_y,
          lng2 = as.numeric(bbox["xmax"]) + pad_x,
          lat2 = as.numeric(bbox["ymax"]) + pad_y
        )
    }
  }

  map_leaf
}

# ════════════════════════════════════════════════════════════════════════════
# CARTE AFRIQUE CENTRALE
# ════════════════════════════════════════════════════════════════════════════
carte_afrique_centrale <- function(d = NULL, highlight_pays = "Cameroon",
                                    indicateur = "GAM",
                                    zoom_region = "Afrique centrale") {
  af_sf <- load_afrique_centrale()
  world  <- ne_countries(scale = "medium", returnclass = "sf")

  # Prévalence globale Cameroun depuis les données
  val_cmr <- if (!is.null(d) && indicateur %in% names(d)) {
    round(mean(d[[indicateur]], na.rm = TRUE) * 100, 1)
  } else NA

  # Zoom
  bbox <- switch(zoom_region,
    "Afrique centrale" = list(xlim = c(8, 30),  ylim = c(-5, 25)),
    "Afrique"          = list(xlim = c(-20, 55), ylim = c(-35, 38)),
    "Monde entier"     = list(xlim = c(-180,180),ylim = c(-90, 90)),
    list(xlim = c(8, 30), ylim = c(-5, 25))
  )

  # Colorier
  pays_hl <- c("Cameroon","Chad","Central African Republic",
               "Niger","Nigeria","Gabon","Congo","Equatorial Guinea",
               "Democratic Republic of the Congo")

  world_plot <- world |>
    mutate(
      categorie = case_when(
        name == "Cameroon" ~ "Cameroun (données)",
        name %in% pays_hl  ~ "Afrique centrale",
        continent == "Africa" ~ "Autre Afrique",
        TRUE ~ "Monde"
      )
    )

  p <- ggplot(world_plot) +
    geom_sf(aes(fill = categorie,
                text = paste0(name,
                              if_else(name == "Cameroon" & !is.na(val_cmr),
                                      paste0("\n", indicateur, " = ",
                                             val_cmr, "%"), ""))),
            color = "white", linewidth = 0.3) +
    scale_fill_manual(
      values = c(
        "Cameroun (données)" = "#c9161d",
        "Afrique centrale"   = "#b3d9ef",
        "Autre Afrique"      = "#dce8f0",
        "Monde"              = "#f0f0f0"
      ),
      name = ""
    ) +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim) +
    labs(
      title    = paste0("Cameroun — Contexte régional (",
                         if(!is.na(val_cmr)) paste0(indicateur," = ",val_cmr,"%")
                         else "Afrique centrale", ")"),
      caption  = "Source : Natural Earth | Données enquête SMART"
    ) +
    theme_void() +
    theme(
      plot.title      = element_text(hjust = .5, face = "bold",
                                      size = 13, color = "#00426A"),
      legend.position = "bottom",
      plot.caption    = element_text(hjust = .5, color = "#888", size = 9)
    )

  ggplotly(p, tooltip = "text") |>
    layout(legend = list(orientation = "h", y = -0.1))
}

# ════════════════════════════════════════════════════════════════════════════
# CARTE STATIQUE ggplot2 — pour export PDF/PNG
# ════════════════════════════════════════════════════════════════════════════
carte_statique_ggplot <- function(d, m,
                                   niveau     = "Région",
                                   indicateur = "GAM",
                                   palette    = "OMS",
                                   type_geom  = "choroplèthe") {
  gadm_level <- switch(niveau, "Région"=1, "Département"=2,
                        "Arrondissement"=3, 1)
  gadm_sf    <- load_gadm(gadm_level)
  gadm_col   <- GADM_NAMES[[paste0("L", gadm_level)]]

  col_geo <- switch(niveau,
    "Région"         = m$region,
    "Département"    = m$dept,
    "Arrondissement" = detect_col(names(d), c("arrondissement","district","commune")),
    m$region
  )

  agg <- if (!is.null(col_geo) && col_geo %in% names(d))
    agreger_geo(d, col_geo, m)
  else
    agreger_geo(d, m$region, m)

  pal_col <- switch(palette,
    "OMS"     = c("#4dac26","#f9c734","#e88400","#c9161d","#640000"),
    "YlOrRd"  = c("#ffffb2","#fed976","#feb24c","#fd8d3c","#e31a1c"),
    "Blues"   = c("#eff3ff","#bdd7e7","#6baed6","#2171b5","#084594"),
    "Viridis" = viridis::viridis(5),
    c("#4dac26","#f9c734","#e88400","#c9161d","#640000")
  )

  has_ind <- indicateur %in% names(agg)
  lbl_ind <- if (!is.null(LABEL_INDICATEURS[indicateur]) &&
                  !is.na(LABEL_INDICATEURS[indicateur]))
              LABEL_INDICATEURS[indicateur] else indicateur

  if (!is.null(gadm_sf)) {
    sf_join <- gadm_sf |>
      rename(Zone = all_of(gadm_col)) |>
      left_join(agg, by = "Zone") |>
      mutate(val = if (has_ind) .data[[indicateur]] else n)

    centroides <- sf_join |>
      filter(!is.na(val)) |>
      st_centroid() |>
      mutate(
        lon_c = st_coordinates(geometry)[,1],
        lat_c = st_coordinates(geometry)[,2]
      ) |>
      st_drop_geometry()

    if (type_geom == "choroplèthe") {
      p <- ggplot(sf_join) +
        geom_sf(fill = "grey92", color = "white", linewidth = 0.3) +
        geom_sf(data = filter(sf_join, !is.na(val)),
                aes(fill = val), color = "white", linewidth = 0.5) +
        geom_text(data = centroides,
                  aes(x = lon_c, y = lat_c,
                      label = paste0(Zone, "\n", round(val, 1), "%")),
                  size = 2.8, fontface = "bold", color = "white",
                  check_overlap = TRUE) +
        scale_fill_gradientn(
          colors   = pal_col,
          na.value = "grey92",
          name     = lbl_ind
        )
    } else {
      p <- ggplot() +
        geom_sf(data = gadm_sf |> rename(Zone = all_of(gadm_col)),
                fill = "grey92", color = "white", linewidth = 0.3) +
        geom_point(data = agg |> filter(!is.na(lat), !is.na(lon)),
                   aes(x = lon, y = lat,
                       size = n,
                       color = if(has_ind) .data[[indicateur]] else n),
                   alpha = 0.85) +
        scale_size_continuous(range = c(4, 20), name = "N enquêtés") +
        scale_color_gradientn(colors = pal_col, name = lbl_ind)
    }

    p <- p +
      geom_hline(yintercept = NA) +
      labs(
        title    = paste0("Cameroun — ", lbl_ind, " par ", niveau),
        subtitle = paste0("N = ", format(nrow(d), big.mark=" "),
                          " enquêtés | Normes OMS 2006"),
        caption  = paste0("Source : Enquête SMART | GADM v4.1 | ",
                           format(Sys.Date(), "%B %Y"))
      ) +
      theme_void() +
      theme(
        plot.title    = element_text(hjust = .5, face = "bold",
                                      size = 14, color = "#00426A"),
        plot.subtitle = element_text(hjust = .5, size = 11, color = "#555"),
        plot.caption  = element_text(hjust = .5, size = 9, color = "#888"),
        legend.position = "right",
        legend.title    = element_text(size = 10, face = "bold"),
        plot.margin     = margin(10, 10, 10, 10)
      )

    return(p)
  }

  # Fallback si GADM non disponible
  ggplot() +
    annotate("text", x=0, y=0,
             label="GADM non disponible — vérifiez votre connexion internet",
             size=5, color="#c9161d") +
    theme_void()
}

# ── Listes utiles pour l'UI ───────────────────────────────────────────────────
NIVEAUX_CARTO     <- c("Région","Département","Arrondissement","District")
TYPES_CARTO       <- c("Choroplèthe","Cercles proportionnels","Tous")
FONDS_CARTO       <- c("Esri.WorldGrayCanvas","OpenStreetMap",
                        "Esri.WorldImagery","Esri.WorldTopoMap")
ZOOMS_AFRIQUE     <- c("Afrique centrale","Afrique","Monde entier")
PALETTES_STATIQUE <- c("OMS","YlOrRd","Blues","Viridis")

inds_disponibles <- function(d) {
  all_inds <- c(
    "GAM","SAM","MAM","Stunting","Sev_Stunting",
    "Underweight","MUAC_GAM","MUAC_SAM",
    "whz_moy","haz_moy","waz_moy","muac_moy","ICV_moy","fcs_moy","n"
  )
  dispo <- intersect(
    c(names(LABEL_INDICATEURS)),
    c(names(d),
      paste0(c("whz","haz","waz","muac","ICV","fcs"),"_moy"),
      "n")
  )
  nom_dispo <- LABEL_INDICATEURS[dispo]
  setNames(dispo, nom_dispo)
}

cat("module_carto_v2.R chargé —", length(ls()), "objets\n")
