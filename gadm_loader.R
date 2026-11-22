# ============================================================================
# GADM_LOADER : Chargeur intelligent des frontières administratives GADM 4.1
# ============================================================================
# Format : ZIP contenant JSON (format officiel geodata.ucdavis.edu)
# Télécharge, décompresse, lit le GeoJSON, puis cache en .rds local
# ============================================================================

library(sf)
library(jsonlite)

# ==============================================================================
# HELPER : trouver la bonne colonne NAME_ dans un objet sf GADM
# ==============================================================================

#' Retourner le nom de la colonne NAME_x pour un niveau donné
#' GADM 4.1 utilise NAME_1, NAME_2, NAME_3 de manière standard
#'
#' @param gadm_sf Objet sf chargé depuis GADM
#' @param level   Niveau administratif (1, 2 ou 3)
#' @return nom de la colonne, ou NULL si introuvable
gadm_name_col <- function(gadm_sf, level) {
  candidate <- paste0("NAME_", level)
  if (candidate %in% colnames(gadm_sf)) return(candidate)
  # Fallback : chercher n'importe quelle colonne NAME_x
  fallbacks <- grep(paste0("NAME_", level), colnames(gadm_sf), 
                    value = TRUE, ignore.case = TRUE)
  if (length(fallbacks) > 0) return(fallbacks[1])
  return(NULL)
}

# ==============================================================================
# 1. CHARGEUR PRINCIPAL GADM  (ZIP → JSON → SF → cache RDS)
# ==============================================================================

#' Charger un niveau GADM avec cache local intelligent
#'
#' @param country_code Code ISO-3 du pays (ex: "CMR", "FRA", "BRA")
#' @param level        Niveau administratif (1 = régions, 2 = dépts, 3 = arrond.)
#' @param cache_dir    Répertoire cache local (défaut: ~/.gadm_cache)
#' @param use_cache    Utiliser cache si disponible (défaut: TRUE)
#' @param timeout      Timeout en secondes (défaut: 60)
#'
#' @return Objet sf (Simple Features)
#' @export

load_gadm_level <- function(country_code, level,
                             cache_dir = "gadm_cache",
                             use_cache  = TRUE,
                             timeout    = 60) {

  # ── 1. Validation ────────────────────────────────────────────────────────────
  if (!is_country_supported(country_code)) {
    stop(sprintf("Pays '%s' non supporté.", country_code))
  }
  if (level < 0 || level > 4) stop("Niveau GADM doit être entre 0 et 4")

  max_level <- get_available_levels(country_code)
  if (level > max_level) {
    message(sprintf("⚠️  Niveau %d non disponible pour %s (max: %d).",
                    level, country_code, max_level))
    level <- max_level
  }

  # ── 2. Préparer répertoire cache ──────────────────────────────────────────────
  cache_dir <- path.expand(cache_dir)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  cache_file <- file.path(cache_dir, sprintf("%s_L%d.rds", country_code, level))

  # ── 3. Charger depuis cache si valide (< 1 an) ───────────────────────────────
  if (use_cache && file.exists(cache_file)) {
    age_h <- as.numeric(difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours"))
    if (age_h < 24 * 365) {
      message(sprintf("✅ Cache local : %s L%d — %s",
                      country_code, level, get_country_name(country_code)))
      return(readRDS(cache_file))
    }
  }

  # ── 4. Télécharger ZIP depuis GADM ───────────────────────────────────────────
  url <- get_gadm_url(country_code, level)
  message(sprintf("⬇️  Téléchargement %s L%d (%s)...",
                  country_code, level, get_country_name(country_code)))

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout))
  options(timeout = timeout)

  tryCatch({

    # Télécharger le ZIP dans un fichier temporaire
    tmp_zip <- tempfile(fileext = ".zip")
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit({ unlink(tmp_zip); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

    download.file(url, destfile = tmp_zip, mode = "wb", quiet = TRUE)

    if (!file.exists(tmp_zip) || file.size(tmp_zip) < 100) {
      stop("Fichier ZIP vide ou corrompu")
    }

    # Lister et décompresser
    zip_contents <- unzip(tmp_zip, list = TRUE)
    json_files   <- zip_contents$Name[grepl("\\.json$", zip_contents$Name, ignore.case = TRUE)]

    if (length(json_files) == 0) {
      stop("Aucun fichier .json trouvé dans le ZIP")
    }

    unzip(tmp_zip, files = json_files, exdir = tmp_dir, overwrite = TRUE)
    json_path <- file.path(tmp_dir, json_files[1])

    # Lire le GeoJSON
    gadm_sf <- sf::st_read(json_path, quiet = TRUE)

    if (nrow(gadm_sf) == 0) {
      stop(sprintf("Données vides pour %s niveau %d", country_code, level))
    }

    # ── 5. Mettre en cache ──────────────────────────────────────────────────────
    saveRDS(gadm_sf, cache_file)
    size_mb <- round(file.size(cache_file) / 1e6, 2)
    message(sprintf("💾 Mis en cache : %s (%.2f MB)", basename(cache_file), size_mb))

    return(gadm_sf)

  }, error = function(e) {
    message(sprintf("❌ Erreur : %s", e$message))

    # Fallback : cache expiré
    if (file.exists(cache_file)) {
      message("⚠️  Pas d'internet — utilisation du cache expiré.")
      return(readRDS(cache_file))
    }
    stop(sprintf("Impossible de charger %s L%d (pas de cache + pas d'internet)",
                 country_code, level))
  })
}

# ==============================================================================
# 2. PRÉCHARGEMENT EN CASCADE
# ==============================================================================

#' Précharger tous les niveaux d'un pays
#'
#' @param country_code Code ISO-3
#' @param levels       Vecteur des niveaux à charger (défaut: 1:3)
#' @param cache_dir    Répertoire cache
#' @return Liste nommée d'objets sf (NULL pour niveaux manquants)

preload_gadm_cascade <- function(country_code,
                                  levels    = 1:3,
                                  cache_dir = "gadm_cache") {

  message(sprintf("📦 Préchargement cascade GADM — %s...",
                  get_country_name(country_code)))

  max_level <- get_available_levels(country_code)
  levels    <- intersect(levels, 0:max_level)

  gadm_list <- lapply(levels, function(l) {
    tryCatch(
      load_gadm_level(country_code, l, cache_dir = cache_dir),
      error = function(e) { warning(sprintf("⚠️  Niveau %d : %s", l, e$message)); NULL }
    )
  })

  names(gadm_list) <- sprintf("L%d", levels)
  loaded <- sum(!sapply(gadm_list, is.null))
  message(sprintf("✅ Cascade terminée (%d/%d niveaux)", loaded, length(levels)))
  return(gadm_list)
}

# ==============================================================================
# 3. GESTION CACHE
# ==============================================================================

cleanup_gadm_cache <- function(cache_dir     = "gadm_cache",
                                max_size_gb   = 2,
                                keep_recent_days = 30) {
  cache_dir <- path.expand(cache_dir)
  if (!dir.exists(cache_dir)) { message("Cache vide."); return(invisible(NULL)) }

  files     <- list.files(cache_dir, full.names = TRUE, pattern = "\\.rds$")
  if (length(files) == 0) { message("Aucun fichier cache."); return(invisible(NULL)) }

  file_info     <- file.info(files)
  total_size_gb <- sum(file_info$size, na.rm = TRUE) / 1e9
  message(sprintf("📊 Cache GADM : %.2f GB (limite: %d GB)", total_size_gb, max_size_gb))

  if (total_size_gb <= max_size_gb) {
    message("✅ Cache dans les limites.")
    return(invisible(NULL))
  }

  cutoff     <- Sys.time() - (keep_recent_days * 24 * 3600)
  candidates <- names(which(file_info$mtime < cutoff))
  candidates <- candidates[order(file_info[candidates, "mtime"])]

  deleted <- 0
  for (f in candidates) {
    sz <- file_info[f, "size"]
    file.remove(f)
    deleted <- deleted + sz
    message(sprintf("🗑️  Supprimé : %s (%.1f MB)", basename(f), sz / 1e6))
    if ((total_size_gb * 1e9 - deleted) / 1e9 <= max_size_gb) break
  }
  return(invisible(TRUE))
}

remove_country_from_cache <- function(country_code, cache_dir = "gadm_cache") {
  cache_dir <- path.expand(cache_dir)
  files <- list.files(cache_dir,
                      pattern  = sprintf("^%s_L[0-9]\\.rds$", country_code),
                      full.names = TRUE)
  if (length(files) == 0) { message(sprintf("Aucun cache pour %s", country_code)); return(invisible(NULL)) }
  for (f in files) { file.remove(f); message(sprintf("🗑️  Supprimé : %s", basename(f))) }
  return(invisible(TRUE))
}

cache_stats <- function(cache_dir = "gadm_cache") {
  cache_dir <- path.expand(cache_dir)
  if (!dir.exists(cache_dir)) { message("Cache vide"); return(NULL) }
  files <- list.files(cache_dir, full.names = TRUE, pattern = "\\.rds$")
  if (length(files) == 0) { message("Aucun fichier cache"); return(NULL) }

  info <- file.info(files)
  stats <- data.frame(
    fichier    = basename(files),
    taille_mb  = round(info$size / 1e6, 2),
    date_cache = as.Date(info$mtime),
    age_jours  = as.integer(difftime(Sys.time(), info$mtime, units = "days")),
    stringsAsFactors = FALSE
  )
  stats <- stats[order(stats$age_jours), ]
  message(sprintf("📊 Cache GADM — Total : %.1f MB", sum(stats$taille_mb)))
  print(stats, row.names = FALSE)
  return(invisible(stats))
}

# ==============================================================================
# 4. UTILITAIRES
# ==============================================================================

summarize_gadm <- function(gadm_sf) {
  list(
    n_zones    = nrow(gadm_sf),
    colonnes   = colnames(gadm_sf),
    crs        = sf::st_crs(gadm_sf)$input,
    bbox       = sf::st_bbox(gadm_sf),
    geom_type  = unique(sf::st_geometry_type(gadm_sf))
  )
}

extract_admin_names <- function(gadm_sf, level = 1) {
  col <- gadm_name_col(gadm_sf, level)
  if (is.null(col)) stop(sprintf("Colonne NAME_%d introuvable.", level))
  sort(unique(gadm_sf[[col]]))
}

# ==============================================================================
# 5. SPATIAL JOIN
# ==============================================================================

spatial_join_to_gadm <- function(df, gadm_sf,
                                  lat_col   = "lat",
                                  lon_col   = "lon",
                                  join_type = "within") {
  if (!any(c("geometry", "geom") %in% colnames(df))) {
    df_sf <- sf::st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  } else {
    df_sf <- sf::st_as_sf(df, crs = 4326)
  }
  result <- if (join_type == "nearest") {
    sf::st_join(df_sf, gadm_sf, join = sf::st_nearest_feature)
  } else {
    sf::st_join(df_sf, gadm_sf, join = sf::st_within)
  }
  sf::st_drop_geometry(result)
}
