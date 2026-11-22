# ============================================================================
# GADM PRÉ-TÉLÉCHARGEMENT MONDIAL
# ============================================================================
# À lancer UNE SEULE FOIS avant de démarrer l'application.
# Télécharge et cache tous les fichiers GADM → l'app fonctionne ensuite
# SANS connexion internet pour la cartographie.
#
# Usage :
#   source("gadm_predownload_all.R")
#   gadm_predownload_all(countries = "priority")  # Priorité 1 seulement
#   gadm_predownload_all(countries = "all")        # Tout le monde (~20-30 min)
# ============================================================================

source("config_gadm_worldwide.R")
source("gadm_loader.R")

# ==============================================================================
# Liste de pays prioritaires (les plus utilisés en humanitaire / nutrition)
# ==============================================================================

PRIORITY_COUNTRIES <- c(
  # Afrique centrale
  "CMR", "TCD", "CAF", "COD", "COG", "GAB", "GNQ",
  # Afrique de l'Ouest
  "NGA", "NER", "MLI", "SEN", "GIN", "CIV", "BFA", "GHA", "TGO", "BEN",
  "MRT", "GMB", "GNB", "SLE", "LBR",
  # Afrique de l'Est
  "ETH", "SOM", "SSD", "SDN", "KEN", "UGA", "TZA", "RWA", "BDI", "ERI",
  # Afrique du Sud
  "MOZ", "ZWE", "ZMB", "MWI", "AGO", "MDG", "COD",
  # Afrique du Nord
  "DZA", "LBY", "EGY", "MAR", "TUN",
  # Proche-Orient & Asie
  "YEM", "AFG", "PAK", "IRQ", "SYR", "LBN", "JOR",
  # Amérique Latine
  "HTI", "HND", "GTM", "SLV", "NIC", "BOL", "PER",
  # Asie du Sud & Sud-Est
  "BGD", "MMR", "NPL", "IND"
)

# ==============================================================================
# FONCTION PRINCIPALE DE PRÉ-TÉLÉCHARGEMENT
# ==============================================================================

gadm_predownload_all <- function(
    countries       = "priority",    # "priority" | "all" | vecteur ISO3
    levels          = 1:2,           # niveaux à télécharger (1=régions, 2=districts)
    cache_dir       = "gadm_cache",  # dossier dans le projet Shiny
    pause_sec       = 1,             # pause entre requêtes (politesse serveur)
    skip_existing   = TRUE,          # ne pas re-télécharger si déjà en cache
    verbose         = TRUE
) {

  # Sélectionner les pays à télécharger
  target_countries <- switch(
    as.character(countries[1]),
    "priority" = intersect(PRIORITY_COUNTRIES, names(GADM_WORLDWIDE)),
    "all"      = names(GADM_WORLDWIDE),
    countries  # vecteur fourni directement
  )

  target_countries <- intersect(target_countries, names(GADM_WORLDWIDE))
  n_pays <- length(target_countries)

  cat(sprintf("\n%s\n", strrep("=", 60)))
  cat(sprintf("  GADM PRÉ-TÉLÉCHARGEMENT — %d pays × %d niveaux\n",
              n_pays, length(levels)))
  cat(sprintf("  Cache : %s\n", path.expand(cache_dir)))
  cat(sprintf("%s\n\n", strrep("=", 60)))

  cache_dir_expanded <- path.expand(cache_dir)
  if (!dir.exists(cache_dir_expanded)) {
    dir.create(cache_dir_expanded, recursive = TRUE)
  }

  # Compteurs
  n_ok      <- 0
  n_skip    <- 0
  n_erreur  <- 0
  erreurs   <- list()

  total <- n_pays * length(levels)
  idx   <- 0

  for (country in target_countries) {
    country_name <- get_country_name(country)
    max_level    <- get_available_levels(country)

    for (level in levels) {

      idx <- idx + 1
      cache_file <- file.path(cache_dir_expanded,
                              sprintf("%s_L%d.rds", country, level))

      # Vérifier si déjà en cache
      if (skip_existing && file.exists(cache_file)) {
        if (verbose) cat(sprintf("[%3d/%d] ✅ DÉJÀ EN CACHE  %s L%d\n",
                                 idx, total, country_name, level))
        n_skip <- n_skip + 1
        next
      }

      # Ignorer niveaux inexistants pour ce pays
      if (level > max_level) {
        if (verbose) cat(sprintf("[%3d/%d] ⏭️  IGNORÉ (max L%d) %s L%d\n",
                                 idx, total, max_level, country_name, level))
        next
      }

      if (verbose) cat(sprintf("[%3d/%d] ⬇️  Téléchargement  %s L%d ... ",
                               idx, total, country_name, level))

      result <- tryCatch({
        sf_data <- load_gadm_level(country, level, cache_dir = cache_dir,
                                   use_cache = FALSE)
        n_ok <<- n_ok + 1
        if (verbose) cat(sprintf("✅ (%d zones)\n", nrow(sf_data)))
        "ok"
      }, error = function(e) {
        n_erreur <<- n_erreur + 1
        erreurs[[length(erreurs) + 1]] <<- list(
          country = country, level = level, error = e$message
        )
        if (verbose) cat(sprintf("❌ %s\n", substr(e$message, 1, 60)))
        "error"
      })

      # Pause pour ne pas surcharger le serveur GADM
      if (result == "ok" && pause_sec > 0) Sys.sleep(pause_sec)
    }
  }

  # Rapport final
  cat(sprintf("\n%s\n", strrep("=", 60)))
  cat(sprintf("  RAPPORT FINAL\n"))
  cat(sprintf("  ✅ Téléchargés  : %d\n", n_ok))
  cat(sprintf("  ⏭️  Déjà en cache : %d\n", n_skip))
  cat(sprintf("  ❌ Erreurs       : %d\n", n_erreur))

  if (length(erreurs) > 0) {
    cat("\n  Erreurs détail :\n")
    for (e in erreurs) {
      cat(sprintf("    • %s L%d : %s\n", e$country, e$level, substr(e$error, 1, 80)))
    }
  }

  total_size_mb <- sum(file.info(
    list.files(cache_dir_expanded, full.names = TRUE, pattern = "\\.rds$")
  )$size, na.rm = TRUE) / 1e6

  cat(sprintf("\n  💾 Taille cache total : %.1f MB\n", total_size_mb))
  cat(sprintf("%s\n\n", strrep("=", 60)))

  invisible(list(ok = n_ok, skip = n_skip, errors = n_erreur,
                 error_list = erreurs))
}

# ==============================================================================
# VÉRIFICATION DU CACHE EXISTANT
# ==============================================================================

gadm_cache_status <- function(cache_dir = "gadm_cache") {
  cache_dir_exp <- path.expand(cache_dir)
  files <- list.files(cache_dir_exp, pattern = "\\.rds$", full.names = FALSE)

  if (length(files) == 0) {
    cat("⚠️  Cache vide. Lancez gadm_predownload_all() pour pré-télécharger.\n")
    return(invisible(NULL))
  }

  # Construire le data.frame avec des vecteurs — évite la matrice de listes
  rows <- lapply(files, function(f) {
    # Nom sans extension
    stem  <- sub("\\.rds$", "", f)
    parts <- strsplit(stem, "_L")[[1]]

    # Ignorer les fichiers au nom inattendu
    if (length(parts) < 2) return(NULL)

    fpath <- file.path(cache_dir_exp, f)
    finfo <- file.info(fpath)
    sz    <- if (!is.na(finfo$size) && length(finfo$size) == 1) {
      round(finfo$size / 1e6, 3)
    } else NA_real_

    iso   <- parts[1]
    lvl   <- suppressWarnings(as.integer(parts[2]))
    nom   <- tryCatch(get_country_name(iso), error = function(e) iso)

    data.frame(
      iso       = iso,
      nom       = as.character(nom),
      niveau    = lvl,
      taille_mb = sz,
      stringsAsFactors = FALSE
    )
  })

  # Retirer les lignes NULL (fichiers au nom invalide)
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    cat("⚠️  Aucun fichier GADM valide dans le cache.\n")
    return(invisible(NULL))
  }

  df <- do.call(rbind, rows)

  cat(sprintf("\n📦 CACHE GADM — %d fichiers | %.1f MB total | %d pays\n\n",
              nrow(df),
              sum(df$taille_mb, na.rm = TRUE),
              length(unique(df$iso))))

  # Affichage trié par pays puis niveau
  df_sorted <- df[order(df$iso, df$niveau), ]
  print(df_sorted, row.names = FALSE)
  invisible(df)
}

cat("\n✅ Script gadm_predownload_all.R chargé\n")
cat("   → gadm_predownload_all('priority')  — 40+ pays humanitaires\n")
cat("   → gadm_predownload_all('all')        — Tous les 170 pays (~30 min)\n")
cat("   → gadm_cache_status()                — État du cache actuel\n\n")
