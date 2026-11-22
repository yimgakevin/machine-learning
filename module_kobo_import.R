# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_kobo_import.R — Import KoboToolbox / ODK                          ║
# ║  Connexion API · Liste des formulaires · Téléchargement · Mapping auto    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ══════════════════════════════════════════════════════════════════════════════
# 1. CONFIGURATION API KoboToolbox
# ══════════════════════════════════════════════════════════════════════════════

# Serveurs KoboToolbox connus
KOBO_SERVERS <- c(

  "OCHA (humanitaire)"        = "https://kobo.humanitarianresponse.info",
  "KoboToolbox (global)"      = "https://kf.kobotoolbox.org",
  "Personnalisé"              = "custom"
)

# ══════════════════════════════════════════════════════════════════════════════
# 2. FONCTIONS API
# ══════════════════════════════════════════════════════════════════════════════

# Tester la connexion
kobo_test_connection <- function(server_url, token) {
  if (is.null(token) || token == "") return(list(ok = FALSE, msg = "Token manquant"))
  if (is.null(server_url) || server_url == "") return(list(ok = FALSE, msg = "URL serveur manquante"))

  url <- paste0(server_url, "/api/v2/assets.json?limit=1")
  tryCatch({
    resp <- httr::GET(url, httr::add_headers(Authorization = paste("Token", token)),
                      httr::timeout(15))
    if (httr::status_code(resp) == 200) {
      list(ok = TRUE, msg = "Connexion réussie ✅")
    } else if (httr::status_code(resp) == 401) {
      list(ok = FALSE, msg = "Token invalide ou expiré (401)")
    } else {
      list(ok = FALSE, msg = paste0("Erreur HTTP ", httr::status_code(resp)))
    }
  }, error = function(e) {
    list(ok = FALSE, msg = paste0("Erreur réseau : ", e$message))
  })
}

# Lister les formulaires (assets) du compte
kobo_list_forms <- function(server_url, token, limit = 100) {
  url <- paste0(server_url, "/api/v2/assets.json?asset_type=survey&limit=", limit)
  tryCatch({
    resp <- httr::GET(url, httr::add_headers(Authorization = paste("Token", token)),
                      httr::timeout(30))
    if (httr::status_code(resp) != 200) {
      return(list(ok = FALSE, msg = paste0("Erreur HTTP ", httr::status_code(resp))))
    }
    body <- httr::content(resp, as = "parsed")
    forms <- lapply(body$results, function(f) {
      list(
        uid           = f$uid %||% "",
        name          = f$name %||% "(Sans nom)",
        date_created  = substr(f$date_created %||% "", 1, 10),
        date_modified = substr(f$date_modified %||% "", 1, 10),
        submissions   = f$deployment__submission_count %||% 0,
        deployed      = isTRUE(f$has_deployment),
        owner         = f$owner__username %||% ""
      )
    })
    list(ok = TRUE, forms = forms, count = body$count %||% length(forms))
  }, error = function(e) {
    list(ok = FALSE, msg = paste0("Erreur : ", e$message))
  })
}

# Télécharger les données d'un formulaire
kobo_download_data <- function(server_url, token, asset_uid, format = "json") {
  # Méthode 1 : API v2 data endpoint
  url <- paste0(server_url, "/api/v2/assets/", asset_uid, "/data.json?limit=30000")

  tryCatch({
    resp <- httr::GET(url, httr::add_headers(Authorization = paste("Token", token)),
                      httr::timeout(120))
    if (httr::status_code(resp) != 200) {
      return(list(ok = FALSE, msg = paste0("Erreur HTTP ", httr::status_code(resp))))
    }

    body <- httr::content(resp, as = "parsed")
    results <- body$results

    if (length(results) == 0) {
      return(list(ok = FALSE, msg = "Aucune soumission trouvée"))
    }

    # Convertir la liste en data.frame
    df <- kobo_json_to_df(results)

    list(ok = TRUE, data = df, n_rows = nrow(df), n_cols = ncol(df),
         total = body$count %||% nrow(df))
  }, error = function(e) {
    list(ok = FALSE, msg = paste0("Erreur : ", e$message))
  })
}

# Convertir les résultats JSON en data.frame propre
kobo_json_to_df <- function(results) {
  # Extraire tous les noms de champs
  all_keys <- unique(unlist(lapply(results, names)))

  # Créer le data.frame
  df <- as.data.frame(do.call(rbind, lapply(results, function(row) {
    sapply(all_keys, function(key) {
      val <- row[[key]]
      if (is.null(val)) NA_character_
      else if (is.list(val)) paste(unlist(val), collapse = " | ")
      else as.character(val)
    }, USE.NAMES = TRUE)
  })), stringsAsFactors = FALSE)

  names(df) <- all_keys

  # Nettoyage des colonnes
  df <- kobo_clean_columns(df)
  as_tibble(df)
}

# Nettoyage automatique des colonnes KoboToolbox
kobo_clean_columns <- function(df) {
  # Supprimer les colonnes système Kobo (préfixées par _ ou meta/)
  system_cols <- c("_id", "_uuid", "_submission_time", "_validation_status",
                   "_notes", "_status", "_submitted_by", "_tags",
                   "meta/instanceID", "meta/deprecatedID",
                   "__version__", "_xform_id_string", "_bamboo_dataset_id",
                   "_attachments", "_geolocation")
  keep_cols <- setdiff(names(df), system_cols)

  # Garder _submission_time renommé
  if ("_submission_time" %in% names(df)) {
    df$date_soumission <- df[["_submission_time"]]
    keep_cols <- c(keep_cols, "date_soumission")
  }
  if ("_id" %in% names(df)) {
    df$kobo_id <- df[["_id"]]
    keep_cols <- c(keep_cols, "kobo_id")
  }

  df <- df[, keep_cols, drop = FALSE]

  # Nettoyer les noms de colonnes (remplacer / par _)
  names(df) <- gsub("/", "_", names(df))
  names(df) <- gsub("\\s+", "_", names(df))
  names(df) <- gsub("[^a-zA-Z0-9_àâäéèêëïîôùûüÿçÀÂÄÉÈÊËÏÎÔÙÛÜŸÇ]", "_", names(df))
  names(df) <- gsub("_+", "_", names(df))
  names(df) <- gsub("^_|_$", "", names(df))

  # Convertir les colonnes numériques (uniquement les character)
  for (col in names(df)) {
    vals <- df[[col]]
    if (!is.character(vals)) next
    vals_nona <- vals[!is.na(vals) & vals != ""]
    if (length(vals_nona) > 0) {
      numeric_vals <- suppressWarnings(as.numeric(vals_nona))
      if (sum(!is.na(numeric_vals)) / length(vals_nona) > 0.8) {
        df[[col]] <- suppressWarnings(as.numeric(vals))
      }
    }
  }

  df
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. MAPPING AUTOMATIQUE ODK → SMART
# ══════════════════════════════════════════════════════════════════════════════

# Dictionnaire de mapping ODK/Kobo → variables SMART standards
KOBO_SMART_MAPPING <- list(
  # Anthropométrie
  whz  = c("whz", "wfhz", "zscore_wh", "z_whz", "zwhz", "weight_for_height_z",
           "wh_zscore", "poids_taille_z"),
  haz  = c("haz", "hfaz", "zscore_ha", "z_haz", "zhaz", "height_for_age_z",
           "ha_zscore", "taille_age_z"),
  waz  = c("waz", "wfaz", "zscore_wa", "z_waz", "zwaz", "weight_for_age_z",
           "wa_zscore", "poids_age_z"),
  muac = c("muac", "pb", "mid_upper_arm", "perimetre_brachial", "peri_brach",
           "brachial", "muac_mm"),
  poids  = c("poids", "weight", "wt", "pds", "child_weight"),
  taille = c("taille", "height", "ht", "length", "longueur", "child_height"),

  # Démographie
  sexe      = c("sexe", "sex", "genre", "gender", "child_sex"),
  age_mois  = c("age_mois", "age_months", "age_m", "agemois", "age_month",
                "child_age", "age_en_mois"),
  date_naissance = c("date_naissance", "dob", "date_of_birth", "birth_date",
                     "date_naiss", "birthdate"),

  # Enquête
  cluster   = c("cluster", "grappe", "psu", "ea", "zd", "zone_denombrment"),
  equipe    = c("equipe", "team", "enqueteur", "enumerator", "supervisor",
                "surveyor", "team_id"),
  strate    = c("strate", "strata", "region", "departement", "zone"),

  # Sécurité alimentaire
  fcs       = c("fcs", "food_consumption_score", "score_consommation"),
  rcsi      = c("rcsi", "reduced_coping", "coping_strategy", "csi_reduit"),

  # HHS
  hhs_nofood = c("hhs_nofood", "hhs1", "no_food", "pas_nourriture"),
  hhs_sleep  = c("hhs_sleep", "hhs2", "sleep_hungry", "dormir_affame"),
  hhs_day    = c("hhs_wholeday", "hhs3", "whole_day", "journee_sans"),

  # Oedèmes
  oedeme    = c("oedeme", "oedema", "edema", "bilateral_oedema")
)

# Fonction : auto-mapper les colonnes Kobo vers SMART
kobo_auto_map <- function(col_names) {
  mapping <- list()

  for (smart_var in names(KOBO_SMART_MAPPING)) {
    patterns <- KOBO_SMART_MAPPING[[smart_var]]
    matched <- NULL

    for (pat in patterns) {
      # Exact match d'abord
      idx <- grep(paste0("^", pat, "$"), col_names, ignore.case = TRUE)
      if (length(idx) > 0) { matched <- col_names[idx[1]]; break }

      # Partial match
      idx <- grep(pat, col_names, ignore.case = TRUE)
      if (length(idx) > 0) { matched <- col_names[idx[1]]; break }
    }

    if (!is.null(matched)) {
      mapping[[smart_var]] <- list(
        kobo_col   = matched,
        smart_var  = smart_var,
        status     = "detected",
        confidence = "high"
      )
    }
  }

  mapping
}

# Résumé du mapping en data.frame
kobo_mapping_summary <- function(mapping) {
  if (length(mapping) == 0) {
    return(tibble(
      `Variable SMART` = character(),
      `Colonne détectée` = character(),
      Statut = character()
    ))
  }

  map_dfr(names(mapping), function(nm) {
    m <- mapping[[nm]]
    tibble(
      `Variable SMART`   = nm,
      `Colonne détectée` = m$kobo_col,
      Statut             = if (m$status == "detected") "🟢 Détecté" else "🟡 Manuel"
    )
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. IMPORT ODK FICHIER (CSV/XLS depuis export KoboToolbox)
# ══════════════════════════════════════════════════════════════════════════════

import_kobo_file <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))

  tryCatch({
    df <- switch(ext,
      "csv"  = readr::read_csv(file_path, show_col_types = FALSE),
      "xlsx" = readxl::read_excel(file_path),
      "xls"  = readxl::read_excel(file_path),
      stop("Format non supporté : ", ext)
    )

    # Nettoyage Kobo
    df <- kobo_clean_columns(as.data.frame(df))
    as_tibble(df)
  }, error = function(e) {
    NULL
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. RENDU HTML
# ══════════════════════════════════════════════════════════════════════════════

render_kobo_form_card <- function(form) {
  deployed_badge <- if (isTRUE(form$deployed))
    tags$span(style = "background:#27ae60;color:white;padding:2px 8px;border-radius:4px;font-size:10px;", "Déployé")
  else
    tags$span(style = "background:#e74c3c;color:white;padding:2px 8px;border-radius:4px;font-size:10px;", "Non déployé")

  tags$div(
    style = "border:1px solid #ddd;border-radius:8px;padding:12px;margin-bottom:8px;background:white;cursor:pointer;",
    tags$div(style = "display:flex;justify-content:space-between;align-items:center;",
      tags$div(
        tags$div(style = "font-weight:700;font-size:13px;color:#00426A;", form$name),
        tags$div(style = "font-size:11px;color:#888;",
                 paste0(form$submissions, " soumissions | Modifié : ", form$date_modified))
      ),
      tags$div(style = "text-align:right;",
        deployed_badge,
        tags$div(style = "font-size:10px;color:#888;margin-top:4px;", form$uid)
      )
    )
  )
}

render_kobo_mapping_html <- function(mapping_df) {
  if (nrow(mapping_df) == 0) {
    return(tags$p(style = "color:#888;", "Aucune variable SMART détectée automatiquement"))
  }

  rows_html <- paste(apply(mapping_df, 1, function(row) {
    paste0(
      "<tr>",
      "<td style='padding:6px 10px;font-weight:600;'>", row[["Variable SMART"]], "</td>",
      "<td style='padding:6px 10px;font-family:monospace;color:#008DC9;'>",
      row[["Colonne détectée"]], "</td>",
      "<td style='padding:6px 10px;'>", row[["Statut"]], "</td>",
      "</tr>"
    )
  }), collapse = "")

  HTML(paste0(
    "<table style='width:100%;border-collapse:collapse;font-size:12px;'>",
    "<thead><tr style='background:#f0f8ff;border-bottom:2px solid #008DC9;'>",
    "<th style='padding:8px 10px;text-align:left;'>Variable SMART</th>",
    "<th style='padding:8px 10px;text-align:left;'>Colonne Kobo</th>",
    "<th style='padding:8px 10px;text-align:left;'>Statut</th>",
    "</tr></thead><tbody>",
    rows_html,
    "</tbody></table>"
  ))
}

cat("module_kobo_import.R chargé — Import KoboToolbox/ODK\n")
