# Extrait de app_v2_final.R — dashboard accueil — carte double vulnérabilité
  # ── Carte double vulnérabilité — helpers de matching ────────────────────────
  normalize_region <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
    gsub("[^a-z0-9]", "", x)
  }

  # Pour chaque nom de région/département enquête → trouve le nom GADM le plus proche
  # name_col : "NAME_1" pour régions, "NAME_2" pour départements
  match_gadm_regions <- function(survey_names, sf_obj, name_col = "NAME_1") {
    gadm_names <- sf_obj[[name_col]]
    var_col    <- sub("^NAME_", "VARNAME_", name_col)
    varname    <- if (var_col %in% names(sf_obj)) sf_obj[[var_col]] else rep(NA_character_, nrow(sf_obj))
    norm_g     <- normalize_region(gadm_names)

    vapply(survey_names, function(s) {
      ns <- normalize_region(s)

      # 1. Correspondance exacte
      idx <- which(norm_g == ns)
      if (length(idx)) return(gadm_names[idx[1]])

      # 2. Variantes GADM (VARNAME_x, séparées par | ou ,)
      for (i in seq_along(varname)) {
        if (!is.na(varname[i]) && nzchar(varname[i])) {
          vn <- normalize_region(strsplit(varname[i], "[|,;]")[[1]])
          if (ns %in% vn) return(gadm_names[i])
        }
      }

      # 3. Inclusion réciproque (min 3 car.)
      if (nchar(ns) >= 3) {
        ci <- which(vapply(norm_g, function(g)
          nchar(g) >= 3 && (grepl(ns, g, fixed = TRUE) || grepl(g, ns, fixed = TRUE)),
          logical(1)))
        if (length(ci)) return(gadm_names[ci[1]])
      }

      # 4. Correspondance approximative (agrep)
      fi <- agrep(ns, norm_g, max.distance = 0.25, value = FALSE)
      if (length(fi)) return(gadm_names[fi[1]])

      NA_character_
    }, character(1), USE.NAMES = FALSE)
  }

  # ── Carte double vulnérabilité — reactive données (multi-niveaux) ────────────
  dv_map_data <- reactive({
    d <- df_f(); m <- map_cols()
    geo <- selected_geo()
    req(!is.null(geo), !is.null(geo$country_code), nzchar(geo$country_code))
    req("rcsi_cat" %in% names(d), "fcs_cat" %in% names(d))

    # Niveau admin demandé (défaut : 1)
    lvl_req <- if (is.null(input$dv_map_level)) 1L else as.integer(input$dv_map_level)

    # Colonne enquête et nom GADM selon le niveau
    grp_col  <- if (lvl_req == 2L && !is.null(m$dept) && m$dept %in% names(d))
                  m$dept else m$region
    lvl      <- if (identical(grp_col, m$dept) && lvl_req == 2L) 2L else 1L
    name_col <- paste0("NAME_", lvl)
    unit_lbl <- if (lvl == 2L) "d\u00e9partements" else "r\u00e9gions"

    req(!is.null(grp_col), grp_col %in% names(d))

    # Prévalence par unité admin
    prev <- d |>
      filter(!is.na(.data[[grp_col]]), !is.na(rcsi_cat), !is.na(fcs_cat)) |>
      group_by(unit_name = as.character(.data[[grp_col]])) |>
      summarise(
        n_total = n(),
        n_dv    = sum(
          rcsi_cat == "Coping \u00e9lev\u00e9 (\u226519)" & fcs_cat == "Pauvre (<21)",
          na.rm = TRUE
        ),
        pct_dv  = round(n_dv / n_total * 100, 1),
        .groups = "drop"
      )

    if (nrow(prev) == 0) return(NULL)

    # Chargement GADM au niveau demandé
    sf_adm <- tryCatch(
      load_gadm_level(geo$country_code, level = lvl),
      error = function(e) { message("GADM L", lvl, " : ", e$message); NULL }
    )
    if (is.null(sf_adm))
      return(list(prev = prev, sf = NULL, level = lvl, unit_lbl = unit_lbl,
                  error = "gadm_unavailable"))

    # Frontières L1 pour overlay (si on est en L2)
    sf_l1_borders <- if (lvl == 2L) {
      tryCatch(load_gadm_level(geo$country_code, level = 1L),
               error = function(e) NULL)
    } else NULL

    # Jointure fuzzy enquête → GADM
    prev[[name_col]] <- match_gadm_regions(prev$unit_name, sf_adm, name_col = name_col)
    n_matched        <- sum(!is.na(prev[[name_col]]))

    # Jointure spatiale (all GADM polygons, left join)
    sf_joined <- sf_adm |>
      dplyr::left_join(
        dplyr::select(prev, all_of(name_col), n_total, n_dv, pct_dv, unit_name),
        by = name_col
      )

    list(
      sf            = sf_joined,
      sf_l1_borders = sf_l1_borders,
      prev          = prev,
      name_col      = name_col,
      n_gadm        = nrow(sf_adm),
      n_survey      = nrow(prev),
      n_matched     = n_matched,
      country       = geo$country_name,
      level         = lvl,
      unit_lbl      = unit_lbl,
      error         = NULL
    )
  })

  # ── Carte double vulnérabilité — UI section ──────────────────────────────────
  output$accueil_dv_map_row <- renderUI({
    d <- df_f(); m <- map_cols()
    if (is.null(m$region) || !(m$region %in% names(d))) return(NULL)
    if (!("rcsi_cat" %in% names(d)) || !("fcs_cat" %in% names(d))) return(NULL)

    geo           <- selected_geo()
    country_ok    <- !is.null(geo) && !is.null(geo$country_code) && nzchar(geo$country_code)
    country_label <- if (country_ok) geo$country_name else ""

    # Disponibilité GADM L2 + colonne département dans les données
    max_gadm_lvl <- if (country_ok) {
      tryCatch(get_available_levels(geo$country_code), error = function(e) 1L)
    } else 1L
    has_dept  <- !is.null(m$dept) && m$dept %in% names(d)
    show_l2   <- country_ok && (max_gadm_lvl >= 2L) && has_dept

    # Sélecteur niveau (uniquement si L2 dispo)
    lvl_selector <- if (show_l2) {
      current_lvl <- if (is.null(isolate(input$dv_map_level))) "1" else
        isolate(input$dv_map_level)
      tags$div(
        style = "display:flex; align-items:center; gap:6px; font-size:13px;",
        tags$span(
          style = "color:#64748b; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px; white-space:nowrap;",
          icon("layer-group"), " Niveau"
        ),
        tags$div(
          style = "min-width:230px;",
          selectInput(
            "dv_map_level",
            label    = NULL,
            choices  = setNames(
              c("1", "2"),
              c(paste0("Niveau 1 \u2014 R\u00e9gion / Province  (", m$region, ")"),
                paste0("Niveau 2 \u2014 D\u00e9partement / District  (", m$dept, ")"))
            ),
            selected = current_lvl,
            width    = "100%"
          )
        )
      )
    } else NULL

    tagList(
      # En-tête section
      tags$div(
        class = "nutri-section-title",
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px;",
        tags$span(
          icon("map-marked-alt"),
          " Carte de double vuln\u00e9rabilit\u00e9 \u2014 FCS Pauvre \u00d7 rCSI \u00e9lev\u00e9"
        ),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
          lvl_selector,
          if (country_ok)
            tags$span(
              style = "font-size:12px; color:#64748b; font-weight:600; white-space:nowrap;",
              icon("globe-africa"), "\u00a0", country_label
            ),
          downloadButton(
            "dl_dv_map_csv",
            label = " CSV",
            icon  = icon("download"),
            style = "font-size:11px; padding:3px 10px; height:28px; line-height:1.4;",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      # Corps
      if (!country_ok) {
        tags$div(
          style = paste0(
            "background:#f0f8ff; border-left:4px solid #0077B6;",
            "border-radius:8px; padding:16px 20px; margin-bottom:16px;",
            "font-size:13px; color:#1e293b; display:flex; align-items:center; gap:12px;"
          ),
          tags$div(
            style = paste0("width:40px; height:40px; border-radius:10px; background:#0077B6;",
                           "display:flex; align-items:center; justify-content:center;",
                           "color:white; font-size:18px; flex-shrink:0;"),
            icon("map")
          ),
          tags$div(
            tags$b("S\u00e9lectionnez un pays pour afficher la carte"),
            tags$br(),
            tags$span(
              style = "font-size:12px; color:#64748b;",
              "Ouvrez l'onglet ", tags$b("Cartographie"),
              " dans la barre lat\u00e9rale, s\u00e9lectionnez votre pays,",
              " puis revenez sur ce tableau de bord."
            )
          )
        )
      } else {
        fluidRow(
          column(12,
            tags$div(class = "nutri-chart-card",
              uiOutput("accueil_dv_map_note"),
              leafletOutput("accueil_dv_map", height = "500px")
            )
          )
        )
      }
    )
  })

  # ── Note de matching ─────────────────────────────────────────────────────────
  output$accueil_dv_map_note <- renderUI({
    data <- dv_map_data()
    if (is.null(data)) return(NULL)

    if (!is.null(data$error) && data$error == "gadm_unavailable") {
      return(tags$div(
        style = "background:#fff3e0; border-left:3px solid #D98B14; border-radius:6px; padding:10px 14px; margin-bottom:10px; font-size:12px; color:#7c2d12;",
        icon("exclamation-triangle"),
        paste0(" Impossible de charger les fronti\u00e8res GADM niveau ", data$level,
               " pour ce pays. V\u00e9rifiez la connexion internet ou le cache GADM.")
      ))
    }

    n_m      <- data$n_matched
    n_s      <- data$n_survey
    ul       <- data$unit_lbl
    nm_col   <- data$name_col
    unmatched <- data$prev$unit_name[is.na(data$prev[[nm_col]])]

    # Badge niveau admin
    lvl_badge <- tags$span(
      style = paste0("background:", if (data$level == 2L) "#6B3FA0" else "#0077B6",
                     "; color:white; padding:1px 8px; border-radius:12px;",
                     "font-size:10px; font-weight:700; margin-right:6px;"),
      paste0("L", data$level, " \u2014 ", toupper(substr(ul, 1, 1)), substr(ul, 2, nchar(ul)))
    )

    if (n_m == n_s) {
      tags$div(
        style = "background:#f0fdf4; border-left:3px solid #2E7D52; border-radius:6px; padding:8px 14px; margin-bottom:10px; font-size:11.5px; color:#166534; display:flex; align-items:center; gap:6px;",
        lvl_badge,
        icon("check-circle"),
        paste0(" ", n_m, "/", n_s, " ", ul, " appari\u00e9es avec GADM (", data$country, ").")
      )
    } else {
      tags$div(
        style = "background:#fffbeb; border-left:3px solid #f59e0b; border-radius:6px; padding:8px 14px; margin-bottom:10px; font-size:11.5px; color:#78350f;",
        lvl_badge,
        icon("exclamation-circle"),
        paste0(" ", n_m, "/", n_s, " ", ul, " appari\u00e9es. "),
        tags$b("Non appari\u00e9es : "),
        paste(unmatched, collapse = ", "),
        paste0(". V\u00e9rifiez l'orthographe par rapport aux noms GADM.")
      )
    }
  })

  # ── Rendu Leaflet choroplèthe (multi-niveaux) ─────────────────────────────────
  output$accueil_dv_map <- renderLeaflet({
    data <- dv_map_data()
    req(!is.null(data), !is.null(data$sf))

    sf_data  <- data$sf
    nm_col   <- data$name_col
    lvl      <- data$level

    # Palette rouge séquentielle
    all_pcts   <- sf_data$pct_dv[!is.na(sf_data$pct_dv)]
    pal_domain <- if (length(all_pcts) == 0 || max(all_pcts) == 0)
      c(0, 30) else c(0, max(all_pcts, 5))

    pal <- colorNumeric(
      palette  = c("#f8fafc", "#fee2e2", "#fca5a5", "#ef4444", "#b91c1c", "#450a0a"),
      domain   = pal_domain,
      na.color = "#e2e8f0"
    )

    # Étiquettes : nom + région parente (L2 uniquement)
    labels <- lapply(seq_len(nrow(sf_data)), function(i) {
      row  <- sf_data[i, ]
      name <- row[[nm_col]]
      pct  <- row$pct_dv
      ndv  <- row$n_dv
      ntot <- row$n_total

      parent <- if (lvl == 2L && "NAME_1" %in% names(sf_data))
        paste0("<span style='color:#64748b;font-size:11px;'>",
               row$NAME_1, "</span><br/>") else ""

      if (is.na(pct)) {
        htmltools::HTML(sprintf(
          "%s<b>%s</b><br/><i style='color:#94a3b8'>Non couvert</i>",
          parent, name
        ))
      } else {
        htmltools::HTML(sprintf(
          "%s<b>%s</b><br/>Double vuln.\u00a0: <b style='color:#BF3636'>%.1f%%</b><br/>%d / %d m\u00e9nages",
          parent, name, pct,
          if (is.na(ndv)) 0L else as.integer(ndv),
          if (is.na(ntot)) 0L else as.integer(ntot)
        ))
      }
    })

    # Contrôle info
    ctrl_html <- htmltools::HTML(paste0(
      "<div style='background:white;padding:6px 10px;border-radius:6px;",
      "border:1px solid #e2e8f0;font-size:11px;color:#64748b;line-height:1.5;'>",
      "<b>", data$country, "</b><br/>",
      "Niveau\u00a0", lvl, " \u2014 ",
      data$n_matched, "/", data$n_survey, "\u00a0", data$unit_lbl,
      "</div>"
    ))

    m <- leaflet(sf_data) |>
      addProviderTiles("Esri.WorldGrayCanvas",
                       options = providerTileOptions(minZoom = 3, maxZoom = 14)) |>
      addPolygons(
        fillColor    = ~pal(pct_dv),
        fillOpacity  = 0.78,
        color        = "white",
        weight       = if (lvl == 2L) 0.7 else 1.2,
        opacity      = 1,
        smoothFactor = 0.4,
        highlight    = highlightOptions(
          weight = 3, color = "#0077B6", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label        = labels,
        labelOptions = labelOptions(
          style  = list("font-size" = "13px", "padding" = "6px 10px",
                        "border-radius" = "6px"),
          sticky = FALSE
        )
      )

    # Overlay frontières L1 (contexte géographique en mode L2)
    if (lvl == 2L && !is.null(data$sf_l1_borders)) {
      m <- m |>
        addPolygons(
          data        = data$sf_l1_borders,
          fill        = FALSE,
          color       = "#1e3a5f",
          weight      = 2,
          opacity     = 0.55,
          smoothFactor = 0.4,
          label       = ~NAME_1,
          labelOptions = labelOptions(
            style    = list("font-size" = "11px", "color" = "#1e3a5f",
                            "font-weight" = "bold"),
            sticky   = FALSE,
            textOnly = TRUE
          ),
          options     = pathOptions(clickable = FALSE)
        )
    }

    m |>
      addLegend(
        pal      = pal,
        values   = ~pct_dv,
        opacity  = 0.85,
        title    = htmltools::HTML(
          "Double vuln\u00e9rabilit\u00e9<br/><small>FCS Pauvre + rCSI \u2265 19</small>"
        ),
        position  = "bottomright",
        labFormat = labelFormat(suffix = "%")
      ) |>
      addControl(html = ctrl_html, position = "topleft")
  })

  # ── Export CSV carte ─────────────────────────────────────────────────────────
  output$dl_dv_map_csv <- downloadHandler(
    filename = function() {
      geo  <- selected_geo()
      pays <- if (!is.null(geo) && nzchar(geo$country_code)) geo$country_code else "pays"
      lvl  <- if (is.null(input$dv_map_level)) 1L else as.integer(input$dv_map_level)
      paste0("dv_carte_L", lvl, "_", pays, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      data <- dv_map_data(); req(!is.null(data))
      nm   <- data$name_col
      ul   <- sub("\u00e9", "e", data$unit_lbl)  # ASCII pour noms de colonnes CSV

      out <- data$prev |>
        rename(
          !!paste0(ul, "_enquete") := unit_name,
          !!paste0(ul, "_gadm")    := all_of(nm)
        ) |>
        mutate(
          apparie       = if_else(!is.na(.data[[paste0(ul, "_gadm")]]), "Oui", "Non"),
          niveau_admin  = paste0("L", data$level)
        ) |>
        rename(
          n_menages       = n_total,
          n_double_vuln   = n_dv,
          pct_double_vuln = pct_dv
        ) |>
        arrange(desc(pct_double_vuln))

      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

