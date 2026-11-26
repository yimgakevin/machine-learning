# carto_server_pro.R - Server cartographie PROFESSIONNELLE COMPLET
# Remplace server_carto_v2() avec toutes les fonctionnalites

source("carto_pro_engine.R")
if (!exists("load_gadm")) source("module_carto_v2.R")

# Fonction server complete
server_carto_pro <- function(input, output, session, df_f, map_cols) {
  cat("[CARTO] server start
")

  m <- reactive(map_cols())

  # == TITRE DYNAMIQUE ==
  output$carto_titre_pro_ui <- renderUI({
    ind <- input$carto_indicateur
    if (is.null(ind)) ind <- "GAM"
    label <- LABELS_FR_PRO[ind]
    if (is.na(label)) label <- ind
    tags$span(icon("map-marked-alt"), paste0(" ", label, " par ", input$carto_niveau))
  })

  # == FILTRES DYNAMIQUES ==
  output$carto_filtre_region_ui <- renderUI({
    d <- df_f(); mc <- m()
    if (is.null(mc$region) || !mc$region %in% names(d)) return(NULL)
    vals <- sort(unique(na.omit(as.character(d[[mc$region]]))))
    selectInput("carto_filtre_region", "Filtrer par region :",
      choices = c("Toutes", vals), selected = "Toutes", width = "100%")
  })

  output$carto_filtre_dept_ui <- renderUI({
    req(input$carto_niveau %in% c("Departement","Arrondissement"))
    d <- df_f(); mc <- m()
    if (is.null(mc$dept) || !mc$dept %in% names(d)) return(NULL)
    d_f <- d
    if (!is.null(input$carto_filtre_region) && input$carto_filtre_region != "Toutes")
      d_f <- d_f[d_f[[mc$region]] == input$carto_filtre_region, ]
    vals <- sort(unique(na.omit(as.character(d_f[[mc$dept]]))))
    selectInput("carto_filtre_dept", "Filtrer par departement :",
      choices = c("Tous", vals), selected = "Tous", width = "100%")
  })

  # == INDICATEURS DISPONIBLES ==
  output$carto_indicateur_ui <- renderUI({
    d <- df_f()
    inds_bin <- intersect(c("GAM","SAM","MAM","Stunting","Sev_Stunting",
      "Underweight","MUAC_GAM","MUAC_SAM"), names(d))
    choices_bin <- setNames(inds_bin, sapply(inds_bin, function(x) {
      l <- LABELS_FR_PRO[x]; if(is.na(l)) x else l }))
    choices_num <- c("WHZ moyen"="whz_moy", "HAZ moyen"="haz_moy",
      "MUAC moyen"="muac_moy", "N enquetes"="n")
    selectInput("carto_indicateur", NULL,
      choices = c(choices_bin, choices_num),
      selected = if("GAM" %in% inds_bin) "GAM" else "n", width = "100%")
  })

  # == KPI PANEL ==
  output$carto_kpi_panel <- renderUI({
    if (!isTRUE(input$carto_kpi_overlay)) return(NULL)
    d <- df_f(); req(d); mc <- m()
    ind <- input$carto_indicateur; if(is.null(ind)) ind <- "GAM"
    niv <- input$carto_niveau; if(is.null(niv)) niv <- "Region"
    col_geo <- switch(niv, "Region"=mc$region, "Departement"=mc$dept,
      "Arrondissement"=detect_col(names(d), c("arrondissement","district")), mc$region)
    req(col_geo, col_geo %in% names(d))
    agg <- agreger_geo(d, col_geo, mc)
    tags$div(style="margin-bottom:8px;", HTML(make_kpi_overlay(agg, ind)))
  })

  # == HELPER: resolve geo col and aggregate ==
  get_agg <- function() {
    d <- df_f(); mc <- m()
    niv <- input$carto_niveau; if(is.null(niv)) niv <- "Region"
    col_geo <- switch(niv, "Region"=mc$region, "Departement"=mc$dept,
      "Arrondissement"=detect_col(names(d), c("arrondissement","district","commune")), mc$region)
    # Apply filters
    d_f <- d
    filtre_r <- input$carto_filtre_region
    filtre_d <- input$carto_filtre_dept
    if (!is.null(filtre_r) && !is.null(mc$region) && mc$region %in% names(d) && filtre_r != "Toutes")
      d_f <- d_f[d_f[[mc$region]] == filtre_r, ]
    if (!is.null(filtre_d) && !is.null(mc$dept) && mc$dept %in% names(d) && filtre_d != "Tous")
      d_f <- d_f[d_f[[mc$dept]] == filtre_d, ]
    agg <- if (!is.null(col_geo) && col_geo %in% names(d_f)) agreger_geo(d_f, col_geo, mc)
    else if (!is.null(mc$region) && mc$region %in% names(d_f)) agreger_geo(d_f, mc$region, mc)
    else NULL
    if (is.null(col_geo) && !is.null(agg)) col_geo <- mc$region
    list(agg=agg, d_f=d_f, col_geo=col_geo, niv=niv, mc=mc)
  }

  get_sf <- function(niv, filtre_r=NULL, filtre_d=NULL) {
    gadm_level <- switch(niv, "Region"=1, "Departement"=2, "Arrondissement"=3, 1)
    d_now <- tryCatch(df_f(), error=function(e) NULL)
    mc_now <- tryCatch(m(), error=function(e) NULL)
    # arrfix : aligner le niveau GADM sur la colonne reellement agregee
    if (niv == "Departement") {
      col_dept <- if (!is.null(mc_now)) mc_now$dept else NULL
      if (is.null(col_dept) || is.null(d_now) || !(col_dept %in% names(d_now))) gadm_level <- 1
    }
    if (niv == "Arrondissement") {
      col_arr <- if (!is.null(d_now)) detect_col(names(d_now), c("arrondissement","district","commune")) else NULL
      if (is.null(col_arr) || !(col_arr %in% names(d_now))) gadm_level <- 1
    }
    gadm_sf <- load_gadm(gadm_level)
    gadm_col <- GADM_NAMES[[paste0("L", gadm_level)]]
    if (!is.null(gadm_sf)) {
      if (niv=="Departement" && !is.null(filtre_r) && filtre_r!="Toutes" && filtre_r!="")
        gadm_sf <- gadm_sf[gadm_sf$NAME_1 == filtre_r, ]
      if (niv=="Arrondissement" && gadm_level==3) {
        if (!is.null(filtre_d) && filtre_d!="Tous" && filtre_d!="")
          gadm_sf <- gadm_sf[gadm_sf$NAME_2 == filtre_d, ]
        else if (!is.null(filtre_r) && filtre_r!="Toutes" && filtre_r!="")
          gadm_sf <- gadm_sf[gadm_sf$NAME_1 == filtre_r, ]
      }
      if (nrow(gadm_sf)==0) gadm_sf <- NULL
    }
    list(sf=gadm_sf, col=gadm_col, level=gadm_level)
  }
  # == CARTE LEAFLET PRINCIPALE ==
  output$carto_leaflet_main <- renderLeaflet({
    ctx <- get_agg(); req(ctx$agg)
    agg <- ctx$agg; niv <- ctx$niv
    ind <- input$carto_indicateur; if(is.null(ind)) ind <- "GAM"
    fond <- input$carto_fond; if(is.null(fond)) fond <- "Esri.WorldGrayCanvas"
    type_c <- input$carto_type; if(is.null(type_c)) type_c <- "Choroplethe"
    show_labels <- isTRUE(input$carto_labels)
    # Symbologie experte
    cls_method <- input$carto_class_method; if(is.null(cls_method)) cls_method <- "oms"
    cls_k      <- input$carto_n_classes;    if(is.null(cls_k))    cls_k    <- 5L
    cls_pal    <- input$carto_palette;      if(is.null(cls_pal))  cls_pal  <- "ylorrd"
    opa_fill   <- input$carto_opacity;      if(is.null(opa_fill)) opa_fill <- 0.78
    habillage  <- isTRUE(input$carto_habillage)
    show_coords <- isTRUE(input$carto_coords)

    sf_info <- get_sf(niv, input$carto_filtre_region, input$carto_filtre_dept)
    gadm_sf <- sf_info$sf; gadm_col <- sf_info$col

    map_leaf <- leaflet() |> addProviderTiles(fond) |>
      addScaleBar(position="bottomleft") |>
      addMiniMap(toggleDisplay=TRUE, minimized=TRUE, tiles="Esri.WorldGrayCanvas") |>
      addFullscreenControl(position="topleft")

    has_ind <- ind %in% names(agg)
    nrow_sf_nd <- 0L

    # CHOROPLETHE avec classification configurable
    if (!is.null(gadm_sf) && type_c %in% c("Choroplethe","Tous")) {
      sf_join <- gadm_sf; sf_join$Zone <- sf_join[[gadm_col]]
      agg_n <- agg; agg_n$ZN <- norm_zone_name(agg_n$Zone); agg_n$Zone <- NULL
      sf_join$ZN <- norm_zone_name(sf_join$Zone)
      sf_join <- merge(sf_join, agg_n[!is.na(agg_n$ZN), ], by="ZN", all.x=TRUE)
      sf_join$ZN <- NULL
      sf_join$val_carte <- if(has_ind) sf_join[[ind]] else sf_join$n

      sf_join$popup_txt <- vapply(seq_len(nrow(sf_join)), function(i) {
        nm <- sf_join$Zone[i]; zn <- norm_zone_name(nm)
        row <- agg[norm_zone_name(agg$Zone) == zn & !is.na(norm_zone_name(agg$Zone)), ]
        if(nrow(row)==0) return(paste0("<b>",nm,"</b><br>Pas de donnees"))
        make_popup_pro(nm, row, names(agg))
      }, character(1))

      vals_ok <- sf_join$val_carte[!is.na(sf_join$val_carte)]
      if (length(vals_ok) > 0) {
        cls <- classify_zones_pro(vals_ok, cls_method, cls_k, ind, cls_pal)
      }
      if (!is.null(cls)) {
        map_leaf <- map_leaf |>
          addPolygons(data=sf_join, fillColor=~cls$pal(val_carte),
            fillOpacity=opa_fill, color="white", weight=1.8, popup=~popup_txt,
            layerId=~Zone,
            label=~paste0(Zone,": ",ifelse(is.na(val_carte),"N/A",paste0(round(val_carte,1),"%"))),
            highlightOptions=highlightOptions(weight=3,color="#008DC9",fillOpacity=0.92,bringToFront=TRUE),
            group="Choroplethe")

        # Zones sans donn\u00e9es : gris + bordure pointill\u00e9e (distingu\u00e9es des donn\u00e9es)
        sf_nd <- sf_join[is.na(sf_join$val_carte), ]
        nrow_sf_nd <- nrow(sf_nd)
        if (nrow(sf_nd) > 0) {
          map_leaf <- map_leaf |>
            addPolygons(data=sf_nd, fillColor="#e8e8e8", fillOpacity=0.85,
              color="#9a9a9a", weight=0.9, dashArray="3,3",
              popup=~paste0("<b>",Zone,"</b><br>Pas de donn\u00e9es pour cette zone"),
              group="Sans donn\u00e9es")
        }

        map_leaf <- map_leaf |> addControl(html=legend_dynamic_html_pro(ind, cls), position="bottomright")
      }
    }

    # CERCLES PROPORTIONNELS
    if (type_c %in% c("Cercles proportionnels","Tous")) {
      agg_pts <- agg[!is.na(agg$lat) & !is.na(agg$lon) & is.finite(agg$lat), ]
      if (nrow(agg_pts)==0 && !is.null(gadm_sf)) {
        ctr <- gadm_sf; ctr$Zone <- ctr[[gadm_col]]
        ctr <- sf::st_centroid(ctr)
        coords <- sf::st_coordinates(ctr)
        ctr_df <- data.frame(Zone=ctr$Zone, lat=coords[,2], lon=coords[,1])
        agg_pts <- merge(agg, ctr_df, by="Zone")
        agg_pts <- agg_pts[!is.na(agg_pts$lat), ]
      }
      if (nrow(agg_pts)>0 && has_ind) {
        agg_pts$val_pts <- agg_pts[[ind]]
        pal_c <- pal_oms_pro(agg_pts$val_pts[!is.na(agg_pts$val_pts)], ind)
        map_leaf <- map_leaf |>
          addCircleMarkers(data=agg_pts, lat=~lat, lng=~lon,
            radius=~scales::rescale(sqrt(n+1), to=c(8,30)),
            fillColor=~pal_c(val_pts), fillOpacity=0.85, color="white", weight=2,
            label=~paste0(Zone,": ",round(val_pts,1),"%"), group="Cercles")
      }
    }

    # ETIQUETTES
    if (show_labels && !is.null(gadm_sf) && has_ind) {
      ctr <- tryCatch({
        sf_tmp <- gadm_sf; sf_tmp$Zone <- sf_tmp[[gadm_col]]
        agg_l <- agg[, c("Zone", ind), drop = FALSE]
        agg_l$ZN <- norm_zone_name(agg_l$Zone); agg_l$Zone <- NULL
        sf_tmp$ZN <- norm_zone_name(sf_tmp$Zone)
        sf_tmp <- merge(sf_tmp, agg_l[!is.na(agg_l$ZN), , drop = FALSE], by = "ZN", all.x = TRUE)
        sf_tmp$ZN <- NULL
        sf_tmp <- sf_tmp[!is.na(sf_tmp[[ind]]), ]
        sf_ctr <- sf::st_centroid(sf_tmp)
        coords <- sf::st_coordinates(sf_ctr)
        data.frame(Zone=sf_ctr$Zone, val=sf_ctr[[ind]], lon2=coords[,1], lat2=coords[,2])
      }, error=function(e) NULL)
      if (!is.null(ctr) && nrow(ctr)>0) {
        ctr$lbl <- paste0(ctr$Zone, "
", round(ctr$val,1), "%")
        map_leaf <- map_leaf |>
          addLabelOnlyMarkers(data=ctr, lat=~lat2, lng=~lon2, label=~lbl,
            labelOptions=labelOptions(noHide=TRUE, direction="center", textOnly=TRUE,
              style=list("color"="#00426A","font-weight"="700","font-size"="11px",
                "font-family"="Segoe UI","text-shadow"="1px 1px 2px white,-1px -1px 2px white")),
            group="Etiquettes")
      }
    }

    # Couches et auto-zoom
    couches <- c()
    if(type_c %in% c("Choroplethe","Tous")) {
      couches <- c(couches,"Choroplethe")
      if (nrow_sf_nd > 0) couches <- c(couches, "Sans donn\u00e9es")
    }
    if(type_c %in% c("Cercles proportionnels","Tous")) couches <- c(couches,"Cercles")
    if(show_labels) couches <- c(couches,"Etiquettes")
    if(length(couches)>1) map_leaf <- map_leaf |> addLayersControl(overlayGroups=couches, options=layersControlOptions(collapsed=FALSE))

    # Habillage : cartouche titre/source + fl\u00e8che du Nord
    if (habillage) {
      lvl_lbl <- switch(niv, Region="R\u00e9gion", Departement="D\u00e9partement",
                        Arrondissement="Arrondissement", niv)
      cc <- tryCatch(get_carto_country(), error=function(e) NULL)
      pays_txt <- tryCatch(if (!is.null(cc)) get_country_name(cc) else NULL, error=function(e) NULL)
      titre_ind <- LABELS_FR_PRO[ind]; if (is.na(titre_ind) || is.null(titre_ind)) titre_ind <- ind
      map_leaf <- map_leaf |>
        addControl(html = make_cartouche(
          titre      = paste0(titre_ind, " par ", lvl_lbl),
          sous_titre = pays_txt,
          source_txt = "Fronti\u00e8res : GADM v4.1 \u00b7 Donn\u00e9es : enqu\u00eate",
          date       = format(Sys.Date(), "%d/%m/%Y")),
          position = "topleft") |>
        addControl(html = north_arrow_html(), position = "topright")
    }

    # Outil de mesure (distance / surface)
    map_leaf <- tryCatch(map_leaf |> leaflet.extras::addMeasurePathToolbar(),
                         error = function(e) map_leaf)

    # Coordonn\u00e9es au survol
    if (show_coords) {
      map_leaf <- htmlwidgets::onRender(map_leaf, htmlwidgets::JS("
        function(el, x) {
          var map = this; window.__cmap = map;
          var ctrl = L.control({position: 'bottomright'});
          ctrl.onAdd = function(m) {
            var d = L.DomUtil.create('div', 'carto-coords-ctrl');
            d.style.cssText = 'background:rgba(255,255,255,.92);padding:2px 8px;border-radius:6px;font:10px Segoe UI,sans-serif;color:#555;box-shadow:0 1px 4px rgba(0,0,0,.15);margin-bottom:2px;';
            m.on('mousemove', function(e) {
              d.textContent = 'Lat ' + e.latlng.lat.toFixed(4) + ' \u00b0 | Lng ' + e.latlng.lng.toFixed(4) + ' \u00b0';
            });
            m.on('mouseout', function() { d.textContent = ''; });
            return d;
          };
          ctrl.addTo(this);
        }"))
    }

    if (!is.null(gadm_sf) && nrow(gadm_sf)>0) {
      bbox <- tryCatch(sf::st_bbox(gadm_sf), error=function(e) NULL)
      if (!is.null(bbox) && all(is.finite(as.numeric(bbox)))) {
        p <- (as.numeric(bbox[3])-as.numeric(bbox[1]))*0.05
        map_leaf <- map_leaf |> fitBounds(as.numeric(bbox[1])-p,as.numeric(bbox[2])-p,as.numeric(bbox[3])+p,as.numeric(bbox[4])+p)
      }
    }
    map_leaf
  })

  # == RECHERCHE DE ZONE (flyTo sur la carte principale) ==
  output$carto_search_ui <- renderUI({
    ctx <- tryCatch(get_agg(), error = function(e) NULL)
    req(!is.null(ctx), !is.null(ctx$agg))
    zones <- sort(unique(ctx$agg$Zone))
    req(length(zones) > 0)
    tagList(
      selectizeInput("carto_search_zone", NULL,
        choices  = c("Choisir une zone\u2026" = "", zones),
        selected = "", width = "100%",
        options = list(placeholder = "Tapez un nom\u2026")),
      actionButton("carto_search_go", label = NULL,
                   icon = icon("location-crosshairs"),
                   class = "btn btn-primary btn-sm", width = "100%")
    )
  })

  observeEvent(input$carto_search_go, {
    z <- input$carto_search_zone
    if (is.null(z) || !nzchar(z)) return()
    niv <- input$carto_niveau; if (is.null(niv)) niv <- "Region"
    sf_info <- tryCatch(get_sf(niv, input$carto_filtre_region, input$carto_filtre_dept),
                        error = function(e) NULL)
    req(!is.null(sf_info), !is.null(sf_info$sf))
    sfz <- sf_info$sf[sf_info$sf[[sf_info$col]] == z, ]
    if (nrow(sfz) == 0) {
      showNotification(paste0("Zone introuvable : ", z), type = "warning", duration = 3)
      return()
    }
    bb <- tryCatch(sf::st_bbox(sfz), error = function(e) NULL)
    req(!is.null(bb) && all(is.finite(as.numeric(bb))))
    leafletProxy("carto_leaflet_main") |>
      flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })

  # == CARTE BIVARIATE ==
  observeEvent(input$run_bivariate, {
    ctx <- get_agg(); req(ctx$agg)
    agg <- ctx$agg; niv <- ctx$niv
    ind_x <- input$carto_bivar_x; ind_y <- input$carto_bivar_y
    req(ind_x %in% names(agg), ind_y %in% names(agg))
    sf_info <- get_sf(niv, input$carto_filtre_region, input$carto_filtre_dept)
    gadm_sf <- sf_info$sf; gadm_col <- sf_info$col; req(gadm_sf)

    sf_join <- gadm_sf; sf_join$Zone <- sf_join[[gadm_col]]
    sf_join <- merge(sf_join, agg, by="Zone", all.x=TRUE)
    valid <- !is.na(sf_join[[ind_x]]) & !is.na(sf_join[[ind_y]])
    sf_valid <- sf_join[valid, ]
    if (nrow(sf_valid) < 3) { showNotification("Pas assez de zones avec donnees", type="warning"); return() }
    bivar <- classify_bivariate(sf_valid[[ind_x]], sf_valid[[ind_y]])
    sf_valid$bivar_color <- bivar$colors

    output$carto_bivariate_map <- renderLeaflet({
      bv <- leaflet() |> addProviderTiles("Esri.WorldGrayCanvas") |>
        addScaleBar(position="bottomleft") |> addFullscreenControl()
      bv <- bv |> addPolygons(data=sf_valid, fillColor=~bivar_color,
        fillOpacity=0.8, color="white", weight=1.5,
        popup=~vapply(seq_len(nrow(sf_valid)), function(i)
          make_popup_pro(sf_valid$Zone[i], sf_valid[i,,drop=FALSE], names(sf_valid)), character(1)),
        label=~paste0(Zone,": ",ind_x,"=",round(sf_valid[[ind_x]],1),"% / ",ind_y,"=",round(sf_valid[[ind_y]],1),"%"),
        highlightOptions=highlightOptions(weight=3,color="#00426A",fillOpacity=0.9,bringToFront=TRUE))
      bv <- bv |> addControl(html=legend_bivariate_html(ind_x, ind_y), position="bottomright")
      sf_na <- sf_join[!valid, ]
      if (nrow(sf_na)>0) bv <- bv |> addPolygons(data=sf_na, fillColor="#e0e0e0", fillOpacity=0.5, color="white", weight=1)
      bbox <- tryCatch(sf::st_bbox(gadm_sf), error=function(e) NULL)
      if (!is.null(bbox)) { p2 <- (as.numeric(bbox[3])-as.numeric(bbox[1]))*0.05
        bv <- bv |> fitBounds(as.numeric(bbox[1])-p2,as.numeric(bbox[2])-p2,as.numeric(bbox[3])+p2,as.numeric(bbox[4])+p2) }
      bv
    })
  })

  # == MULTI-INDICATEURS ==
  carto_multi_sf <- reactiveVal(NULL)
  carto_multi_inds_sel <- reactiveVal(NULL)

  observeEvent(input$run_multi_carte, {
    ctx <- get_agg(); req(ctx$agg)
    inds <- input$carto_multi_inds; req(length(inds) >= 2)
    sf_info <- get_sf(ctx$niv, input$carto_filtre_region, input$carto_filtre_dept)
    req(sf_info$sf)
    sf_join <- sf_info$sf; sf_join$Zone <- sf_join[[sf_info$col]]
    sf_join <- merge(sf_join, ctx$agg, by="Zone", all.x=TRUE)
    carto_multi_sf(sf_join)
    carto_multi_inds_sel(inds)
  })

  # Hauteur dynamique selon le nombre d'indicateurs et colonnes
  output$carto_multi_plot_ui <- renderUI({
    req(carto_multi_sf(), carto_multi_inds_sel())
    n_inds <- length(carto_multi_inds_sel())
    ncol_sel <- as.integer(input$carto_multi_ncol %||% 2)
    n_rows <- ceiling(n_inds / ncol_sel)
    plot_height <- max(450, n_rows * 420)
    plotOutput("carto_multi_plot", height = paste0(plot_height, "px"))
  })

  output$carto_multi_plot <- renderPlot({
    req(carto_multi_sf(), carto_multi_inds_sel())
    ncol_sel <- as.integer(input$carto_multi_ncol %||% 2)
    carte_multi_indicateurs(carto_multi_sf(), carto_multi_inds_sel(),
                            get_agg()$niv, nrow(get_agg()$d_f),
                            ncol_maps = ncol_sel)
  }, res = 120)

  # Modal plein écran
  observeEvent(input$carto_multi_fullscreen, {
    req(carto_multi_sf(), carto_multi_inds_sel())
    n_inds <- length(carto_multi_inds_sel())
    full_h <- max(600, ceiling(n_inds / 2) * 500)
    showModal(modalDialog(
      title = tags$span(icon("expand"), " Cartes multi-indicateurs — Plein écran"),
      size = "l",
      easyClose = TRUE,
      tags$div(style = "max-height:85vh; overflow-y:auto;",
        plotOutput("carto_multi_plot_full", height = paste0(full_h, "px"))
      ),
      footer = modalButton("Fermer")
    ))
    output$carto_multi_plot_full <- renderPlot({
      carte_multi_indicateurs(carto_multi_sf(), carto_multi_inds_sel(),
                              get_agg()$niv, nrow(get_agg()$d_f),
                              ncol_maps = 2)
    }, res = 140)
  })

  # == CARTE STATIQUE ggplot2 ==
  output$carto_ggplot_main <- renderPlot({
    ctx <- get_agg(); req(ctx$agg)
    ind <- input$carto_indicateur; if(is.null(ind)) ind <- "GAM"
    pal <- input$carto_pal_static; if(is.null(pal)) pal <- "oms_sequentiel"
    sf_info <- get_sf(ctx$niv, input$carto_filtre_region, input$carto_filtre_dept)
    req(sf_info$sf)
    sf_join <- sf_info$sf; sf_join$Zone <- sf_join[[sf_info$col]]
    sf_join <- merge(sf_join, ctx$agg, by="Zone", all.x=TRUE)
    carte_statique_pro(sf_join, ind, ctx$niv, ind, nrow(ctx$d_f), pal, isTRUE(input$carto_static_labels))
  }, res=120, width=900, height=650)
  # == EXPORT STATIQUE QUALITE PUBLICATION (PNG 300 dpi / PDF) ==
  carto_static_gg <- reactive({
    ctx <- get_agg(); req(ctx$agg)
    ind <- input$carto_indicateur; if (is.null(ind)) ind <- "GAM"
    req(ind %in% names(ctx$agg))
    sf_info <- get_sf(ctx$niv, input$carto_filtre_region, input$carto_filtre_dept)
    req(!is.null(sf_info$sf), !is.null(sf_info$col))
    sfj <- sf_info$sf; sfj$Zone <- sfj[[sf_info$col]]
    agg_x <- ctx$agg; agg_x$ZN <- norm_zone_name(agg_x$Zone); agg_x$Zone <- NULL
    sfj$ZN <- norm_zone_name(sfj$Zone)
    sfj <- merge(sfj, agg_x[!is.na(agg_x$ZN), ], by = "ZN", all.x = TRUE)
    sfj$ZN <- NULL
    req(ind %in% names(sfj))
    lvl_lbl <- switch(ctx$niv, Region="R\u00e9gion", Departement="D\u00e9partement",
                      Arrondissement="Arrondissement", ctx$niv)
    titre_ind <- LABELS_FR_PRO[ind]; if (is.na(titre_ind) || is.null(titre_ind)) titre_ind <- ind
    cc <- tryCatch(get_carto_country(), error = function(e) NULL)
    pays_txt <- tryCatch(if (!is.null(cc)) get_country_name(cc) else NULL, error = function(e) NULL)
    mth <- if (is.null(input$carto_class_method)) "oms" else input$carto_class_method
    carte_statique_pro(sfj, ind,
      titre = paste0(titre_ind, " par ", lvl_lbl),
      sous_titre = pays_txt,
      source_txt = paste0("Fronti\u00e8res : GADM v4.1 \u00b7 Donn\u00e9es : enqu\u00eate \u00b7 M\u00e9thode : ",
        if (!is.null(CLASS_METHODS_PRO[mth])) unname(CLASS_METHODS_PRO[mth]) else mth,
        " \u00b7 ", format(Sys.Date(), "%d/%m/%Y")),
      method = mth,
      k = if (is.null(input$carto_n_classes)) 5 else as.integer(input$carto_n_classes),
      palette = if (is.null(input$carto_palette)) "ylorrd" else input$carto_palette)
  })

  output$dl_carte_hd <- downloadHandler(
    filename = function() paste0("carte_",
      if (is.null(input$carto_indicateur)) "carto" else input$carto_indicateur,
      "_", Sys.Date(), "_300dpi.png"),
    content = function(file) {
      p <- carto_static_gg()
      if (is.null(p)) {
        png(file, width = 3300, height = 2550, res = 300); par(mar = c(2, 2, 2, 2))
        plot.new(); text(0.5, 0.5, "Carte indisponible : donn\u00e9es insuffisantes", cex = 1.4); dev.off()
      } else {
        ggsave(file, p, width = 11, height = 8.5, dpi = 300, bg = "white")
      }
    })

  output$dl_carte_pdf <- downloadHandler(
    filename = function() paste0("carte_",
      if (is.null(input$carto_indicateur)) "carto" else input$carto_indicateur,
      "_", Sys.Date(), ".pdf"),
    content = function(file) {
      p <- carto_static_gg()
      if (is.null(p)) {
        pdf(file, width = 11, height = 8.5); plot.new()
        text(0.5, 0.5, "Carte indisponible : donn\u00e9es insuffisantes", cex = 1.4); dev.off()
      } else {
        ok <- tryCatch({ ggsave(file, p, width = 11, height = 8.5, device = cairo_pdf, bg = "white"); TRUE },
                       error = function(e) FALSE)
        if (!ok) ggsave(file, p, width = 11, height = 8.5, device = pdf, bg = "white")
      }
    })

  # == TABLE ATTRIBUTAIRE SYNCHRONISEE AVEC LA CARTE ==
  output$carto_table_attrib <- renderDT({
    ctx <- get_agg(); req(ctx$agg)
    ind <- input$carto_indicateur; if (is.null(ind)) ind <- "GAM"
    inds_dispo <- intersect(c("GAM","SAM","MAM","Stunting","Underweight","MUAC_GAM"), names(ctx$agg))
    tbl <- ctx$agg |> select(Zone, n, any_of(inds_dispo))
    datatable(tbl, rownames = FALSE, selection = "single",
      options = list(pageLength = 8, dom = "t", scrollX = TRUE), class = "compact stripe hover")
  })
  carto_attrib_proxy <- dataTableProxy("carto_table_attrib")

  # Clic sur un polygone -> selection de la ligne correspondante
  observeEvent(input$carto_leaflet_main_shape_click, {
    z <- input$carto_leaflet_main_shape_click$id; req(!is.null(z))
    ctx <- tryCatch(get_agg(), error = function(e) NULL); req(!is.null(ctx$agg))
    zn <- norm_zone_name(z)
    row <- which(norm_zone_name(ctx$agg$Zone) == zn & !is.na(norm_zone_name(ctx$agg$Zone)))
    req(length(row) == 1)
    selectRows(carto_attrib_proxy, row)
  })

  # Selection d'une ligne -> zoom carte sur la zone
  observeEvent(input$carto_table_attrib_rows_selected, {
    r <- input$carto_table_attrib_rows_selected
    req(length(r) == 1)
    ctx <- tryCatch(get_agg(), error = function(e) NULL); req(!is.null(ctx$agg))
    z <- ctx$agg$Zone[r]; req(!is.na(z))
    niv <- input$carto_niveau; if (is.null(niv)) niv <- "Region"
    sf_info <- tryCatch(get_sf(niv, input$carto_filtre_region, input$carto_filtre_dept),
                        error = function(e) NULL)
    req(!is.null(sf_info$sf), !is.null(sf_info$col))
    zn_sf <- norm_zone_name(sf_info$sf[[sf_info$col]])
    gz <- zn_sf == norm_zone_name(z) & !is.na(zn_sf)
    req(any(gz))
    bb <- sf::st_bbox(sf_info$sf[gz, , drop = FALSE])
    pa <- (as.numeric(bb[["xmax"]]) - as.numeric(bb[["xmin"]])) * 0.25
    tryCatch(leafletProxy("carto_leaflet_main") |>
      flyToBounds(as.numeric(bb[["xmin"]]) - pa, as.numeric(bb[["ymin"]]) - pa,
                  as.numeric(bb[["xmax"]]) + pa, as.numeric(bb[["ymax"]]) + pa),
      error = function(e) NULL)
  })

  # == CARTE ANIMEE PAR VAGUE D'ENQUETE ==
  carto_anim_rounds <- reactive({
    d <- df_f(); mc <- m(); sc <- mc$survey
    if (is.null(sc) || !(sc %in% names(d))) return(NULL)
    v <- sort(unique(na.omit(d[[sc]])))
    if (length(v) < 2) return(NULL); v
  })

  output$carto_anim_ui <- renderUI({
    v <- carto_anim_rounds(); if (is.null(v)) return(NULL)
    tags$div(style = "background:#f8fbff;padding:10px 14px;border-radius:10px;border:1px solid #e0e0e0;margin-bottom:10px;",
      fluidRow(
        column(4, tags$span(style = "font-weight:700;color:#00426A;font-size:13px;",
          icon("play-circle"), " Carte anim\u00e9e par vague")),
        column(8, sliderInput("carto_anim_idx", NULL, min = 1, max = length(v), value = 1,
          step = 1, ticks = FALSE,
          animate = animationOptions(interval = 1600, loop = TRUE,
            playButton = icon("play"), pauseButton = icon("pause")))
        )
      ),
      tags$div(style = "font-size:11px;color:#666;",
        "Lecture automatique \u25b6 pour animer la carte sur toutes les vagues ; arr\u00eat pour figer une vague."))
  })

  observeEvent(input$carto_anim_idx, {
    idx <- input$carto_anim_idx; req(idx)
    v <- carto_anim_rounds(); req(!is.null(v), idx <= length(v))
    r <- v[idx]
    d <- df_f(); mc <- m(); sc <- mc$survey; req(sc, sc %in% names(d))
    niv <- input$carto_niveau; if (is.null(niv)) niv <- "Region"
    col_geo <- switch(niv, "Region" = mc$region, "Departement" = mc$dept,
      "Arrondissement" = detect_col(names(d), c("arrondissement", "district", "commune")), mc$region)
    req(col_geo, col_geo %in% names(d))
    dr <- d[d[[sc]] == r, , drop = FALSE]
    agg_r <- agreger_geo(dr, col_geo, mc); req(nrow(agg_r) > 0)
    ind <- input$carto_indicateur; if (is.null(ind)) ind <- "GAM"
    cls_method <- input$carto_class_method; if (is.null(cls_method)) cls_method <- "oms"
    cls_k <- input$carto_n_classes; if (is.null(cls_k)) cls_k <- 5
    cls_pal <- input$carto_palette; if (is.null(cls_pal)) cls_pal <- "ylorrd"
    sf_info <- get_sf(niv, input$carto_filtre_region, input$carto_filtre_dept)
    req(!is.null(sf_info$sf), !is.null(sf_info$col))
    sf_r <- sf_info$sf; sf_r$Zone <- sf_r[[sf_info$col]]
    agg_r_n <- agg_r; agg_r_n$ZN <- norm_zone_name(agg_r_n$Zone); agg_r_n$Zone <- NULL
    sf_r$ZN <- norm_zone_name(sf_r$Zone)
    sf_r <- merge(sf_r, agg_r_n[!is.na(agg_r_n$ZN), ], by = "ZN", all.x = TRUE)
    sf_r$ZN <- NULL
    sf_r$val_r <- if (ind %in% names(sf_r)) sf_r[[ind]] else sf_r$n
    vals_ok <- sf_r$val_r[is.finite(sf_r$val_r)]; req(length(vals_ok) > 0)
    cls <- classify_zones_pro(vals_ok, cls_method, cls_k, ind, cls_pal); req(!is.null(cls))
    pr <- leafletProxy("carto_leaflet_main")
    pr <- tryCatch(
      pr |> clearGroup("Choroplethe") |> clearGroup("Sans donn\u00e9es") |>
        clearGroup("Cercles") |> clearGroup("Etiquettes") |> clearGroup("AnimLbl"),
      error = function(e) pr)
    pr <- pr |> addPolygons(data = sf_r, fillColor = ~cls$pal(val_r),
      fillOpacity = 0.85, color = "white", weight = 1.5,
      popup = ~paste0("<b>", Zone, "</b><br>", ind, " : ",
        ifelse(is.na(val_r), "N/A", paste0(round(val_r, 1), "%")), "<br>Vague ", r),
      label = ~paste0(Zone, ": ", ifelse(is.na(val_r), "N/A", paste0(round(val_r, 1), "%")), " (", r, ")"),
      layerId = ~Zone, group = "Choroplethe")
    sf_nd <- sf_r[is.na(sf_r$val_r), , drop = FALSE]
    if (nrow(sf_nd) > 0) {
      pr <- pr |> addPolygons(data = sf_nd, fillColor = "#e8e8e8", fillOpacity = 0.85,
        color = "#9a9a9a", weight = 0.9, dashArray = "3,3", group = "Sans donn\u00e9es")
    }
    bb <- tryCatch(sf::st_bbox(sf_r), error = function(e) NULL)
    if (!is.null(bb) && all(is.finite(as.numeric(bb)))) {
      pr <- pr |> addLabelOnlyMarkers(
        lng = (as.numeric(bb[["xmin"]]) + as.numeric(bb[["xmax"]])) / 2,
        lat = as.numeric(bb[["ymax"]]),
        label = paste0("Vague : ", r),
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE,
          style = list("font-size" = "18px", "font-weight" = "700", "color" = "#00426A",
            "text-shadow" = "2px 2px 3px white, -2px -2px 3px white")),
        group = "AnimLbl")
    }
  })

  # == CARTE AFRIQUE / MONDE INTERACTIVE (Leaflet + Natural Earth) ==
  carto_af_pays_en <- function() {
    cc <- tryCatch(get_carto_country(), error = function(e) NULL)
    if (is.null(cc)) return(NULL)
    en <- tryCatch(get_country_name(cc), error = function(e) NULL)
    if (!is.null(en) && !grepl("^Inconnu", en)) en else NULL
  }
  # Valeurs d'indicateur par pays (donnees multi-pays) : colonne country/pays
  carto_af_vals_pays <- reactive({
    d <- df_f(); ind <- input$carto_af_ind
    req(!is.null(d), nrow(d) > 0, !is.null(ind), ind %in% names(d))
    cc_col <- detect_col(names(d), c("country", "pays", "national", "pays_enquete"))
    req(!is.null(cc_col), cc_col %in% names(d))
    v <- tapply(d[[ind]], d[[cc_col]], function(x) {
      if (all(is.na(x))) return(NA_real_)
      round(mean(x, na.rm = TRUE) * 100, 1)
    })
    v <- v[!is.na(v)]
    req(length(v) > 0)
    as.list(v)
  })
  output$carto_afrique <- renderLeaflet({
    cc <- tryCatch(get_carto_country(), error = function(e) NULL)
    fr <- if (!is.null(cc)) tryCatch(GADM_WORLDWIDE[[cc]]$name, error = function(e) NULL) else NULL
    carte_monde_interactive_pro(d = df_f(), indicateur = input$carto_af_ind,
      zoom_region = input$carto_af_zoom, pays_donnees_en = carto_af_pays_en(),
      pays_donnees_fr = fr, vals_pays = carto_af_vals_pays())
  })

  # == TABLEAU DES PREVALENCES ==
  output$carto_tableau_prev <- renderDT({
    ctx <- get_agg(); req(ctx$agg)
    agg <- ctx$agg; ind <- input$carto_indicateur
    inds_dispo <- intersect(c("GAM","SAM","MAM","Stunting","Underweight","MUAC_GAM","n"), names(agg))
    tbl <- agg |> select(Zone, n, any_of(inds_dispo))
    if (ind %in% names(agg)) tbl <- tbl |> arrange(desc(.data[[ind]]))
    else tbl <- tbl |> arrange(desc(n))
    if ("GAM" %in% names(agg)) {
      tbl$Severite <- case_when(
        tbl$GAM >= 30 ~ "Urgence", tbl$GAM >= 15 ~ "Critique",
        tbl$GAM >= 10 ~ "Serieux", tbl$GAM >= 5 ~ "Alerte", TRUE ~ "Acceptable")
    }
    datatable(tbl, rownames=FALSE, filter="top",
      options=list(pageLength=15, dom="ftp", scrollX=TRUE),
      class="compact stripe hover")
  })

  # == BARPLOT PREVALENCES ==
  output$carto_bar_prev <- renderPlotly({
    ctx <- get_agg(); req(ctx$agg)
    agg <- ctx$agg; ind <- input$carto_indicateur
    req(ind %in% names(agg))
    lbl <- LABELS_FR_PRO[ind]; if(is.na(lbl)) lbl <- ind
    p <- ggplot(agg, aes(x=fct_reorder(Zone,.data[[ind]]), y=.data[[ind]], fill=.data[[ind]])) +
      geom_col(alpha=0.88, width=0.7) +
      geom_hline(yintercept=15, linetype="dashed", color="#c9161d", linewidth=0.8) +
      scale_fill_gradientn(colors=c("#2d8e47","#f5c242","#e8822a","#c9161d"), guide="none") +
      coord_flip() + labs(x="", y=lbl, title=paste0(lbl," par ",ctx$niv)) + theme_minimal(base_size=10)
    ggplotly(p)
  })

  # == FOREST PLOT IC95% ==
  output$carto_forest_prev <- renderPlotly({
    ctx <- get_agg(); req(ctx$agg)
    d <- ctx$d_f; agg <- ctx$agg; ind <- input$carto_indicateur
    inds_bin <- intersect(c("GAM","SAM","Stunting","Underweight","MUAC_GAM"), names(d))
    req(ind %in% inds_bin)
    res <- d |> group_by(Zone=.data[[ctx$col_geo]]) |>
      group_modify(~{
        x <- sum(.x[[ind]], na.rm=TRUE); nn <- sum(!is.na(.x[[ind]]))
        if(nn==0) return(NULL)
        ic <- binom::binom.confint(x, nn, methods="wilson")
        tibble(prev=round(ic$mean*100,1), lo=round(ic$lower*100,1), hi=round(ic$upper*100,1), n=nn)
      }) |> ungroup() |> filter(!is.na(Zone))
    lbl <- LABELS_FR_PRO[ind]; if(is.na(lbl)) lbl <- ind
    p <- ggplot(res, aes(x=prev, y=fct_reorder(Zone,prev))) +
      geom_vline(xintercept=15, linetype="dashed", color="#c9161d") +
      geom_errorbarh(aes(xmin=lo, xmax=hi), height=0.35, color="#008DC9") +
      geom_point(aes(color=prev), size=3.5) +
      scale_color_gradientn(colors=c("#2d8e47","#f5c242","#e8822a","#c9161d"), guide="none") +
      labs(x=paste0(lbl," (%)"), y="", title=paste0("Forest plot IC 95% - ",lbl)) +
      theme_minimal(base_size=10)
    ggplotly(p)
  })

  # == CARTE GPS ==
  output$carto_gps_color_ui <- renderUI({
    d <- df_f()
    cols <- intersect(c("nutr_cat","GAM","SAM","Stunting"), names(d))
    selectInput("carto_gps_color", "Colorer par :",
      choices=c("Statut nutritionnel"="nutr_cat", cols), selected="nutr_cat", width="100%")
  })

  output$carto_gps_map <- renderLeaflet({
    d <- df_f(); mc <- m()
    req(mc$lat, mc$lon, mc$lat %in% names(d), mc$lon %in% names(d))
    d_pts <- d[!is.na(d[[mc$lat]]) & !is.na(d[[mc$lon]]), ]
    if(nrow(d_pts)>50000) { set.seed(4821); d_pts <- d_pts[sample(nrow(d_pts),50000), ] }
    req(nrow(d_pts)>0)
    fond <- input$carto_gps_fond; if(is.null(fond)) fond <- "Esri.WorldGrayCanvas"
    sz <- input$carto_gps_size; if(is.null(sz)) sz <- 5

    gps_map <- leaflet() |> addProviderTiles(fond) |> addScaleBar(position="bottomleft") |> addFullscreenControl()
    pal_pt <- colorFactor(c("#2d8e47","#e8822a","#c9161d"), domain=c("Normal","MAM","SAM"))
    col_nutr <- if("nutr_cat" %in% names(d_pts)) "nutr_cat" else NULL
    fill_col <- if(!is.null(col_nutr)) pal_pt(d_pts[[col_nutr]]) else rep("#008DC9", nrow(d_pts))

    if (isTRUE(input$carto_gps_cluster)) {
      gps_map <- gps_map |> addCircleMarkers(lat=d_pts[[mc$lat]], lng=d_pts[[mc$lon]],
        radius=sz, fillColor=fill_col, fillOpacity=0.8, color="white", weight=0.5,
        clusterOptions=markerClusterOptions(disableClusteringAtZoom=12))
    } else {
      gps_map <- gps_map |> addCircleMarkers(lat=d_pts[[mc$lat]], lng=d_pts[[mc$lon]],
        radius=sz, fillColor=fill_col, fillOpacity=0.8, color="white", weight=0.5)
    }
    if (!is.null(col_nutr)) gps_map <- gps_map |> addLegend(pal=pal_pt, values=d_pts[[col_nutr]], title="Statut", position="topleft")
    gps_map
  })
  # ══════════════════════════════════════════════════════════════════════════
  # EXPORTS & PLEIN ECRAN — tous onglets
  # ══════════════════════════════════════════════════════════════════════════

  # Helper : construire la carte statique courante
  build_static_map <- function() {
    ctx <- get_agg(); req(ctx$agg)
    ind <- input$carto_indicateur; if (is.null(ind)) ind <- "GAM"
    pal <- input$carto_pal_static %||% "oms_sequentiel"
    sf_info <- get_sf(ctx$niv, input$carto_filtre_region, input$carto_filtre_dept)
    req(sf_info$sf)
    sf_join <- sf_info$sf; sf_join$Zone <- sf_join[[sf_info$col]]
    sf_join <- merge(sf_join, ctx$agg, by="Zone", all.x=TRUE)
    carte_statique_pro(sf_join, ind, ctx$niv, ind, nrow(ctx$d_f), pal, isTRUE(input$carto_static_labels))
  }

  # ── Tab 1 : Carte interactive → PNG (version statique) ──────────────────
  output$dl_carte_png <- downloadHandler(
    filename = function() paste0("carte_", input$carto_indicateur, "_", input$carto_niveau, "_", Sys.Date(), ".png"),
    content = function(file) {
      p <- build_static_map()
      ggsave(file, p, width = 14, height = 10, dpi = 300, bg = "white")
    }
  )

  # ── Tab 2 : Bivariate → PNG ─────────────────────────────────────────────
  output$dl_carto_bivar <- downloadHandler(
    filename = function() paste0("carte_bivariate_", input$carto_bivar_x, "_", input$carto_bivar_y, "_", Sys.Date(), ".png"),
    content = function(file) {
      # Générer la version statique de la carte bivariate
      ctx <- get_agg(); req(ctx$agg)
      ind_x <- input$carto_bivar_x; ind_y <- input$carto_bivar_y
      req(ind_x %in% names(ctx$agg), ind_y %in% names(ctx$agg))
      sf_info <- get_sf(ctx$niv, input$carto_filtre_region, input$carto_filtre_dept)
      req(sf_info$sf)
      sf_join <- sf_info$sf; sf_join$Zone <- sf_join[[sf_info$col]]
      sf_join <- merge(sf_join, ctx$agg, by = "Zone", all.x = TRUE)
      valid <- !is.na(sf_join[[ind_x]]) & !is.na(sf_join[[ind_y]])
      sf_v <- sf_join[valid, ]
      bivar <- classify_bivariate(sf_v[[ind_x]], sf_v[[ind_y]])
      sf_v$bivar_color <- bivar$colors
      p <- ggplot(sf_v) +
        geom_sf(aes(fill = bivar_color), color = "white", linewidth = 0.4) +
        scale_fill_identity() +
        labs(title = paste0("Carte bivariate : ", ind_x, " \u00d7 ", ind_y),
             subtitle = paste0("Par ", ctx$niv, " | N = ", nrow(ctx$d_f))) +
        theme_void(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#00426A", hjust = 0.5),
              plot.subtitle = element_text(color = "#555", hjust = 0.5))
      ggsave(file, p, width = 12, height = 9, dpi = 300, bg = "white")
    }
  )

  # ── Tab 3 : Multi-indicateurs → PNG ─────────────────────────────────────
  output$dl_carto_multi <- downloadHandler(
    filename = function() paste0("multi_indicateurs_", Sys.Date(), ".png"),
    content = function(file) {
      req(carto_multi_sf(), carto_multi_inds_sel())
      ncol_sel <- as.integer(input$carto_multi_ncol %||% 2)
      p <- carte_multi_indicateurs(carto_multi_sf(), carto_multi_inds_sel(),
                                    get_agg()$niv, nrow(get_agg()$d_f), ncol_maps = ncol_sel)
      n_rows <- ceiling(length(carto_multi_inds_sel()) / ncol_sel)
      ggsave(file, p, width = 14, height = max(6, n_rows * 5), dpi = 300, bg = "white")
    }
  )

  # ── Tab 4 : Carte statique → PNG + PDF + Plein écran ────────────────────
  output$dl_carto_static_png <- downloadHandler(
    filename = function() paste0("carte_statique_", input$carto_indicateur, "_", Sys.Date(), ".png"),
    content = function(file) {
      p <- build_static_map()
      ggsave(file, p, width = 14, height = 10, dpi = 300, bg = "white")
    }
  )

  output$dl_carto_static_pdf <- downloadHandler(
    filename = function() paste0("carte_statique_", input$carto_indicateur, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      p <- build_static_map()
      ggsave(file, p, width = 14, height = 10, device = cairo_pdf)
    }
  )

  observeEvent(input$carto_static_fullscreen, {
    showModal(modalDialog(
      title = tags$span(icon("expand"), " Carte statique \u2014 Plein \u00e9cran"),
      size = "l", easyClose = TRUE,
      tags$div(style = "max-height:85vh; overflow-y:auto;",
        plotOutput("carto_static_full", height = "800px")
      ),
      footer = modalButton("Fermer")
    ))
    output$carto_static_full <- renderPlot({ build_static_map() }, res = 140)
  })

  # ── Tab 5 : Afrique → Plein écran ──────────────────────────────────────
  observeEvent(input$carto_af_fullscreen, {
    showModal(modalDialog(
      title = tags$span(icon("expand"), " Afrique centrale \u2014 Plein \u00e9cran"),
      size = "l", easyClose = TRUE,
      tags$div(style = "max-height:85vh; overflow-y:auto;",
        plotlyOutput("carto_afrique_full", height = "750px")
      ),
      footer = modalButton("Fermer")
    ))
    output$carto_afrique_full <- renderLeaflet({
      cc <- tryCatch(get_carto_country(), error = function(e) NULL)
      fr <- if (!is.null(cc)) tryCatch(GADM_WORLDWIDE[[cc]]$name, error = function(e) NULL) else NULL
      carte_monde_interactive_pro(d = df_f(), indicateur = input$carto_af_ind,
        zoom_region = input$carto_af_zoom, pays_donnees_en = carto_af_pays_en(),
        pays_donnees_fr = fr, vals_pays = carto_af_vals_pays())
    })
  })

  # ── Tab 6 : Tableau prévalences → CSV ───────────────────────────────────
  output$dl_carto_tableau <- downloadHandler(
    filename = function() paste0("prevalences_", input$carto_niveau, "_", Sys.Date(), ".csv"),
    content = function(file) {
      ctx <- get_agg(); req(ctx$agg)
      write.csv(ctx$agg, file, row.names = FALSE)
    }
  )

  # ── Tab 8 : LISA → CSV ─────────────────────────────────────────────────
  output$dl_carto_lisa <- downloadHandler(
    filename = function() paste0("lisa_", input$carto_lisa_ind, "_", Sys.Date(), ".csv"),
    content = function(file) {
      res <- carto_lisa_results(); req(!is.null(res))
      if (!is.null(res$sf_data)) {
        write.csv(sf::st_drop_geometry(res$sf_data), file, row.names = FALSE)
      } else {
        write.csv(data.frame(info = "Pas de r\u00e9sultats LISA"), file, row.names = FALSE)
      }
    }
  )

  # ── Tab 10 : Priorisation → CSV ────────────────────────────────────────
  output$dl_carto_priority <- downloadHandler(
    filename = function() paste0("priorisation_", Sys.Date(), ".csv"),
    content = function(file) {
      ctx <- get_agg(); req(ctx$agg)
      # Recalculer si la fonction existe
      if (exists("calc_intervention_priority")) {
        prio <- tryCatch(calc_intervention_priority(ctx$agg), error = function(e) ctx$agg)
        write.csv(prio, file, row.names = FALSE)
      } else {
        write.csv(ctx$agg, file, row.names = FALSE)
      }
    }
  )

  # ── Tab 11 : Temporelle → PNG + Plein écran ─────────────────────────────
  carto_temporal_gg <- reactiveVal(NULL)

  output$dl_carto_temporal <- downloadHandler(
    filename = function() paste0("temporelle_", input$carto_temporal_ind, "_", Sys.Date(), ".png"),
    content = function(file) {
      p <- carto_temporal_gg()
      if (!is.null(p)) {
        ggsave(file, p, width = 14, height = 8, dpi = 300, bg = "white")
      } else {
        # Fallback
        ggsave(file, ggplot() + labs(title = "Lancez d'abord l'analyse temporelle"),
               width = 10, height = 6, bg = "white")
      }
    }
  )

  observeEvent(input$carto_temporal_fullscreen, {
    showModal(modalDialog(
      title = tags$span(icon("expand"), " Comparaison temporelle \u2014 Plein \u00e9cran"),
      size = "l", easyClose = TRUE,
      tags$div(style = "max-height:85vh; overflow-y:auto;",
        plotOutput("carto_temporal_full", height = "800px")
      ),
      footer = modalButton("Fermer")
    ))
    output$carto_temporal_full <- renderPlot({
      p <- carto_temporal_gg()
      if (!is.null(p)) p else ggplot() + labs(title = "Lancez d'abord l'analyse temporelle")
    }, res = 140)
  })


  # == COMPARAISON TEMPORELLE ==
  output$carto_temporal_available_ui <- renderUI({
    d <- df_f(); mc <- m(); sc <- mc$survey
    if (is.null(sc) || !sc %in% names(d)) {
      return(tags$div(style="background:#fff3cd;padding:14px;border-radius:10px;border:1px solid #ffc107;margin-bottom:12px;",
        tags$h6(style="font-weight:700;color:#856404;margin:0;", icon("exclamation-triangle"), " Colonne vague non detectee"),
        tags$p(style="font-size:12px;color:#856404;margin:6px 0 0;", "Colonnes: survey_round, round, vague, enquete_round, wave.")))
    }
    nv <- length(unique(na.omit(d[[sc]])))
    if (nv < 2) return(tags$div(style="background:#fff3cd;padding:14px;border-radius:10px;border:1px solid #ffc107;margin-bottom:12px;",
      tags$h6(style="font-weight:700;color:#856404;margin:0;", icon("info-circle"), paste0(" 1 seule vague (",sc,")"))))
    tags$div(style="background:#d4edda;padding:10px 14px;border-radius:10px;border:1px solid #28a745;margin-bottom:12px;",
      tags$span(style="font-weight:600;color:#155724;", icon("check-circle"), paste0(" ",nv," vagues dans ",sc)))
  })

  output$carto_temporal_round_ui <- renderUI({
    d <- df_f(); mc <- m(); sc <- mc$survey; req(sc, sc %in% names(d))
    vagues <- sort(unique(na.omit(d[[sc]]))); req(length(vagues)>=2)
    tagList(selectInput("carto_temporal_r1","Vague 1:",choices=vagues,selected=vagues[1],width="100%"),
      selectInput("carto_temporal_r2","Vague 2:",choices=vagues,selected=vagues[length(vagues)],width="100%"))
  })

  observeEvent(input$run_temporal, {
    d <- df_f(); mc <- m(); sc <- mc$survey; req(sc, sc %in% names(d))
    r1 <- input$carto_temporal_r1; r2 <- input$carto_temporal_r2; req(r1,r2,r1!=r2)
    ind <- input$carto_temporal_ind
    niv <- input$carto_niveau; if(is.null(niv)) niv <- "Region"
    col_geo <- switch(niv,"Region"=mc$region,"Departement"=mc$dept,
      "Arrondissement"=detect_col(names(d),c("arrondissement","district","commune")),mc$region)
    req(col_geo, col_geo %in% names(d))
    d_r1 <- d[d[[sc]]==r1,]; d_r2 <- d[d[[sc]]==r2,]
    agg_r1 <- agreger_geo(d_r1,col_geo,mc); agg_r2 <- agreger_geo(d_r2,col_geo,mc)
    sf_info <- get_sf(niv,input$carto_filtre_region,input$carto_filtre_dept)
    gadm_sf <- sf_info$sf; gadm_col <- sf_info$col; req(gadm_sf)

    output$carto_temporal_sidebyside <- renderPlot({
      sf1 <- gadm_sf; sf1$Zone <- sf1[[gadm_col]]; sf1 <- merge(sf1,agg_r1,by="Zone",all.x=TRUE)
      sf2 <- gadm_sf; sf2$Zone <- sf2[[gadm_col]]; sf2 <- merge(sf2,agg_r2,by="Zone",all.x=TRUE)
      req(ind %in% names(sf1), ind %in% names(sf2))
      p <- carte_temporal_sidebyside(list(sf1,sf2),ind,c(as.character(r1),as.character(r2)),niv)
      carto_temporal_gg(p)
      p
    }, res=120, width=1100, height=600)

    output$carto_temporal_variation <- renderLeaflet({
      ck1 <- c("Zone","n",intersect(ind,names(agg_r1))); ck2 <- c("Zone","n",intersect(ind,names(agg_r2)))
      am <- merge(agg_r1[,ck1],agg_r2[,ck2],by="Zone",suffixes=c("_r1","_r2"),all=TRUE)
      am$delta <- am[[paste0(ind,"_r2")]]-am[[paste0(ind,"_r1")]]
      sf_v <- gadm_sf; sf_v$Zone <- sf_v[[gadm_col]]; sf_v <- merge(sf_v,am,by="Zone",all.x=TRUE)
      deltas <- sf_v$delta[!is.na(sf_v$delta)]
      if(length(deltas)==0) return(leaflet()|>addTiles()|>addControl("Pas de donnees"))
      mx <- max(abs(deltas))
      pd <- colorNumeric(c("#2d8e47","#a8c834","#fffde7","#e8822a","#c9161d"),c(-mx,mx),na.color="#e0e0e0")
      sf_v$popup_txt <- vapply(seq_len(nrow(sf_v)), function(i)
        make_popup_temporal(sf_v$Zone[i],sf_v[[paste0(ind,"_r1")]][i],sf_v[[paste0(ind,"_r2")]][i],
          sf_v$delta[i],ind,as.character(r1),as.character(r2),sf_v$n_r1[i],sf_v$n_r2[i]),character(1))
      mv <- leaflet()|>addProviderTiles("Esri.WorldGrayCanvas")|>addScaleBar(position="bottomleft")|>addFullscreenControl()
      mv <- mv|>addPolygons(data=sf_v,fillColor=~pd(delta),fillOpacity=0.78,color="white",weight=1.8,popup=~popup_txt,
        label=~paste0(Zone,": ",ifelse(is.na(delta),"N/A",paste0(ifelse(delta>0,"+",""),round(delta,1)," pp"))),
        highlightOptions=highlightOptions(weight=3,color="#00426A",fillOpacity=0.92,bringToFront=TRUE))
      mv <- mv|>addLegend(pal=pd,values=deltas,title=paste0("Var.",ind),position="bottomright",
        labFormat=labelFormat(suffix=" pp",transform=function(x) round(x,1)))
      bb <- tryCatch(sf::st_bbox(gadm_sf),error=function(e) NULL)
      if(!is.null(bb)&&all(is.finite(as.numeric(bb)))){
        pa<-(as.numeric(bb[3])-as.numeric(bb[1]))*0.05
        mv<-mv|>fitBounds(as.numeric(bb[1])-pa,as.numeric(bb[2])-pa,as.numeric(bb[3])+pa,as.numeric(bb[4])+pa)}
      mv
    })

    output$carto_temporal_trend <- renderPlotly({
      vagues <- sort(unique(na.omit(d[[sc]])))
      td <- do.call(rbind, lapply(vagues, function(v){
        dv<-d[d[[sc]]==v,]; av<-agreger_geo(dv,col_geo,mc)
        if(ind %in% names(av)) data.frame(Vague=v,Zone=av$Zone,val=av[[ind]],n=av$n) else NULL}))
      req(nrow(td)>0); td$Vague <- factor(td$Vague, levels=vagues)
      li <- LABELS_FR_PRO[ind]; if(is.na(li)) li <- ind
      su <- SEUILS_OMS_PRO[[ind]]; sc3 <- if(!is.null(su)) su[3] else 15
      p <- ggplot(td,aes(x=Vague,y=val,group=Zone,color=Zone))+
        geom_line(linewidth=1,alpha=0.7)+geom_point(aes(size=n),alpha=0.8)+
        geom_hline(yintercept=sc3,linetype="dashed",color="#c9161d",linewidth=0.7)+
        scale_size_continuous(range=c(3,8),name="Effectif")+
        labs(x="Vague",y=paste0(li," (%)"),title=paste0("Evolution de ",li),color="Zone")+
        theme_minimal(base_size=11)+theme(legend.position="right",plot.title=element_text(face="bold",color="#00426A"))
      ggplotly(p)
    })

    output$carto_temporal_table <- renderDT({
      ck1<-c("Zone","n",intersect(ind,names(agg_r1))); ck2<-c("Zone","n",intersect(ind,names(agg_r2)))
      am<-merge(agg_r1[,ck1],agg_r2[,ck2],by="Zone",suffixes=c("_r1","_r2"),all=TRUE)
      am$delta<-round(am[[paste0(ind,"_r2")]]-am[[paste0(ind,"_r1")]],1)
      am$tendance<-ifelse(am$delta>2,"Degradation",ifelse(am$delta< -2,"Amelioration","Stable"))
      names(am)[names(am)==paste0(ind,"_r1")]<-paste0(ind," (",r1,")")
      names(am)[names(am)==paste0(ind,"_r2")]<-paste0(ind," (",r2,")")
      names(am)[names(am)=="n_r1"]<-paste0("N (",r1,")"); names(am)[names(am)=="n_r2"]<-paste0("N (",r2,")")
      datatable(am,rownames=FALSE,filter="top",options=list(pageLength=15,dom="ftp",scrollX=TRUE),
        class="compact stripe hover")|>
        formatStyle("tendance",backgroundColor=styleEqual(c("Degradation","Amelioration","Stable"),c("#ffe6e6","#e6f7e6","#f0f0f0")),
          fontWeight="bold",color=styleEqual(c("Degradation","Amelioration","Stable"),c("#c9161d","#2d8e47","#666")))
    })
    showNotification(paste0("Analyse temporelle ",r1," vs ",r2," terminee"),type="message")
  })

  # ── EXPOSITION DES HELPERS ─────────────────────────────────────────────────
  # Les observers de carto_server_outputs.R vivent dans l'environnement parent
  # (source local=TRUE du server) : get_agg/get_sf/m doivent y etre visibles.
  assign("get_agg", get_agg, envir = parent.env(environment()))
  assign("get_sf",  get_sf,  envir = parent.env(environment()))
  assign("m",       m,       envir = parent.env(environment()))
} # fin server_carto_pro

cat("carto_server_pro.R charge
")
