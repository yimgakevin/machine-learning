# Extrait de app_v2_final.R — statut sidebar, filtres du dashboard, thème
  # ── SIDEBAR STATUS ──────────────────────────────────────────────────────────
  output$sidebar_status <- renderUI({
    df <- mapped_data()
    if (is.null(df)) {
      tags$div(style="text-align:center;color:#5a9ec4;font-size:11px;margin-top:6px;",
               "Aucune donnée · Importez un fichier ci-dessus")
    } else {
      tags$div(style="margin-top:6px;display:flex;flex-wrap:wrap;gap:4px;",
        tags$span(class="badge-ok", paste0(nrow(df)," obs.")),
        tags$span(class="badge-info", paste0(ncol(df)," vars")),
        if(has_oms()) tags$span(class="badge-ok","✓ OMS")
        else tags$span(class="badge-warn","Mapper OMS")
      )
    }
  })

  output$header_status <- renderUI({
    df <- mapped_data()
    if (is.null(df)) tags$span("Aucune donnée")
    else tags$span(nrow(df), " observations |", ncol(df), " variables",
                   if(has_oms()) " | ✓ Indicateurs OMS calculés" else " | Mapper les colonnes OMS")
  })

  output$dashboard_filters <- renderUI({
    df <- mapped_data(); req(df)
    m <- map_cols(); req(m)
    out  <- list()
    idx  <- 1

    # ── Helper pour créer un en-tête de section ──────────────────────
    section_hdr <- function(label)
      tags$div(class="f-section-hdr",
               style="margin:8px 0 3px;font-size:9px;text-transform:uppercase;
                      letter-spacing:1px;color:#4da8d4;border-bottom:1px solid
                      rgba(255,255,255,.15);padding-bottom:3px;", label)

    # ════════════════════════════════════════════
    # §1 — GÉOGRAPHIE
    # ════════════════════════════════════════════
    geo_items <- list()
    if (!is.null(m$region) && m$region %in% names(df)) {
      vals <- c("Toutes", sort(unique(na.omit(as.character(df[[m$region]])))))
      geo_items <- c(geo_items, list(
        selectInput("f_region","🗺️ Région :", choices=vals, selected="Toutes", width="100%")
      ))
    }
    if (!is.null(m$dept) && m$dept %in% names(df)) {
      vals <- c("Tous", sort(unique(na.omit(as.character(df[[m$dept]])))))
      geo_items <- c(geo_items, list(
        selectInput("f_dept","📍 Département :", choices=vals, selected="Tous", width="100%")
      ))
    }
    # Zone (urbain/rural/camp) — auto-détection
    zone_col <- detect_col(names(df), c("zone","milieu","area","type_zone","setting","camp"))
    if (!is.null(zone_col)) {
      vals <- sort(unique(na.omit(as.character(df[[zone_col]]))))
      geo_items <- c(geo_items, list(
        selectizeInput("f_zone","🏘️ Zone :", choices=c("Toutes"="",vals),
                       selected="", multiple=TRUE, width="100%",
                       options=list(placeholder="Toutes les zones"))
      ))
    }
    # Cluster / Grappe
    cluster_col <- detect_col(names(df), c("cluster","grappe","psu","cluster_id","clust"))
    if (!is.null(cluster_col) && length(unique(df[[cluster_col]])) <= 200) {
      vals <- sort(unique(na.omit(as.character(df[[cluster_col]]))))
      geo_items <- c(geo_items, list(
        selectizeInput("f_cluster","🎯 Cluster/Grappe :", choices=c("Tous"="",vals),
                       selected="", multiple=TRUE, width="100%",
                       options=list(placeholder="Tous",maxItems=50))
      ))
    }
    # Strate
    strate_col <- detect_col(names(df), c("strate","strata","stratum","strat"))
    if (!is.null(strate_col)) {
      vals <- sort(unique(na.omit(as.character(df[[strate_col]]))))
      geo_items <- c(geo_items, list(
        selectizeInput("f_strate","🗂️ Strate :", choices=c("Toutes"="",vals),
                       selected="", multiple=TRUE, width="100%",
                       options=list(placeholder="Toutes"))
      ))
    }
    # Vague / Round d'enquête — auto-détection sur m$survey
    if (!is.null(m$survey) && m$survey %in% names(df)) {
      vals_sv <- sort(unique(na.omit(as.character(df[[m$survey]]))))
      if (length(vals_sv) >= 2) {
        geo_items <- c(geo_items, list(
          selectizeInput("f_survey_round", paste0("\U0001f504 ", m$survey, " :"),
                         choices = c("Toutes" = "", vals_sv),
                         selected = "", multiple = TRUE, width = "100%",
                         options = list(placeholder = "Toutes les vagues"))
        ))
      }
    }
    if (length(geo_items) > 0) {
      out[[idx]] <- tagList(section_hdr("📌 Géographie & Enquête"), geo_items)
      idx <- idx + 1
    }

    # ════════════════════════════════════════════
    # §2 — DÉMOGRAPHIE
    # ════════════════════════════════════════════
    demo_items <- list()
    if (!is.null(m$sexe) && m$sexe %in% names(df)) {
      vals <- sort(unique(na.omit(as.character(df[[m$sexe]]))))
      demo_items <- c(demo_items, list(
        selectInput("f_sexe","👤 Sexe :", choices=c("Tous",vals), selected="Tous", width="100%")
      ))
    }
    if (!is.null(m$depl) && m$depl %in% names(df)) {
      vals <- sort(unique(na.omit(as.character(df[[m$depl]]))))
      demo_items <- c(demo_items, list(
        selectizeInput("f_depl","🏕️ Déplacement :", choices=c("Tous"="",vals),
                       selected="", multiple=TRUE, width="100%",
                       options=list(placeholder="Tous statuts"))
      ))
    }
    if (".age" %in% names(df)) {
      r <- range(df$.age, na.rm=TRUE)
      demo_items <- c(demo_items, list(
        sliderInput("f_age","👶 Âge (mois) :",
                    min=floor(r[1]), max=ceiling(r[2]),
                    value=c(floor(r[1]),ceiling(r[2])), step=1, width="100%")
      ))
    }
    # Groupe d'âge
    age_grp_col <- detect_col(names(df), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
    if (!is.null(age_grp_col)) {
      vals <- sort(unique(na.omit(as.character(df[[age_grp_col]]))))
      demo_items <- c(demo_items, list(
        checkboxGroupInput("f_age_grp","📅 Groupe d'âge :",
                           choices=vals, selected=vals, inline=TRUE)
      ))
    }
    if (length(demo_items) > 0) {
      out[[idx]] <- tagList(section_hdr("👥 Démographie"), demo_items)
      idx <- idx + 1
    }

    # ════════════════════════════════════════════
    # §3 — INDICATEURS OMS
    # ════════════════════════════════════════════
    oms_items <- list()
    if ("nutr_cat" %in% names(df)) {
      oms_items <- c(oms_items, list(
        checkboxGroupInput("f_nutr","📊 Statut nutritionnel :",
          choices=c("Normal","MAM","SAM"), selected=c("Normal","MAM","SAM"), inline=TRUE)
      ))
    }
    if ("GAM" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_gam","🔴 GAM :",
          choices=c("Tous","Cas GAM"="1","Non GAM"="0"), selected="Tous", width="100%")
      ))
    }
    if ("SAM" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_sam","🚨 SAM :",
          choices=c("Tous","Cas SAM"="1","Non SAM"="0"), selected="Tous", width="100%")
      ))
    }
    if ("MAM" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_mam","🟠 MAM :",
          choices=c("Tous","Cas MAM"="1","Non MAM"="0"), selected="Tous", width="100%")
      ))
    }
    if ("Stunting" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_stunting","📏 Stunting :",
          choices=c("Tous","Stunté"="1","Non stunté"="0"), selected="Tous", width="100%")
      ))
    }
    if ("Underweight" %in% names(df)) {
      oms_items <- c(oms_items, list(
        selectInput("f_underweight","⚖️ Underweight :",
          choices=c("Tous","Insuffisance pondérale"="1","Normal"="0"),
          selected="Tous", width="100%")
      ))
    }
    oedeme_col <- detect_col(names(df), c("oedeme","oedema","edema","oedèmes","oedème"))
    if (!is.null(oedeme_col)) {
      vals_o <- sort(unique(na.omit(as.character(df[[oedeme_col]]))))
      oms_items <- c(oms_items, list(
        selectInput("f_oedeme","💧 Œdèmes :",
          choices=c("Tous",vals_o), selected="Tous", width="100%")
      ))
    }
    if (length(oms_items) > 0) {
      out[[idx]] <- tagList(section_hdr("🏥 Indicateurs OMS"), oms_items)
      idx <- idx + 1
    }

    # ════════════════════════════════════════════
    # §4 — ANTHROPOMÉTRIE
    # ════════════════════════════════════════════
    anthro_items <- list()
    if (".muac" %in% names(df)) {
      r <- range(df$.muac, na.rm=TRUE)
      anthro_items <- c(anthro_items, list(
        sliderInput("f_muac","📐 MUAC (mm) :",
                    min=floor(r[1]), max=ceiling(r[2]),
                    value=c(floor(r[1]),ceiling(r[2])), step=1, width="100%")
      ))
    }
    if (".whz" %in% names(df)) {
      r <- round(range(df$.whz, na.rm=TRUE), 1)
      anthro_items <- c(anthro_items, list(
        sliderInput("f_whz","⚖️ WHZ (z-score) :",
                    min=r[1], max=r[2], value=r, step=0.1, width="100%")
      ))
    }
    if (".haz" %in% names(df)) {
      r <- round(range(df$.haz, na.rm=TRUE), 1)
      anthro_items <- c(anthro_items, list(
        sliderInput("f_haz","📏 HAZ (z-score) :",
                    min=r[1], max=r[2], value=r, step=0.1, width="100%")
      ))
    }
    if (".waz" %in% names(df)) {
      r <- round(range(df$.waz, na.rm=TRUE), 1)
      anthro_items <- c(anthro_items, list(
        sliderInput("f_waz","🏋️ WAZ (z-score) :",
                    min=r[1], max=r[2], value=r, step=0.1, width="100%")
      ))
    }
    if (length(anthro_items) > 0) {
      out[[idx]] <- tagList(section_hdr("📐 Anthropométrie"), anthro_items)
      idx <- idx + 1
    }

    # ════════════════════════════════════════════
    # §5 — QUALITÉ / ÉCHANTILLONNAGE
    # ════════════════════════════════════════════
    qual_items <- list()
    # Variables clés disponibles pour drop_na
    cles <- intersect(c(m$region,m$dept,m$sexe,m$age_m,m$muac,m$whz,m$haz,m$waz),
                      names(df))
    cles <- cles[!is.na(cles)]
    if (length(cles) > 0) {
      qual_items <- c(qual_items, list(
        checkboxGroupInput("f_excl_na","🚫 Exclure NA sur :",
          choices=setNames(cles, cles), selected=NULL, inline=FALSE)
      ))
    }
    qual_items <- c(qual_items, list(
      numericInput("f_top_n","✂️ Top-N lignes (0=tous) :",
                   value=0, min=0, step=100, width="100%"),
      checkboxInput("f_no_outlier","❌ Exclure outliers z±3 (WHZ/HAZ/WAZ)",
                    value=FALSE)
    ))
    out[[idx]] <- tagList(section_hdr("🔬 Qualité & Échantillonnage"), qual_items)
    idx <- idx + 1

    # ════════════════════════════════════════════
    # Badge filtres actifs
    # ════════════════════════════════════════════
    out[[idx]] <- tags$div(style="margin-top:6px;",
      uiOutput("f_active_badge")
    )

    tagList(out)
  })

  # Badge : nombre de filtres actifs
  output$f_active_badge <- renderUI({
    df_all <- mapped_data(); req(df_all); m <- map_cols()
    n_actif <- 0
    if (!is.null(input$f_region) && input$f_region != "Toutes") n_actif <- n_actif + 1
    if (!is.null(input$f_dept)   && input$f_dept   != "Tous")   n_actif <- n_actif + 1
    if (!is.null(input$f_zone)   && length(input$f_zone)   > 0) n_actif <- n_actif + 1
    if (!is.null(input$f_cluster)&& length(input$f_cluster)> 0) n_actif <- n_actif + 1
    if (!is.null(input$f_strate)        && length(input$f_strate)        > 0) n_actif <- n_actif + 1
    if (!is.null(input$f_survey_round)  && length(input$f_survey_round)  > 0) n_actif <- n_actif + 1
    if (!is.null(input$f_sexe)   && input$f_sexe   != "Tous")   n_actif <- n_actif + 1
    if (!is.null(input$f_depl)   && length(input$f_depl)   > 0) n_actif <- n_actif + 1
    if (".age" %in% names(df_all) && !is.null(input$f_age)) {
      r <- range(df_all$.age, na.rm=TRUE)
      if (input$f_age[1] > floor(r[1]) || input$f_age[2] < ceiling(r[2]))
        n_actif <- n_actif + 1
    }
    if (!is.null(input$f_age_grp) && length(input$f_age_grp) > 0) {
      age_grp_col <- detect_col(names(df_all), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
      if (!is.null(age_grp_col)) {
        all_vals <- sort(unique(na.omit(as.character(df_all[[age_grp_col]]))))
        if (!setequal(input$f_age_grp, all_vals)) n_actif <- n_actif + 1
      }
    }
    if (!is.null(input$f_nutr) && length(input$f_nutr) < 3) n_actif <- n_actif + 1
    if (!is.null(input$f_gam)  && input$f_gam  != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_sam)  && input$f_sam  != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_mam)  && input$f_mam  != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_stunting)    && input$f_stunting    != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_underweight) && input$f_underweight != "Tous") n_actif <- n_actif + 1
    if (!is.null(input$f_oedeme)      && input$f_oedeme      != "Tous") n_actif <- n_actif + 1
    for (sl in c("f_muac","f_whz","f_haz","f_waz")) {
      col <- switch(sl, f_muac=".muac", f_whz=".whz", f_haz=".haz", f_waz=".waz")
      if (col %in% names(df_all) && !is.null(input[[sl]])) {
        r <- range(df_all[[col]], na.rm=TRUE)
        if (input[[sl]][1] > r[1] || input[[sl]][2] < r[2]) n_actif <- n_actif + 1
      }
    }
    if (!is.null(input$f_excl_na) && length(input$f_excl_na) > 0) n_actif <- n_actif + 1
    if (!is.null(input$f_top_n)   && input$f_top_n > 0)           n_actif <- n_actif + 1
    if (isTRUE(input$f_no_outlier))                                n_actif <- n_actif + 1

    if (n_actif == 0)
      tags$div(style="font-size:10px;color:#64748b;text-align:center;",
               "Aucun filtre actif")
    else
      tags$div(style="text-align:center;",
        tags$span(style=paste0("background:#D98B14;color:white;border-radius:20px;",
                               "padding:2px 10px;font-size:11px;font-weight:600;"),
                  paste0("🔽 ", n_actif, " filtre", if(n_actif>1)"s" else "", " actif",
                         if(n_actif>1)"s" else ""))
      )
  })

  # Reset tous les filtres
  observeEvent(input$btn_reset_filters, {
    df <- mapped_data(); req(df); m <- map_cols()

    # § Géographie
    if (!is.null(m$region) && m$region %in% names(df))
      updateSelectInput(session, "f_region", selected="Toutes")
    if (!is.null(m$dept) && m$dept %in% names(df))
      updateSelectInput(session, "f_dept", selected="Tous")
    updateSelectizeInput(session, "f_zone",    selected=character(0))
    updateSelectizeInput(session, "f_cluster",       selected=character(0))
    updateSelectizeInput(session, "f_strate",        selected=character(0))
    updateSelectizeInput(session, "f_survey_round",  selected=character(0))

    # § Démographie
    if (!is.null(m$sexe) && m$sexe %in% names(df))
      updateSelectInput(session, "f_sexe", selected="Tous")
    updateSelectizeInput(session, "f_depl", selected=character(0))
    if (".age" %in% names(df)) {
      r <- range(df$.age, na.rm=TRUE)
      updateSliderInput(session, "f_age", value=c(floor(r[1]), ceiling(r[2])))
    }
    age_grp_col <- detect_col(names(df), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
    if (!is.null(age_grp_col)) {
      all_vals <- sort(unique(na.omit(as.character(df[[age_grp_col]]))))
      updateCheckboxGroupInput(session, "f_age_grp", selected=all_vals)
    }

    # § Indicateurs OMS
    if ("nutr_cat" %in% names(df))
      updateCheckboxGroupInput(session, "f_nutr", selected=c("Normal","MAM","SAM"))
    if ("GAM"         %in% names(df)) updateSelectInput(session, "f_gam",         selected="Tous")
    if ("SAM"         %in% names(df)) updateSelectInput(session, "f_sam",         selected="Tous")
    if ("MAM"         %in% names(df)) updateSelectInput(session, "f_mam",         selected="Tous")
    if ("Stunting"    %in% names(df)) updateSelectInput(session, "f_stunting",    selected="Tous")
    if ("Underweight" %in% names(df)) updateSelectInput(session, "f_underweight", selected="Tous")
    oedeme_col <- detect_col(names(df), c("oedeme","oedema","edema","oedèmes","oedème"))
    if (!is.null(oedeme_col))         updateSelectInput(session, "f_oedeme",      selected="Tous")

    # § Anthropométrie
    for (info in list(list("f_muac",".muac",1), list("f_whz",".whz",.1),
                      list("f_haz",".haz",.1),  list("f_waz",".waz",.1))) {
      id <- info[[1]]; col <- info[[2]]
      if (col %in% names(df)) {
        r <- range(df[[col]], na.rm=TRUE)
        updateSliderInput(session, id, value=r)
      }
    }

    # § Qualité
    updateCheckboxGroupInput(session, "f_excl_na", selected=NULL)
    updateNumericInput(session, "f_top_n", value=0)
    updateCheckboxInput(session, "f_no_outlier", value=FALSE)

    showNotification("✅ Tous les filtres réinitialisés", type="message", duration=2)
  })

  # ═══ Synchronisation palette globale → sélecteurs locaux ═══
  # ── Synchro palette couleurs vers les modules d'exploration ──
  observeEvent(input$global_palette, {
    pn <- input$global_palette
    if (!is.null(pn) && pn %in% PALETTE_CHOICES) {
      updateSelectInput(session, "uni_pal", selected = pn)
      updateSelectInput(session, "biv_pal", selected = pn)
    }
  }, ignoreInit = TRUE)

  # ── Thème d'interface — injection CSS dynamique complète ──
  observeEvent(input$ui_theme, {
    tn <- input$ui_theme
    if (is.null(tn) || !tn %in% UI_THEME_CHOICES) return()

    css_full <- build_theme_css(tn)
    # Échapper les quotes simples pour le JS
    css_escaped <- gsub("'", "\\\\'", gsub("[\n\r]+", " ", css_full))

    shinyjs::runjs(sprintf(
      "var el=document.getElementById('dynamic-ui-theme');
       if(!el){el=document.createElement('style');el.id='dynamic-ui-theme';document.head.appendChild(el);}
       el.textContent='%s';",
      css_escaped
    ))

    showNotification(
      sprintf("\U0001f3a8 Th\u00e8me d'interface : %s", tn),
      type = "message", duration = 2
    )
  }, ignoreInit = TRUE)

  # Déclencheur historique conservé pour df_f()
  dash_filter_trigger <- reactiveVal(0L)

