# Extrait de app_v2_final.R — import fichier, KoboToolbox / ODK
  # ── IMPORT ──────────────────────────────────────────────────────────────────
  observeEvent(input$upload_file, {
    req(input$upload_file)
    showNotification("⏳ Importation en cours...", id="notif_import", duration=NULL)
    df <- import_data(input$upload_file$datapath, input$upload_file$name)
    if (!is.list(df) || is.data.frame(df)) {
      raw_data(as.data.frame(df))
      m <- auto_detect(df)
      map_cols(m)
      df_enrich <- tryCatch(calc_indicateurs(df, m), error=function(e) {
        message("[IMPORT] calc_indicateurs ERREUR: ", e$message)
        df
      })
      # Filet de sécurité : créer .fcs/.rcsi/.hdds si calc_indicateurs a échoué
      df_enrich <- tryCatch(ensure_sa_columns(df_enrich), error=function(e) df_enrich)
      message("[IMPORT] Après enrichissement: .fcs=", ".fcs" %in% names(df_enrich),
              " | .rcsi=", ".rcsi" %in% names(df_enrich),
              " | ncol=", ncol(df_enrich))
      mapped_data(df_enrich)
      removeNotification("notif_import")
      showNotification(paste0("✅ ",nrow(df)," lignes · ",ncol(df)," variables importées"),
                       type="message", duration=4)
      # Audit trail
      add_audit_entry(list(
        timestamp = Sys.time(), action = "import",
        module = "Import", details = paste0("Fichier: ", input$upload_file$name,
          " — ", nrow(df), " lignes × ", ncol(df), " colonnes")
      ))
    } else {
      removeNotification("notif_import")
      showNotification(paste("❌ Erreur:", df$error), type="error", duration=6)
      add_audit_entry(list(
        timestamp = Sys.time(), action = "error",
        module = "Import", details = paste0("Erreur import: ", df$error)
      ))
    }
  })

  # ── IMPORT — module namespacé (mod_import.R) ──────────────────────────────
  mod_import_server("import", raw_data, mapped_data, map_cols)

  # Données filtrées
  df_f <- reactive({
    df <- mapped_data(); req(df)
    m  <- map_cols()
    d  <- df
    # Écouter les changements des filtres dashboard (via le trigger)
    dash_filter_trigger()

    # ── §1 GÉOGRAPHIE ─────────────────────────────────────────────────
    if (!is.null(m$region) && m$region %in% names(d) && !is.null(input$f_region) && length(input$f_region) > 0)
      if (input$f_region != "Toutes")
        d <- filter(d, .data[[m$region]] == input$f_region)

    if (!is.null(m$dept) && m$dept %in% names(d) && !is.null(input$f_dept) && length(input$f_dept) > 0)
      if (input$f_dept != "Tous")
        d <- filter(d, .data[[m$dept]] == input$f_dept)

    zone_col <- detect_col(names(d), c("zone","milieu","area","type_zone","setting","camp"))
    if (!is.null(zone_col) && !is.null(input$f_zone) && length(input$f_zone) > 0)
      d <- filter(d, as.character(.data[[zone_col]]) %in% input$f_zone)

    cluster_col <- detect_col(names(d), c("cluster","grappe","psu","cluster_id","clust"))
    if (!is.null(cluster_col) && !is.null(input$f_cluster) && length(input$f_cluster) > 0)
      d <- filter(d, as.character(.data[[cluster_col]]) %in% input$f_cluster)

    strate_col <- detect_col(names(d), c("strate","strata","stratum","strat"))
    if (!is.null(strate_col) && !is.null(input$f_strate) && length(input$f_strate) > 0)
      d <- filter(d, as.character(.data[[strate_col]]) %in% input$f_strate)

    # Vague / Round d'enquête
    if (!is.null(m$survey) && m$survey %in% names(d) &&
        !is.null(input$f_survey_round) && length(input$f_survey_round) > 0)
      d <- filter(d, as.character(.data[[m$survey]]) %in% input$f_survey_round)

    # ── §2 DÉMOGRAPHIE ────────────────────────────────────────────────
    if (!is.null(m$sexe) && m$sexe %in% names(d) && !is.null(input$f_sexe) && length(input$f_sexe) > 0)
      if (input$f_sexe != "Tous")
        d <- filter(d, .data[[m$sexe]] == input$f_sexe)

    if (!is.null(m$depl) && m$depl %in% names(d) && !is.null(input$f_depl))
      if (length(input$f_depl) > 0)
        d <- filter(d, as.character(.data[[m$depl]]) %in% input$f_depl)

    if (!is.null(m$age_m) && ".age" %in% names(d) && !is.null(input$f_age))
      d <- filter(d, is.na(.age) | between(.age, input$f_age[1], input$f_age[2]))

    age_grp_col <- detect_col(names(d), c("age_group","age_grp","grp_age","groupe_age","tranche_age"))
    if (!is.null(age_grp_col) && !is.null(input$f_age_grp) && length(input$f_age_grp) > 0)
      d <- filter(d, as.character(.data[[age_grp_col]]) %in% input$f_age_grp)

    # ── §3 INDICATEURS OMS ────────────────────────────────────────────
    if ("nutr_cat" %in% names(d) && !is.null(input$f_nutr) && length(input$f_nutr) > 0)
      d <- filter(d, as.character(nutr_cat) %in% input$f_nutr)

    if ("GAM" %in% names(d) && !is.null(input$f_gam) && input$f_gam != "Tous")
      d <- filter(d, GAM == as.logical(as.integer(input$f_gam)))

    if ("SAM" %in% names(d) && !is.null(input$f_sam) && input$f_sam != "Tous")
      d <- filter(d, SAM == as.logical(as.integer(input$f_sam)))

    if ("MAM" %in% names(d) && !is.null(input$f_mam) && input$f_mam != "Tous")
      d <- filter(d, MAM == as.logical(as.integer(input$f_mam)))

    if ("Stunting" %in% names(d) && !is.null(input$f_stunting) && input$f_stunting != "Tous")
      d <- filter(d, Stunting == as.logical(as.integer(input$f_stunting)))

    if ("Underweight" %in% names(d) && !is.null(input$f_underweight) && input$f_underweight != "Tous")
      d <- filter(d, Underweight == as.logical(as.integer(input$f_underweight)))

    oedeme_col <- detect_col(names(d), c("oedeme","oedema","edema","oedèmes","oedème"))
    if (!is.null(oedeme_col) && !is.null(input$f_oedeme) && input$f_oedeme != "Tous")
      d <- filter(d, as.character(.data[[oedeme_col]]) == input$f_oedeme)

    # ── §4 ANTHROPOMÉTRIE ─────────────────────────────────────────────
    if (".muac" %in% names(d) && !is.null(input$f_muac))
      d <- filter(d, is.na(.muac) | between(.muac, input$f_muac[1], input$f_muac[2]))

    if (".whz" %in% names(d) && !is.null(input$f_whz))
      d <- filter(d, is.na(.whz) | between(.whz, input$f_whz[1], input$f_whz[2]))

    if (".haz" %in% names(d) && !is.null(input$f_haz))
      d <- filter(d, is.na(.haz) | between(.haz, input$f_haz[1], input$f_haz[2]))

    if (".waz" %in% names(d) && !is.null(input$f_waz))
      d <- filter(d, is.na(.waz) | between(.waz, input$f_waz[1], input$f_waz[2]))

    # ── §5 QUALITÉ / ÉCHANTILLONNAGE ──────────────────────────────────
    if (!is.null(input$f_excl_na) && length(input$f_excl_na) > 0)
      d <- drop_na(d, any_of(input$f_excl_na))

    if (isTRUE(input$f_no_outlier)) {
      for (z_col in intersect(c(".whz",".haz",".waz"), names(d))) {
        q   <- quantile(d[[z_col]], c(.25,.75), na.rm=TRUE)
        iqr <- q[2] - q[1]
        d   <- filter(d, is.na(.data[[z_col]]) |
                         between(.data[[z_col]], q[1] - 3*iqr, q[2] + 3*iqr))
      }
    }

    if (!is.null(input$f_top_n) && input$f_top_n > 0)
      d <- slice_head(d, n = as.integer(input$f_top_n))

    d
  })

  # Types de variables
  vars_num <- reactive({
    df <- df_f(); req(df)
    v <- names(df)[vapply(df, is.numeric, logical(1)) & !startsWith(names(df), ".")]
    if (length(v) == 0) return(character(0))
    v[vapply(v, function(x) sum(!is.na(df[[x]])) > 2, logical(1))]
  })
  vars_cat <- reactive({
    df <- df_f(); req(df)
    nms <- names(df)
    if (length(nms) == 0) return(character(0))
    nms[vapply(nms, function(n) {
      x <- df[[n]]
      !startsWith(n, ".") && (is.factor(x) || is.character(x)) &&
      length(unique(na.omit(x))) >= 2 && length(unique(na.omit(x))) <= 50
    }, logical(1))]
  })
  vars_cat_pie <- reactive({
    vc <- vars_cat(); df <- df_f()
    if (length(vc) == 0) return(character(0))
    vc[vapply(vc, function(v) length(unique(na.omit(df[[v]]))) <= 8, logical(1))]
  })
  has_oms <- reactive("GAM" %in% names(df_f()))

  filter_context <- reactive({
    d <- df_f(); req(d)
    build_filter_context(d, mapped_data())
  })

  output$data_loaded <- reactive(!is.null(mapped_data()))
  outputOptions(output, "data_loaded", suspendWhenHidden=FALSE)
  # (kw_sig supprimé — remplacé par module unifié inferentiel_v4)

