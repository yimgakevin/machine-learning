# Extrait de app_v2_final.R — sécurité alimentaire — calculs FCS/rCSI/HDDS/HHS
  # ══════════════════════════════════════════════════════════════════
  # SÉCURITÉ ALIMENTAIRE — FCS · rCSI · HDDS · HHS
  # ══════════════════════════════════════════════════════════════════

  # Helper : sélecteur rapide de colonne numérique avec guess
  sa_select <- function(id, label, patterns, d, vn) {
    guess <- guess_col(d, patterns)
    sel <- guess %||% ""
    selectInput(id, label, choices = c("— Aucun —" = "", vn),
                selected = sel, width = "100%")
  }

  # ── UI dynamiques FCS pré-calculé ──
  output$sa_fcs_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_precalc", "Colonne FCS :",
              c("^fcs$","^FCS$","^food.*cons.*score","^fcs_score","^score.*fcs",
                "^fcscore","FCSScore","food_consumption","sca$","^sca_score",
                "^score_consommation"), d, vn)
  })

  # ── UI dynamiques FCS groupes ──
  output$sa_fcs_cereales_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_cereales", "Céréales/Tubercules :",
              c("^cereale","^cereal","^staple","^fcs.*cereal","^fcs.*staple",
                "FCSStap","fcs_staple","tubercul","main_staple","grp.*cereal",
                "g1.*cereal","food.*cereal","alim.*cereal"), d, vn)
  })
  output$sa_fcs_legumineuses_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_legumineuses", "Légumineuses :",
              c("^legum","^pulse","^fcs.*pulse","^fcs.*legum","FCSPulse",
                "fcs_pulse","bean","haricot","grp.*legum","g2.*pulse",
                "food.*pulse","alim.*legum","noix_legum"), d, vn)
  })
  output$sa_fcs_legumes_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_legumes", "Légumes :",
              c("^legume","^veget","^fcs.*veg","^fcs.*legum","FCSVeg","fcs_veg",
                "grp.*legume","g3.*veg","food.*veg","alim.*legume"), d, vn)
  })
  output$sa_fcs_fruits_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_fruits", "Fruits :",
              c("^fruit","^fcs.*fruit","FCSFruit","fcs_fruit",
                "grp.*fruit","g4.*fruit","food.*fruit","alim.*fruit"), d, vn)
  })
  output$sa_fcs_viande_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_viande", "Viande/Poisson :",
              c("^viande","^meat","^protein","^fcs.*meat","^fcs.*prot",
                "FCSPr","fcs_meat","fcs_protein","poisson","fish",
                "grp.*viande","grp.*meat","g5.*meat","food.*meat",
                "alim.*viande","animal_protein"), d, vn)
  })
  output$sa_fcs_lait_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_lait", "Lait/Prod. laitiers :",
              c("^lait","^milk","^dairy","^fcs.*milk","^fcs.*dairy","^fcs.*lait",
                "FCSDairy","fcs_dairy","prod.*lait","grp.*lait",
                "g6.*dairy","g6.*milk","food.*dairy","alim.*lait"), d, vn)
  })
  output$sa_fcs_sucre_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_sucre", "Sucre :",
              c("^sucre","^sugar","^fcs.*sugar","^fcs.*sucre","FCSSugar","fcs_sugar",
                "grp.*sucre","g7.*sugar","food.*sugar","alim.*sucre",
                "sweet","sucrerie"), d, vn)
  })
  output$sa_fcs_huile_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fcs_huile", "Huile/Graisse :",
              c("^huile","^oil","^fat","^fcs.*oil","^fcs.*fat","^fcs.*huile",
                "FCSFat","FCSOil","fcs_oil","fcs_fat","graisse",
                "grp.*huile","g8.*oil","g8.*fat","food.*oil","alim.*huile"), d, vn)
  })

  # ── UI dynamiques rCSI ──
  output$sa_rcsi_prefere_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_prefere", "Aliments moins préférés :",
              c("^rcsi.*pref","^less.*prefer","^moins.*pref","rcsi1",
                "rCSILessQlty","rcsi_less","coping.*pref","strat.*pref",
                "alim.*moins.*pref","less_preferred"), d, vn)
  })
  output$sa_rcsi_emprunter_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_emprunter", "Emprunter nourriture :",
              c("^rcsi.*borrow","^emprunt","rcsi2","rCSIBorrow",
                "rcsi_borrow","coping.*borrow","coping.*emprunt",
                "strat.*emprunt","borrow_food"), d, vn)
  })
  output$sa_rcsi_repas_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_repas", "Réduire nb de repas :",
              c("^rcsi.*meal","^redui.*repas","rcsi3","rCSIMealNb",
                "rcsi_meal","reduce.*meal","coping.*meal","coping.*repas",
                "strat.*repas","limit.*meal","fewer_meal"), d, vn)
  })
  output$sa_rcsi_portions_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_portions", "Réduire les portions :",
              c("^rcsi.*port","^redui.*port","rcsi4","rCSIMealSize","meal.*size","meal.*si",
                "rcsi_portion","reduce.*portion","coping.*portion",
                "strat.*portion","limit.*portion","smaller_meal"), d, vn)
  })
  output$sa_rcsi_adultes_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_adultes", "Réduire conso adultes :",
              c("^rcsi.*adult","^redui.*adult","rcsi5","rCSIMealAdult",
                "rcsi_adult","restrict.*adult","coping.*adult",
                "strat.*adult","adults_restrict"), d, vn)
  })

  # ── UI dynamiques HDDS ──
  output$sa_hdds_cols_ui <- renderUI({
    vn <- vars_num()
    hdds_guess <- setdiff(
      grep("^group|^grp.*alim|^hdds_|^dd_|^dietary|^diversit",
           vn, ignore.case = TRUE, value = TRUE),
      c("hdds", "hdds_score"))
    checkboxGroupInput("sa_hdds_cols", "Colonnes des groupes alimentaires (0/1) :",
                       choices = vn,
                       selected = if (length(hdds_guess) > 0) hdds_guess else NULL,
                       width = "100%")
  })

  # ── UI dynamiques HHS ──
  output$sa_hhs_nofood_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_nofood", "Pas de nourriture :",
              c("^hhs.*nofood","^hhs.*no.*food","^hhs1","pas.*nourr",
                "HHSNoFood","hhs_nofood","no_food_home","hunger.*nofood",
                "faim.*nourr","manque.*nourr"), d, vn)
  })
  output$sa_hhs_sleep_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_sleep", "Dormir affamé :",
              c("^hhs.*sleep","^hhs.*bed","^hhs2","dormir.*affam",
                "HHSBedHungry","hhs_sleep","hhs_bed","sleep.*hungry",
                "bed_hungry","coucher.*faim"), d, vn)
  })
  output$sa_hhs_wholeday_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_wholeday", "Journée sans manger :",
              c("^hhs.*whole","^hhs.*day","^hhs3","journee.*sans",
                "HHSNoMealDay","hhs_wholeday","hhs_allday","whole_day",
                "no_eat_day","jour.*sans.*mang"), d, vn)
  })

  # ── UI dynamiques : scores pré-calculés (HDDS · rCSI · HHS) ──
  output$sa_hdds_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hdds_precalc", "Score HDDS (0-12) :",
              c("^hdds$","^hdds_score$","^score.*hdds","^dietary.*diversity",
                "^dd_score","^dds$","diversit"), d, vn)
  })
  output$sa_rcsi_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_rcsi_precalc", "Score rCSI (0-56) :",
              c("^coping.*strategy","^rcsi$","^rcsi_score$","^score.*rcsi",
                "casesi","^csi$","coping.*index"), d, vn)
  })
  output$sa_hhs_precalc_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_hhs_precalc", "Score HHS (0-6) :",
              c("^household.*hunger","^hhs$","^hhs_score$","^score.*hhs",
                "^hhs_total","faim.*score","hunger.*score"), d, vn)
  })

  # ── UI dynamiques : LCSI · FIES · MDD-W ──
  sa_select_any <- function(id, label, patterns) {
    d <- df_f()
    guess <- guess_col(d, patterns, type = "any")
    selectInput(id, label, choices = c("— Aucun —" = "", names(d)),
                selected = guess %||% "", width = "100%")
  }
  output$sa_lcsi_class_ui <- renderUI({
    sa_select_any("sa_lcsi_class", "Classe LCSI pré-calculée :",
                  c("^lcsi.*class","^lcsi$","live.*coping.*index","lcs.*class",
                    "coping.*subsistance","strategie.*subsistance"))
  })
  output$sa_lcsi_stress_ui <- renderUI({
    sa_select_any("sa_lcsi_stress", "Stratégies de stress (oui/non) :",
                  c("^lcsi.*stress","stress.*lcsi","^stress","lcsi.*s1"))
  })
  output$sa_lcsi_crisis_ui <- renderUI({
    sa_select_any("sa_lcsi_crisis", "Stratégies de crise (oui/non) :",
                  c("^lcsi.*crise","^lcsi.*crisis","crise.*lcsi","^crise","^crisis"))
  })
  output$sa_lcsi_emerg_ui <- renderUI({
    sa_select_any("sa_lcsi_emerg", "Stratégies d'urgence (oui/non) :",
                  c("^lcsi.*urg","urg.*lcsi","^urgence","^emerg","emergency.*lcsi"))
  })
  output$sa_fies_col_ui <- renderUI({
    d <- df_f(); vn <- vars_num()
    sa_select("sa_fies_col", "Score FIES (0-8) :",
              c("^fies","food.*insecurity.*experience","^score.*fies","insecurite.*experience"),
              d, vn)
  })
  output$sa_mddw_col_ui <- renderUI({
    sa_select_any("sa_mddw_col", "MDD-W (≥5 groupes, oui/non) :",
                  c("^mdd.*w","^mddw","mdd_w","dietary.*diversity.*wom",
                    "women.*dietary","diversite.*femme"))
  })

  # ── UI stratification ──
  output$sa_strat_ui <- renderUI({
    vc <- vars_cat()
    selectInput("sa_strat_var", "Stratifier par :",
                choices = c("— Aucune —" = "", vc), selected = "", width = "100%")
  })

  # ── Stockage résultats ──
  sa_res <- reactiveValues(fcs = NULL, rcsi = NULL, hdds = NULL, hhs = NULL,
                           lcsi = NULL, fies = NULL, mddw = NULL)

  # ── Calcul ──
  observeEvent(input$run_sa, {
    d <- df_f(); req(nrow(d) > 0)
    nullify <- function(x) if (is.null(x) || x == "") NULL else x
    mode <- input$sa_mode

    # Reset des resultats
    sa_res$fcs <- NULL; sa_res$rcsi <- NULL; sa_res$hdds <- NULL; sa_res$hhs <- NULL
    sa_res$lcsi <- NULL; sa_res$fies <- NULL; sa_res$mddw <- NULL
    computed <- character(0)
    skipped  <- character(0)

    # Compteur de colonnes mappées par indicateur (pour diagnostic)
    sa_diag <- list()

    withProgress(message = "Calcul des indicateurs de s\u00e9curit\u00e9 alimentaire...", value = 0, {

      # FCS pre-calcule
      if (mode %in% c("fcs_precalc", "all")) {
        incProgress(0.1, detail = "FCS (pr\u00e9-calcul\u00e9)...")
        col_fcs <- nullify(input$sa_fcs_precalc)
        if (!is.null(col_fcs) && col_fcs %in% names(d)) {
          sa_res$fcs <- classify_fcs_precalc(d[[col_fcs]])
          d$FCS_class <- sa_res$fcs$class
          computed <- c(computed, "FCS (pr\u00e9-calcul\u00e9)")
        } else {
          sa_diag[["FCS pr\u00e9-calcul\u00e9"]] <- "aucune colonne FCS s\u00e9lectionn\u00e9e"
          if (mode == "fcs_precalc") {
            skipped <- c(skipped, "FCS pr\u00e9-calcul\u00e9 : s\u00e9lectionnez une colonne contenant le score FCS d\u00e9j\u00e0 calcul\u00e9")
          }
        }
      }

      # FCS calcule depuis groupes
      if (mode %in% c("fcs_calc", "all")) {
        incProgress(0.15, detail = "FCS (groupes alimentaires)...")
        fcs_cols <- list(
          cereales = nullify(input$sa_fcs_cereales),
          legumineuses = nullify(input$sa_fcs_legumineuses),
          legumes = nullify(input$sa_fcs_legumes),
          fruits = nullify(input$sa_fcs_fruits),
          viande = nullify(input$sa_fcs_viande),
          lait = nullify(input$sa_fcs_lait),
          sucre = nullify(input$sa_fcs_sucre),
          huile = nullify(input$sa_fcs_huile)
        )
        n_fcs_mapped <- sum(!sapply(fcs_cols, is.null))
        fcs <- calc_fcs(d,
          col_cereales = fcs_cols$cereales,
          col_legumineuses = fcs_cols$legumineuses,
          col_legumes = fcs_cols$legumes,
          col_fruits = fcs_cols$fruits,
          col_viande = fcs_cols$viande,
          col_lait = fcs_cols$lait,
          col_sucre = fcs_cols$sucre,
          col_huile = fcs_cols$huile
        )
        if (!is.null(fcs)) {
          sa_res$fcs <- fcs
          d$FCS_class <- fcs$class
          computed <- c(computed, paste0("FCS (", fcs$n_groups, " groupes)"))
        } else {
          sa_diag[["FCS groupes"]] <- paste0(n_fcs_mapped, "/8 groupes mapp\u00e9s (min 4)")
          if (mode == "fcs_calc") {
            skipped <- c(skipped, paste0("FCS : ", n_fcs_mapped, "/8 groupes mapp\u00e9s (minimum 4 requis). ",
                         "S\u00e9lectionnez les colonnes de fr\u00e9quence alimentaire (0-7 jours/semaine)"))
          }
        }
      }

      # rCSI
      if (mode %in% c("rcsi", "all")) {
        incProgress(0.2, detail = "rCSI...")
        col_rcsi_pre <- nullify(input$sa_rcsi_precalc)
        rcsi_cols <- list(
          prefere = nullify(input$sa_rcsi_prefere),
          emprunter = nullify(input$sa_rcsi_emprunter),
          repas = nullify(input$sa_rcsi_repas),
          portions = nullify(input$sa_rcsi_portions),
          adultes = nullify(input$sa_rcsi_adultes)
        )
        n_rcsi_mapped <- sum(!sapply(rcsi_cols, is.null))
        rcsi <- if (!is.null(col_rcsi_pre) && col_rcsi_pre %in% names(d)) {
          calc_rcsi_precalc(d, col_rcsi_pre)
        } else {
          calc_rcsi(d,
            col_moins_prefere = rcsi_cols$prefere,
            col_emprunter = rcsi_cols$emprunter,
            col_reduire_repas = rcsi_cols$repas,
            col_reduire_portions = rcsi_cols$portions,
            col_reduire_adultes = rcsi_cols$adultes
          )
        }
        if (!is.null(rcsi)) {
          sa_res$rcsi <- rcsi
          d$rCSI_class <- rcsi$class
          computed <- c(computed, if (isTRUE(rcsi$precalc)) "rCSI (pr\u00e9-calcul\u00e9)" else "rCSI")
        } else {
          sa_diag[["rCSI"]] <- paste0(n_rcsi_mapped, "/5 strat\u00e9gies mapp\u00e9es (min 3)")
          if (mode == "rcsi") {
            skipped <- c(skipped, paste0("rCSI : ", n_rcsi_mapped, "/5 strat\u00e9gies mapp\u00e9es (minimum 3 requises). ",
                         "S\u00e9lectionnez les colonnes de strat\u00e9gies d'adaptation (0-7 jours)"))
          }
        }
      }

      # HDDS
      if (mode %in% c("hdds", "all")) {
        incProgress(0.2, detail = "HDDS...")
        col_hdds_pre <- nullify(input$sa_hdds_precalc)
        cols_hdds <- input$sa_hdds_cols
        n_hdds <- if (!is.null(cols_hdds)) length(cols_hdds) else 0
        if (!is.null(col_hdds_pre) && col_hdds_pre %in% names(d)) {
          hdds <- calc_hdds_precalc(d, col_hdds_pre)
          sa_res$hdds <- hdds
          d$HDDS_class <- hdds$class
          computed <- c(computed, "HDDS (pr\u00e9-calcul\u00e9)")
        } else if (n_hdds >= 4) {
          hdds <- calc_hdds(d, cols_hdds)
          if (!is.null(hdds)) {
            sa_res$hdds <- hdds
            d$HDDS_class <- hdds$class
            computed <- c(computed, paste0("HDDS (", hdds$n_groups_used, " groupes)"))
          }
        } else {
          sa_diag[["HDDS"]] <- paste0(n_hdds, " groupes coch\u00e9s (min 4)")
          if (mode == "hdds") {
            skipped <- c(skipped, paste0("HDDS : ", n_hdds, " groupes coch\u00e9s (minimum 4 requis). ",
                         "Cochez les colonnes binaires (0/1) des groupes alimentaires"))
          }
        }
      }

      # HHS
      if (mode %in% c("hhs", "all")) {
        incProgress(0.2, detail = "HHS...")
        col_hhs_pre <- nullify(input$sa_hhs_precalc)
        hhs_cols <- list(
          nofood = nullify(input$sa_hhs_nofood),
          sleep = nullify(input$sa_hhs_sleep),
          wholeday = nullify(input$sa_hhs_wholeday)
        )
        n_hhs_mapped <- sum(!sapply(hhs_cols, is.null))
        hhs <- if (!is.null(col_hhs_pre) && col_hhs_pre %in% names(d)) {
          calc_hhs_precalc(d, col_hhs_pre)
        } else {
          calc_hhs(d,
            col_no_food = hhs_cols$nofood,
            col_sleep_hungry = hhs_cols$sleep,
            col_whole_day = hhs_cols$wholeday
          )
        }
        if (!is.null(hhs)) {
          sa_res$hhs <- hhs
          d$HHS_class <- hhs$class
          computed <- c(computed, if (isTRUE(hhs$precalc)) "HHS (pr\u00e9-calcul\u00e9)" else "HHS")
        } else {
          sa_diag[["HHS"]] <- paste0(n_hhs_mapped, "/3 questions mapp\u00e9es (min 2)")
          if (mode == "hhs") {
            skipped <- c(skipped, paste0("HHS : ", n_hhs_mapped, "/3 questions mapp\u00e9es (minimum 2 requises). ",
                         "S\u00e9lectionnez les colonnes de fr\u00e9quence de faim (0-2)"))
          }
        }
      }

      # LCSI
      if (mode %in% c("lcsi", "all")) {
        incProgress(0.1, detail = "LCSI...")
        lcsi_cols <- list(
          stress = nullify(input$sa_lcsi_stress),
          crisis = nullify(input$sa_lcsi_crisis),
          emerg  = nullify(input$sa_lcsi_emerg),
          class  = nullify(input$sa_lcsi_class)
        )
        n_lcsi_mapped <- sum(!sapply(lcsi_cols, is.null))
        if (n_lcsi_mapped >= 1) {
          lcsi <- calc_lcsi(d,
            col_stress = lcsi_cols$stress,
            col_crisis = lcsi_cols$crisis,
            col_emergency = lcsi_cols$emerg,
            col_lcsi_class = lcsi_cols$class
          )
          if (!is.null(lcsi$summary) && nrow(lcsi$summary) > 0) {
            sa_res$lcsi <- lcsi
            computed <- c(computed, "LCSI")
          } else {
            sa_diag[["LCSI"]] <- "colonnes LCSI invalides"
          }
        } else {
          sa_diag[["LCSI"]] <- "aucune colonne LCSI s\u00e9lectionn\u00e9e"
          if (mode == "lcsi") {
            skipped <- c(skipped, paste0("LCSI : s\u00e9lectionnez la classe pr\u00e9-calcul\u00e9e ",
                         "ou au moins une famille de strat\u00e9gies (stress/crise/urgence)"))
          }
        }
      }

      # FIES
      if (mode %in% c("fies", "all")) {
        incProgress(0.1, detail = "FIES...")
        col_fies <- nullify(input$sa_fies_col)
        if (!is.null(col_fies) && col_fies %in% names(d)) {
          fies <- calc_fies(d, col_fies)
          if (!is.null(fies$summary) && nrow(fies$summary) > 0 &&
              sum(fies$summary$N, na.rm = TRUE) > 0) {
            sa_res$fies <- fies
            computed <- c(computed, "FIES")
          } else {
            sa_diag[["FIES"]] <- "score FIES non valide (0-8 attendu)"
          }
        } else {
          sa_diag[["FIES"]] <- "aucune colonne FIES s\u00e9lectionn\u00e9e"
          if (mode == "fies") {
            skipped <- c(skipped, "FIES : s\u00e9lectionnez une colonne de score FIES (0-8)")
          }
        }
      }

      # MDD-W
      if (mode %in% c("mddw", "all")) {
        incProgress(0.1, detail = "MDD-W...")
        col_mddw <- nullify(input$sa_mddw_col)
        if (!is.null(col_mddw) && col_mddw %in% names(d)) {
          mddw <- calc_mddw(d, col_mddw)
          if (!is.na(mddw$pct) && mddw$n_valid > 0) {
            sa_res$mddw <- mddw
            computed <- c(computed, "MDD-W")
          } else {
            sa_diag[["MDD-W"]] <- "colonne MDD-W non exploitable (oui/non attendu)"
          }
        } else {
          sa_diag[["MDD-W"]] <- "aucune colonne MDD-W s\u00e9lectionn\u00e9e"
          if (mode == "mddw") {
            skipped <- c(skipped, "MDD-W : s\u00e9lectionnez la colonne binaire (oui/non) du seuil \u2265 5 groupes")
          }
        }
      }

      incProgress(0.15, detail = "Termin\u00e9")
    })

    # Feedback utilisateur
    if (length(computed) > 0) {
      showNotification(
        paste0("\u2705 Indicateurs calcul\u00e9s : ", paste(computed, collapse = ", ")),
        type = "message", duration = 6)
    }
    if (length(computed) == 0) {
      # Message détaillé selon le mode
      if (mode == "all") {
        # En mode "all", donner un diagnostic précis
        diag_msg <- paste(names(sa_diag), sa_diag, sep = " \u2192 ", collapse = " | ")
        showNotification(
          HTML(paste0(
            "<b>\u26a0 Aucun indicateur calcul\u00e9</b><br/>",
            "En mode \u00ab Tous les indicateurs \u00bb, aucune colonne de s\u00e9curit\u00e9 alimentaire ",
            "n'a \u00e9t\u00e9 d\u00e9tect\u00e9e automatiquement.<br/><br/>",
            "<b>Actions recommand\u00e9es :</b><br/>",
            "\u2022 S\u00e9lectionnez un mode sp\u00e9cifique (FCS, rCSI, HDDS ou HHS)<br/>",
            "\u2022 Mappez manuellement vos colonnes dans le panneau de gauche<br/>",
            "\u2022 V\u00e9rifiez que vos donn\u00e9es contiennent des variables de s\u00e9curit\u00e9 alimentaire"
          )),
          type = "warning", duration = 12)
      } else {
        showNotification(
          HTML(paste0(
            "<b>\u26a0 Aucun indicateur calcul\u00e9</b><br/>",
            "V\u00e9rifiez que les colonnes sont correctement s\u00e9lectionn\u00e9es ",
            "dans le panneau de gauche.<br/><br/>",
            if (length(skipped) > 0) paste0("<b>D\u00e9tail :</b> ", paste(skipped, collapse = "<br/>"))
            else "S\u00e9lectionnez les colonnes correspondant aux variables demand\u00e9es."
          )),
          type = "warning", duration = 10)
      }
    }
    if (length(computed) > 0 && length(skipped) > 0) {
      showNotification(
        paste0("Indicateurs ignor\u00e9s : ", paste(skipped, collapse = " | ")),
        type = "warning", duration = 8)
    }
  })

  # ── Rendu : Synthèse ──
  sa_severity_r <- reactive({
    calc_sa_severity_index(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs)
  })
  sa_verdict_r <- reactive({
    calc_sa_verdict(sa_res$fcs, sa_res$rcsi, sa_res$hdds, sa_res$hhs,
                    sev_res = sa_severity_r(), ipc_summary = calc_ipc_summary(.ipc_phases_r()))
  })

  output$sa_verdict <- renderUI({
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_res) return(NULL)
    v <- sa_verdict_r(); req(v)
    tagList(
      tags$div(style = paste0("background:", v$bg, ";border-left:5px solid ", v$color,
        ";border-radius:10px;padding:14px 18px;margin-bottom:12px;"),
        tags$div(style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
          tags$span(style = paste0("background:", v$color,
            ";color:white;font-size:11px;font-weight:700;padding:3px 10px;",
            "border-radius:20px;letter-spacing:.5px;text-transform:uppercase;"), v$badge),
          tags$span(style = "font-size:13px;color:#333;",
            tags$b(v$headline), " ", v$recommendation)
        ),
        if (length(v$bullets) > 0)
          tags$ul(style = "margin:8px 0 0 0;padding-left:18px;font-size:12px;color:#444;line-height:1.7;",
            lapply(v$bullets, function(b) tags$li(b)))
      )
    )
  })

  output$sa_summary_cards <- renderUI({
    any_res <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
               !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_res) return(NULL)
    card <- function(icon_name, col, title, value, sub = NULL) {
      tags$div(class = "zs-kpi-card", style = "flex:1;min-width:150px;padding:14px 16px;",
        tags$div(class = "zs-header",
          tags$div(class = "zs-header-icon", style = paste0("background:", col, ";"),
                   icon(icon_name)),
          tags$div(class = "zs-title", title)),
        tags$div(class = "zs-value", style = paste0("color:", col, ";font-size:26px;"), value),
        if (!is.null(sub)) tags$div(class = "zs-subtitle", sub))
    }
    n_obs <- max(c(length(sa_res$fcs$score), length(sa_res$rcsi$score),
                   length(sa_res$hdds$score), length(sa_res$hhs$score)), 0)
    pct_worst <- function(r, level) {
      if (is.null(r)) return(NULL)
      round(sum(r$class == level, na.rm = TRUE) / sum(!is.na(r$class)) * 100, 1)
    }
    tags$div(style = "display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
      card("users", "#0B3954", "Ménages analysés", n_obs,
           paste0(sum(c(!is.null(sa_res$fcs), !is.null(sa_res$rcsi),
                        !is.null(sa_res$hdds), !is.null(sa_res$hhs))), " indicateur(s) core")),
      if (!is.null(sa_res$fcs))
        card("utensils", "#0077B6", "FCS moyen",
             round(mean(sa_res$fcs$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$fcs, "Pauvre"), " % pauvre")),
      if (!is.null(sa_res$rcsi))
        card("triangle-exclamation", "#E67E22", "rCSI moyen",
             round(mean(sa_res$rcsi$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$rcsi, "Coping \u00e9lev\u00e9"), " % coping \u00e9lev\u00e9")),
      if (!is.null(sa_res$hdds))
        card("seedling", "#27AE60", "HDDS moyen",
             round(mean(sa_res$hdds$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$hdds, "Faible diversit\u00e9"), " % faible diversit\u00e9")),
      if (!is.null(sa_res$hhs))
        card("heartbeat", "#C0392B", "HHS moyen",
             round(mean(sa_res$hhs$score, na.rm = TRUE), 1),
             paste0(pct_worst(sa_res$hhs, "Faim s\u00e9v\u00e8re"), " % faim s\u00e9v\u00e8re")),
      if (!is.null(sa_res$mddw))
        card("bowl-food", "#8E44AD", "MDD-W adéquat",
             paste0(sa_res$mddw$pct, " %"),
             paste0("IC 95 % [", sa_res$mddw$ci_lower, "\u2013", sa_res$mddw$ci_upper, "]")),
      if (!is.null(sa_res$fies))
        card("face-frown", "#b36b00", "FIES mod./sévère",
             paste0(sa_res$fies$pct_moderate_severe, " %"),
             paste0("moyenne ", sa_res$fies$mean_score, "/8"))
    )
  })

  output$sa_severity_plot <- renderPlotly({
    sev <- sa_severity_r()
    if (is.null(sev))
      return(plotly_empty() |> layout(title = list(
        text = "Calculez au moins un indicateur pour l'indice composite", x = .5)))
    plot_sa_donut(sev$summary |> rename(Catégorie = Classe),
                  "Indice composite de sévérité")
  })

  output$sa_synthesis_reading <- renderUI({
    txt <- sa_reading_synthesis(sa_verdict_r())
    txt_sev <- sa_reading_severity(sa_severity_r())
    if (is.null(txt) && is.null(txt_sev)) return(NULL)
    tagList(
      if (!is.null(txt)) pk_info(txt, "lightbulb", "#0077B6", "#e8f6fc"),
      if (!is.null(txt_sev)) pk_info(txt_sev, "chart-simple", "#8E44AD", "#f5eefa")
    )
  })

  output$sa_synthesis_details <- renderUI({
    any_result <- !is.null(sa_res$fcs) || !is.null(sa_res$rcsi) ||
                  !is.null(sa_res$hdds) || !is.null(sa_res$hhs)
    if (!any_result) {
      sa_gs <- "padding:6px 8px;border:1px solid #e5e5e5;"
      sa_hs <- "padding:8px;border:1px solid #d0e8f5;"
      return(tags$div(style = "padding:15px;",
        tags$div(style = "background:#f0f8ff;border-left:4px solid #0077B6;padding:14px 16px;border-radius:0 8px 8px 0;margin-bottom:16px;",
          tags$div(style = "font-size:14px;font-weight:700;color:#0B3954;margin-bottom:8px;",
            icon("info-circle"), " Guide : Comment utiliser ce module"),
          tags$ol(style = "font-size:12px;color:#333;line-height:1.8;margin:0;padding-left:20px;",
            tags$li("Choisissez un ", tags$b("mode de calcul"), " dans le panneau de gauche."),
            tags$li("Pour chaque indicateur, ", tags$b("s\u00e9lectionnez manuellement"), " les colonnes correspondantes."),
            tags$li("Cliquez sur ", tags$b("\u00ab Calculer les indicateurs \u00bb"), ".")
          ),
          tags$div(style="color:#888;font-size:10px;margin-top:6px;",
            "Astuce : les colonnes aux noms standards (FCSStap, rCSILessQlty, HHSNoFood\u2026) sont pr\u00e9-s\u00e9lectionn\u00e9es automatiquement.")
        ),
        tags$table(style = "width:100%;border-collapse:collapse;font-size:11px;",
          tags$thead(tags$tr(style = "background:#e8f4fd;",
            tags$th(style = sa_hs, "Indicateur"),
            tags$th(style = sa_hs, "Description"),
            tags$th(style = sa_hs, "Format attendu"),
            tags$th(style = sa_hs, "Seuils")
          )),
          tags$tbody(
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "FCS"),
              tags$td(style = sa_gs, "Somme pond\u00e9r\u00e9e de 8 groupes alimentaires"),
              tags$td(style = sa_gs, "8 colonnes, 0-7 jours"),
              tags$td(style = sa_gs, "Pauvre <21 | Limite 21-35 | Acceptable >35")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "rCSI"),
              tags$td(style = sa_gs, "Somme pond\u00e9r\u00e9e de 5 strat\u00e9gies d'adaptation"),
              tags$td(style = sa_gs, "5 colonnes, 0-7 jours"),
              tags$td(style = sa_gs, "Faible 0-3 | Moyen 4-18 | \u00c9lev\u00e9 \u226519")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "HDDS"),
              tags$td(style = sa_gs, "Groupes alimentaires consomm\u00e9s en 24h"),
              tags$td(style = sa_gs, "Colonnes binaires 0/1"),
              tags$td(style = sa_gs, "Faible 0-3 | Moyen 4-5 | \u00c9lev\u00e9 \u22656")
            ),
            tags$tr(
              tags$td(style = paste0(sa_gs, "font-weight:700;color:#0077B6;"), "HHS"),
              tags$td(style = sa_gs, "3 questions d'exp\u00e9rience de faim"),
              tags$td(style = sa_gs, "3 colonnes, valeurs 0-2"),
              tags$td(style = sa_gs, "Aucune 0-1 | Mod\u00e9r\u00e9e 2-3 | S\u00e9v\u00e8re 4-6")
            )
          )
        )
      ))
    }
    # Tableau de synthèse des stats
    stats_all <- bind_rows(
      if (!is.null(sa_res$fcs))  sa_res$fcs$stats  |> mutate(Indicateur = "FCS"),
      if (!is.null(sa_res$rcsi)) sa_res$rcsi$stats |> mutate(Indicateur = "rCSI"),
      if (!is.null(sa_res$hdds)) sa_res$hdds$stats |> mutate(Indicateur = "HDDS"),
      if (!is.null(sa_res$hhs))  sa_res$hhs$stats  |> mutate(Indicateur = "HHS")
    )
    if (nrow(stats_all) == 0) return(NULL)
    tags$div(
      tags$div(class = "section-header", "Statistiques descriptives"),
      renderDT(
        datatable(stats_all |> pivot_wider(names_from = Indicateur, values_from = Valeur),
                  rownames = FALSE, options = list(dom = "t"), class = "compact stripe"),
        outputArgs = list()
      )
    )
  })

  # ── Rendu : FCS ──
  output$sa_fcs_donut <- renderPlotly({
    req(sa_res$fcs); plot_sa_donut(sa_res$fcs$summary, "FCS — Classification PAM")
  })
  output$sa_fcs_hist <- renderPlotly({
    req(sa_res$fcs)
    plot_sa_histogram(sa_res$fcs$score, "Distribution du FCS",
      breaks_lines = list(
        list(x = 21, color = "#e74c3c", label = "Pauvre"),
        list(x = 35, color = "#f39c12", label = "Limite")
      ))
  })
  output$sa_fcs_stats <- renderDT({
    req(sa_res$fcs)
    datatable(sa_add_ci(sa_res$fcs$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : rCSI ──
  output$sa_rcsi_donut <- renderPlotly({
    req(sa_res$rcsi); plot_sa_donut(sa_res$rcsi$summary, "rCSI — Stratégies d'adaptation")
  })
  output$sa_rcsi_hist <- renderPlotly({
    req(sa_res$rcsi)
    plot_sa_histogram(sa_res$rcsi$score, "Distribution du rCSI",
      breaks_lines = list(
        list(x = 4, color = "#f39c12", label = "Moyen"),
        list(x = 19, color = "#e74c3c", label = "Élevé")
      ))
  })
  output$sa_rcsi_stats <- renderDT({
    req(sa_res$rcsi)
    datatable(sa_add_ci(sa_res$rcsi$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : HDDS ──
  output$sa_hdds_donut <- renderPlotly({
    req(sa_res$hdds); plot_sa_donut(sa_res$hdds$summary, "HDDS — Diversité alimentaire")
  })
  output$sa_hdds_hist <- renderPlotly({
    req(sa_res$hdds)
    plot_sa_histogram(sa_res$hdds$score, "Distribution du HDDS",
      breaks_lines = list(
        list(x = 3.5, color = "#e74c3c", label = "Faible"),
        list(x = 5.5, color = "#f39c12", label = "Moyen")
      ))
  })
  output$sa_hdds_groups <- renderDT({
    req(sa_res$hdds)
    if (isTRUE(sa_res$hdds$precalc)) {
      datatable(sa_add_ci(sa_res$hdds$summary) |> select(-Couleur) |> rename(`N ménages` = N),
                rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
    } else {
      datatable(sa_res$hdds$group_freq, rownames = FALSE,
                options = list(dom = "t", pageLength = 15), class = "compact stripe") |>
        formatStyle("% consommé",
                    background = styleColorBar(c(0, 100), "#0077B6"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
    }
  })

  # ── Rendu : HHS ──
  output$sa_hhs_donut <- renderPlotly({
    req(sa_res$hhs); plot_sa_donut(sa_res$hhs$summary, "HHS — Échelle de la Faim")
  })
  output$sa_hhs_hist <- renderPlotly({
    req(sa_res$hhs)
    plot_sa_histogram(sa_res$hhs$score, "Distribution du HHS",
      breaks_lines = list(
        list(x = 1.5, color = "#f39c12", label = "Modérée"),
        list(x = 3.5, color = "#e74c3c", label = "Sévère")
      ))
  })
  output$sa_hhs_stats <- renderDT({
    req(sa_res$hhs)
    datatable(sa_add_ci(sa_res$hhs$summary) |> select(-Couleur) |> rename(`N ménages` = N),
              rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
  })

  # ── Rendu : Par strate ──
  output$sa_strat_plot <- renderPlotly({
    d <- df_f()
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    req(strat_var)

    # Utiliser le FCS comme indicateur principal pour la stratification
    if (!is.null(sa_res$fcs)) {
      d$FCS_class <- factor(sa_res$fcs$class,
                             levels = c("Pauvre", "Limite", "Acceptable"))
      plot_sa_by_group(d, "FCS_class", strat_var, "FCS par strate",
                        color_map = c("Pauvre" = "#e74c3c", "Limite" = "#f39c12",
                                      "Acceptable" = "#27ae60")) |> nutri_config()
    } else if (!is.null(sa_res$rcsi)) {
      d$rCSI_class <- factor(sa_res$rcsi$class,
                              levels = c("Pas/Peu de coping", "Coping moyen", "Coping élevé"))
      plot_sa_by_group(d, "rCSI_class", strat_var, "rCSI par strate",
                        color_map = c("Pas/Peu de coping" = "#27ae60",
                                      "Coping moyen" = "#f39c12",
                                      "Coping élevé" = "#e74c3c")) |> nutri_config()
    } else {
      plotly_empty() |> layout(title = list(text = "Calculez un indicateur d'abord", x = .5))
    }
  })

  output$sa_strat_table <- renderDT({
    d <- df_f()
    strat_var <- if (!is.null(input$sa_strat_var) && input$sa_strat_var != "")
      input$sa_strat_var else NULL
    req(strat_var)

    results <- list()
    if (!is.null(sa_res$fcs)) {
      d$FCS_class <- sa_res$fcs$class
      results$FCS <- d |>
        filter(!is.na(.data[[strat_var]])) |>
        group_by(Groupe = .data[[strat_var]]) |>
        summarise(
          `FCS moyen` = round(mean(sa_res$fcs$score[cur_group_rows()], na.rm = TRUE), 1),
          `% Pauvre` = round(sum(FCS_class == "Pauvre") / n() * 100, 1),
          `% Limite` = round(sum(FCS_class == "Limite") / n() * 100, 1),
          `% Acceptable` = round(sum(FCS_class == "Acceptable") / n() * 100, 1),
          N = n(), .groups = "drop"
        )
    }
    if (!is.null(sa_res$hhs)) {
      d$HHS_class <- sa_res$hhs$class
      results$HHS <- d |>
        filter(!is.na(.data[[strat_var]])) |>
        group_by(Groupe = .data[[strat_var]]) |>
        summarise(
          `HHS moyen` = round(mean(sa_res$hhs$score[cur_group_rows()], na.rm = TRUE), 1),
          `% Faim sévère` = round(sum(HHS_class == "Faim sévère") / n() * 100, 1),
          N = n(), .groups = "drop"
        )
    }

    if (length(results) > 0) {
      tbl <- results[[1]]
      if (length(results) > 1) {
        for (i in 2:length(results)) {
          tbl <- left_join(tbl, results[[i]] |> select(-N), by = "Groupe")
        }
      }
      datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 20),
                class = "compact stripe hover")
    } else {
      datatable(data.frame(Message = "Calculez les indicateurs d'abord"), rownames = FALSE)
    }
  })

