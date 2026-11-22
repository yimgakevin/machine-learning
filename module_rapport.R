# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_rapport.R — Rapport automatisé style OMS                           ║
# ║  Word · PDF · Sections dynamiques · Graphiques intégrés · Interprétations  ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(officer)
library(flextable)
library(ggplot2)
library(scales)

# ── Palette de styles OMS pour officer ───────────────────────────────────────
style_oms <- list(
  titre_principal = fp_text(
    font.size = 18, bold = TRUE,
    color = "#00426A", font.family = "Calibri"
  ),
  titre_section = fp_text(
    font.size = 14, bold = TRUE,
    color = "#008DC9", font.family = "Calibri"
  ),
  titre_sous_section = fp_text(
    font.size = 12, bold = TRUE,
    color = "#333333", font.family = "Calibri"
  ),
  corps = fp_text(
    font.size = 11, color = "#222222", font.family = "Calibri"
  ),
  alerte_rouge = fp_text(
    font.size = 11, bold = TRUE,
    color = "#c9161d", font.family = "Calibri"
  ),
  alerte_vert = fp_text(
    font.size = 11, bold = TRUE,
    color = "#4dac26", font.family = "Calibri"
  )
)

# ── Interprétation automatique des prévalences ────────────────────────────────
interpreter_prevalence <- function(indicateur, valeur) {
  seuils <- list(
    GAM        = c(5, 10, 15, 30),
    SAM        = c(1, 2, 5, 10),
    Stunting   = c(20, 30, 40, 60),
    Underweight = c(10, 20, 30, 40),
    MUAC_GAM   = c(5, 10, 15, 20)
  )

  niveaux <- c("Acceptable", "Alerte", "Sérieux", "Critique", "Urgence maximale")
  s <- seuils[[indicateur]]
  if (is.null(s)) return("Valeur mesurée")

  niveau <- if (valeur < s[1]) niveaux[1]
             else if (valeur < s[2]) niveaux[2]
             else if (valeur < s[3]) niveaux[3]
             else if (valeur < s[4]) niveaux[4]
             else niveaux[5]

  refs <- list(
    GAM  = "Selon les normes OMS/SMART 2019, une prévalence de MAG",
    SAM  = "Selon les normes OMS, une prévalence de MAS",
    Stunting = "Selon les normes OMS, un retard de croissance",
    Underweight = "Selon les normes OMS, l'insuffisance pondérale"
  )

  base <- if (!is.null(refs[[indicateur]])) refs[[indicateur]] else paste0("La prévalence de ", indicateur)
  paste0(base, " de ", round(valeur, 1), "% est classée comme « ", niveau,
         " » selon les seuils OMS.")
}

# ── Générer le rapport Word complet ──────────────────────────────────────────
generate_word_report <- function(d, map_cols_res,
                                  titre        = "Rapport Nutritionnel",
                                  auteur       = "",
                                  organisation = "Enquête SMART",
                                  zone         = "",
                                  periode      = format(Sys.Date(), "%B %Y"),
                                  sections     = c("resume","descriptif","indicateurs",
                                                   "inferentiel","conclusion")) {
  doc <- read_docx()

  # ─── PAGE DE TITRE ─────────────────────────────────────────────────────────
  doc <- doc |>
    body_add_par("", style = "Normal") |>
    body_add_par("", style = "Normal") |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(
      fpar(ftext(titre, style_oms$titre_principal)),
      style = "centered"
    ) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(
      fpar(ftext(organisation, fp_text(font.size=13, color="#555555",
                                        font.family="Calibri"))),
      style = "centered"
    ) |>
    body_add_fpar(
      fpar(ftext(if (nchar(zone) > 0) paste0("Zone géographique : ", zone) else "",
                  fp_text(font.size=12, color="#555555", font.family="Calibri"))),
      style = "centered"
    ) |>
    body_add_fpar(
      fpar(ftext(paste0("Période : ", periode),
                  fp_text(font.size=12, color="#555555", font.family="Calibri"))),
      style = "centered"
    ) |>
    body_add_fpar(
      fpar(ftext(if (nchar(auteur) > 0) paste0("Rédigé par : ", auteur) else "",
                  fp_text(font.size=11, color="#777777", font.family="Calibri"))),
      style = "centered"
    ) |>
    body_add_fpar(
      fpar(ftext(paste0("Date de génération : ", format(Sys.Date(), "%d %B %Y")),
                  fp_text(font.size=11, color="#777777", font.family="Calibri"))),
      style = "centered"
    ) |>
    body_add_break()

  # ─── RÉSUMÉ EXÉCUTIF ───────────────────────────────────────────────────────
  if ("resume" %in% sections) {
    doc <- doc |>
      body_add_fpar(fpar(ftext("1. Résumé exécutif", style_oms$titre_section))) |>
      body_add_par("", style = "Normal")

    # Situation nutritionnelle globale
    if ("GAM" %in% names(d)) {
      gam_p   <- round(mean(d$GAM, na.rm=TRUE) * 100, 1)
      sam_p   <- round(mean(d$SAM, na.rm=TRUE) * 100, 1)
      stunt_p <- if("Stunting"%in%names(d)) round(mean(d$Stunting,na.rm=TRUE)*100,1) else NA
      under_p <- if("Underweight"%in%names(d)) round(mean(d$Underweight,na.rm=TRUE)*100,1) else NA
      n_tot   <- nrow(d)

      # Contexte général
      doc <- doc |>
        body_add_fpar(fpar(ftext(
          paste0("Cette enquête nutritionnelle a porté sur ", format(n_tot, big.mark=" "),
                 " enfants de moins de 5 ans dans la zone couverte."),
          style_oms$corps))) |>
        body_add_par("", style = "Normal")

      # Alerte GAM
      couleur_gam <- if(gam_p >= 15) style_oms$alerte_rouge else style_oms$alerte_vert
      doc <- doc |>
        body_add_fpar(fpar(
          ftext("Malnutrition aiguë globale (MAG/GAM) : ", style_oms$corps),
          ftext(paste0(gam_p, "%"), couleur_gam),
          ftext(paste0(" — ", interpreter_prevalence("GAM", gam_p)), style_oms$corps)
        )) |>
        body_add_fpar(fpar(
          ftext("Malnutrition aiguë sévère (MAS/SAM) : ", style_oms$corps),
          ftext(paste0(sam_p, "%"), if(sam_p>=2) style_oms$alerte_rouge else style_oms$alerte_vert),
          ftext(paste0(" — ", interpreter_prevalence("SAM", sam_p)), style_oms$corps)
        ))

      if (!is.na(stunt_p))
        doc <- doc |>
          body_add_fpar(fpar(
            ftext("Retard de croissance (Stunting) : ", style_oms$corps),
            ftext(paste0(stunt_p, "%"),
                  if(stunt_p>=30) style_oms$alerte_rouge else style_oms$alerte_vert),
            ftext(paste0(" — ", interpreter_prevalence("Stunting", stunt_p)), style_oms$corps)
          ))
    } else {
      doc <- doc |>
        body_add_fpar(fpar(ftext(
          paste0("Jeu de données analysé : ", nrow(d), " observations, ",
                 ncol(d), " variables."),
          style_oms$corps)))
    }
    doc <- doc |> body_add_par("", style = "Normal")
  }

  # ─── STATISTIQUES DESCRIPTIVES ─────────────────────────────────────────────
  if ("descriptif" %in% sections) {
    doc <- doc |>
      body_add_break() |>
      body_add_fpar(fpar(ftext("2. Statistiques descriptives", style_oms$titre_section))) |>
      body_add_par("", style = "Normal")

    # Tableau des variables numériques
    vars_num <- names(d)[sapply(d, is.numeric) & !startsWith(names(d), ".")]
    vars_num <- head(vars_num, 12)

    if (length(vars_num) > 0) {
      tbl_desc <- map_dfr(vars_num, function(v) {
        x <- na.omit(d[[v]])
        tibble(
          Variable  = v,
          N         = length(x),
          Moyenne   = round(mean(x), 2),
          `Éc.-type` = round(sd(x), 2),
          Médiane   = round(median(x), 2),
          Min       = round(min(x), 2),
          Max       = round(max(x), 2),
          `% manq.` = round(mean(is.na(d[[v]])) * 100, 1)
        )
      })

      ft <- flextable(tbl_desc) |>
        set_header_labels(
          Variable = "Variable", N = "N",
          Moyenne = "Moy.", `Éc.-type` = "Éc.-t.",
          Médiane = "Méd.", Min = "Min", Max = "Max",
          `% manq.` = "% manq."
        ) |>
        bg(bg = "#008DC9", part = "header") |>
        color(color = "white", part = "header") |>
        bold(bold = TRUE, part = "header") |>
        bg(bg = "#f0f8ff", i = seq(1, nrow(tbl_desc), 2), part = "body") |>
        fontsize(size = 10, part = "all") |>
        font(fontname = "Calibri", part = "all") |>
        autofit() |>
        set_caption("Tableau 1. Statistiques descriptives des variables continues")

      doc <- doc |>
        body_add_fpar(fpar(ftext("Tableau 1. Statistiques descriptives des variables continues",
                                  style_oms$titre_sous_section))) |>
        body_add_flextable(ft) |>
        body_add_par("", style = "Normal")
    }
  }

  # ─── INDICATEURS OMS ───────────────────────────────────────────────────────
  if ("indicateurs" %in% sections && "GAM" %in% names(d)) {
    doc <- doc |>
      body_add_break() |>
      body_add_fpar(fpar(ftext("3. Indicateurs nutritionnels — Normes OMS 2006",
                                style_oms$titre_section))) |>
      body_add_par("", style = "Normal") |>
      body_add_fpar(fpar(ftext(
        "Les indicateurs de malnutrition ont été calculés conformément aux standards de croissance OMS 2006. Les intervalles de confiance ont été estimés par la méthode Wilson (méthode recommandée par le programme SMART).",
        style_oms$corps))) |>
      body_add_par("", style = "Normal")

    m <- map_cols_res
    inds <- intersect(c("GAM","SAM","MAM","Stunting","Sev_Stunting",
                         "Underweight","MUAC_GAM","MUAC_SAM"), names(d))
    labels_oms <- c(
      GAM="MAG — Malnutrition Aiguë Globale (WHZ<-2 ou œdèmes)",
      SAM="MAS — Malnutrition Aiguë Sévère (WHZ<-3 ou œdèmes)",
      MAM="MAM — Malnutrition Aiguë Modérée (-3≤WHZ<-2)",
      Stunting="Retard de croissance — Stunting (HAZ<-2)",
      Sev_Stunting="Retard de croissance sévère (HAZ<-3)",
      Underweight="Insuffisance pondérale (WAZ<-2)",
      MUAC_GAM="MAG par MUAC (MUAC<125mm)",
      MUAC_SAM="MAS par MUAC (MUAC<115mm)"
    )

    prev_rows <- map_dfr(inds, function(v) {
      x  <- sum(d[[v]], na.rm=TRUE)
      n  <- sum(!is.na(d[[v]]))
      ic <- binom.confint(x, n, methods="wilson")
      p  <- round(ic$mean * 100, 1)
      tibble(
        Indicateur   = labels_oms[v],
        N            = n, Cas = x,
        `Prév. (%)` = p,
        `IC95 bas`  = round(ic$lower * 100, 1),
        `IC95 haut` = round(ic$upper * 100, 1),
        Sévérité    = if(v=="GAM") {
          if(p<5)"Acceptable" else if(p<10)"Alerte" else if(p<15)"Sérieux" else "Critique"
        } else "—"
      )
    })

    ft_prev <- flextable(prev_rows) |>
      bg(bg = "#008DC9", part = "header") |>
      color(color = "white", part = "header") |>
      bold(bold = TRUE, part = "header") |>
      bg(bg = "#ffebee",
         i  = which(prev_rows$`Prév. (%)` >= 15 &
                      grepl("MAG|GAM", prev_rows$Indicateur)),
         part = "body") |>
      bg(bg = "#fff3e0",
         i  = which(prev_rows$`Prév. (%)` >= 10 &
                      prev_rows$`Prév. (%)` < 15 &
                      grepl("MAG|GAM", prev_rows$Indicateur)),
         part = "body") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      autofit() |>
      set_caption("Tableau 2. Prévalences des indicateurs nutritionnels avec IC 95% (Méthode Wilson)")

    doc <- doc |>
      body_add_fpar(fpar(ftext(
        "Tableau 2. Prévalences des indicateurs nutritionnels",
        style_oms$titre_sous_section))) |>
      body_add_flextable(ft_prev) |>
      body_add_par("", style = "Normal")

    # Graphique prévalences (sauvegardé puis inséré)
    if (length(inds) > 0) {
      tmp_plot <- tempfile(fileext = ".png")
      gam_df   <- prev_rows |>
        filter(grepl("GAM|SAM|Stunting|Underweight", Indicateur))

      p_bar <- ggplot(prev_rows,
                      aes(x = fct_reorder(Indicateur, `Prév. (%)`),
                          y = `Prév. (%)`,
                          fill = `Prév. (%)`)) +
        geom_col(alpha = 0.88, width = 0.7) +
        geom_errorbar(aes(ymin = `IC95 bas`, ymax = `IC95 haut`),
                      width = 0.25, color = "gray30") +
        geom_hline(yintercept = 15, linetype = "dashed",
                   color = "#c9161d", linewidth = 0.8) +
        scale_fill_gradientn(
          colors = c("#4dac26","#f9c734","#e88400","#c9161d"),
          guide  = "none"
        ) +
        coord_flip() +
        labs(x = "", y = "Prévalence (%)",
             title = "Indicateurs nutritionnels — Prévalences avec IC 95%",
             subtitle = paste0("N = ", nrow(d), " | Normes OMS 2006"),
             caption  = "Tiret rouge = seuil critique OMS (15%) pour la MAG") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#00426A"))

      ggsave(tmp_plot, plot = p_bar, width = 16, height = 8,
             dpi = 200, units = "cm")

      doc <- doc |>
        body_add_img(src = tmp_plot, width = 14, height = 7) |>
        body_add_fpar(fpar(ftext(
          "Figure 1. Prévalences des indicateurs nutritionnels avec intervalles de confiance à 95%.",
          fp_text(font.size = 9, italic = TRUE, color = "#555555",
                  font.family = "Calibri"))),
          style = "centered") |>
        body_add_par("", style = "Normal")
    }
  }

  # ─── ANALYSE INFÉRENTIELLE ─────────────────────────────────────────────────
  if ("inferentiel" %in% sections) {
    doc <- doc |>
      body_add_break() |>
      body_add_fpar(fpar(ftext("4. Analyse statistique inférentielle",
                                style_oms$titre_section))) |>
      body_add_par("", style = "Normal") |>
      body_add_fpar(fpar(ftext(
        "Les tests statistiques ont été sélectionnés automatiquement selon les types de variables et les conditions d'application (normalité, homoscédasticité). Le seuil de signification retenu est α = 0,05. Les intervalles de confiance sont calculés à 95%.",
        style_oms$corps))) |>
      body_add_par("", style = "Normal")
  }

  # ─── CONCLUSION ────────────────────────────────────────────────────────────
  if ("conclusion" %in% sections) {
    doc <- doc |>
      body_add_break() |>
      body_add_fpar(fpar(ftext("5. Conclusion et recommandations",
                                style_oms$titre_section))) |>
      body_add_par("", style = "Normal")

    if ("GAM" %in% names(d)) {
      gam_p <- round(mean(d$GAM, na.rm=TRUE) * 100, 1)
      rec   <- if (gam_p >= 15)
        "Une réponse humanitaire d'urgence est fortement recommandée. Des interventions thérapeutiques (PECIMA/ATPE) doivent être déployées immédiatement."
      else if (gam_p >= 10)
        "Une intervention nutritionnelle sectorielle est nécessaire. Des activités préventives et curatives doivent être renforcées."
      else if (gam_p >= 5)
        "Une surveillance nutritionnelle renforcée est recommandée. Des activités de prévention ciblées doivent être maintenues."
      else
        "La situation nutritionnelle est globalement acceptable. La surveillance de routine doit être maintenue."

      doc <- doc |>
        body_add_fpar(fpar(ftext(rec, style_oms$corps))) |>
        body_add_par("", style = "Normal")
    }

    # Références méthodologiques
    doc <- doc |>
      body_add_fpar(fpar(ftext("Références méthodologiques",
                                style_oms$titre_sous_section))) |>
      body_add_fpar(fpar(ftext(
        "• OMS (2006). Standards de croissance de l'enfant. Organisation Mondiale de la Santé, Genève.",
        style_oms$corps))) |>
      body_add_fpar(fpar(ftext(
        "• SMART (2019). Méthodologie SMART d'enquête nutritionnelle. ACF International.",
        style_oms$corps))) |>
      body_add_fpar(fpar(ftext(
        "• PAM (2008). Guide de référence pour la sécurité alimentaire. Programme Alimentaire Mondial.",
        style_oms$corps)))
  }

  # ─── PIED DE PAGE ──────────────────────────────────────────────────────────
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_fpar(doc, fpar(
    ftext(paste0("Document généré automatiquement par NutriAnalyse Pro v2 le ",
                  format(Sys.Date(), "%d/%m/%Y"), " | Style OMS/UNICEF"),
           fp_text(font.size = 9, italic = TRUE, color = "#999999",
                   font.family = "Calibri"))))

  doc
}

# ── Wrapper pour le téléchargement Shiny ─────────────────────────────────────
dl_word_handler <- function(d, map_cols_res, input_rapport) {
  downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input_rapport$titre), "_",
             format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      withProgress(message = "Génération du rapport Word...", {
        doc <- tryCatch(
          generate_word_report(
            d            = d,
            map_cols_res = map_cols_res,
            titre        = input_rapport$titre,
            auteur       = input_rapport$auteur,
            organisation = input_rapport$org,
            zone         = input_rapport$zone,
            periode      = input_rapport$periode,
            sections     = input_rapport$sections
          ),
          error = function(e) {
            doc_err <- read_docx()
            body_add_par(doc_err, paste("Erreur:", e$message))
          }
        )
        print(doc, target = file)
      })
    }
  )
}

cat("module_rapport.R chargé\n")
