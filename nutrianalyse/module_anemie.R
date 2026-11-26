# ══════════════════════════════════════════════════════════════════════════════
# module_anemie.R — Anémie SENS (Hémoglobine, prévalence, ajustements OMS)
# Standards : OMS 2011, SENS Guidelines UNHCR
# ══════════════════════════════════════════════════════════════════════════════

# ── Seuils OMS d'anémie ─────────────────────────────────────────────────────
ANEMIA_THRESHOLDS <- list(
  enfant_6_59  = list(severe = 7.0, moderate = 10.0, mild = 11.0,
                      label = "Enfants 6-59 mois"),
  femme_np     = list(severe = 8.0, moderate = 11.0, mild = 12.0,
                      label = "Femmes non enceintes"),
  femme_enc    = list(severe = 7.0, moderate = 10.0, mild = 11.0,
                      label = "Femmes enceintes"),
  homme        = list(severe = 8.0, moderate = 11.0, mild = 13.0,
                      label = "Hommes adultes")
)

# Sévérité santé publique OMS
ANEMIA_SEVERITY_PUBLIC <- list(
  labels = c("Pas de problème (< 5%)", "Léger (5-19.9%)",
             "Modéré (20-39.9%)", "Sévère (≥ 40%)"),
  colors = c("#27ae60", "#f39c12", "#e67e22", "#e74c3c"),
  breaks = c(0, 5, 20, 40, 100)
)

# ── Table d'ajustement altitude OMS ──────────────────────────────────────────
altitude_adjustment_oms <- function(altitude_m) {
  # Table OMS officielle
  adj <- dplyr::case_when(
    altitude_m < 1000 ~ 0,
    altitude_m < 1500 ~ -0.2,
    altitude_m < 2000 ~ -0.5,
    altitude_m < 2500 ~ -0.8,
    altitude_m < 3000 ~ -1.3,
    altitude_m < 3500 ~ -1.9,
    altitude_m < 4000 ~ -2.7,
    altitude_m < 4500 ~ -3.5,
    TRUE              ~ -4.5
  )
  return(adj)
}

smoking_adjustment_oms <- function(smoker) {
  ifelse(!is.na(smoker) & tolower(smoker) %in% c("oui", "yes", "1", "true"), -0.3, 0)
}

# ── Calcul prévalence anémie ─────────────────────────────────────────────────
calc_anemia_prevalence <- function(d, col_hb, col_altitude = NULL,
                                    col_smoker = NULL, col_pregnant = NULL,
                                    population = "enfant_6_59") {
  tryCatch({
    hb <- as.numeric(d[[col_hb]])

    # Ajustement altitude
    adj_alt <- if (!is.null(col_altitude) && col_altitude %in% names(d))
      altitude_adjustment_oms(as.numeric(d[[col_altitude]])) else 0

    # Ajustement tabac
    adj_smoke <- if (!is.null(col_smoker) && col_smoker %in% names(d))
      smoking_adjustment_oms(d[[col_smoker]]) else 0

    hb_adj <- hb + adj_alt + adj_smoke

    thresholds <- ANEMIA_THRESHOLDS[[population]]
    valid <- !is.na(hb_adj)

    classification <- dplyr::case_when(
      !valid ~ NA_character_,
      hb_adj < thresholds$severe   ~ "Sévère",
      hb_adj < thresholds$moderate ~ "Modérée",
      hb_adj < thresholds$mild     ~ "Légère",
      TRUE                         ~ "Pas d'anémie"
    )

    n_valid <- sum(valid)
    freq <- table(factor(classification[valid],
                         levels = c("Pas d'anémie", "Légère", "Modérée", "Sévère")))
    pct <- round(prop.table(freq) * 100, 1)

    total_anemia_pct <- round(sum(pct[c("Légère", "Modérée", "Sévère")]), 1)

    # Sévérité santé publique
    pub_severity <- dplyr::case_when(
      total_anemia_pct < 5   ~ "Pas de problème",
      total_anemia_pct < 20  ~ "Léger",
      total_anemia_pct < 40  ~ "Modéré",
      TRUE                   ~ "Sévère"
    )

    summary_df <- data.frame(
      Catégorie = names(freq),
      N = as.integer(freq),
      Pct = as.numeric(pct),
      Couleur = c("#27ae60", "#f39c12", "#e67e22", "#e74c3c"),
      stringsAsFactors = FALSE
    )

    list(
      summary       = summary_df,
      hb_adjusted   = hb_adj,
      classification = classification,
      n_valid       = n_valid,
      n_missing     = sum(!valid),
      mean_hb       = round(mean(hb_adj, na.rm = TRUE), 1),
      sd_hb         = round(sd(hb_adj, na.rm = TRUE), 1),
      median_hb     = round(median(hb_adj, na.rm = TRUE), 1),
      total_anemia  = total_anemia_pct,
      pub_severity  = pub_severity,
      population    = thresholds$label,
      thresholds    = thresholds
    )
  }, error = function(e) list(summary = data.frame(), error = conditionMessage(e)))
}

# ── Distribution Hb ──────────────────────────────────────────────────────────
plot_hb_distribution <- function(anemia_res) {
  tryCatch({
    hb <- anemia_res$hb_adjusted[!is.na(anemia_res$hb_adjusted)]
    if (length(hb) == 0) return(NULL)

    th <- anemia_res$thresholds
    df <- data.frame(hb = hb)

    ggplot2::ggplot(df, ggplot2::aes(x = hb)) +
      ggplot2::geom_histogram(binwidth = 0.5, fill = "#3498db", alpha = 0.7, color = "white") +
      ggplot2::geom_vline(xintercept = th$severe, color = "#e74c3c", linetype = "dashed", linewidth = 1) +
      ggplot2::geom_vline(xintercept = th$moderate, color = "#e67e22", linetype = "dashed", linewidth = 1) +
      ggplot2::geom_vline(xintercept = th$mild, color = "#f39c12", linetype = "dashed", linewidth = 1) +
      ggplot2::annotate("text", x = th$severe - 0.3, y = Inf, label = "Sévère",
                        color = "#e74c3c", vjust = 2, size = 3, fontface = "bold") +
      ggplot2::annotate("text", x = th$moderate - 0.3, y = Inf, label = "Modérée",
                        color = "#e67e22", vjust = 2, size = 3) +
      ggplot2::annotate("text", x = th$mild - 0.3, y = Inf, label = "Légère",
                        color = "#f39c12", vjust = 2, size = 3) +
      ggplot2::labs(title = paste("Distribution de l'hémoglobine ajustée —", anemia_res$population),
                    x = "Hémoglobine (g/dL)", y = "Effectif",
                    subtitle = paste0("Moyenne: ", anemia_res$mean_hb, " g/dL | ",
                                      "Anémie totale: ", anemia_res$total_anemia, "%")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }, error = function(e) NULL)
}

# ── Donut anémie ─────────────────────────────────────────────────────────────
plot_anemia_donut <- function(anemia_res) {
  tryCatch({
    df <- anemia_res$summary
    if (nrow(df) == 0) return(NULL)

    df$label <- paste0(df$Catégorie, "\n", df$Pct, "% (n=", df$N, ")")

    plotly::plot_ly(df, labels = ~Catégorie, values = ~N,
                    type = "pie", hole = 0.5,
                    marker = list(colors = df$Couleur),
                    textinfo = "label+percent",
                    hovertext = ~paste0(Catégorie, ": ", N, " (", Pct, "%)")) |>
      plotly::layout(title = list(text = paste("Classification anémie —", anemia_res$population)),
                     showlegend = TRUE)
  }, error = function(e) NULL)
}

# ── KPI Cards Anémie ────────────────────────────────────────────────────────
render_anemia_kpi <- function(anemia_res) {
  if (is.null(anemia_res) || is.null(anemia_res$summary) || nrow(anemia_res$summary) == 0) return("")

  pub_col <- switch(anemia_res$pub_severity,
    "Pas de problème" = "#27ae60", "Léger" = "#f39c12",
    "Modéré" = "#e67e22", "Sévère" = "#e74c3c", "#95a5a6")

  cards <- sprintf(
    '<div style="display:flex;flex-wrap:wrap;gap:10px;justify-content:center;">
     <div style="width:220px;padding:14px;background:#3498db;color:white;border-radius:10px;text-align:center;">
       <div style="font-size:12px;">Hb moyenne</div>
       <div style="font-size:30px;font-weight:bold;">%s g/dL</div>
       <div style="font-size:11px;">SD: %s | n=%d</div></div>
     <div style="width:220px;padding:14px;background:%s;color:white;border-radius:10px;text-align:center;">
       <div style="font-size:12px;">Anémie totale</div>
       <div style="font-size:30px;font-weight:bold;">%s%%</div>
       <div style="font-size:11px;">Sévérité SP: %s</div></div>
     <div style="width:220px;padding:14px;background:#e74c3c;color:white;border-radius:10px;text-align:center;">
       <div style="font-size:12px;">Anémie sévère</div>
       <div style="font-size:30px;font-weight:bold;">%s%%</div>
       <div style="font-size:11px;">Hb < %s g/dL</div></div>
    </div>',
    anemia_res$mean_hb, anemia_res$sd_hb, anemia_res$n_valid,
    pub_col, anemia_res$total_anemia, anemia_res$pub_severity,
    anemia_res$summary$Pct[anemia_res$summary$Catégorie == "Sévère"],
    anemia_res$thresholds$severe
  )
  cards
}

cat("module_anemie.R chargé — Hb · Prévalence · Ajustements OMS · Classification\n")
