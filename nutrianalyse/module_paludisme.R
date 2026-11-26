# ══════════════════════════════════════════════════════════════════════════════
# module_paludisme.R — Paludisme & Moustiquaires SENS
# Standards : Roll Back Malaria, SENS UNHCR, OMS
# ══════════════════════════════════════════════════════════════════════════════

# ── Indicateurs LLIN (moustiquaires) ────────────────────────────────────────
calc_llin_indicators <- function(d, col_possession = NULL, col_use = NULL,
                                  col_access = NULL, col_nb_nets = NULL,
                                  col_hh_size = NULL) {
  tryCatch({
    results <- list()

    # Possession
    if (!is.null(col_possession) && col_possession %in% names(d)) {
      v <- tolower(d[[col_possession]])
      valid <- !is.na(v)
      pos <- v[valid] %in% c("oui", "yes", "1", "true")
      results$possession <- list(
        pct = round(mean(pos) * 100, 1), n = sum(valid), n_pos = sum(pos),
        ci = binom_ci(sum(pos), sum(valid))
      )
    }
    # Utilisation
    if (!is.null(col_use) && col_use %in% names(d)) {
      v <- tolower(d[[col_use]])
      valid <- !is.na(v)
      pos <- v[valid] %in% c("oui", "yes", "1", "true")
      results$utilisation <- list(
        pct = round(mean(pos) * 100, 1), n = sum(valid), n_pos = sum(pos),
        ci = binom_ci(sum(pos), sum(valid))
      )
    }
    # Accès
    if (!is.null(col_access) && col_access %in% names(d)) {
      v <- tolower(d[[col_access]])
      valid <- !is.na(v)
      pos <- v[valid] %in% c("oui", "yes", "1", "true")
      results$acces <- list(
        pct = round(mean(pos) * 100, 1), n = sum(valid), n_pos = sum(pos),
        ci = binom_ci(sum(pos), sum(valid))
      )
    }
    # Ratio moustiquaires / personnes
    if (!is.null(col_nb_nets) && !is.null(col_hh_size) &&
        col_nb_nets %in% names(d) && col_hh_size %in% names(d)) {
      nets <- as.numeric(d[[col_nb_nets]])
      hh   <- as.numeric(d[[col_hh_size]])
      valid <- !is.na(nets) & !is.na(hh) & hh > 0
      ratio <- nets[valid] / hh[valid]
      results$ratio_nets_person <- list(
        mean = round(mean(ratio), 2),
        median = round(median(ratio), 2),
        adequate = round(mean(ratio >= 0.5) * 100, 1)  # ≥ 1 LLIN / 2 personnes
      )
    }

    # Cascade summary
    cascade <- data.frame(
      Indicateur = c("Possession", "Accès", "Utilisation"),
      Pct = c(
        results$possession$pct %||% NA,
        results$acces$pct %||% NA,
        results$utilisation$pct %||% NA
      ),
      N = c(
        results$possession$n %||% NA,
        results$acces$n %||% NA,
        results$utilisation$n %||% NA
      ),
      Couleur = c("#3498db", "#2ecc71", "#e74c3c"),
      stringsAsFactors = FALSE
    )

    list(results = results, cascade = cascade)
  }, error = function(e) list(results = list(), cascade = data.frame(), error = conditionMessage(e)))
}

# ── Prévalence paludisme (TDR) ───────────────────────────────────────────────
calc_malaria_prevalence <- function(d, col_tdr, col_fever = NULL, col_group = NULL) {
  tryCatch({
    tdr <- tolower(d[[col_tdr]])
    valid <- !is.na(tdr) & tdr != ""
    positive <- tdr[valid] %in% c("positif", "positive", "pos", "1", "oui", "yes")
    n_valid <- sum(valid)
    n_pos   <- sum(positive)
    pct     <- round(n_pos / n_valid * 100, 1)
    ci      <- binom_ci(n_pos, n_valid)

    # Par fièvre si disponible
    by_fever <- NULL
    if (!is.null(col_fever) && col_fever %in% names(d)) {
      fever <- tolower(d[[col_fever]])
      df_temp <- data.frame(tdr = tdr, fever = fever, stringsAsFactors = FALSE) |>
        dplyr::filter(!is.na(tdr) & tdr != "") |>
        dplyr::mutate(
          tdr_pos = tdr %in% c("positif", "positive", "pos", "1", "oui", "yes"),
          fever_yes = fever %in% c("oui", "yes", "1", "true")
        )
      by_fever <- df_temp |>
        dplyr::group_by(Fièvre = ifelse(fever_yes, "Avec fièvre", "Sans fièvre")) |>
        dplyr::summarise(N = dplyr::n(), Positifs = sum(tdr_pos),
                         Prévalence = round(mean(tdr_pos) * 100, 1), .groups = "drop")
    }

    # Par groupe si disponible
    by_group <- NULL
    if (!is.null(col_group) && col_group %in% names(d)) {
      df_temp <- data.frame(tdr = tdr, grp = d[[col_group]], stringsAsFactors = FALSE) |>
        dplyr::filter(!is.na(tdr) & tdr != "" & !is.na(grp))
      by_group <- df_temp |>
        dplyr::mutate(tdr_pos = tdr %in% c("positif", "positive", "pos", "1", "oui", "yes")) |>
        dplyr::group_by(Groupe = grp) |>
        dplyr::summarise(N = dplyr::n(), Positifs = sum(tdr_pos),
                         Prévalence = round(mean(tdr_pos) * 100, 1), .groups = "drop")
    }

    list(pct = pct, ci_lower = ci[1], ci_upper = ci[2],
         n_valid = n_valid, n_positive = n_pos,
         by_fever = by_fever, by_group = by_group)
  }, error = function(e) list(pct = NA, error = conditionMessage(e)))
}

# ── Helper: IC binomial Wilson ───────────────────────────────────────────────
binom_ci <- function(x, n, level = 0.95) {
  if (n == 0) return(c(NA, NA))
  p <- x / n
  z <- qnorm(1 - (1 - level) / 2)
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  c(lower = round(max(0, center - margin) * 100, 1),
    upper = round(min(1, center + margin) * 100, 1))
}

# ── Cascade LLIN (plot) ─────────────────────────────────────────────────────
plot_llin_cascade <- function(llin_res) {
  tryCatch({
    df <- llin_res$cascade |> dplyr::filter(!is.na(Pct))
    if (nrow(df) == 0) return(NULL)

    df$Indicateur <- factor(df$Indicateur, levels = c("Possession", "Accès", "Utilisation"))

    ggplot2::ggplot(df, ggplot2::aes(x = Indicateur, y = Pct, fill = Indicateur)) +
      ggplot2::geom_bar(stat = "identity", width = 0.6, alpha = 0.85) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Pct, "%")),
                         vjust = -0.5, fontface = "bold", size = 5) +
      ggplot2::scale_fill_manual(values = c("#3498db", "#2ecc71", "#e74c3c")) +
      ggplot2::scale_y_continuous(limits = c(0, 105)) +
      ggplot2::labs(title = "Cascade LLIN — Possession → Accès → Utilisation",
                    x = "", y = "%") +
      ggplot2::theme_minimal() +
      ggplot2::guides(fill = "none") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }, error = function(e) NULL)
}

# ── KPI Cards Paludisme ────────────────────────────────────────────────────
render_malaria_kpi <- function(malaria_res = NULL, llin_res = NULL) {
  cards <- c()

  if (!is.null(malaria_res) && !is.na(malaria_res$pct)) {
    col <- if (malaria_res$pct < 10) "#27ae60" else if (malaria_res$pct < 30) "#f39c12" else "#e74c3c"
    cards <- c(cards, sprintf(
      '<div style="display:inline-block;width:240px;margin:8px;padding:14px;
       background:%s;color:white;border-radius:10px;text-align:center;">
       <div style="font-size:12px;">Prévalence paludisme (TDR+)</div>
       <div style="font-size:30px;font-weight:bold;">%s%%</div>
       <div style="font-size:11px;">IC: [%s — %s] · n=%d</div></div>',
      col, malaria_res$pct, malaria_res$ci_lower, malaria_res$ci_upper, malaria_res$n_valid
    ))
  }

  if (!is.null(llin_res) && length(llin_res$results) > 0) {
    for (ind in c("possession", "utilisation", "acces")) {
      r <- llin_res$results[[ind]]
      if (!is.null(r)) {
        label <- switch(ind, possession = "LLIN Possession", utilisation = "LLIN Utilisation", acces = "LLIN Accès")
        col <- if (r$pct >= 80) "#27ae60" else if (r$pct >= 50) "#f39c12" else "#e74c3c"
        cards <- c(cards, sprintf(
          '<div style="display:inline-block;width:200px;margin:8px;padding:14px;
           background:%s;color:white;border-radius:10px;text-align:center;">
           <div style="font-size:12px;">%s</div>
           <div style="font-size:28px;font-weight:bold;">%s%%</div>
           <div style="font-size:11px;">n=%d</div></div>',
          col, label, r$pct, r$n
        ))
      }
    }
  }

  paste(cards, collapse = "\n")
}

cat("module_paludisme.R chargé — LLIN · TDR · Cascade · Prévalence\n")
