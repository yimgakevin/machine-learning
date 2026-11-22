# ══════════════════════════════════════════════════════════════════════════════
# module_wash.R — WASH (Eau, Hygiène, Assainissement)
# Standards : JMP OMS/UNICEF, Sphère, WASH Cluster
# ══════════════════════════════════════════════════════════════════════════════

# ── Classifications JMP ──────────────────────────────────────────────────────
WATER_IMPROVED <- c("forage/pompe", "forage", "pompe", "puits protégé",
                    "puits protege", "robinet/réseau", "robinet", "réseau",
                    "source protégée", "source protegee", "eau courante",
                    "borehole", "protected well", "piped", "tap water",
                    "protected spring", "camion-citerne", "camion citerne")

SANITATION_IMPROVED <- c("latrine améliorée ventilée", "latrine amelioree ventilee",
                         "latrine à chasse", "latrine a chasse", "toilette à fosse",
                         "toilette a fosse", "wc", "flush toilet", "improved latrine",
                         "ventilated improved pit", "vip", "pour-flush")

# ── Calcul indicateurs WASH ──────────────────────────────────────────────────
calc_wash_indicators <- function(d, col_water_source = NULL, col_water_time = NULL,
                                  col_water_qty = NULL, col_treatment = NULL,
                                  col_latrine = NULL, col_shared = NULL,
                                  col_oda = NULL, col_handwash = NULL,
                                  col_soap = NULL, col_hw_soap = NULL) {
  tryCatch({
    results <- list()

    # ── EAU ──
    if (!is.null(col_water_source) && col_water_source %in% names(d)) {
      ws <- tolower(d[[col_water_source]])
      valid <- !is.na(ws)
      improved <- ws[valid] %in% WATER_IMPROVED
      results$water_improved <- list(
        pct = round(mean(improved) * 100, 1), n = sum(valid), n_pos = sum(improved),
        ci = binom_ci_wash(sum(improved), sum(valid))
      )
      # Fréquences par source
      freq <- as.data.frame(table(d[[col_water_source]][valid]), stringsAsFactors = FALSE)
      names(freq) <- c("Source", "N")
      freq <- freq |> dplyr::arrange(dplyr::desc(N)) |>
        dplyr::mutate(Pct = round(N / sum(N) * 100, 1))
      results$water_sources <- freq
    }

    if (!is.null(col_water_time) && col_water_time %in% names(d)) {
      wt <- as.numeric(d[[col_water_time]])
      valid <- !is.na(wt)
      results$water_time <- list(
        mean = round(mean(wt[valid]), 1),
        median = round(median(wt[valid])),
        adequate = round(mean(wt[valid] <= 30) * 100, 1),  # ≤ 30 min
        n = sum(valid)
      )
    }

    if (!is.null(col_water_qty) && col_water_qty %in% names(d)) {
      wq <- as.numeric(d[[col_water_qty]])
      valid <- !is.na(wq)
      results$water_quantity <- list(
        mean = round(mean(wq[valid]), 1),
        median = round(median(wq[valid]), 1),
        adequate = round(mean(wq[valid] >= 15) * 100, 1),  # ≥ 15 L/p/j (Sphère)
        n = sum(valid)
      )
    }

    if (!is.null(col_treatment) && col_treatment %in% names(d)) {
      tr <- d[[col_treatment]]
      valid <- !is.na(tr) & tr != ""
      treats <- tolower(tr[valid])
      no_treat <- treats %in% c("aucun traitement", "aucun", "none", "no treatment", "rien")
      results$water_treatment <- list(
        pct_treated = round(mean(!no_treat) * 100, 1),
        n = sum(valid)
      )
      freq <- as.data.frame(table(d[[col_treatment]][valid]), stringsAsFactors = FALSE)
      names(freq) <- c("Méthode", "N")
      freq <- freq |> dplyr::arrange(dplyr::desc(N)) |>
        dplyr::mutate(Pct = round(N / sum(N) * 100, 1))
      results$treatment_methods <- freq
    }

    # ── ASSAINISSEMENT ──
    if (!is.null(col_latrine) && col_latrine %in% names(d)) {
      lat <- tolower(d[[col_latrine]])
      valid <- !is.na(lat)
      improved <- lat[valid] %in% SANITATION_IMPROVED
      results$sanitation_improved <- list(
        pct = round(mean(improved) * 100, 1), n = sum(valid), n_pos = sum(improved),
        ci = binom_ci_wash(sum(improved), sum(valid))
      )
    }

    if (!is.null(col_oda) && col_oda %in% names(d)) {
      oda <- tolower(d[[col_oda]])
      valid <- !is.na(oda)
      oda_yes <- oda[valid] %in% c("oui", "yes", "1", "true")
      results$open_defecation <- list(
        pct = round(mean(oda_yes) * 100, 1), n = sum(valid), n_pos = sum(oda_yes)
      )
    }

    # ── HYGIÈNE ──
    if (!is.null(col_handwash) && col_handwash %in% names(d)) {
      hw <- tolower(d[[col_handwash]])
      valid <- !is.na(hw)
      hw_yes <- hw[valid] %in% c("oui", "yes", "1", "true")
      results$handwashing <- list(
        pct = round(mean(hw_yes) * 100, 1), n = sum(valid), n_pos = sum(hw_yes)
      )
    }

    if (!is.null(col_soap) && col_soap %in% names(d)) {
      sp <- tolower(d[[col_soap]])
      valid <- !is.na(sp)
      sp_yes <- sp[valid] %in% c("oui", "yes", "1", "true")
      results$soap <- list(
        pct = round(mean(sp_yes) * 100, 1), n = sum(valid), n_pos = sum(sp_yes)
      )
    }

    if (!is.null(col_hw_soap) && col_hw_soap %in% names(d)) {
      hws <- tolower(d[[col_hw_soap]])
      valid <- !is.na(hws)
      hws_yes <- hws[valid] %in% c("oui", "yes", "1", "true")
      results$handwash_soap <- list(
        pct = round(mean(hws_yes) * 100, 1), n = sum(valid), n_pos = sum(hws_yes)
      )
    }

    # ── Échelle JMP résumé ──
    jmp_ladder <- data.frame(
      Domaine = c("Eau améliorée", "Eau < 30 min", "Eau ≥ 15 L/p/j",
                  "Assainissement amélioré", "Pas de DAL",
                  "Dispositif lavage", "Savon disponible", "Lavage + savon"),
      Pct = c(
        results$water_improved$pct %||% NA,
        results$water_time$adequate %||% NA,
        results$water_quantity$adequate %||% NA,
        results$sanitation_improved$pct %||% NA,
        if (!is.null(results$open_defecation)) 100 - results$open_defecation$pct else NA,
        results$handwashing$pct %||% NA,
        results$soap$pct %||% NA,
        results$handwash_soap$pct %||% NA
      ),
      Seuil = c(rep("Sphère / JMP", 5), rep("OMS", 3)),
      stringsAsFactors = FALSE
    )

    list(results = results, jmp_ladder = jmp_ladder)
  }, error = function(e) list(results = list(), jmp_ladder = data.frame(), error = conditionMessage(e)))
}

# ── Helper IC binomial ───────────────────────────────────────────────────────
binom_ci_wash <- function(x, n, level = 0.95) {
  if (n == 0) return(c(NA, NA))
  p <- x / n; z <- qnorm(1 - (1 - level) / 2)
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  c(lower = round(max(0, center - margin) * 100, 1),
    upper = round(min(1, center + margin) * 100, 1))
}

# ── Échelle JMP (plot) ───────────────────────────────────────────────────────
plot_wash_ladder <- function(wash_res) {
  tryCatch({
    df <- wash_res$jmp_ladder |> dplyr::filter(!is.na(Pct))
    if (nrow(df) == 0) return(NULL)

    df$Domaine <- factor(df$Domaine, levels = rev(df$Domaine))
    df$color <- ifelse(df$Pct >= 80, "#27ae60",
                       ifelse(df$Pct >= 50, "#f39c12", "#e74c3c"))

    ggplot2::ggplot(df, ggplot2::aes(x = Domaine, y = Pct, fill = color)) +
      ggplot2::geom_bar(stat = "identity", width = 0.6, alpha = 0.85) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Pct, "%")),
                         hjust = -0.1, size = 3.5, fontface = "bold") +
      ggplot2::scale_fill_identity() +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 110)) +
      ggplot2::geom_hline(yintercept = 80, linetype = "dashed", color = "#95a5a6") +
      ggplot2::labs(title = "Échelle WASH — Indicateurs JMP / Sphère",
                    x = "", y = "%",
                    caption = "Ligne pointillée = seuil 80% (Sphère)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }, error = function(e) NULL)
}

# ── KPI WASH ─────────────────────────────────────────────────────────────────
render_wash_kpi <- function(wash_res) {
  if (is.null(wash_res) || length(wash_res$results) == 0) return("")

  make <- function(label, pct, n) {
    if (is.null(pct) || is.na(pct)) return("")
    col <- if (pct >= 80) "#27ae60" else if (pct >= 50) "#f39c12" else "#e74c3c"
    sprintf('<div style="display:inline-block;width:200px;margin:6px;padding:12px;
     background:%s;color:white;border-radius:10px;text-align:center;">
     <div style="font-size:11px;">%s</div>
     <div style="font-size:28px;font-weight:bold;">%s%%</div>
     <div style="font-size:10px;">n=%s</div></div>', col, label, pct, n %||% "—")
  }

  r <- wash_res$results
  cards <- c(
    make("Eau améliorée", r$water_improved$pct, r$water_improved$n),
    make("Eau ≤ 30 min", r$water_time$adequate, r$water_time$n),
    make("Eau ≥ 15 L/p/j", r$water_quantity$adequate, r$water_quantity$n),
    make("Assainissement amélioré", r$sanitation_improved$pct, r$sanitation_improved$n),
    make("Lavage mains + savon", r$handwash_soap$pct, r$handwash_soap$n),
    make("DAL", r$open_defecation$pct, r$open_defecation$n)
  )
  paste(cards[cards != ""], collapse = "\n")
}

cat("module_wash.R chargé — Eau · Assainissement · Hygiène · JMP · Sphère\n")
