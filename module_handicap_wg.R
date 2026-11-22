# ══════════════════════════════════════════════════════════════════════════════
# module_handicap_wg.R — Handicap (Washington Group Short Set)
# Standards : Washington Group on Disability Statistics
# ══════════════════════════════════════════════════════════════════════════════

# ── Domaines WG ──────────────────────────────────────────────────────────────
WG_DOMAINS <- list(
  voir       = list(label = "Vision",        color = "#3498db", icon = "👁️"),
  entendre   = list(label = "Audition",      color = "#9b59b6", icon = "👂"),
  marcher    = list(label = "Mobilité",      color = "#e67e22", icon = "🦿"),
  memoire    = list(label = "Cognition",     color = "#1abc9c", icon = "🧠"),
  soins_personnels = list(label = "Soins personnels", color = "#e74c3c", icon = "🤲"),
  communication    = list(label = "Communication",    color = "#f39c12", icon = "💬")
)

WG_SEVERITY <- list(
  levels = c(1, 2, 3, 4),
  labels = c("Aucune difficulté", "Quelques difficultés",
             "Beaucoup de difficultés", "Impossible"),
  colors = c("#27ae60", "#f39c12", "#e67e22", "#e74c3c")
)

# ── Prévalence handicap ─────────────────────────────────────────────────────
calc_wg_prevalence <- function(d, col_voir = NULL, col_entendre = NULL,
                                col_marcher = NULL, col_memoire = NULL,
                                col_soins = NULL, col_comm = NULL,
                                threshold = 3) {
  tryCatch({
    cols <- list(
      voir = col_voir, entendre = col_entendre, marcher = col_marcher,
      memoire = col_memoire, soins_personnels = col_soins, communication = col_comm
    )

    # Filtrer colonnes disponibles
    available <- cols[!sapply(cols, is.null)]
    available <- available[sapply(available, function(c) c %in% names(d))]

    if (length(available) == 0) return(list(error = "Aucune colonne WG disponible"))

    # Convertir en numérique
    wg_matrix <- do.call(cbind, lapply(available, function(col) as.numeric(d[[col]])))
    colnames(wg_matrix) <- names(available)

    n_total <- nrow(wg_matrix)

    # Handicap = au moins 1 domaine ≥ threshold (défaut: 3 = "beaucoup de difficultés")
    max_score <- apply(wg_matrix, 1, max, na.rm = TRUE)
    max_score[is.infinite(max_score)] <- NA

    valid <- !is.na(max_score)
    disabled <- max_score[valid] >= threshold

    prevalence <- round(mean(disabled) * 100, 1)
    ci <- binom_ci_wg(sum(disabled), sum(valid))

    # Par domaine
    domain_results <- lapply(names(available), function(dom) {
      v <- wg_matrix[, dom]
      valid_d <- !is.na(v)
      n_d <- sum(valid_d)

      # Distribution sévérité
      sev_freq <- table(factor(v[valid_d], levels = 1:4))
      sev_pct  <- round(prop.table(sev_freq) * 100, 1)

      # % avec difficulté ≥ threshold
      pct_disabled <- round(mean(v[valid_d] >= threshold) * 100, 1)

      list(
        domain = dom,
        label = WG_DOMAINS[[dom]]$label,
        color = WG_DOMAINS[[dom]]$color,
        n = n_d,
        pct_disabled = pct_disabled,
        severity = data.frame(
          Niveau = WG_SEVERITY$labels,
          N = as.integer(sev_freq),
          Pct = as.numeric(sev_pct),
          stringsAsFactors = FALSE
        )
      )
    })
    names(domain_results) <- names(available)

    # Résumé par domaine
    domain_summary <- do.call(rbind, lapply(domain_results, function(dr) {
      data.frame(
        Domaine = dr$label, `Difficulté_sévère` = dr$pct_disabled,
        N = dr$n, Couleur = dr$color, stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))

    list(
      prevalence = prevalence,
      ci_lower = ci[1], ci_upper = ci[2],
      n_valid = sum(valid), n_disabled = sum(disabled),
      threshold_label = WG_SEVERITY$labels[threshold],
      domain_results = domain_results,
      domain_summary = domain_summary,
      n_domains = length(available)
    )
  }, error = function(e) list(prevalence = NA, error = conditionMessage(e)))
}

# ── Helper IC binomial ───────────────────────────────────────────────────────
binom_ci_wg <- function(x, n, level = 0.95) {
  if (n == 0) return(c(NA, NA))
  p <- x / n; z <- qnorm(1 - (1 - level) / 2)
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  c(lower = round(max(0, center - margin) * 100, 1),
    upper = round(min(1, center + margin) * 100, 1))
}

# ── Radar des 6 domaines ────────────────────────────────────────────────────
plot_wg_radar <- function(wg_res) {
  tryCatch({
    df <- wg_res$domain_summary
    if (is.null(df) || nrow(df) == 0) return(NULL)

    # Utiliser un bar chart horizontal comme alternative au radar
    df$Domaine <- factor(df$Domaine, levels = rev(df$Domaine))

    ggplot2::ggplot(df, ggplot2::aes(x = Domaine, y = `Difficulté_sévère`, fill = Couleur)) +
      ggplot2::geom_bar(stat = "identity", width = 0.6, alpha = 0.85) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(`Difficulté_sévère`, "%")),
                         hjust = -0.1, fontface = "bold", size = 4) +
      ggplot2::scale_fill_identity() +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, max(df$`Difficulté_sévère` + 10, na.rm = TRUE))) +
      ggplot2::labs(title = "Prévalence du handicap par domaine fonctionnel",
                    subtitle = paste0("Seuil: ", wg_res$threshold_label),
                    x = "", y = "% avec difficulté sévère") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }, error = function(e) NULL)
}

# ── Distribution sévérité ───────────────────────────────────────────────────
plot_wg_severity <- function(wg_res) {
  tryCatch({
    dfs <- lapply(wg_res$domain_results, function(dr) {
      dr$severity$Domaine <- dr$label
      dr$severity
    })
    df <- do.call(rbind, dfs)
    if (nrow(df) == 0) return(NULL)

    df$Niveau <- factor(df$Niveau, levels = WG_SEVERITY$labels)

    ggplot2::ggplot(df, ggplot2::aes(x = Domaine, y = Pct, fill = Niveau)) +
      ggplot2::geom_bar(stat = "identity", position = "stack", alpha = 0.85) +
      ggplot2::scale_fill_manual(values = WG_SEVERITY$colors) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Distribution de la sévérité par domaine (Washington Group)",
                    x = "", y = "%", fill = "Niveau") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }, error = function(e) NULL)
}

# ── KPI Cards WG ────────────────────────────────────────────────────────────
render_wg_kpi <- function(wg_res) {
  if (is.null(wg_res) || is.na(wg_res$prevalence)) return("")

  col <- if (wg_res$prevalence < 10) "#27ae60" else
    if (wg_res$prevalence < 20) "#f39c12" else "#e74c3c"

  sprintf(
    '<div style="display:flex;flex-wrap:wrap;gap:10px;justify-content:center;">
     <div style="width:280px;padding:16px;background:%s;color:white;border-radius:12px;text-align:center;">
       <div style="font-size:13px;">Prévalence du handicap</div>
       <div style="font-size:36px;font-weight:bold;">%s%%</div>
       <div style="font-size:12px;">IC 95%%: [%s — %s]</div>
       <div style="font-size:11px;margin-top:4px;">n=%d · %d domaines · Seuil: %s</div>
     </div>
     <div style="width:280px;padding:16px;background:#34495e;color:white;border-radius:12px;text-align:center;">
       <div style="font-size:13px;">Personnes avec handicap</div>
       <div style="font-size:36px;font-weight:bold;">%d</div>
       <div style="font-size:11px;">sur %d évaluées</div>
     </div>
    </div>',
    col, wg_res$prevalence, wg_res$ci_lower, wg_res$ci_upper,
    wg_res$n_valid, wg_res$n_domains, wg_res$threshold_label,
    wg_res$n_disabled, wg_res$n_valid
  )
}

cat("module_handicap_wg.R chargé — 6 domaines · Prévalence · Sévérité · Radar\n")
