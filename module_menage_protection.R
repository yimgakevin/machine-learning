# ══════════════════════════════════════════════════════════════════════════════
# module_menage_protection.R — Ménage & Protection SENS/UNHCR
# Standards : SENS UNHCR, IPC, Sphère
# ══════════════════════════════════════════════════════════════════════════════

# ── Profil ménage ────────────────────────────────────────────────────────────
calc_household_profile <- function(d, col_hh_size = NULL, col_u5 = NULL,
                                    col_5_17 = NULL, col_adults = NULL,
                                    col_head_sex = NULL, col_head_age = NULL,
                                    col_education = NULL) {
  tryCatch({
    results <- list()

    if (!is.null(col_hh_size) && col_hh_size %in% names(d)) {
      hs <- as.numeric(d[[col_hh_size]])
      valid <- !is.na(hs)
      results$hh_size <- list(
        mean = round(mean(hs[valid]), 1),
        median = round(median(hs[valid])),
        sd = round(sd(hs[valid]), 1),
        n = sum(valid)
      )
    }

    if (!is.null(col_head_sex) && col_head_sex %in% names(d)) {
      hs <- d[[col_head_sex]]
      valid <- !is.na(hs)
      fem <- tolower(hs[valid]) %in% c("f", "femme", "female", "féminin", "feminin")
      results$female_headed <- list(pct = round(mean(fem) * 100, 1), n = sum(valid))
    }

    if (!is.null(col_head_age) && col_head_age %in% names(d)) {
      ha <- as.numeric(d[[col_head_age]])
      valid <- !is.na(ha)
      results$head_age <- list(
        mean = round(mean(ha[valid]), 1),
        elderly = round(mean(ha[valid] >= 60) * 100, 1),
        n = sum(valid)
      )
    }

    if (!is.null(col_education) && col_education %in% names(d)) {
      ed <- d[[col_education]]
      valid <- !is.na(ed) & ed != ""
      freq <- as.data.frame(table(ed[valid]), stringsAsFactors = FALSE)
      names(freq) <- c("Niveau", "N")
      freq <- freq |> dplyr::arrange(dplyr::desc(N)) |>
        dplyr::mutate(Pct = round(N / sum(N) * 100, 1))
      results$education <- freq
    }

    # Composition moyenne
    composition <- data.frame(
      Catégorie = c("Enfants < 5 ans", "Enfants 5-17 ans", "Adultes 18+"),
      Moyenne = c(
        if (!is.null(col_u5) && col_u5 %in% names(d)) round(mean(as.numeric(d[[col_u5]]), na.rm = TRUE), 1) else NA,
        if (!is.null(col_5_17) && col_5_17 %in% names(d)) round(mean(as.numeric(d[[col_5_17]]), na.rm = TRUE), 1) else NA,
        if (!is.null(col_adults) && col_adults %in% names(d)) round(mean(as.numeric(d[[col_adults]]), na.rm = TRUE), 1) else NA
      ),
      stringsAsFactors = FALSE
    )

    list(results = results, composition = composition)
  }, error = function(e) list(results = list(), error = conditionMessage(e)))
}

# ── Économie ménage ──────────────────────────────────────────────────────────
calc_household_economy <- function(d, col_income = NULL, col_income_source = NULL,
                                    col_expenditure = NULL, col_food_exp = NULL,
                                    col_livelihood = NULL) {
  tryCatch({
    results <- list()

    if (!is.null(col_income) && col_income %in% names(d)) {
      inc <- as.numeric(d[[col_income]])
      valid <- !is.na(inc)
      results$income <- list(
        mean = round(mean(inc[valid])),
        median = round(median(inc[valid])),
        n = sum(valid)
      )
    }

    if (!is.null(col_expenditure) && col_expenditure %in% names(d)) {
      exp <- as.numeric(d[[col_expenditure]])
      valid <- !is.na(exp)
      results$expenditure <- list(
        mean = round(mean(exp[valid])),
        median = round(median(exp[valid])),
        n = sum(valid)
      )
    }

    if (!is.null(col_food_exp) && !is.null(col_expenditure) &&
        col_food_exp %in% names(d) && col_expenditure %in% names(d)) {
      fe <- as.numeric(d[[col_food_exp]])
      te <- as.numeric(d[[col_expenditure]])
      valid <- !is.na(fe) & !is.na(te) & te > 0
      ratio <- fe[valid] / te[valid]
      results$food_share <- list(
        mean = round(mean(ratio) * 100, 1),
        median = round(median(ratio) * 100, 1),
        high_share = round(mean(ratio >= 0.65) * 100, 1),  # ≥ 65% = vulnérable
        n = sum(valid)
      )
    }

    # Sources revenus / moyens de subsistance
    for (info in list(list(col = col_income_source, name = "income_sources"),
                      list(col = col_livelihood, name = "livelihood"))) {
      if (!is.null(info$col) && info$col %in% names(d)) {
        v <- d[[info$col]]
        valid <- !is.na(v) & v != ""
        freq <- as.data.frame(table(v[valid]), stringsAsFactors = FALSE)
        names(freq) <- c("Catégorie", "N")
        freq <- freq |> dplyr::arrange(dplyr::desc(N)) |>
          dplyr::mutate(Pct = round(N / sum(N) * 100, 1))
        results[[info$name]] <- freq
      }
    }

    list(results = results)
  }, error = function(e) list(results = list(), error = conditionMessage(e)))
}

# ── Abri ─────────────────────────────────────────────────────────────────────
calc_shelter <- function(d, col_shelter = NULL, col_tenure = NULL) {
  tryCatch({
    results <- list()
    for (info in list(list(col = col_shelter, name = "shelter_types"),
                      list(col = col_tenure, name = "tenure_status"))) {
      if (!is.null(info$col) && info$col %in% names(d)) {
        v <- d[[info$col]]
        valid <- !is.na(v) & v != ""
        freq <- as.data.frame(table(v[valid]), stringsAsFactors = FALSE)
        names(freq) <- c("Type", "N")
        freq <- freq |> dplyr::arrange(dplyr::desc(N)) |>
          dplyr::mutate(Pct = round(N / sum(N) * 100, 1))
        results[[info$name]] <- freq
      }
    }
    list(results = results)
  }, error = function(e) list(results = list(), error = conditionMessage(e)))
}

# ── Protection ───────────────────────────────────────────────────────────────
calc_protection <- function(d, col_registration = NULL, col_documentation = NULL,
                             col_incident = NULL, col_child_labor = NULL,
                             col_early_marriage = NULL) {
  tryCatch({
    results <- list()
    indicators <- list(
      list(col = col_registration,   name = "enregistrement",    label = "Enregistrement HCR"),
      list(col = col_documentation,  name = "documentation",     label = "Documents d'identité"),
      list(col = col_incident,       name = "incident",          label = "Incident protection"),
      list(col = col_child_labor,    name = "travail_enfant",    label = "Travail des enfants"),
      list(col = col_early_marriage, name = "mariage_precoce",   label = "Mariage précoce")
    )

    summary_rows <- list()
    for (ind in indicators) {
      if (!is.null(ind$col) && ind$col %in% names(d)) {
        v <- tolower(d[[ind$col]])
        valid <- !is.na(v) & v != ""
        pos <- v[valid] %in% c("oui", "yes", "1", "true")
        pct <- round(mean(pos) * 100, 1)
        results[[ind$name]] <- list(pct = pct, n = sum(valid), n_pos = sum(pos))
        summary_rows <- c(summary_rows, list(data.frame(
          Indicateur = ind$label, Pct = pct, N = sum(valid),
          stringsAsFactors = FALSE
        )))
      }
    }

    summary_df <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
    list(results = results, summary = summary_df)
  }, error = function(e) list(results = list(), summary = data.frame(), error = conditionMessage(e)))
}

# ── Plots ────────────────────────────────────────────────────────────────────
plot_income_expenditure <- function(econ_res) {
  tryCatch({
    inc <- econ_res$results$income
    exp <- econ_res$results$expenditure
    if (is.null(inc) || is.null(exp)) return(NULL)

    df <- data.frame(
      Type = c("Revenus", "Dépenses totales",
               if (!is.null(econ_res$results$food_share)) "Dépenses alimentaires" else NULL),
      Montant = c(inc$mean, exp$mean,
                  if (!is.null(econ_res$results$food_share))
                    round(exp$mean * econ_res$results$food_share$mean / 100) else NULL)
    )

    ggplot2::ggplot(df, ggplot2::aes(x = reorder(Type, Montant), y = Montant, fill = Type)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5, alpha = 0.85) +
      ggplot2::geom_text(ggplot2::aes(label = scales::comma(Montant)),
                         hjust = -0.1, fontface = "bold") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("#27ae60", "#e74c3c", "#f39c12")) +
      ggplot2::labs(title = "Revenus et dépenses moyens du ménage",
                    x = "", y = "Montant moyen (FCFA)") +
      ggplot2::theme_minimal() +
      ggplot2::guides(fill = "none")
  }, error = function(e) NULL)
}

plot_protection_indicators <- function(prot_res) {
  tryCatch({
    df <- prot_res$summary
    if (nrow(df) == 0) return(NULL)

    df$Indicateur <- factor(df$Indicateur, levels = rev(df$Indicateur))
    df$color <- ifelse(df$Pct < 10, "#27ae60",
                       ifelse(df$Pct < 30, "#f39c12", "#e74c3c"))

    ggplot2::ggplot(df, ggplot2::aes(x = Indicateur, y = Pct, fill = color)) +
      ggplot2::geom_bar(stat = "identity", width = 0.6, alpha = 0.85) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Pct, "%")),
                         hjust = -0.1, fontface = "bold") +
      ggplot2::scale_fill_identity() +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, max(df$Pct + 15, na.rm = TRUE))) +
      ggplot2::labs(title = "Indicateurs de protection", x = "", y = "%") +
      ggplot2::theme_minimal()
  }, error = function(e) NULL)
}

# ── KPI Ménage ───────────────────────────────────────────────────────────────
render_household_kpi <- function(hh_res, econ_res = NULL) {
  cards <- c()
  r <- hh_res$results

  if (!is.null(r$hh_size)) {
    cards <- c(cards, sprintf(
      '<div style="display:inline-block;width:200px;margin:6px;padding:12px;
       background:#3498db;color:white;border-radius:10px;text-align:center;">
       <div style="font-size:11px;">Taille ménage</div>
       <div style="font-size:28px;font-weight:bold;">%s</div>
       <div style="font-size:10px;">Médiane: %s · n=%d</div></div>',
      r$hh_size$mean, r$hh_size$median, r$hh_size$n))
  }

  if (!is.null(r$female_headed)) {
    col <- if (r$female_headed$pct > 30) "#e74c3c" else "#f39c12"
    cards <- c(cards, sprintf(
      '<div style="display:inline-block;width:200px;margin:6px;padding:12px;
       background:%s;color:white;border-radius:10px;text-align:center;">
       <div style="font-size:11px;">Ménages dirigés par femme</div>
       <div style="font-size:28px;font-weight:bold;">%s%%</div>
       <div style="font-size:10px;">n=%d</div></div>',
      col, r$female_headed$pct, r$female_headed$n))
  }

  if (!is.null(econ_res) && !is.null(econ_res$results$food_share)) {
    fs <- econ_res$results$food_share
    col <- if (fs$mean >= 65) "#e74c3c" else if (fs$mean >= 50) "#f39c12" else "#27ae60"
    cards <- c(cards, sprintf(
      '<div style="display:inline-block;width:200px;margin:6px;padding:12px;
       background:%s;color:white;border-radius:10px;text-align:center;">
       <div style="font-size:11px;">Part dépenses alimentaires</div>
       <div style="font-size:28px;font-weight:bold;">%s%%</div>
       <div style="font-size:10px;">≥ 65%% = vulnérable</div></div>',
      col, fs$mean))
  }

  paste(cards, collapse = "\n")
}

cat("module_menage_protection.R chargé — Profil · Économie · Abri · Protection\n")
