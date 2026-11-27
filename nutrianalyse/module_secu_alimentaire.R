# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_secu_alimentaire.R — Indicateurs de Sécurité Alimentaire (PAM)    ║
# ║  FCS · rCSI · HDDS · HHS — Calcul, classification, graphiques            ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ══════════════════════════════════════════════════════════════════════════════
# 1. FCS — Food Consumption Score (Score de Consommation Alimentaire)
# ══════════════════════════════════════════════════════════════════════════════
#
# Le FCS mesure la diversité et la fréquence de la consommation alimentaire
# sur les 7 derniers jours. Chaque groupe alimentaire a un poids PAM.
#
# Groupes alimentaires PAM et poids :
#   Céréales/Tubercules : 2
#   Légumineuses        : 3
#   Légumes             : 1
#   Fruits              : 1
#   Viande/Poisson      : 4
#   Lait/Produits laitiers : 4
#   Sucre               : 0.5
#   Huile/Graisse       : 0.5
#   Condiments          : 0 (souvent exclu)
#
# Fréquence : 0-7 jours par semaine pour chaque groupe
# Score = somme(fréquence × poids)
# Seuils PAM : Pauvre <21 | Limite 21.5-35 | Acceptable >35

FCS_WEIGHTS <- list(
  cereales     = 2,
  legumineuses = 3,
  legumes      = 1,
  fruits       = 1,
  viande       = 4,
  lait         = 4,
  sucre        = 0.5,
  huile        = 0.5,
  condiments   = 0
)

FCS_THRESHOLDS <- list(
  pauvre   = 21,    # < 21
  limite  = 35     # 21-35 = limite, > 35 = acceptable
)

# Calcul du FCS
calc_fcs <- function(d, col_cereales = NULL, col_legumineuses = NULL,
                      col_legumes = NULL, col_fruits = NULL,
                      col_viande = NULL, col_lait = NULL,
                      col_sucre = NULL, col_huile = NULL,
                      col_condiments = NULL) {

  # Mapper les colonnes aux poids
  mapping <- list(
    list(col = col_cereales,     w = FCS_WEIGHTS$cereales,     nom = "Céréales/Tubercules"),
    list(col = col_legumineuses, w = FCS_WEIGHTS$legumineuses, nom = "Légumineuses"),
    list(col = col_legumes,      w = FCS_WEIGHTS$legumes,      nom = "Légumes"),
    list(col = col_fruits,       w = FCS_WEIGHTS$fruits,       nom = "Fruits"),
    list(col = col_viande,       w = FCS_WEIGHTS$viande,       nom = "Viande/Poisson"),
    list(col = col_lait,         w = FCS_WEIGHTS$lait,         nom = "Lait/Prod. laitiers"),
    list(col = col_sucre,        w = FCS_WEIGHTS$sucre,        nom = "Sucre"),
    list(col = col_huile,        w = FCS_WEIGHTS$huile,        nom = "Huile/Graisse"),
    list(col = col_condiments,   w = FCS_WEIGHTS$condiments,   nom = "Condiments")
  )

  # Filtrer les colonnes disponibles
  valid <- mapping[sapply(mapping, function(m) !is.null(m$col) && m$col %in% names(d))]
  if (length(valid) < 4) return(NULL)  # minimum 4 groupes

  # Calculer le score FCS par ligne
  fcs_score <- rep(0, nrow(d))
  detail <- list()

  for (m in valid) {
    vals <- as.numeric(d[[m$col]])
    vals[is.na(vals)] <- 0
    vals <- pmin(vals, 7)  # cap à 7 jours
    vals <- pmax(vals, 0)
    fcs_score <- fcs_score + vals * m$w
    detail[[m$nom]] <- list(col = m$col, poids = m$w,
                             moy = round(mean(vals, na.rm = TRUE), 2))
  }

  # Classification PAM
  fcs_class <- case_when(
    fcs_score < FCS_THRESHOLDS$pauvre ~ "Pauvre",
    fcs_score <= FCS_THRESHOLDS$limite ~ "Limite",
    TRUE ~ "Acceptable"
  )

  list(
    score     = fcs_score,
    class     = fcs_class,
    n_groups  = length(valid),
    summary   = tibble(
      Catégorie   = c("Pauvre (< 21)", "Limite (21-35)", "Acceptable (> 35)"),
      N           = c(sum(fcs_class == "Pauvre"), sum(fcs_class == "Limite"),
                      sum(fcs_class == "Acceptable")),
      `%`         = round(c(sum(fcs_class == "Pauvre"), sum(fcs_class == "Limite"),
                            sum(fcs_class == "Acceptable")) / length(fcs_class) * 100, 1),
      Couleur     = c("#e74c3c", "#f39c12", "#27ae60")
    ),
    detail    = detail,
    stats     = tibble(
      Statistique = c("Moyenne", "Médiane", "Écart-type", "Min", "Max"),
      Valeur = round(c(mean(fcs_score), median(fcs_score), sd(fcs_score),
                        min(fcs_score), max(fcs_score)), 2)
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. rCSI — reduced Coping Strategy Index
# ══════════════════════════════════════════════════════════════════════════════
#
# Le rCSI mesure les stratégies d'adaptation alimentaire des ménages.
# 5 stratégies standardisées avec poids de sévérité :
#   1. Manger des aliments moins préférés/appréciés : poids 1
#   2. Emprunter de la nourriture / aide de proches : poids 2
#   3. Réduire le nombre de repas par jour          : poids 1
#   4. Réduire les portions                         : poids 1
#   5. Réduire la consommation des adultes          : poids 3
#
# Fréquence : 0-7 jours dans les 7 derniers jours
# Score = somme(fréquence × poids de sévérité)
# Seuils (contextuels, mais courants) : Pas/Peu <4 | Moyen 4-18 | Élevé ≥19

RCSI_WEIGHTS <- list(
  moins_prefere    = 1,
  emprunter        = 2,
  reduire_repas    = 1,
  reduire_portions = 1,
  reduire_adultes  = 3
)

calc_rcsi <- function(d, col_moins_prefere = NULL, col_emprunter = NULL,
                       col_reduire_repas = NULL, col_reduire_portions = NULL,
                       col_reduire_adultes = NULL) {

  mapping <- list(
    list(col = col_moins_prefere,    w = RCSI_WEIGHTS$moins_prefere,
         nom = "Aliments moins préférés"),
    list(col = col_emprunter,        w = RCSI_WEIGHTS$emprunter,
         nom = "Emprunter nourriture"),
    list(col = col_reduire_repas,    w = RCSI_WEIGHTS$reduire_repas,
         nom = "Réduire nb repas"),
    list(col = col_reduire_portions, w = RCSI_WEIGHTS$reduire_portions,
         nom = "Réduire portions"),
    list(col = col_reduire_adultes,  w = RCSI_WEIGHTS$reduire_adultes,
         nom = "Réduire conso adultes")
  )

  valid <- mapping[sapply(mapping, function(m) !is.null(m$col) && m$col %in% names(d))]
  if (length(valid) < 3) return(NULL)

  rcsi_score <- rep(0, nrow(d))
  for (m in valid) {
    vals <- as.numeric(d[[m$col]])
    vals[is.na(vals)] <- 0
    vals <- pmin(vals, 7); vals <- pmax(vals, 0)
    rcsi_score <- rcsi_score + vals * m$w
  }

  rcsi_class <- case_when(
    rcsi_score < 4  ~ "Pas/Peu de coping",
    rcsi_score < 19 ~ "Coping moyen",
    TRUE            ~ "Coping élevé"
  )

  list(
    score   = rcsi_score,
    class   = rcsi_class,
    summary = tibble(
      Catégorie = c("Pas/Peu (< 4)", "Moyen (4-18)", "Élevé (≥ 19)"),
      N         = c(sum(rcsi_class == "Pas/Peu de coping"),
                    sum(rcsi_class == "Coping moyen"),
                    sum(rcsi_class == "Coping élevé")),
      `%`       = round(c(sum(rcsi_class == "Pas/Peu de coping"),
                          sum(rcsi_class == "Coping moyen"),
                          sum(rcsi_class == "Coping élevé")) / length(rcsi_class) * 100, 1),
      Couleur   = c("#27ae60", "#f39c12", "#e74c3c")
    ),
    stats = tibble(
      Statistique = c("Moyenne", "Médiane", "Écart-type", "Min", "Max"),
      Valeur = round(c(mean(rcsi_score), median(rcsi_score), sd(rcsi_score),
                        min(rcsi_score), max(rcsi_score)), 2)
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. HDDS — Household Dietary Diversity Score
# ══════════════════════════════════════════════════════════════════════════════
#
# Le HDDS compte le nombre de groupes alimentaires consommés dans les
# dernières 24 heures. 12 groupes FAO standard.
# Score = nombre de groupes consommés (0-12)
# Seuils courants : Faible ≤3 | Moyen 4-5 | Élevé ≥6

HDDS_GROUPS <- c(
  "Céréales", "Racines/Tubercules", "Légumes", "Fruits",
  "Viande", "Œufs", "Poisson/Fruits de mer", "Légumineuses/Noix",
  "Lait/Produits laitiers", "Huile/Graisse", "Sucre/Miel", "Condiments/Épices"
)

calc_hdds <- function(d, cols_groups) {
  # cols_groups : vecteur de noms de colonnes (binaires 0/1 ou fréquence)
  cols_valid <- cols_groups[cols_groups %in% names(d)]
  if (length(cols_valid) < 4) return(NULL)

  # Convertir en binaire (consommé = 1 si valeur > 0)
  consumed <- sapply(cols_valid, function(col) {
    vals <- as.numeric(d[[col]])
    as.integer(!is.na(vals) & vals > 0)
  })

  hdds_score <- rowSums(consumed, na.rm = TRUE)

  hdds_class <- case_when(
    hdds_score <= 3 ~ "Faible diversité",
    hdds_score <= 5 ~ "Diversité moyenne",
    TRUE            ~ "Bonne diversité"
  )

  list(
    score   = hdds_score,
    class   = hdds_class,
    n_groups_used = length(cols_valid),
    summary = tibble(
      Catégorie = c("Faible (≤ 3)", "Moyenne (4-5)", "Bonne (≥ 6)"),
      N         = c(sum(hdds_class == "Faible diversité"),
                    sum(hdds_class == "Diversité moyenne"),
                    sum(hdds_class == "Bonne diversité")),
      `%`       = round(c(sum(hdds_class == "Faible diversité"),
                          sum(hdds_class == "Diversité moyenne"),
                          sum(hdds_class == "Bonne diversité")) / length(hdds_class) * 100, 1),
      Couleur   = c("#e74c3c", "#f39c12", "#27ae60")
    ),
    # Fréquence de consommation par groupe
    group_freq = tibble(
      Groupe = cols_valid,
      `N consommé` = colSums(consumed, na.rm = TRUE),
      `% consommé` = round(colSums(consumed, na.rm = TRUE) / nrow(d) * 100, 1)
    ) |> arrange(desc(`% consommé`)),
    stats = tibble(
      Statistique = c("Moyenne", "Médiane", "Écart-type", "Min", "Max"),
      Valeur = round(c(mean(hdds_score), median(hdds_score), sd(hdds_score),
                        min(hdds_score), max(hdds_score)), 2)
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. HHS — Household Hunger Scale (Échelle de la Faim des Ménages)
# ══════════════════════════════════════════════════════════════════════════════
#
# Le HHS est basé sur 3 questions de fréquence (4 semaines) :
#   Q1 : Pas de nourriture dans la maison
#   Q2 : Aller dormir affamé
#   Q3 : Passer une journée entière sans manger
#
# Pour chaque question : 0=Jamais, 1=Rarement/Parfois, 2=Souvent
# Score HHS = somme des 3 recodages (0-6)
# Classification :
#   0-1 : Peu ou pas de faim
#   2-3 : Faim modérée
#   4-6 : Faim sévère

calc_hhs <- function(d, col_no_food = NULL, col_sleep_hungry = NULL,
                      col_whole_day = NULL) {

  cols <- list(
    list(col = col_no_food,       nom = "Pas de nourriture dans la maison"),
    list(col = col_sleep_hungry,  nom = "Dormir affamé"),
    list(col = col_whole_day,     nom = "Journée entière sans manger")
  )

  valid <- cols[sapply(cols, function(m) !is.null(m$col) && m$col %in% names(d))]
  if (length(valid) < 2) return(NULL)

  hhs_score <- rep(0, nrow(d))
  for (m in valid) {
    vals <- as.numeric(d[[m$col]])
    vals[is.na(vals)] <- 0
    # Recodage : 0=Jamais(0), 1=Rarement/Parfois(1), ≥2=Souvent(2)
    vals_recode <- case_when(
      vals == 0 ~ 0L,
      vals == 1 ~ 1L,
      vals >= 2 ~ 2L,
      TRUE      ~ 0L
    )
    hhs_score <- hhs_score + vals_recode
  }

  hhs_class <- case_when(
    hhs_score <= 1 ~ "Peu ou pas de faim",
    hhs_score <= 3 ~ "Faim modérée",
    TRUE           ~ "Faim sévère"
  )

  list(
    score   = hhs_score,
    class   = hhs_class,
    summary = tibble(
      Catégorie = c("Peu/pas de faim (0-1)", "Faim modérée (2-3)", "Faim sévère (4-6)"),
      N         = c(sum(hhs_class == "Peu ou pas de faim"),
                    sum(hhs_class == "Faim modérée"),
                    sum(hhs_class == "Faim sévère")),
      `%`       = round(c(sum(hhs_class == "Peu ou pas de faim"),
                          sum(hhs_class == "Faim modérée"),
                          sum(hhs_class == "Faim sévère")) / length(hhs_class) * 100, 1),
      Couleur   = c("#27ae60", "#f39c12", "#e74c3c")
    ),
    stats = tibble(
      Statistique = c("Moyenne", "Médiane", "Écart-type", "Min", "Max"),
      Valeur = round(c(mean(hhs_score), median(hhs_score), sd(hhs_score),
                        min(hhs_score), max(hhs_score)), 2)
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. FCS PRÉ-CALCULÉ — Si la colonne FCS existe déjà dans les données
# ══════════════════════════════════════════════════════════════════════════════

classify_fcs_precalc <- function(fcs_scores) {
  fcs_class <- case_when(
    is.na(fcs_scores) ~ NA_character_,
    fcs_scores < FCS_THRESHOLDS$pauvre ~ "Pauvre",
    fcs_scores <= FCS_THRESHOLDS$limite ~ "Limite",
    TRUE ~ "Acceptable"
  )
  list(
    score   = fcs_scores,
    class   = fcs_class,
    summary = tibble(
      Catégorie = c("Pauvre (< 21)", "Limite (21-35)", "Acceptable (> 35)"),
      N         = c(sum(fcs_class == "Pauvre", na.rm = TRUE),
                    sum(fcs_class == "Limite", na.rm = TRUE),
                    sum(fcs_class == "Acceptable", na.rm = TRUE)),
      `%`       = round(c(sum(fcs_class == "Pauvre", na.rm = TRUE),
                          sum(fcs_class == "Limite", na.rm = TRUE),
                          sum(fcs_class == "Acceptable", na.rm = TRUE)) /
                        sum(!is.na(fcs_class)) * 100, 1),
      Couleur   = c("#e74c3c", "#f39c12", "#27ae60")
    ),
    stats = tibble(
      Statistique = c("N valide", "Moyenne", "Médiane", "Écart-type", "Min", "Max"),
      Valeur = round(c(sum(!is.na(fcs_scores)),
                        mean(fcs_scores, na.rm = TRUE),
                        median(fcs_scores, na.rm = TRUE),
                        sd(fcs_scores, na.rm = TRUE),
                        min(fcs_scores, na.rm = TRUE),
                        max(fcs_scores, na.rm = TRUE)), 2)
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. GRAPHIQUES
# ══════════════════════════════════════════════════════════════════════════════

# Donut chart pour les classifications
plot_sa_donut <- function(summary_df, title = "Classification") {
  if (is.null(summary_df) || nrow(summary_df) == 0) return(plotly_empty())

  plot_ly(summary_df, labels = ~Catégorie, values = ~N,
          type = "pie", hole = 0.55,
          marker = list(colors = summary_df$Couleur,
                        line = list(color = "#fff", width = 2)),
          textinfo = "label+percent",
          textposition = "outside",
          textfont = list(size = 12),
          hoverinfo = "label+value+percent") |>
    layout(title = list(text = title, x = 0.5, font = list(size = 14)),
           showlegend = FALSE,
           margin = list(t = 50, b = 20))
}

# Histogramme des scores
plot_sa_histogram <- function(scores, title = "Distribution", breaks_lines = NULL,
                               colors_zones = NULL) {
  if (is.null(scores) || length(scores) < 5) return(plotly_empty())

  df_plot <- tibble(Score = scores)

  p <- ggplot(df_plot, aes(x = Score)) +
    geom_histogram(bins = 30, fill = "#0077B6", alpha = 0.8,
                   color = "white", linewidth = 0.2)

  # Lignes de seuils
  if (!is.null(breaks_lines)) {
    for (i in seq_along(breaks_lines)) {
      bl <- breaks_lines[[i]]
      p <- p + geom_vline(xintercept = bl$x, linetype = "dashed",
                           color = bl$color, linewidth = 0.8) +
        annotate("text", x = bl$x, y = Inf, label = bl$label,
                 vjust = 2, hjust = -0.1, color = bl$color,
                 size = 3, fontface = "bold")
    }
  }

  p <- p +
    labs(x = "Score", y = "Fréquence", title = title) +
    theme_minimal(base_size = 11)

  ggplotly(p)
}

# Barplot par strate
plot_sa_by_group <- function(d, col_class, col_group, title = "Par strate",
                              color_map = NULL) {
  if (is.null(col_class) || is.null(col_group)) return(plotly_empty())
  if (!col_class %in% names(d) || !col_group %in% names(d)) return(plotly_empty())

  cnt <- d |>
    filter(!is.na(.data[[col_class]]), !is.na(.data[[col_group]])) |>
    count(.data[[col_group]], .data[[col_class]]) |>
    group_by(.data[[col_group]]) |>
    mutate(pct = round(n / sum(n) * 100, 1)) |>
    ungroup()

  p <- ggplot(cnt, aes(x = .data[[col_group]], y = pct,
                        fill = .data[[col_class]],
                        text = paste0(.data[[col_group]], "\n",
                                      .data[[col_class]], ": ", pct, "%"))) +
    geom_col(position = "stack", alpha = 0.85) +
    {if (!is.null(color_map)) scale_fill_manual(values = color_map)} +
    labs(x = col_group, y = "%", title = title, fill = "") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1),
          legend.position = "top")

  ggplotly(p, tooltip = "text")
}

# Tableau croisé FCS × autre indicateur
plot_sa_cross <- function(d, col_class1, col_class2, label1, label2) {
  if (!col_class1 %in% names(d) || !col_class2 %in% names(d)) return(plotly_empty())

  cnt <- d |>
    filter(!is.na(.data[[col_class1]]), !is.na(.data[[col_class2]])) |>
    count(.data[[col_class1]], .data[[col_class2]])

  p <- ggplot(cnt, aes(x = .data[[col_class1]], y = .data[[col_class2]],
                        fill = n, text = paste0(n, " ménages"))) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = n), color = "white", fontface = "bold", size = 4) +
    scale_fill_gradient(low = "#f0f8ff", high = "#BF3636") +
    labs(x = label1, y = label2, title = paste0(label1, " × ", label2)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")

  ggplotly(p, tooltip = "text")
}

# ══════════════════════════════════════════════════════════════════════════════
# 7. RÉSUMÉ GLOBAL — Tableau de synthèse multi-indicateurs
# ══════════════════════════════════════════════════════════════════════════════

render_sa_summary_html <- function(fcs_res = NULL, rcsi_res = NULL,
                                    hdds_res = NULL, hhs_res = NULL) {
  cards <- list()

  make_card <- function(title, icon_txt, score_mean, worst_cat, worst_pct,
                         color, detail) {
    tags$div(
      style = paste0("flex:1;min-width:220px;background:white;border:1px solid #ddd;",
                     "border-radius:10px;overflow:hidden;"),
      tags$div(
        style = paste0("background:linear-gradient(135deg,", color, ",", color, "88);",
                       "padding:12px 16px;color:white;"),
        tags$div(style = "font-size:20px;font-weight:800;", icon_txt),
        tags$div(style = "font-size:12px;font-weight:600;", title)
      ),
      tags$div(
        style = "padding:12px 16px;",
        tags$div(style = "font-size:28px;font-weight:800;color:#333;text-align:center;",
                 score_mean),
        tags$div(style = "font-size:10px;color:#888;text-align:center;text-transform:uppercase;",
                 "Score moyen"),
        tags$hr(style = "margin:8px 0;"),
        tags$div(style = paste0("font-size:13px;font-weight:700;color:", color, ";text-align:center;"),
                 paste0(worst_pct, "% ", worst_cat)),
        tags$div(style = "font-size:11px;color:#555;text-align:center;margin-top:4px;", detail)
      )
    )
  }

  tagList(
    tags$div(
      style = "display:flex;gap:12px;flex-wrap:wrap;margin-bottom:16px;",

      if (!is.null(fcs_res)) {
        pct_pauvre <- fcs_res$summary$`%`[fcs_res$summary$Catégorie == "Pauvre (< 21)"]
        make_card("FCS", "🍽️", round(mean(fcs_res$score, na.rm = TRUE), 1),
                  "Pauvre", pct_pauvre, "#e74c3c",
                  paste0(fcs_res$n_groups %||% "?", " groupes alimentaires"))
      },

      if (!is.null(rcsi_res)) {
        pct_eleve <- rcsi_res$summary$`%`[rcsi_res$summary$Catégorie == "Élevé (≥ 19)"]
        make_card("rCSI", "⚠️", round(mean(rcsi_res$score, na.rm = TRUE), 1),
                  "Coping élevé", pct_eleve, "#e67e22",
                  "Stratégies d'adaptation")
      },

      if (!is.null(hdds_res)) {
        pct_faible <- hdds_res$summary$`%`[hdds_res$summary$Catégorie == "Faible (≤ 3)"]
        make_card("HDDS", "🥗", round(mean(hdds_res$score, na.rm = TRUE), 1),
                  "Faible diversité", pct_faible, "#8E44AD",
                  paste0(hdds_res$n_groups_used, " groupes évalués"))
      },

      if (!is.null(hhs_res)) {
        pct_severe <- hhs_res$summary$`%`[hhs_res$summary$Catégorie == "Faim sévère (4-6)"]
        make_card("HHS", "🔴", round(mean(hhs_res$score, na.rm = TRUE), 1),
                  "Faim sévère", pct_severe, "#c0392b",
                  "Échelle de la faim")
      }
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 8. FCS COMPOSITION — Contribution des groupes alimentaires
# ══════════════════════════════════════════════════════════════════════════════

#' Calcule la contribution de chaque groupe alimentaire au score FCS moyen
calc_fcs_composition <- function(fcs_res) {
  if (is.null(fcs_res) || is.null(fcs_res$detail)) return(NULL)

  detail <- fcs_res$detail
  score_total <- sum(sapply(detail, function(x) x$moy * x$poids))
  if (score_total == 0) return(NULL)

  tibble(
    Groupe       = names(detail),
    Poids_PAM    = sapply(detail, function(x) x$poids),
    Consommation_moy = sapply(detail, function(x) x$moy),
    Contribution_moy = sapply(detail, function(x) round(x$moy * x$poids, 2)),
    Contribution_pct = round(sapply(detail, function(x) x$moy * x$poids) / score_total * 100, 1)
  ) |>
    arrange(desc(Contribution_moy))
}

#' Graphique horizontal : contribution des groupes FCS au score moyen
plot_fcs_composition_bar <- function(fcs_res) {
  comp <- calc_fcs_composition(fcs_res)
  if (is.null(comp)) return(plotly_empty() |> layout(title = list(text = "Données insuffisantes", x = .5)))

  couleurs <- c(
    "Céréales/Tubercules"    = "#E67E22",
    "Légumineuses"           = "#27AE60",
    "Légumes"                = "#2ECC71",
    "Fruits"                 = "#F1C40F",
    "Viande/Poisson"         = "#C0392B",
    "Lait/Prod. laitiers"    = "#3498DB",
    "Sucre"                  = "#9B59B6",
    "Huile/Graisse"          = "#95A5A6",
    "Condiments"             = "#BDC3C7"
  )
  fill_colors <- unname(sapply(comp$Groupe, function(g) {
    m <- names(couleurs)[which(startsWith(g, sub("/.*", "", names(couleurs))))[1]]
    if (length(m) == 0 || is.na(m)) "#0077B6" else couleurs[m]
  }))

  comp$Groupe <- factor(comp$Groupe, levels = comp$Groupe[order(comp$Contribution_moy)])
  comp$text_hover <- paste0(
    "<b>", comp$Groupe, "</b><br>",
    "Consommation moy : ", comp$Consommation_moy, " j/sem<br>",
    "Poids PAM : ×", comp$Poids_PAM, "<br>",
    "Contribution : ", comp$Contribution_moy, " pts (", comp$Contribution_pct, "%)"
  )

  plot_ly(comp, x = ~Contribution_moy, y = ~Groupe, type = "bar", orientation = "h",
          text = ~text_hover, hoverinfo = "text",
          marker = list(color = fill_colors,
                        line = list(color = "white", width = 1))) |>
    layout(
      title = list(text = "<b>Contribution des groupes alimentaires au FCS moyen</b>",
                   font = list(size = 14, color = "#0B3954"), x = 0),
      xaxis = list(title = "Contribution au score FCS (pts pondérés)", gridcolor = "#f0f0f0"),
      yaxis = list(title = "", tickfont = list(size = 11)),
      plot_bgcolor  = "white", paper_bgcolor = "white",
      margin = list(l = 160)
    )
}

#' Radar / Spider chart multi-indicateurs SA (FCS, rCSI normalisé, HDDS, HHS)
plot_sa_radar <- function(fcs_res = NULL, rcsi_res = NULL,
                           hdds_res = NULL, hhs_res = NULL) {
  vals <- c()
  lbls <- c()

  # Normaliser chaque indicateur sur 0-100 (100 = meilleure situation)
  if (!is.null(fcs_res)) {
    fcs_norm <- min(100, round(mean(fcs_res$score, na.rm = TRUE) / 112 * 100, 1))
    vals <- c(vals, fcs_norm); lbls <- c(lbls, "FCS<br>(0=pauvre)")
  }
  if (!is.null(rcsi_res)) {
    rcsi_norm <- max(0, round((1 - mean(rcsi_res$score, na.rm = TRUE) / 56) * 100, 1))
    vals <- c(vals, rcsi_norm); lbls <- c(lbls, "rCSI<br>(100=faible coping)")
  }
  if (!is.null(hdds_res)) {
    hdds_norm <- round(mean(hdds_res$score, na.rm = TRUE) / 12 * 100, 1)
    vals <- c(vals, hdds_norm); lbls <- c(lbls, "HDDS<br>(diversité)")
  }
  if (!is.null(hhs_res)) {
    hhs_norm <- max(0, round((1 - mean(hhs_res$score, na.rm = TRUE) / 6) * 100, 1))
    vals <- c(vals, hhs_norm); lbls <- c(lbls, "HHS<br>(100=sans faim)")
  }

  if (length(vals) < 2) return(NULL)

  vals_closed <- c(vals, vals[1])
  lbls_closed <- c(lbls, lbls[1])

  plot_ly(
    type = "scatterpolar", mode = "lines+markers",
    r = vals_closed, theta = lbls_closed, fill = "toself",
    fillcolor = "rgba(0,141,201,0.25)",
    line = list(color = "#0077B6", width = 2.5),
    marker = list(color = "#0077B6", size = 8),
    hovertemplate = "<b>%{theta}</b><br>Score normalisé : %{r:.1f}%<extra></extra>"
  ) |>
    layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 100),
                          tickfont = list(size = 9), gridcolor = "#e0e0e0"),
        angularaxis = list(tickfont = list(size = 11))
      ),
      title = list(text = "<b>Profil global de sécurité alimentaire</b><br><sup>(100 = situation optimale)</sup>",
                   font = list(size = 13, color = "#0B3954"), x = 0.5),
      showlegend = FALSE,
      paper_bgcolor = "white"
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# 9. rCSI COMPOSANTES — Analyse détaillée par stratégie
# ══════════════════════════════════════════════════════════════════════════════

#' Analyse des composantes rCSI (quelle stratégie est utilisée, fréquence, contribution)
calc_rcsi_components <- function(d, col_moins_prefere = NULL, col_emprunter = NULL,
                                  col_reduire_repas = NULL, col_reduire_portions = NULL,
                                  col_reduire_adultes = NULL) {
  strategies <- list(
    list(col = col_moins_prefere,    w = 1, nom = "Aliments moins préférés",    icon = "🍲"),
    list(col = col_emprunter,        w = 2, nom = "Emprunter nourriture",        icon = "🤝"),
    list(col = col_reduire_repas,    w = 1, nom = "Réduire nb repas/jour",       icon = "🍽️"),
    list(col = col_reduire_portions, w = 1, nom = "Réduire portions",            icon = "📉"),
    list(col = col_reduire_adultes,  w = 3, nom = "Restreindre adultes",         icon = "👥")
  )

  valid <- strategies[sapply(strategies, function(s) !is.null(s$col) && s$col %in% names(d))]
  if (length(valid) < 2) return(NULL)

  rows <- lapply(valid, function(s) {
    vals <- as.numeric(d[[s$col]]); vals[is.na(vals)] <- 0
    vals <- pmin(pmax(vals, 0), 7)
    tibble(
      Stratégie         = s$nom,
      Poids             = s$w,
      Moy_jours         = round(mean(vals), 2),
      Pct_recours       = round(mean(vals > 0) * 100, 1),
      Contribution_moy  = round(mean(vals) * s$w, 2)
    )
  })
  bind_rows(rows) |> arrange(desc(Contribution_moy))
}

#' Graphique à barres des stratégies rCSI (% de recours et contribution pondérée)
plot_rcsi_components <- function(d, col_moins_prefere = NULL, col_emprunter = NULL,
                                  col_reduire_repas = NULL, col_reduire_portions = NULL,
                                  col_reduire_adultes = NULL) {
  comp <- calc_rcsi_components(d, col_moins_prefere, col_emprunter,
                                col_reduire_repas, col_reduire_portions, col_reduire_adultes)
  if (is.null(comp)) return(plotly_empty() |> layout(title = list(text = "Données insuffisantes", x = .5)))

  couleurs_poids <- c("1" = "#3498DB", "2" = "#E67E22", "3" = "#C0392B")
  fill_col <- unname(sapply(as.character(comp$Poids), function(w) couleurs_poids[w]))

  comp$Stratégie <- factor(comp$Stratégie, levels = comp$Stratégie[order(comp$Pct_recours)])
  comp$hover <- paste0(
    "<b>", comp$Stratégie, "</b><br>",
    "% ménages y recourant : ", comp$Pct_recours, "%<br>",
    "Moy jours/sem : ", comp$Moy_jours, "<br>",
    "Poids PAM : ×", comp$Poids, "<br>",
    "Contribution pondérée : ", comp$Contribution_moy, " pts"
  )

  fig <- plot_ly() |>
    add_trace(
      data = comp, x = ~Pct_recours, y = ~Stratégie, type = "bar", orientation = "h",
      name = "% ménages recourant",
      text = ~hover, hoverinfo = "text",
      marker = list(color = fill_col, line = list(color = "white", width = 1))
    ) |>
    add_trace(
      data = comp, x = ~Contribution_moy, y = ~Stratégie, type = "scatter",
      mode = "markers", name = "Contribution pondérée (pts)",
      xaxis = "x2",
      marker = list(color = "#0B3954", symbol = "diamond", size = 10),
      hovertemplate = paste0("<b>%{y}</b><br>Contribution : %{x:.2f} pts<extra></extra>")
    ) |>
    layout(
      title = list(text = "<b>Détail des stratégies d'adaptation (rCSI)</b>",
                   font = list(size = 14, color = "#0B3954"), x = 0),
      xaxis  = list(title = "% ménages y recourant", range = c(0, 105),
                    gridcolor = "#f0f0f0"),
      xaxis2 = list(title = "Contribution pondérée (pts)", overlaying = "x", side = "top",
                    range = c(0, max(comp$Contribution_moy) * 1.3), gridcolor = "#eee"),
      yaxis  = list(title = ""),
      legend = list(orientation = "h", y = -0.2),
      barmode = "overlay",
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(l = 190, t = 60)
    )
  fig
}

# ══════════════════════════════════════════════════════════════════════════════
# 10. CLASSIFICATION IPC / CADRE HARMONISÉ — Phases de sécurité alimentaire
# ══════════════════════════════════════════════════════════════════════════════
#
# Phase I  — Minimale  : FCS Acceptable ET rCSI faible ET HHS sans faim
# Phase II — Sous pression : FCS Limite OU rCSI moyen OU HHS légère
# Phase III — Crise   : FCS Pauvre OU rCSI élevé OU HHS modérée OU MUAC < 125
# Phase IV — Urgence  : Combinaison critique (FCS Pauvre + rCSI élevé)
# Phase V  — Famine   : Extrême (rare, critères OMS/IPC stricts)
#
# Note : Ceci est une approximation IPC basée sur les indicateurs disponibles.
# La classification officielle nécessite une triangulation de sources multiples.

IPC_PHASES <- data.frame(
  Phase  = 1:5,
  Label  = c("Phase 1 – Minimale", "Phase 2 – Sous pression",
              "Phase 3 – Crise", "Phase 4 – Urgence", "Phase 5 – Famine"),
  Couleur = c("#59A608", "#E6CF00", "#E67800", "#C80000", "#5C1A1A"),
  stringsAsFactors = FALSE
)

#' Classifie chaque ménage en Phase IPC approximative
#' @param fcs_score numeric vecteur FCS (peut être NULL)
#' @param rcsi_score numeric vecteur rCSI (peut être NULL)
#' @param hhs_score numeric vecteur HHS (peut être NULL)
#' @param muac_mm numeric vecteur MUAC en mm (peut être NULL, optionnel)
#' @return vecteur de phases (1-5) de même longueur
calc_ipc_phase <- function(fcs_score = NULL, rcsi_score = NULL,
                            hhs_score = NULL, muac_mm = NULL) {
  n <- max(c(length(fcs_score), length(rcsi_score),
              length(hhs_score), length(muac_mm)), na.rm = TRUE)
  if (n == 0) return(NULL)

  # Pad vectors to length n
  pad <- function(x, n) if (is.null(x) || length(x) == 0) rep(NA_real_, n) else x
  fcs  <- pad(fcs_score,  n)
  rcsi <- pad(rcsi_score, n)
  hhs  <- pad(hhs_score,  n)
  muac <- pad(muac_mm,    n)

  # Indicateurs de sévérité
  fcs_pauvre    <- !is.na(fcs)  & fcs < 21
  fcs_limite    <- !is.na(fcs)  & fcs >= 21 & fcs <= 35
  rcsi_eleve    <- !is.na(rcsi) & rcsi >= 19
  rcsi_moyen    <- !is.na(rcsi) & rcsi >= 4  & rcsi < 19
  hhs_severe    <- !is.na(hhs)  & hhs >= 4
  hhs_modere    <- !is.na(hhs)  & hhs >= 2  & hhs < 4
  muac_mag      <- !is.na(muac) & muac < 125

  phase <- rep(1L, n)

  # Phase 2 : Sous pression — au moins 1 indicateur modéré
  phase[fcs_limite | rcsi_moyen | hhs_modere] <- 2L

  # Phase 3 : Crise — indicateur pauvre OU MUAC MAG
  phase[(fcs_pauvre | rcsi_eleve | hhs_modere | muac_mag) &
        !(fcs_pauvre & rcsi_eleve)] <- 3L

  # Phase 4 : Urgence — combinaison critique
  phase[fcs_pauvre & rcsi_eleve] <- 4L

  # Phase 5 : Famine — extrême (HHS sévère + phase 4)
  phase[fcs_pauvre & rcsi_eleve & hhs_severe] <- 5L

  phase
}

#' Tableau de résumé des phases IPC
calc_ipc_summary <- function(phases) {
  if (is.null(phases)) return(NULL)
  n_total <- length(phases)
  tab <- table(factor(phases, levels = 1:5))
  tibble(
    Phase   = IPC_PHASES$Phase,
    Label   = IPC_PHASES$Label,
    Couleur = IPC_PHASES$Couleur,
    N       = as.integer(tab),
    Pct     = round(as.numeric(tab) / n_total * 100, 1)
  )
}

#' Graphique en barres empilées des phases IPC (global et par groupe)
plot_ipc_phases <- function(phases, group_var = NULL, group_labels = NULL) {
  if (is.null(phases)) return(plotly_empty() |>
    layout(title = list(text = "Calculez les indicateurs SA pour estimer les phases IPC", x = .5)))

  df_ipc <- tibble(phase = factor(phases, levels = 1:5))
  if (!is.null(group_var) && length(group_var) == length(phases)) {
    df_ipc$Groupe <- group_var
    df_plot <- df_ipc |>
      group_by(Groupe, phase) |>
      summarise(n = n(), .groups = "drop") |>
      group_by(Groupe) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      ungroup()
  } else {
    df_ipc$Groupe <- "Global"
    df_plot <- df_ipc |>
      group_by(Groupe, phase) |>
      summarise(n = n(), .groups = "drop") |>
      group_by(Groupe) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      ungroup()
  }

  df_plot <- df_plot |>
    left_join(IPC_PHASES |> rename(phase_num = Phase, Label = Label, Couleur = Couleur) |>
                mutate(phase = factor(phase_num, levels = 1:5)), by = "phase")

  fig <- plot_ly()
  for (ph in 1:5) {
    d_ph <- df_plot |> filter(phase == ph)
    if (nrow(d_ph) == 0) next
    lbl <- IPC_PHASES$Label[ph]
    col <- IPC_PHASES$Couleur[ph]
    fig <- fig |> add_trace(
      data = d_ph, x = ~pct, y = ~Groupe, type = "bar", orientation = "h",
      name = lbl, text = ~paste0(pct, "%"),
      textposition = "inside", insidetextanchor = "middle",
      hovertemplate = paste0("<b>", lbl, "</b><br>Groupe : %{y}<br>%{x:.1f}% (n=%{customdata})<extra></extra>"),
      customdata = ~n,
      marker = list(color = col, line = list(color = "white", width = 0.5))
    )
  }
  fig |> layout(
    barmode = "stack",
    title = list(text = "<b>Estimation des Phases IPC/CH par groupe</b><br><sup>Approximation basée sur FCS + rCSI + HHS (triangulation terrain recommandée)</sup>",
                 font = list(size = 13, color = "#0B3954"), x = 0),
    xaxis = list(title = "% ménages", ticksuffix = "%", range = c(0, 100)),
    yaxis = list(title = ""),
    legend = list(orientation = "h", y = -0.25, x = 0),
    plot_bgcolor = "white", paper_bgcolor = "white",
    margin = list(b = 100)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 11. COMPARAISON INTER-ROUNDS — Évolution des indicateurs SA
# ══════════════════════════════════════════════════════════════════════════════

#' Calcule les statistiques SA par round de collecte
calc_sa_by_round <- function(d, round_col,
                              fcs_score = NULL, rcsi_score = NULL,
                              hdds_score = NULL, hhs_score = NULL) {
  if (!round_col %in% names(d)) return(NULL)
  n <- nrow(d)

  df_r <- tibble(round = d[[round_col]])
  if (!is.null(fcs_score)  && length(fcs_score)  == n) df_r$fcs  <- fcs_score
  if (!is.null(rcsi_score) && length(rcsi_score) == n) df_r$rcsi <- rcsi_score
  if (!is.null(hdds_score) && length(hdds_score) == n) df_r$hdds <- hdds_score
  if (!is.null(hhs_score)  && length(hhs_score)  == n) df_r$hhs  <- hhs_score

  indicators <- intersect(c("fcs", "rcsi", "hdds", "hhs"), names(df_r))
  if (length(indicators) == 0) return(NULL)

  df_r |>
    group_by(round) |>
    summarise(
      n = dplyr::n(),
      across(all_of(indicators), list(
        mean  = ~round(mean(.x, na.rm = TRUE), 2),
        ci_lo = ~round(mean(.x, na.rm = TRUE) - 1.96 * sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))), 2),
        ci_hi = ~round(mean(.x, na.rm = TRUE) + 1.96 * sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))), 2)
      ), .names = "{.col}_{.fn}"),
      `% FCS pauvre` = if ("fcs" %in% indicators) round(mean(fcs < 21, na.rm = TRUE) * 100, 1) else NA_real_,
      `% rCSI élevé` = if ("rcsi" %in% indicators) round(mean(rcsi >= 19, na.rm = TRUE) * 100, 1) else NA_real_,
      .groups = "drop"
    )
}

#' Graphique de comparaison inter-rounds (lignes + IC)
plot_sa_rounds <- function(d, round_col, fcs_score = NULL, rcsi_score = NULL,
                            hdds_score = NULL, hhs_score = NULL) {
  req_vec <- list(fcs = fcs_score, rcsi = rcsi_score, hdds = hdds_score, hhs = hhs_score)
  available <- req_vec[!sapply(req_vec, is.null)]

  if (length(available) == 0 || !round_col %in% names(d))
    return(plotly_empty() |> layout(title = list(text = "Données insuffisantes", x = .5)))

  rounds_sorted <- sort(unique(d[[round_col]]))
  couleurs_ind <- c(fcs = "#0077B6", rcsi = "#E67E22", hdds = "#27AE60", hhs = "#C0392B")
  labels_ind   <- c(fcs = "FCS", rcsi = "rCSI", hdds = "HDDS", hhs = "HHS")

  # Normaliser chaque indicateur sur 0-100 pour comparaison visuelle
  max_vals <- c(fcs = 112, rcsi = 56, hdds = 12, hhs = 6)

  fig <- plot_ly()
  for (ind in names(available)) {
    scores <- available[[ind]]
    df_r <- tibble(round = d[[round_col]], score = scores) |>
      filter(!is.na(round), !is.na(score)) |>
      group_by(round) |>
      summarise(
        moy   = mean(score, na.rm = TRUE),
        se    = sd(score, na.rm = TRUE) / sqrt(n()),
        n_obs = n(), .groups = "drop"
      ) |>
      mutate(
        ci_lo = moy - 1.96 * se,
        ci_hi = moy + 1.96 * se,
        norm  = round(moy / max_vals[ind] * 100, 1)
      ) |>
      arrange(match(round, rounds_sorted))

    col <- couleurs_ind[ind]
    lbl <- labels_ind[ind]

    # Ruban IC
    fig <- fig |> add_ribbons(
      data = df_r, x = ~round, ymin = ~ci_lo, ymax = ~ci_hi,
      fillcolor = paste0(col, "22"), line = list(color = "transparent"),
      name = paste(lbl, "IC 95%"), showlegend = FALSE, legendgroup = ind
    )
    # Ligne principale
    fig <- fig |> add_trace(
      data = df_r, x = ~round, y = ~moy, type = "scatter", mode = "lines+markers",
      name = lbl, line = list(color = col, width = 2.5),
      marker = list(color = col, size = 8),
      legendgroup = ind,
      hovertemplate = paste0(
        "<b>", lbl, " — %{x}</b><br>Moy : %{y:.2f}<br>N : %{customdata}<extra></extra>"
      ),
      customdata = ~n_obs
    )
  }

  fig |> layout(
    title = list(text = "<b>Évolution des indicateurs de sécurité alimentaire par round</b>",
                 font = list(size = 13, color = "#0B3954"), x = 0),
    xaxis = list(title = "Round de collecte", gridcolor = "#f0f0f0"),
    yaxis = list(title = "Score moyen", gridcolor = "#f0f0f0"),
    legend = list(orientation = "h", y = -0.25, x = 0),
    plot_bgcolor = "white", paper_bgcolor = "white",
    hovermode = "x unified"
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# EXTENSION : LCSI · FIES · MDD-W (PAM VAM / IPC / FAO)
# ══════════════════════════════════════════════════════════════════════════════

# ── LCSI (Livelihood Coping Strategy Index) ──────────────────────────────────
LCSI_LEVELS <- data.frame(
  Phase = c("Aucune stratégie", "Stress", "Crise", "Urgence"),
  Couleur = c("#27ae60", "#f39c12", "#e67e22", "#e74c3c"),
  Ordre = 1:4,
  stringsAsFactors = FALSE
)

calc_lcsi <- function(d, col_stress = NULL, col_crisis = NULL, col_emergency = NULL,
                      col_lcsi_class = NULL) {
  tryCatch({
    # Si classification pré-calculée
    if (!is.null(col_lcsi_class) && col_lcsi_class %in% names(d)) {
      lcsi <- d[[col_lcsi_class]]
      valid <- !is.na(lcsi) & lcsi != ""
      freq <- as.data.frame(table(factor(lcsi[valid],
        levels = c("Aucune stratégie", "Stress", "Crise", "Urgence"))),
        stringsAsFactors = FALSE)
      names(freq) <- c("Phase", "N")
      freq <- dplyr::left_join(freq, LCSI_LEVELS[, c("Phase", "Couleur")], by = "Phase")
      freq$Pct <- round(freq$N / sum(freq$N) * 100, 1)
      return(list(summary = freq, classification = lcsi, n_valid = sum(valid)))
    }

    # Calculer depuis composantes
    stress <- if (!is.null(col_stress)) tolower(d[[col_stress]]) else rep(NA, nrow(d))
    crisis <- if (!is.null(col_crisis)) tolower(d[[col_crisis]]) else rep(NA, nrow(d))
    emerg  <- if (!is.null(col_emergency)) tolower(d[[col_emergency]]) else rep(NA, nrow(d))

    is_yes <- function(x) x %in% c("oui", "yes", "1", "true")

    classification <- dplyr::case_when(
      is_yes(emerg) ~ "Urgence",
      is_yes(crisis) ~ "Crise",
      is_yes(stress) ~ "Stress",
      TRUE ~ "Aucune stratégie"
    )

    valid <- !is.na(stress) | !is.na(crisis) | !is.na(emerg)
    freq <- as.data.frame(table(factor(classification[valid],
      levels = c("Aucune stratégie", "Stress", "Crise", "Urgence"))),
      stringsAsFactors = FALSE)
    names(freq) <- c("Phase", "N")
    freq <- dplyr::left_join(freq, LCSI_LEVELS[, c("Phase", "Couleur")], by = "Phase")
    freq$Pct <- round(freq$N / sum(freq$N) * 100, 1)

    list(summary = freq, classification = classification, n_valid = sum(valid))
  }, error = function(e) list(summary = data.frame(), error = conditionMessage(e)))
}

# ── FIES (Food Insecurity Experience Scale) ──────────────────────────────────
FIES_LEVELS <- data.frame(
  Catégorie = c("Sécurité alimentaire", "Insécurité légère",
                "Insécurité modérée", "Insécurité sévère"),
  Seuil_inf = c(0, 1, 4, 7),
  Seuil_sup = c(0, 3, 6, 8),
  Couleur = c("#27ae60", "#f39c12", "#e67e22", "#e74c3c"),
  stringsAsFactors = FALSE
)

calc_fies <- function(d, col_fies) {
  tryCatch({
    score <- as.numeric(d[[col_fies]])
    valid <- !is.na(score) & score >= 0 & score <= 8

    classification <- dplyr::case_when(
      !valid ~ NA_character_,
      score == 0 ~ "Sécurité alimentaire",
      score <= 3 ~ "Insécurité légère",
      score <= 6 ~ "Insécurité modérée",
      score <= 8 ~ "Insécurité sévère"
    )

    freq <- as.data.frame(table(factor(classification[valid],
      levels = FIES_LEVELS$Catégorie)), stringsAsFactors = FALSE)
    names(freq) <- c("Catégorie", "N")
    freq <- dplyr::left_join(freq, FIES_LEVELS[, c("Catégorie", "Couleur")], by = "Catégorie")
    freq$Pct <- round(freq$N / sum(freq$N) * 100, 1)

    list(
      summary = freq,
      classification = classification,
      score = score,
      n_valid = sum(valid),
      mean_score = round(mean(score[valid]), 1),
      median_score = round(median(score[valid])),
      pct_moderate_severe = round(mean(score[valid] >= 4) * 100, 1),
      stats = data.frame(
        Statistique = c("Moyenne", "Médiane", "Écart-type", "N valides"),
        Valeur = c(round(mean(score[valid]), 2), median(score[valid]),
                   round(sd(score[valid]), 2), sum(valid))
      )
    )
  }, error = function(e) list(summary = data.frame(), error = conditionMessage(e)))
}

# ── MDD-W (Minimum Dietary Diversity Women) ──────────────────────────────────
calc_mddw <- function(d, col_mddw) {
  tryCatch({
    v <- tolower(as.character(d[[col_mddw]]))
    valid <- !is.na(v) & v != ""
    positive <- rep(FALSE, length(v))
    positive[valid] <- v[valid] %in% c("oui", "yes", "1", "true")
    n_valid <- sum(valid)
    n_pos <- sum(positive)
    pct <- round(n_pos / n_valid * 100, 1)

    # IC Wilson
    z <- 1.96
    p <- n_pos / n_valid
    denom <- 1 + z^2 / n_valid
    center <- (p + z^2 / (2 * n_valid)) / denom
    margin <- z * sqrt((p * (1 - p) + z^2 / (4 * n_valid)) / n_valid) / denom
    ci_lo <- round(max(0, center - margin) * 100, 1)
    ci_hi <- round(min(1, center + margin) * 100, 1)

    classification <- ifelse(!valid, NA_character_,
                             ifelse(positive, "MDD-W adéquat", "MDD-W inadéquat"))

    list(
      pct = pct, ci_lower = ci_lo, ci_upper = ci_hi,
      n_valid = n_valid, n_positive = n_pos, col_used = col_mddw,
      classification = classification,
      summary = data.frame(
        Catégorie = c("MDD-W adéquat (≥5 grp)", "MDD-W inadéquat (<5 grp)"),
        N = c(n_pos, n_valid - n_pos),
        Pct = c(pct, round(100 - pct, 1)),
        Couleur = c("#27ae60", "#e74c3c"),
        stringsAsFactors = FALSE
      )
    )
  }, error = function(e) list(pct = NA, error = conditionMessage(e)))
}

# ── Plots LCSI ───────────────────────────────────────────────────────────────
plot_lcsi_stacked <- function(lcsi_res, group_var = NULL, d = NULL) {
  tryCatch({
    df <- lcsi_res$summary
    if (nrow(df) == 0) return(NULL)

    df$Phase <- factor(df$Phase, levels = c("Aucune stratégie", "Stress", "Crise", "Urgence"))

    ggplot2::ggplot(df, ggplot2::aes(x = "", y = Pct, fill = Phase)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::scale_fill_manual(values = c("Aucune stratégie" = "#27ae60", "Stress" = "#f39c12",
                                            "Crise" = "#e67e22", "Urgence" = "#e74c3c")) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(Pct >= 3, paste0(Pct, "%"), "")),
                         position = ggplot2::position_stack(vjust = 0.5), color = "white",
                         fontface = "bold", size = 4) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Classification LCSI — Stratégies de subsistance",
                    x = "", y = "%", fill = "Phase") +
      ggplot2::theme_minimal()
  }, error = function(e) NULL)
}

# ── Plots FIES ───────────────────────────────────────────────────────────────
plot_fies_distribution <- function(fies_res) {
  tryCatch({
    score <- fies_res$score[!is.na(fies_res$score)]
    if (length(score) == 0) return(NULL)

    df <- data.frame(score = score)
    ggplot2::ggplot(df, ggplot2::aes(x = factor(score))) +
      ggplot2::geom_bar(fill = "#3498db", alpha = 0.8) +
      ggplot2::labs(title = "Distribution des scores FIES",
                    subtitle = paste0("Moyenne: ", fies_res$mean_score,
                                      " | Insécurité modérée/sévère: ", fies_res$pct_moderate_severe, "%"),
                    x = "Score FIES (0-8)", y = "Effectif") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }, error = function(e) NULL)
}

# ══════════════════════════════════════════════════════════════════════════════
# 12. INFÉRENCE — IC de Wilson · test χ² · comparaison inter-rounds
# ══════════════════════════════════════════════════════════════════════════════

#' Intervalle de confiance de Wilson (proportions, bornes en %)
wilson_ci <- function(k, n, conf = 0.95) {
  if (is.na(k) || is.na(n) || n <= 0) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  marge <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  c(max(0, centre - marge) * 100, min(1, centre + marge) * 100)
}

#' Libellé "a–b" des bornes Wilson (en %)
sa_ci_col <- function(k, n) {
  ci <- wilson_ci(k, n)
  if (anyNA(ci)) return("\u2014")
  sprintf("%.1f\u2013%.1f", ci[1], ci[2])
}

#' Ajoute une colonne IC 95% (Wilson) à un tableau de classification
sa_add_ci <- function(summary_df, n_tot = NULL) {
  if (is.null(summary_df) || nrow(summary_df) == 0) return(summary_df)
  if (is.null(n_tot) || is.na(n_tot) || n_tot <= 0)
    n_tot <- sum(summary_df$N, na.rm = TRUE)
  summary_df$`IC 95%` <- vapply(summary_df$N, function(k) sa_ci_col(k, n_tot),
                                 character(1))
  summary_df
}

#' Statistiques descriptives standard (6 lignes)
sa_stats_5 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0)
    return(tibble(Statistique = c("N valide", "Moyenne", "M\u00e9diane",
                                   "\u00c9cart-type", "Min", "Max"),
                  Valeur = NA_real_))
  tibble(
    Statistique = c("N valide", "Moyenne", "M\u00e9diane", "\u00c9cart-type", "Min", "Max"),
    Valeur = round(c(length(x), mean(x), median(x), sd(x), min(x), max(x)), 2)
  )
}

#' Formate une p-value (style APA)
fmt_p <- function(p) {
  if (is.na(p)) return("p = \u2014")
  if (p < 0.001) "p < 0,001" else sprintf("p = %.3f", p)
}

#' Test du χ² d'indépendance + V de Cramér entre une classification et une strate
calc_strat_test <- function(class_vec, group_vec) {
  ok <- !is.na(class_vec) & !is.na(group_vec)
  class_vec <- class_vec[ok]
  group_vec <- as.character(group_vec[ok])
  tab <- table(group_vec, class_vec)
  if (nrow(tab) < 2 || ncol(tab) < 2)
    return(list(p = NA_real_, V = NA_real_, n = sum(ok),
                note = "Groupes ou modalit\u00e9s insuffisants pour tester l'association"))
  exp <- suppressWarnings(chisq.test(tab)$expected)
  if (any(exp < 5)) {
    p <- tryCatch(suppressWarnings(
      chisq.test(tab, simulate.p.value = TRUE, B = 2000)$p.value),
      error = function(e) NA_real_)
    note <- "\u03c7\u00b2 par simulation de Monte-Carlo (effectifs attendus < 5)"
  } else {
    p <- tryCatch(suppressWarnings(chisq.test(tab)$p.value),
                  error = function(e) NA_real_)
    note <- "\u03c7\u00b2 d'ind\u00e9pendance"
  }
  chi2 <- tryCatch(suppressWarnings(as.numeric(chisq.test(tab, correct = FALSE)$statistic)),
                   error = function(e) NA_real_)
  V <- sqrt(chi2 / (sum(tab) * (min(dim(tab)) - 1)))
  list(p = p, V = V, n = sum(ok), note = note)
}

#' Tendance inter-rounds d'une proportion (IC Wilson par round + test 1er vs dernier)
calc_rounds_trend <- function(round_vec, class_vec, bad_level, label = "FCS pauvre") {
  ok <- !is.na(round_vec) & !is.na(class_vec)
  df <- tibble(round = as.character(round_vec[ok]),
               bad = class_vec[ok] == bad_level)
  if (nrow(df) == 0) return(NULL)
  summ <- df |>
    group_by(round) |>
    summarise(n = n(), k = sum(bad), pct = round(mean(bad) * 100, 1), .groups = "drop")
  ci <- t(vapply(seq_len(nrow(summ)), function(i) wilson_ci(summ$k[i], summ$n[i]),
                 numeric(2)))
  summ$ci_lo <- round(ci[, 1], 1)
  summ$ci_hi <- round(ci[, 2], 1)
  p_val <- NA_real_
  diff_pp <- NA_real_
  if (nrow(summ) >= 2) {
    a <- summ[1, ]; b <- summ[nrow(summ), ]
    diff_pp <- round(b$pct - a$pct, 1)
    p_val <- tryCatch(
      prop.test(c(a$k, b$k), c(a$n, b$n), correct = FALSE)$p.value,
      error = function(e) NA_real_)
  }
  list(table = summ, p = p_val, diff_pp = diff_pp,
       first = summ$round[1], last = summ$round[nrow(summ)],
       label = label)
}

# ══════════════════════════════════════════════════════════════════════════════
# 13. PRÉ-CALCULÉS — HDDS · rCSI · HHS depuis colonnes existantes
# ══════════════════════════════════════════════════════════════════════════════

calc_hdds_precalc <- function(d, col_hdds) {
  score <- suppressWarnings(as.numeric(d[[col_hdds]]))
  valid <- !is.na(score) & score >= 0 & score <= 12
  class <- dplyr::case_when(
    !valid ~ NA_character_,
    score <= 3 ~ "Faible diversit\u00e9",
    score <= 5 ~ "Diversit\u00e9 moyenne",
    TRUE       ~ "Bonne diversit\u00e9"
  )
  n_ok <- sum(valid)
  k <- c(sum(class == "Faible diversit\u00e9", na.rm = TRUE),
         sum(class == "Diversit\u00e9 moyenne", na.rm = TRUE),
         sum(class == "Bonne diversit\u00e9", na.rm = TRUE))
  list(
    score = score, class = class, n_groups_used = NA_integer_, precalc = TRUE,
    summary = tibble(
      Catégorie = c("Faible (\u2264 3)", "Moyenne (4-5)", "Bonne (\u2265 6)"),
      N = k, `%` = round(k / max(1, n_ok) * 100, 1),
      Couleur = c("#e74c3c", "#f39c12", "#27ae60")
    ),
    group_freq = tibble(Message = "HDDS charg\u00e9 depuis la colonne pr\u00e9-calcul\u00e9e"),
    stats = sa_stats_5(score[valid])
  )
}

calc_rcsi_precalc <- function(d, col_rcsi) {
  score <- suppressWarnings(as.numeric(d[[col_rcsi]]))
  valid <- !is.na(score) & score >= 0 & score <= 56
  class <- dplyr::case_when(
    !valid ~ NA_character_,
    score < 4  ~ "Pas/Peu de coping",
    score < 19 ~ "Coping moyen",
    TRUE       ~ "Coping \u00e9lev\u00e9"
  )
  n_ok <- sum(valid)
  k <- c(sum(class == "Pas/Peu de coping", na.rm = TRUE),
         sum(class == "Coping moyen", na.rm = TRUE),
         sum(class == "Coping \u00e9lev\u00e9", na.rm = TRUE))
  list(
    score = score, class = class, precalc = TRUE,
    summary = tibble(
      Catégorie = c("Pas/Peu (< 4)", "Moyen (4-18)", "\u00c9lev\u00e9 (\u2265 19)"),
      N = k, `%` = round(k / max(1, n_ok) * 100, 1),
      Couleur = c("#27ae60", "#f39c12", "#e74c3c")
    ),
    stats = sa_stats_5(score[valid])
  )
}

calc_hhs_precalc <- function(d, col_hhs) {
  score <- suppressWarnings(as.numeric(d[[col_hhs]]))
  valid <- !is.na(score) & score >= 0 & score <= 6
  class <- dplyr::case_when(
    !valid ~ NA_character_,
    score <= 1 ~ "Peu ou pas de faim",
    score <= 3 ~ "Faim mod\u00e9r\u00e9e",
    TRUE       ~ "Faim s\u00e9v\u00e8re"
  )
  n_ok <- sum(valid)
  k <- c(sum(class == "Peu ou pas de faim", na.rm = TRUE),
         sum(class == "Faim mod\u00e9r\u00e9e", na.rm = TRUE),
         sum(class == "Faim s\u00e9v\u00e8re", na.rm = TRUE))
  list(
    score = score, class = class, precalc = TRUE,
    summary = tibble(
      Catégorie = c("Peu/pas de faim (0-1)", "Faim mod\u00e9r\u00e9e (2-3)", "Faim s\u00e9v\u00e8re (4-6)"),
      N = k, `%` = round(k / max(1, n_ok) * 100, 1),
      Couleur = c("#27ae60", "#f39c12", "#e74c3c")
    ),
    stats = sa_stats_5(score[valid])
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 14. INDICE COMPOSITE DE SÉVÉRITÉ + VERDICT EXPERT
# ══════════════════════════════════════════════════════════════════════════════

SA_SEVERITY_LEVELS <- data.frame(
  Classe  = c("S\u00e9curit\u00e9 alimentaire", "Sous pression", "Crise", "Urgence"),
  Couleur = c("#2E7D52", "#D4A843", "#D98B14", "#BF3636"),
  stringsAsFactors = FALSE
)

#' Indice composite 0-100 (50 % pire indicateur + 50 % moyenne des indicateurs)
calc_sa_severity_index <- function(fcs_res = NULL, rcsi_res = NULL,
                                   hdds_res = NULL, hhs_res = NULL) {
  risk_of <- function(cls, from, to) to[match(cls, from)]
  risks <- list()
  if (!is.null(fcs_res))
    risks$FCS <- risk_of(fcs_res$class,
      c("Pauvre", "Limite", "Acceptable"), c(100, 40, 0))
  if (!is.null(rcsi_res))
    risks$rCSI <- risk_of(rcsi_res$class,
      c("Coping \u00e9lev\u00e9", "Coping moyen", "Pas/Peu de coping"), c(100, 40, 0))
  if (!is.null(hdds_res))
    risks$HDDS <- risk_of(hdds_res$class,
      c("Faible diversit\u00e9", "Diversit\u00e9 moyenne", "Bonne diversit\u00e9"), c(100, 40, 0))
  if (!is.null(hhs_res))
    risks$HHS <- risk_of(hhs_res$class,
      c("Faim s\u00e9v\u00e8re", "Faim mod\u00e9r\u00e9e", "Peu ou pas de faim"), c(100, 40, 0))
  if (length(risks) == 0) return(NULL)

  m <- do.call(cbind, risks)
  risk_mean <- rowMeans(m, na.rm = TRUE)
  risk_max  <- apply(m, 1, max, na.rm = TRUE)
  idx <- round(0.5 * risk_max + 0.5 * risk_mean, 1)
  idx[!is.finite(idx)] <- NA_real_

  cls <- dplyr::case_when(
    is.na(idx)  ~ NA_character_,
    idx < 20    ~ "S\u00e9curit\u00e9 alimentaire",
    idx < 45    ~ "Sous pression",
    idx < 75    ~ "Crise",
    TRUE        ~ "Urgence"
  )
  n_ok <- sum(!is.na(cls))
  k <- vapply(SA_SEVERITY_LEVELS$Classe,
              function(l) sum(cls == l, na.rm = TRUE), integer(1))

  list(
    score = idx, class = cls, n_indicators = length(risks),
    summary = tibble(
      Classe = SA_SEVERITY_LEVELS$Classe,
      Couleur = SA_SEVERITY_LEVELS$Couleur,
      N = unname(k),
      `%` = round(unname(k) / max(1, n_ok) * 100, 1)
    ),
    stats = sa_stats_5(idx)
  )
}

#' Verdict global de sécurité alimentaire (badge + constats + recommandation)
calc_sa_verdict <- function(fcs_res = NULL, rcsi_res = NULL, hdds_res = NULL,
                            hhs_res = NULL, sev_res = NULL, ipc_summary = NULL) {
  worst <- list()
  add_worst <- function(nm, res, level) {
    if (is.null(res) || is.null(res$class)) return(invisible())
    valid <- !is.na(res$class)
    worst[[nm]] <<- c(k = sum(res$class == level, na.rm = TRUE),
                      n = sum(valid))
  }
  add_worst("FCS",  fcs_res,  "Pauvre")
  add_worst("rCSI", rcsi_res, "Coping \u00e9lev\u00e9")
  add_worst("HDDS", hdds_res, "Faible diversit\u00e9")
  add_worst("HHS",  hhs_res,  "Faim s\u00e9v\u00e8re")

  risk_pct <- NA_real_
  basis <- ""
  if (!is.null(sev_res) && !is.null(sev_res$summary)) {
    s <- sev_res$summary
    risk_pct <- sum(s$`%`[s$Classe %in% c("Crise", "Urgence")])
    basis <- "indice composite"
  } else if (length(worst) > 0) {
    shares <- vapply(worst, function(v) v[["k"]] / max(1, v[["n"]]) * 100, numeric(1))
    risk_pct <- max(shares)
    basis <- "indicateur le plus d\u00e9favorable"
  }

  q <- if (is.na(risk_pct)) list(lbl = "Non \u00e9valuable", col = "#6c757d", bg = "#f1f3f5")
       else if (risk_pct >= 40) list(lbl = "Situation critique", col = "#C80000", bg = "#fdf0f0")
       else if (risk_pct >= 20) list(lbl = "Sous pression",     col = "#E67800", bg = "#fff4e6")
       else if (risk_pct >= 10) list(lbl = "Vigilance",         col = "#B8860B", bg = "#fff9e6")
       else list(lbl = "S\u00e9curit\u00e9 alimentaire",        col = "#2E7D52", bg = "#eff8f1")

  bullets <- character(0)
  if (!is.null(fcs_res)) {
    k <- worst$FCS[["k"]]; n <- worst$FCS[["n"]]
    bullets <- c(bullets, sprintf(
      "FCS : %.1f %% en consommation pauvre (< 21), IC 95 %% [%s] ; %.1f %% en zone limite.",
      k / max(1, n) * 100, sa_ci_col(k, n),
      sum(fcs_res$class == "Limite", na.rm = TRUE) / max(1, n) * 100))
  }
  if (!is.null(rcsi_res)) {
    k <- worst$rCSI[["k"]]; n <- worst$rCSI[["n"]]
    bullets <- c(bullets, sprintf(
      "rCSI : %.1f %% de recours \u00e9lev\u00e9 aux strat\u00e9gies d'adaptation (\u2265 19), IC 95 %% [%s].",
      k / max(1, n) * 100, sa_ci_col(k, n)))
  }
  if (!is.null(hdds_res)) {
    k <- worst$HDDS[["k"]]; n <- worst$HDDS[["n"]]
    bullets <- c(bullets, sprintf(
      "HDDS : %.1f %% en faible diversit\u00e9 (\u2264 3 groupes/24 h), IC 95 %% [%s].",
      k / max(1, n) * 100, sa_ci_col(k, n)))
  }
  if (!is.null(hhs_res)) {
    k <- worst$HHS[["k"]]; n <- worst$HHS[["n"]]
    bullets <- c(bullets, sprintf(
      "HHS : %.1f %% en faim s\u00e9v\u00e8re (4-6), IC 95 %% [%s].",
      k / max(1, n) * 100, sa_ci_col(k, n)))
  }
  if (!is.null(ipc_summary) && nrow(ipc_summary) > 0) {
    p3 <- sum(ipc_summary$Pct[ipc_summary$Phase >= 3])
    p45 <- sum(ipc_summary$Pct[ipc_summary$Phase >= 4])
    bullets <- c(bullets, sprintf(
      "IPC/CH estim\u00e9 : %.1f %% en phase 3+ (dont %.1f %% en phase 4-5).", p3, p45))
  }
  if (length(worst) > 1) {
    shares <- vapply(worst, function(v) v[["k"]] / max(1, v[["n"]]) * 100, numeric(1))
    n_alert <- sum(shares >= 20)
    bullets <- c(bullets, if (n_alert >= 2)
      sprintf("%d/%d indicateurs d\u00e9passent 20 %% de m\u00e9nages en situation d\u00e9favorable : signaux convergents.",
              n_alert, length(worst))
      else "Les signaux restent dispers\u00e9s entre indicateurs : interpr\u00e9ter avec prudence (contexte, saison, \u00e9chantillon).")
  }
  n_obs <- max(vapply(worst, function(v) v[["n"]], numeric(1)), 0)
  if (n_obs > 0)
    bullets <- c(bullets, sprintf(
      "Base : %d m\u00e9nages \u00e9valu\u00e9s sur %d indicateur(s).", n_obs, length(worst)))

  rec <- if (is.na(risk_pct))
    "Mapper les colonnes des indicateurs puis relancer le calcul."
  else if (risk_pct >= 40)
    "Prioriser une r\u00e9ponse d'urgence : ciblage g\u00e9ographique, assistance alimentaire et suivi IPC renforc\u00e9."
  else if (risk_pct >= 20)
    "Renforcer le suivi et pr\u00e9-positionner la r\u00e9ponse ; analyser les strates les plus touch\u00e9es."
  else if (risk_pct >= 10)
    "Maintenir la surveillance ; v\u00e9rifier la convergence avec WASH, march\u00e9s et moyens d'existence."
  else
    "Situation globalement satisfaisante : poursuivre la surveillance r\u00e9guli\u00e8re des indicateurs."

  list(
    badge = q$lbl, color = q$col, bg = q$bg,
    risk_pct = risk_pct, basis = basis,
    headline = if (is.na(risk_pct))
      "S\u00e9curit\u00e9 alimentaire \u2014 aucun indicateur calcul\u00e9"
    else sprintf("%.1f %% des m\u00e9nages en situation d\u00e9favorable (%s).",
                 risk_pct, basis),
    bullets = bullets, recommendation = rec
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 15. LECTURES EXPERTES — interprétation automatique par indicateur
# ══════════════════════════════════════════════════════════════════════════════

sa_reading_fcs <- function(r) {
  if (is.null(r)) return(NULL)
  s <- r$summary
  get <- function(lbl) s$`%`[s$Catégorie == lbl][1]
  k_p <- get("Pauvre (< 21)"); k_l <- get("Limite (21-35)")
  moy <- round(mean(r$score, na.rm = TRUE), 1)
  n <- sum(!is.na(r$score))
  seuil <- if (!is.na(k_p) && k_p >= 20) "d\u00e9passe le seuil d'alerte usuel de 20 %"
           else if (!is.na(k_p) && k_p >= 10) "reste sous le seuil d'alerte de 20 % mais appelle une vigilance"
           else "reste faible"
  sprintf(paste0(
    "Lecture experte : %.1f %% des m\u00e9nages ont un FCS pauvre (< 21) et %.1f %% un FCS limite ; ",
    "score moyen = %.1f/112 (n = %d). La pr\u00e9valence de consommation pauvre %s. ",
    "R\u00e9f\u00e9rence PAM : ciblage prioritaire au-del\u00e0 de 20 %% de m\u00e9nages en FCS pauvre."),
    k_p, k_l, moy, n, seuil)
}

sa_reading_rcsi <- function(r) {
  if (is.null(r)) return(NULL)
  s <- r$summary
  get <- function(lbl) s$`%`[s$Catégorie == lbl][1]
  k_e <- get("\u00c9lev\u00e9 (\u2265 19)"); k_m <- get("Moyen (4-18)")
  moy <- round(mean(r$score, na.rm = TRUE), 1)
  sprintf(paste0(
    "Lecture experte : %.1f %% des m\u00e9nages recourent fortement aux strat\u00e9gies d'adaptation ",
    "(rCSI \u2265 19) et %.1f %% de fa\u00e7on mod\u00e9r\u00e9e (4-18) ; moyenne = %.1f/56. ",
    "Un rCSI \u00e9lev\u00e9 traduit une ins\u00e9curit\u00e9 alimentaire d\u00e9j\u00e0 install\u00e9e, souvent avant les baisses de consommation visibles."),
    k_e, k_m, moy)
}

sa_reading_hdds <- function(r) {
  if (is.null(r)) return(NULL)
  s <- r$summary
  get <- function(lbl) s$`%`[s$Catégorie == lbl][1]
  k_f <- get("Faible (\u2264 3)"); k_m <- get("Moyenne (4-5)")
  moy <- round(mean(r$score, na.rm = TRUE), 1)
  sprintf(paste0(
    "Lecture experte : %.1f %% des m\u00e9nages ont une diversit\u00e9 alimentaire faible (\u2264 3 groupes/24 h) ",
    "et %.1f %% moyenne ; moyenne = %.1f/12. Un HDDS \u2264 5 est g\u00e9n\u00e9ralement associ\u00e9 \u00e0 une ",
    "consommation peu diversifi\u00e9e et \u00e0 un risque nutritionnel accru."),
    k_f, k_m, moy)
}

sa_reading_hhs <- function(r) {
  if (is.null(r)) return(NULL)
  s <- r$summary
  get <- function(lbl) s$`%`[s$Catégorie == lbl][1]
  k_s <- get("Faim s\u00e9v\u00e8re (4-6)"); k_m <- get("Faim mod\u00e9r\u00e9e (2-3)")
  sprintf(paste0(
    "Lecture experte : %.1f %% des m\u00e9nages sont en faim s\u00e9v\u00e8re (HHS 4-6) et %.1f %% en faim mod\u00e9r\u00e9e. ",
    "R\u00e9f\u00e9rence FANTA/FAO : une pr\u00e9valence de faim s\u00e9v\u00e8re > 5 %% est consid\u00e9r\u00e9e comme alarmante ",
    "et impose une r\u00e9ponse imm\u00e9diate."),
    k_s, k_m)
}

sa_reading_severity <- function(sev) {
  if (is.null(sev)) return(NULL)
  s <- sev$summary
  get <- function(lbl) s$`%`[s$Classe == lbl][1]
  sprintf(paste0(
    "Indice composite de s\u00e9v\u00e9rit\u00e9 (0 = optimal, 100 = critique) : %.1f %% des m\u00e9nages en crise ",
    "ou urgence (%s %% en crise, %s %% en urgence) ; score moyen = %.1f. ",
    "Calcul\u00e9 sur %d indicateur(s) disponible(s), il agr\u00e8ge le pire indicateur et la moyenne du risque."),
    (get("Crise") + get("Urgence")), get("Crise"), get("Urgence"),
    round(mean(sev$score, na.rm = TRUE), 1), sev$n_indicators)
}

sa_reading_ipc <- function(ipc_summary) {
  if (is.null(ipc_summary) || nrow(ipc_summary) == 0) return(NULL)
  p3 <- sum(ipc_summary$Pct[ipc_summary$Phase >= 3])
  p45 <- sum(ipc_summary$Pct[ipc_summary$Phase >= 4])
  p2 <- sum(ipc_summary$Pct[ipc_summary$Phase == 2])
  sprintf(paste0(
    "Lecture experte : %.1f %% des m\u00e9nages sont estim\u00e9s en phase 3+ (crise ou pire), ",
    "dont %.1f %% en phase 4-5 (urgence/famine) ; %.1f %% en phase 2 (sous pression). ",
    "Cette classification est une approximation bas\u00e9e sur FCS/rCSI/HHS/MUAC \u2014 ",
    "la classification IPC officielle exige une triangulation multi-sources."),
    p3, p45, p2)
}

sa_reading_strat <- function(test, strat_label = "la strate") {
  if (is.null(test)) return(NULL)
  if (is.na(test$p))
    return(sprintf("Comparaison par %s : %s. Aucun test statistique n'a pu \u00eatre conduit.",
                   strat_label, test$note))
  sig <- if (test$p < 0.05)
    sprintf("les diff\u00e9rences entre groupes sont statistiquement significatives (%s)", fmt_p(test$p))
  else
    sprintf("aucune diff\u00e9rence significative n'est d\u00e9tect\u00e9e entre groupes (%s)", fmt_p(test$p))
  force <- if (!is.na(test$V) && test$V >= 0.3) "association forte"
           else if (!is.na(test$V) && test$V >= 0.1) "association mod\u00e9r\u00e9e"
           else "association faible"
  sprintf(paste0(
    "Lecture experte : %s (V de Cram\u00e9r = %s, %s ; n = %d). ",
    "La stratification guide le ciblage g\u00e9ographique et programmatique."),
    sig, ifelse(is.na(test$V), "\u2014", sprintf("%.3f", test$V)), force, test$n)
}

sa_reading_rounds <- function(trend) {
  if (is.null(trend) || nrow(trend$table) < 2) return(NULL)
  sprintf(paste0(
    "Lecture experte : la part de m\u00e9nages en %s passe de %.1f %% (%s) \u00e0 %.1f %% (%s), ",
    "soit %+.1f points de pourcentage ; %s. ",
    "Interpr\u00e9ter \u00e0 la lumi\u00e8re de la saisonnalit\u00e9 et des changements d'\u00e9chantillon entre rounds."),
    trend$label,
    trend$table$pct[1], trend$first,
    trend$table$pct[nrow(trend$table)], trend$last,
    trend$diff_pp, fmt_p(trend$p))
}

sa_reading_lcsi <- function(r) {
  if (is.null(r) || is.null(r$summary) || nrow(r$summary) == 0) return(NULL)
  s <- r$summary
  get <- function(lbl) s$Pct[s$Phase == lbl][1]
  sprintf(paste0(
    "Lecture experte : %.1f %% des m\u00e9nages appliquent des strat\u00e9gies de stress, ",
    "%.1f %% de crise et %.1f %% d'urgence (LCSI). ",
    "Les strat\u00e9gies d'urgence (vente d'actifs productifs, d\u00e9penses de sant\u00e9 sacrifi\u00e9es) ",
    "signalent un \u00e9puisement des moyens d'existence."),
    get("Stress"), get("Crise"), get("Urgence"))
}

sa_reading_fies <- function(r) {
  if (is.null(r) || is.null(r$pct_moderate_severe)) return(NULL)
  sprintf(paste0(
    "Lecture experte : %.1f %% des m\u00e9nages sont en ins\u00e9curit\u00e9 alimentaire mod\u00e9r\u00e9e ou s\u00e9v\u00e8re ",
    "(FIES \u2265 4) ; score moyen = %.1f/8 (n = %d). ",
    "La FIES (FAO) mesure l'exp\u00e9rience v\u00e9cue de l'ins\u00e9curit\u00e9 et se pr\u00eate aux comparaisons internationales."),
    r$pct_moderate_severe, r$mean_score, r$n_valid)
}

sa_reading_mddw <- function(r) {
  if (is.null(r) || is.na(r$pct)) return(NULL)
  sprintf(paste0(
    "Lecture experte : %.1f %% des femmes atteignent le seuil MDD-W (\u2265 5 groupes sur 24 h), ",
    "IC 95 %% [%.1f\u2013%.1f] (n = %d). ",
    "Un MDD-W faible traduit un risque de carences multiples, en particulier chez les femmes en \u00e2ge de procr\u00e9er."),
    r$pct, r$ci_lower, r$ci_upper, r$n_valid)
}

sa_reading_synthesis <- function(verdict) {
  if (is.null(verdict)) return(NULL)
  sprintf(paste0(
    "%s %s ",
    "Ce diagnostic agr\u00e8ge les seuils PAM/FAO/IPC ; les intervalles de confiance de Wilson sont fournis ",
    "pour chaque pr\u00e9valence. %s"),
    verdict$headline, verdict$recommendation,
    if (length(verdict$bullets) > 0) "D\u00e9tail dans les constats ci-dessus." else "")
}

# ══════════════════════════════════════════════════════════════════════════════
# 16. GRAPHIQUES EXPERTES — LCSI · FIES · MDD-W (plotly)
# ══════════════════════════════════════════════════════════════════════════════

plot_lcsi_plotly <- function(lcsi_res) {
  if (is.null(lcsi_res) || is.null(lcsi_res$summary) || nrow(lcsi_res$summary) == 0)
    return(plotly_empty() |> layout(title = list(text = "Donn\u00e9es LCSI insuffisantes", x = .5)))
  df <- lcsi_res$summary
  df$Phase <- factor(df$Phase, levels = LCSI_LEVELS$Phase)
  plot_ly(
    df, x = ~Pct, y = ~"LCSI", type = "bar", orientation = "h",
    color = ~Phase,
    colors = stats::setNames(LCSI_LEVELS$Couleur, LCSI_LEVELS$Phase),
    text = ~ifelse(Pct >= 3, paste0(Pct, "%"), ""),
    textposition = "inside", insidetextanchor = "middle",
    hovertext = ~paste0(Phase, " : ", N, " m\u00e9nages (", Pct, "%)"),
    hoverinfo = "text"
  ) |>
    layout(
      barmode = "stack",
      title = list(text = "<b>LCSI \u2014 Strat\u00e9gies de subsistance</b><br><sup>Classification PAM (stress / crise / urgence)</sup>",
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "% ménages", ticksuffix = "%", range = c(0, 100)),
      yaxis = list(title = "", showticklabels = FALSE),
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.25),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(l = 20, b = 90)
    )
}

plot_fies_plotly <- function(fies_res) {
  if (is.null(fies_res) || is.null(fies_res$score))
    return(plotly_empty() |> layout(title = list(text = "Donn\u00e9es FIES insuffisantes", x = .5)))
  score <- fies_res$score[!is.na(fies_res$score)]
  if (length(score) == 0)
    return(plotly_empty() |> layout(title = list(text = "Donn\u00e9es FIES insuffisantes", x = .5)))
  cnt <- as.data.frame(table(factor(score, levels = 0:8)), stringsAsFactors = FALSE)
  names(cnt) <- c("Score", "N")
  cnt$Score <- as.integer(cnt$Score)
  sev_col <- function(s) if (s == 0) "#27ae60" else if (s <= 3) "#f39c12"
                          else if (s <= 6) "#e67e22" else "#e74c3c"
  plot_ly(
    cnt, x = ~Score, y = ~N, type = "bar",
    marker = list(color = vapply(cnt$Score, sev_col, character(1)),
                  line = list(color = "white", width = 0.5)),
    text = ~N, textposition = "outside",
    hovertemplate = "FIES %{x} : %{y} m\u00e9nages<extra></extra>"
  ) |>
    layout(
      title = list(text = "<b>FIES \u2014 Exp\u00e9rience de l'ins\u00e9curit\u00e9 alimentaire</b><br><sup>0 = s\u00e9curit\u00e9 \u00b7 4-6 = mod\u00e9r\u00e9e/\u00e9lev\u00e9e \u00b7 7-8 = s\u00e9v\u00e8re</sup>",
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "Score FIES (0-8)", dtick = 1),
      yaxis = list(title = "Effectif"),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(t = 70)
    )
}

plot_mddw_plotly <- function(mddw_res) {
  if (is.null(mddw_res) || is.null(mddw_res$summary))
    return(plotly_empty() |> layout(title = list(text = "Donn\u00e9es MDD-W insuffisantes", x = .5)))
  df <- mddw_res$summary
  plot_ly(
    df, labels = ~Catégorie, values = ~N, type = "pie", hole = 0.6,
    marker = list(colors = df$Couleur, line = list(color = "#fff", width = 2)),
    textinfo = "label+percent", textposition = "outside",
    hoverinfo = "label+value+percent"
  ) |>
    layout(
      title = list(
        text = paste0("<b>MDD-W \u2014 Diversit\u00e9 alimentaire des femmes</b><br><sup>",
                      mddw_res$pct, "% ad\u00e9quat \u00b7 IC 95 % [", mddw_res$ci_lower,
                      "\u2013", mddw_res$ci_upper, "] \u00b7 n = ", mddw_res$n_valid, "</sup>"),
        font = list(size = 13, color = "#0B3954"), x = 0.5),
      showlegend = FALSE,
      margin = list(t = 70)
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# 17. TABLE MÉNAGES + FRAMES D'EXPORT
# ══════════════════════════════════════════════════════════════════════════════

build_sa_household_table <- function(d, fcs_res = NULL, rcsi_res = NULL,
                                     hdds_res = NULL, hhs_res = NULL,
                                     lcsi_res = NULL, fies_res = NULL,
                                     mddw_res = NULL, sev_res = NULL,
                                     ipc_phases = NULL) {
  n <- nrow(d)
  id_cols <- intersect(c("household_id", "cluster_id", "region", "department",
                          "survey_round"), names(d))
  tbl <- as.data.frame(d[, id_cols, drop = FALSE])
  add <- function(nm, x) if (!is.null(x) && length(x) == n) x else NULL
  x <- add("FCS", fcs_res$score);                 if (!is.null(x)) { tbl$FCS <- x; tbl$FCS_classe <- fcs_res$class }
  x <- add("rCSI", rcsi_res$score);               if (!is.null(x)) { tbl$rCSI <- x; tbl$rCSI_classe <- rcsi_res$class }
  x <- add("HDDS", hdds_res$score);               if (!is.null(x)) { tbl$HDDS <- x; tbl$HDDS_classe <- hdds_res$class }
  x <- add("HHS", hhs_res$score);                 if (!is.null(x)) { tbl$HHS <- x; tbl$HHS_classe <- hhs_res$class }
  x <- add("LCSI", lcsi_res$classification);      if (!is.null(x)) tbl$LCSI_classe <- x
  x <- add("FIES", fies_res$score);               if (!is.null(x)) { tbl$FIES <- x; tbl$FIES_classe <- fies_res$classification }
  x <- add("MDD-W", mddw_res$classification);     if (!is.null(x)) tbl$MDD_W_classe <- x
  x <- add("Sev", sev_res$score);                 if (!is.null(x)) { tbl$Indice_severite <- x; tbl$Niveau_severite <- sev_res$class }
  x <- add("IPC", ipc_phases);                    if (!is.null(x)) tbl$IPC_phase <- paste0("Phase ", x)
  as_tibble(tbl)
}

sa_export_frames <- function(fcs_res = NULL, rcsi_res = NULL, hdds_res = NULL,
                              hhs_res = NULL, lcsi_res = NULL, fies_res = NULL,
                              mddw_res = NULL, sev_res = NULL, ipc_summary = NULL,
                              hh_tbl = NULL, verdict = NULL) {
  sheets <- list()
  add_summary <- function(nm, r) {
    if (is.null(r) || is.null(r$summary)) return(invisible())
    s <- sa_add_ci(r$summary)
    s <- s[, setdiff(names(s), "Couleur"), drop = FALSE]
    sheets[[nm]] <<- s
  }
  add_summary("FCS", fcs_res)
  add_summary("rCSI", rcsi_res)
  add_summary("HDDS", hdds_res)
  add_summary("HHS", hhs_res)
  add_summary("LCSI", lcsi_res)
  add_summary("FIES", fies_res)
  if (!is.null(mddw_res) && !is.null(mddw_res$summary))
    sheets[["MDD-W"]] <- mddw_res$summary
  if (!is.null(sev_res) && !is.null(sev_res$summary)) {
    s <- sev_res$summary
    sheets[["Indice severite"]] <- s[, setdiff(names(s), "Couleur"), drop = FALSE]
  }
  if (!is.null(ipc_summary) && nrow(ipc_summary) > 0)
    sheets[["IPC CH"]] <- ipc_summary[, setdiff(names(ipc_summary), "Couleur"), drop = FALSE]
  if (!is.null(verdict))
    sheets[["Verdict"]] <- data.frame(
      Rubrique = c("Niveau", "Constats", "Recommandation"),
      Contenu = c(verdict$badge,
                  paste(verdict$bullets, collapse = " | "),
                  verdict$recommendation),
      stringsAsFactors = FALSE)
  if (!is.null(hh_tbl))
    sheets[["Menages"]] <- as.data.frame(hh_tbl)
  sheets
}

cat("module_secu_alimentaire.R chargé — FCS · rCSI · HDDS · HHS · LCSI · FIES · MDD-W · IPC/CH · verdict expert · inférence\n")
