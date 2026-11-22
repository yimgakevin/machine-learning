# module_rdim_expert.R -- Extensions expert pour la reduction de dimension
# ═══════════════════════════════════════════════════════════════════════════════
# Nouvelles techniques : t-SNE | UMAP | Analyse Parallele (Horn)
#                        HCPC  | Bootstrap stabilite des loadings
#                        Biplot ameliore | Variables supplementaires
# ═══════════════════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  for (pkg in c("Rtsne", "uwot")) {
    tryCatch(library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
             error = function(e) message("\u26a0\ufe0f ", pkg, " non disponible"))
  }
})

# ═══════════════════════════════════════════════════════════════════════════════
# 1. t-SNE (t-distributed Stochastic Neighbor Embedding)
# ═══════════════════════════════════════════════════════════════════════════════

#' @param d data.frame de variables numeriques (NA exclus)
#' @param perplexity parametre de perplexite (5-50, defaut 30)
#' @param dims dimensions de sortie (2 ou 3)
#' @param max_iter iterations maximum
#' @param seed graine aleatoire
run_tsne <- function(d, perplexity = 30, dims = 2, max_iter = 1000, seed = 3847) {
  if (!requireNamespace("Rtsne", quietly = TRUE))
    stop("Le package Rtsne est requis. install.packages('Rtsne')")

  d_num <- d |> select(where(is.numeric)) |> tidyr::drop_na()
  if (nrow(d_num) < 10) stop("t-SNE requiert au moins 10 observations compl\u00e8tes")

  # Perplexite doit etre < n/3

  perp <- min(perplexity, floor((nrow(d_num) - 1) / 3))
  if (perp < 2) perp <- 2

  # Supprimer les doublons stricts (Rtsne echoue sinon)
  d_scaled <- scale(d_num)
  dupl     <- duplicated(d_scaled)
  if (sum(dupl) > 0) {
    d_scaled <- d_scaled[!dupl, , drop = FALSE]
    d_num    <- d_num[!dupl, , drop = FALSE]
  }

  set.seed(seed)
  res <- Rtsne::Rtsne(d_scaled, dims = dims, perplexity = perp,
                       max_iter = max_iter, check_duplicates = FALSE,
                       verbose = FALSE, normalize = FALSE)

  coords <- as.data.frame(res$Y)
  names(coords) <- paste0("tSNE_", seq_len(dims))
  coords$.row <- seq_len(nrow(coords))

  list(
    coords      = coords,
    perplexity  = perp,
    n_obs       = nrow(d_scaled),
    n_vars      = ncol(d_scaled),
    kl_divergence = res$itercosts[length(res$itercosts)],
    data_used   = d_num
  )
}

#' @param tsne_res resultat de run_tsne()
#' @param group_var vecteur facteur/character pour colorer (optionnel)
plot_tsne <- function(tsne_res, group_var = NULL) {
  df <- tsne_res$coords
  if (!is.null(group_var) && length(group_var) == nrow(df)) {
    df$Groupe <- as.factor(group_var)
    p <- ggplot(df, aes(x = tSNE_1, y = tSNE_2, color = Groupe)) +
      geom_point(alpha = 0.7, size = 2) +
      stat_ellipse(level = 0.90, linetype = "dashed", linewidth = 0.6)
  } else {
    p <- ggplot(df, aes(x = tSNE_1, y = tSNE_2)) +
      geom_point(alpha = 0.7, size = 2, color = "#008DC9")
  }
  p <- p +
    labs(x = "t-SNE Dim 1", y = "t-SNE Dim 2",
         title = paste0("t-SNE (perplexit\u00e9 = ", tsne_res$perplexity,
                        ", n = ", tsne_res$n_obs, ")"),
         subtitle = paste0("KL divergence = ",
                           round(tsne_res$kl_divergence, 4))) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p)
}

# Sensibilite a la perplexite (grille)
plot_tsne_perplexity_grid <- function(d, perplexities = c(5, 15, 30, 50),
                                       group_var = NULL, seed = 3847) {
  plots <- lapply(perplexities, function(perp) {
    res <- tryCatch(run_tsne(d, perplexity = perp, seed = seed),
                    error = function(e) NULL)
    if (is.null(res)) return(plotly_empty())
    plot_tsne(res, group_var)
  })
  subplot(plots, nrows = 2, shareX = FALSE, shareY = FALSE,
          margin = 0.06, titleX = TRUE, titleY = TRUE)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 2. UMAP (Uniform Manifold Approximation and Projection)
# ═══════════════════════════════════════════════════════════════════════════════

#' @param n_neighbors nombre de voisins (5-50, defaut 15)
#' @param min_dist distance minimum (0.01-0.99, defaut 0.1)
#' @param metric distance : "euclidean", "cosine", "manhattan"
run_umap <- function(d, n_neighbors = 15, min_dist = 0.1,
                     metric = "euclidean", seed = 3847) {
  if (!requireNamespace("uwot", quietly = TRUE))
    stop("Le package uwot est requis. install.packages('uwot')")

  d_num <- d |> select(where(is.numeric)) |> tidyr::drop_na()
  if (nrow(d_num) < 10) stop("UMAP requiert au moins 10 observations compl\u00e8tes")

  nn <- min(n_neighbors, nrow(d_num) - 1)
  if (nn < 2) nn <- 2

  d_scaled <- scale(d_num)
  set.seed(seed)

  res <- uwot::umap(d_scaled, n_neighbors = nn, min_dist = min_dist,
                    metric = metric, n_components = 2, ret_model = FALSE,
                    verbose = FALSE)

  coords <- as.data.frame(res)
  names(coords) <- c("UMAP_1", "UMAP_2")
  coords$.row <- seq_len(nrow(coords))

  list(
    coords       = coords,
    n_neighbors  = nn,
    min_dist     = min_dist,
    metric       = metric,
    n_obs        = nrow(d_scaled),
    n_vars       = ncol(d_scaled),
    data_used    = d_num
  )
}

plot_umap <- function(umap_res, group_var = NULL) {
  df <- umap_res$coords
  if (!is.null(group_var) && length(group_var) == nrow(df)) {
    df$Groupe <- as.factor(group_var)
    p <- ggplot(df, aes(x = UMAP_1, y = UMAP_2, color = Groupe)) +
      geom_point(alpha = 0.7, size = 2) +
      stat_ellipse(level = 0.90, linetype = "dashed", linewidth = 0.6)
  } else {
    p <- ggplot(df, aes(x = UMAP_1, y = UMAP_2)) +
      geom_point(alpha = 0.7, size = 2, color = "#4dac26")
  }
  p <- p +
    labs(x = "UMAP 1", y = "UMAP 2",
         title = paste0("UMAP (voisins = ", umap_res$n_neighbors,
                        ", min_dist = ", umap_res$min_dist, ")"),
         subtitle = paste0("n = ", umap_res$n_obs,
                           " \u00b7 distance : ", umap_res$metric)) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 3. ANALYSE PARALLELE (Horn's Parallel Analysis)
# ═══════════════════════════════════════════════════════════════════════════════

#' Determine le nombre d'axes significatifs en comparant les valeurs propres
#' reelles a celles obtenues par permutation de donnees aleatoires.
#' @param d data.frame de variables numeriques (sans NA)
#' @param n_sim nombre de simulations (defaut 500)
#' @param quantile_thresh quantile des simulations pour le seuil (defaut 0.95)
calc_parallel_analysis <- function(d, n_sim = 500, quantile_thresh = 0.95, seed = 3847) {
  set.seed(seed)
  d_num <- d |> select(where(is.numeric)) |> tidyr::drop_na()
  n     <- nrow(d_num)
  p     <- ncol(d_num)
  if (n < 5 || p < 2) stop("Donn\u00e9es insuffisantes pour l'analyse parall\u00e8le")

  d_scaled  <- scale(d_num)
  real_eig  <- eigen(cor(d_scaled))$values

  # Simulations : valeurs propres de donnees aleatoires normales
  sim_mat <- replicate(n_sim, {
    d_rand <- matrix(rnorm(n * p), nrow = n, ncol = p)
    eigen(cor(d_rand))$values
  })

  # Quantiles des simulations
  sim_q95  <- apply(sim_mat, 1, quantile, probs = quantile_thresh)
  sim_mean <- rowMeans(sim_mat)

  # Nombre d'axes retenus : lambda reelle > quantile 95% simulations
  n_retained <- sum(real_eig > sim_q95)
  if (n_retained < 1) n_retained <- 1

  tibble_res <- tibble(
    Dimension    = seq_len(p),
    Lambda_reelle = round(real_eig, 4),
    Sim_moyenne   = round(sim_mean, 4),
    Sim_Q95       = round(sim_q95, 4),
    Retenu        = real_eig > sim_q95
  )

  list(
    table       = tibble_res,
    n_retained  = n_retained,
    real_eig    = real_eig,
    sim_mean    = sim_mean,
    sim_q95     = sim_q95,
    n_sim       = n_sim,
    n_obs       = n,
    n_vars      = p
  )
}

plot_parallel_analysis <- function(pa_res) {
  if (is.null(pa_res)) return(plotly_empty())
  df <- pa_res$table |>
    tidyr::pivot_longer(c(Lambda_reelle, Sim_moyenne, Sim_Q95),
                        names_to = "Type", values_to = "Lambda") |>
    mutate(Type = dplyr::recode(Type,
      "Lambda_reelle" = "Valeurs propres r\u00e9elles",
      "Sim_moyenne"   = "Simulations (moyenne)",
      "Sim_Q95"       = "Simulations (Q95%)"
    ))

  p <- ggplot(df, aes(x = Dimension, y = Lambda, color = Type, linetype = Type)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = pa_res$n_retained + 0.5, linetype = "dashed",
               color = "gray40", linewidth = 0.7) +
    annotate("text", x = pa_res$n_retained + 0.7, y = max(pa_res$real_eig) * 0.9,
             label = paste0("Retenus : ", pa_res$n_retained, " axes"),
             color = "gray30", size = 3.5, hjust = 0) +
    scale_color_manual(values = c(
      "Valeurs propres r\u00e9elles" = "#008DC9",
      "Simulations (moyenne)"  = "#e88400",
      "Simulations (Q95%)" = "#c9161d"
    )) +
    scale_linetype_manual(values = c(
      "Valeurs propres r\u00e9elles" = "solid",
      "Simulations (moyenne)"  = "dashed",
      "Simulations (Q95%)" = "dotted"
    )) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray70") +
    labs(x = "Dimension", y = "Valeur propre (\u03bb)",
         title = paste0("Analyse parall\u00e8le de Horn (",
                        pa_res$n_sim, " simulations)"),
         subtitle = paste0(pa_res$n_retained, " axe",
                           if (pa_res$n_retained > 1) "s" else "",
                           " significatif",
                           if (pa_res$n_retained > 1) "s" else "",
                           " (n = ", pa_res$n_obs,
                           ", p = ", pa_res$n_vars, ")"),
         color = "", linetype = "") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.25))
}

# ═══════════════════════════════════════════════════════════════════════════════
# 4. HCPC (Classification Hierarchique sur Composantes Principales)
# ═══════════════════════════════════════════════════════════════════════════════

#' @param pca_res objet PCA() de FactoMineR
#' @param nb_clust nombre de clusters (0 = automatique)
#' @param min_clust minimum de clusters pour le choix auto
#' @param max_clust maximum de clusters pour le choix auto
run_hcpc <- function(pca_res, nb_clust = 0, min_clust = 2, max_clust = 10) {
  if (!inherits(pca_res, c("PCA", "MCA", "FAMD")))
    stop("L'entr\u00e9e doit \u00eatre un objet PCA(), MCA() ou FAMD() de FactoMineR")

  res <- FactoMineR::HCPC(pca_res, nb.clust = nb_clust,
                           min = min_clust, max = max_clust,
                           graph = FALSE, consol = TRUE)

  # Extraire les clusters
  clusters <- res$data.clust$clust
  n_clust  <- nlevels(clusters)

  # Inertie inter / inertie totale (critere qualite)
  inertia_ratio <- tryCatch({
    ss_total <- sum(scale(pca_res$ind$coord, scale = FALSE)^2)
    means_cl <- tapply(seq_along(clusters), clusters, function(idx) {
      colMeans(pca_res$ind$coord[idx, , drop = FALSE])
    })
    means_mat <- do.call(rbind, means_cl)
    n_per_cl  <- table(clusters)
    ss_between <- sum(sweep(means_mat, 2, colMeans(pca_res$ind$coord))^2 *
                        as.numeric(n_per_cl))
    round(ss_between / ss_total * 100, 1)
  }, error = function(e) NA_real_)

  list(
    hcpc         = res,
    clusters     = clusters,
    n_clust      = n_clust,
    inertia_pct  = inertia_ratio,
    desc_axes    = tryCatch(res$desc.axes, error = function(e) NULL),
    desc_var     = tryCatch(res$desc.var, error = function(e) NULL)
  )
}

#' Carte factorielle coloree par cluster HCPC
plot_hcpc_map <- function(pca_res, hcpc_res) {
  if (is.null(hcpc_res)) return(plotly_empty())
  coords <- as.data.frame(pca_res$ind$coord[, 1:2])
  names(coords) <- c("Dim1", "Dim2")
  coords$Cluster <- hcpc_res$clusters
  eig <- factoextra::get_eigenvalue(pca_res)
  lab_x <- paste0("Dim 1 (", round(eig[1, 2], 1), "%)")
  lab_y <- paste0("Dim 2 (", round(eig[2, 2], 1), "%)")
  pal <- c("#008DC9", "#c9161d", "#4dac26", "#7b2d8b", "#e88400",
           "#00A676", "#FF6B6B", "#6C5B7B", "#F7DC6F", "#1ABC9C")

  p <- ggplot(coords, aes(x = Dim1, y = Dim2, color = Cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    stat_ellipse(level = 0.90, linewidth = 0.8, linetype = "dashed") +
    scale_color_manual(values = pal[seq_len(nlevels(coords$Cluster))]) +
    labs(x = lab_x, y = lab_y,
         title = paste0("HCPC \u2014 ", hcpc_res$n_clust, " clusters"),
         subtitle = paste0("Inertie inter-classes : ",
                           hcpc_res$inertia_pct, "%")) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p)
}

#' Dendrogramme HCPC
plot_hcpc_dendrogram <- function(hcpc_res) {
  if (is.null(hcpc_res)) return(plotly_empty())
  tryCatch({
    tree <- hcpc_res$hcpc$call$t$tree
    p <- factoextra::fviz_dend(tree, k = hcpc_res$n_clust,
                                rect = TRUE, rect_fill = TRUE,
                                rect_border = "gray60",
                                k_colors = c("#008DC9", "#c9161d", "#4dac26",
                                             "#7b2d8b", "#e88400"),
                                cex = 0.5, main = "Dendrogramme HCPC",
                                xlab = "", ylab = "Hauteur") +
      theme_minimal(base_size = 10) + theme(legend.position = "none")
    ggplotly(p)
  }, error = function(e) plotly_empty())
}

#' Tableau de caracterisation des clusters (variables quanti)
hcpc_cluster_description <- function(hcpc_res) {
  if (is.null(hcpc_res) || is.null(hcpc_res$desc_var)) return(NULL)
  dv <- hcpc_res$desc_var$quanti
  if (is.null(dv)) return(NULL)
  purrr::map_dfr(names(dv), function(cl_name) {
    tbl <- tryCatch(as.data.frame(dv[[cl_name]]), error = function(e) NULL)
    if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
    tbl |>
      tibble::rownames_to_column("Variable") |>
      mutate(Cluster = cl_name) |>
      select(Cluster, Variable, everything()) |>
      mutate(across(where(is.numeric), ~round(., 3)))
  })
}

# ═══════════════════════════════════════════════════════════════════════════════
# 5. BOOTSTRAP STABILITE DES LOADINGS (ACP)
# ═══════════════════════════════════════════════════════════════════════════════

#' Evalue la stabilite des loadings PCA par bootstrap
#' @param d data.frame de variables numeriques
#' @param n_boot nombre de reechantillonnages (defaut 500)
#' @param n_dim nombre de dimensions a evaluer
calc_pca_bootstrap <- function(d, n_boot = 500, n_dim = 3, seed = 3847) {
  set.seed(seed)
  d_num <- d |> select(where(is.numeric)) |> tidyr::drop_na()
  n     <- nrow(d_num)
  p     <- ncol(d_num)
  if (n < 10 || p < 2) stop("Donn\u00e9es insuffisantes pour le bootstrap")

  nd <- min(n_dim, p - 1)

  # Loadings de reference
  pca_ref  <- prcomp(d_num, center = TRUE, scale. = TRUE)
  ref_load <- pca_ref$rotation[, seq_len(nd), drop = FALSE]

  # Bootstrap
  boot_loads <- array(NA, dim = c(p, nd, n_boot))
  for (b in seq_len(n_boot)) {
    idx <- sample(n, replace = TRUE)
    pca_b <- tryCatch(prcomp(d_num[idx, ], center = TRUE, scale. = TRUE),
                      error = function(e) NULL)
    if (!is.null(pca_b)) {
      ld <- pca_b$rotation[, seq_len(min(nd, ncol(pca_b$rotation))), drop = FALSE]
      # Aligner les signes (Procrustes simplifie)
      for (j in seq_len(ncol(ld))) {
        if (j <= ncol(ref_load) && cor(ld[, j], ref_load[, j]) < 0)
          ld[, j] <- -ld[, j]
      }
      boot_loads[, seq_len(ncol(ld)), b] <- ld
    }
  }

  # Calcul IC 95% et SD
  results <- purrr::map_dfr(seq_len(nd), function(dim_j) {
    purrr::map_dfr(seq_len(p), function(var_i) {
      vals <- boot_loads[var_i, dim_j, ]
      vals <- vals[!is.na(vals)]
      tibble(
        Variable  = colnames(d_num)[var_i],
        Dimension = paste0("Dim.", dim_j),
        Loading   = round(ref_load[var_i, dim_j], 4),
        Boot_Mean = round(mean(vals), 4),
        Boot_SD   = round(sd(vals), 4),
        IC_lo     = round(quantile(vals, 0.025), 4),
        IC_hi     = round(quantile(vals, 0.975), 4),
        Stable    = abs(quantile(vals, 0.025)) > 0.05 |
                    abs(quantile(vals, 0.975)) > 0.05
      )
    })
  })

  list(table = results, n_boot = n_boot, n_dim = nd, n_vars = p)
}

plot_pca_bootstrap <- function(boot_res, dim_selected = "Dim.1") {
  if (is.null(boot_res)) return(plotly_empty())
  df <- boot_res$table |>
    filter(Dimension == dim_selected) |>
    arrange(desc(abs(Loading)))

  p <- ggplot(df, aes(y = fct_reorder(Variable, abs(Loading)),
                      x = Loading)) +
    geom_point(aes(color = Stable), size = 3) +
    geom_errorbarh(aes(xmin = IC_lo, xmax = IC_hi), height = 0.3, color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    scale_color_manual(values = c("TRUE" = "#008DC9", "FALSE" = "#c9161d"),
                       labels = c("TRUE" = "Stable", "FALSE" = "Instable")) +
    labs(y = "", x = "Loading (poids factoriel)",
         title = paste0("Stabilit\u00e9 bootstrap des loadings \u2014 ", dim_selected),
         subtitle = paste0("IC 95% sur ", boot_res$n_boot, " r\u00e9\u00e9chantillonnages"),
         color = "") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 6. BIPLOT AMELIORE (individus + vecteurs variables)
# ═══════════════════════════════════════════════════════════════════════════════

#' Biplot publication-grade : individus + fleches variables + ellipses
plot_enhanced_biplot <- function(pca_res, group_var = NULL,
                                 dim_x = 1, dim_y = 2,
                                 top_n_vars = 10, arrow_scale = 1) {
  ind  <- as.data.frame(pca_res$ind$coord[, c(dim_x, dim_y)])
  names(ind) <- c("Dim1", "Dim2")
  var  <- as.data.frame(pca_res$var$coord[, c(dim_x, dim_y)])
  names(var) <- c("Dim1", "Dim2")
  var$Variable <- rownames(var)
  var$Norme    <- sqrt(var$Dim1^2 + var$Dim2^2)

  # Garder les top_n_vars variables les plus contributrices
  var <- var |> arrange(desc(Norme)) |> slice_head(n = top_n_vars)

  # Scaler les fleches pour etre visibles sur le biplot
  ind_range <- max(abs(range(ind$Dim1, ind$Dim2, na.rm = TRUE)))
  var_range <- max(abs(range(var$Dim1, var$Dim2, na.rm = TRUE)))
  scale_f   <- (ind_range / var_range) * 0.8 * arrow_scale
  var$Dim1  <- var$Dim1 * scale_f
  var$Dim2  <- var$Dim2 * scale_f

  eig <- factoextra::get_eigenvalue(pca_res)
  lab_x <- paste0("Dim ", dim_x, " (", round(eig[dim_x, 2], 1), "%)")
  lab_y <- paste0("Dim ", dim_y, " (", round(eig[dim_y, 2], 1), "%)")

  if (!is.null(group_var) && length(group_var) == nrow(ind)) {
    ind$Groupe <- as.factor(group_var)
    p <- ggplot(ind, aes(x = Dim1, y = Dim2, color = Groupe)) +
      geom_point(alpha = 0.5, size = 1.5) +
      stat_ellipse(level = 0.90, linewidth = 0.6, linetype = "dashed")
  } else {
    p <- ggplot(ind, aes(x = Dim1, y = Dim2)) +
      geom_point(alpha = 0.5, size = 1.5, color = "#AAAAAA")
  }

  p <- p +
    geom_segment(data = var, aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
                 arrow = arrow(length = unit(0.15, "cm")),
                 color = "#c9161d", linewidth = 0.7, inherit.aes = FALSE) +
    geom_text(data = var, aes(x = Dim1 * 1.08, y = Dim2 * 1.08, label = Variable),
              color = "#c9161d", size = 3, fontface = "bold", inherit.aes = FALSE) +
    geom_hline(yintercept = 0, color = "gray80", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "gray80", linetype = "dashed") +
    labs(x = lab_x, y = lab_y,
         title = "Biplot am\u00e9lior\u00e9 (individus + variables)",
         subtitle = paste0("Top ", nrow(var), " variables les plus contributrices")) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggplotly(p)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 7. COMPARAISON METHODES (PCA vs t-SNE vs UMAP)
# ═══════════════════════════════════════════════════════════════════════════════

#' Compare les 3 representations 2D cote a cote
plot_methods_comparison <- function(d, group_var = NULL, seed = 3847) {
  d_num <- d |> select(where(is.numeric)) |> tidyr::drop_na()
  n <- nrow(d_num)

  # PCA
  pca_res <- prcomp(d_num, center = TRUE, scale. = TRUE)
  df_pca <- data.frame(X = pca_res$x[, 1], Y = pca_res$x[, 2])
  ev <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)

  # t-SNE
  df_tsne <- tryCatch({
    ts <- run_tsne(d, perplexity = min(30, floor((n-1)/3)), seed = seed)
    data.frame(X = ts$coords$tSNE_1, Y = ts$coords$tSNE_2)
  }, error = function(e) data.frame(X = rep(NA, n), Y = rep(NA, n)))

  # UMAP
  df_umap <- tryCatch({
    um <- run_umap(d, n_neighbors = min(15, n-1), seed = seed)
    data.frame(X = um$coords$UMAP_1, Y = um$coords$UMAP_2)
  }, error = function(e) data.frame(X = rep(NA, n), Y = rep(NA, n)))

  # Combiner
  df_all <- bind_rows(
    df_pca  |> mutate(Method = paste0("ACP (", ev[1], "% + ", ev[2], "%)")),
    df_tsne |> mutate(Method = "t-SNE"),
    df_umap |> mutate(Method = "UMAP")
  )
  if (!is.null(group_var) && length(group_var) == n) {
    grp <- rep(group_var, 3)
    df_all$Groupe <- as.factor(grp)
    p <- ggplot(df_all, aes(x = X, y = Y, color = Groupe)) +
      geom_point(alpha = 0.6, size = 1.5)
  } else {
    p <- ggplot(df_all, aes(x = X, y = Y)) +
      geom_point(alpha = 0.6, size = 1.5, color = "#008DC9")
  }
  p <- p +
    facet_wrap(~Method, scales = "free") +
    labs(x = "", y = "", title = "Comparaison ACP vs t-SNE vs UMAP") +
    theme_minimal(base_size = 10) + theme(legend.position = "bottom")
  ggplotly(p, height = 400)
}

cat("module_rdim_expert.R charg\u00e9 (t-SNE | UMAP | Horn | HCPC | Bootstrap | Biplot)\n")
