# Vérifie le module réduction de dimension en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_rdim_ui("rdim"))
ids <- c(
  "rdim-rdim_method", "rdim-rdim_render_mode", "rdim-rdim_vars_ui",
  "rdim-run_rdim", "rdim-rdim_table", "rdim-rdim_scree_ui",
  "rdim-rdim_circle_ui", "rdim-rdim_biplot_ui", "rdim-rdim_contrib_ui",
  "rdim-dl_rdim_scree", "rdim-tsne_plot", "rdim-umap_plot",
  "rdim-hcpc_dendro", "rdim-cmp_methods_plot", "rdim-rdim_method_info"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_RDIM_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_RDIM_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(51)
n <- 60
d <- data.frame(
  x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n), x5 = rnorm(n),
  sexe = rep(c("M", "F"), n / 2),
  region = rep(c("Nord", "Sud"), length.out = n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_rdim_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = shiny::reactive(c("sexe", "region")),
      vars_num = shiny::reactive(c("x1", "x2", "x3", "x4", "x5")),
      mapped_data = shiny::reactive(d),
      gpal = function(n, type = "cat") grDevices::rainbow(n),
      gprimary = function() "#0077B6"
    ),
    {
      session$setInputs(
        rdim_method = "pca",
        rdim_vars = c("x1", "x2", "x3", "x4"),
        rdim_ndim = 2,
        rdim_scale = TRUE,
        rdim_render_mode = "plotly",
        rdim_grp = "",
        rdim_biplot_grp = "",
        rdim_contrib_dim = 1,
        rdim_circle_color = "cos2",
        rdim_ind_color = "grp",
        rdim_axes_x = "Dim.1",
        rdim_axes_y = "Dim.2",
        rdim_contrib_thresh = TRUE,
        run_rdim = 1
      )
      cat("rdim_vars_ui    :", class(output$rdim_vars_ui)[1], "\n")
      cat("rdim_kaiser_badge:", class(output$rdim_kaiser_badge)[1], "\n")
      cat("rdim_table      :", class(output$rdim_table)[1], "\n")
      cat("rdim_scree_ui   :", class(output$rdim_scree_ui)[1], "\n")
      cat("rdim_scree      :", class(output$rdim_scree)[1], "\n")
      cat("rdim_circle_ui  :", class(output$rdim_circle_ui)[1], "\n")
      cat("rdim_circle     :", class(output$rdim_circle)[1], "\n")
      cat("rdim_biplot     :", class(output$rdim_biplot)[1], "\n")
      cat("rdim_contrib    :", class(output$rdim_contrib)[1], "\n")
      cat("rdim_method_info:", class(output$rdim_method_info)[1], "\n")

      session$setInputs(rdim_render_mode = "factoextra")
      cat("scree (factoext):", class(output$rdim_scree_ui)[1], "\n")
      cat("circle (factoext):", class(output$rdim_circle_ui)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_RDIM_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_RDIM_OK\n") else quit(status = 1)
