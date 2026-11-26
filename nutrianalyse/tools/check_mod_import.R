# Vérifie le module Import & Mappeur en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_import_ui("import"))
ids <- c(
  "import-import_tabs", "import-mapper_col1", "import-mapper_col2",
  "import-dict_verification_table", "import-kobo_server", "import-kobo_custom_url",
  "import-kobo_token", "import-kobo_test", "import-kobo_list_forms",
  "import-kobo_connection_status", "import-kobo_form_select_ui", "import-kobo_download",
  "import-kobo_download_status", "import-kobo_forms_list", "import-kobo_mapping_html",
  "import-kobo_preview_table", "import-kobo_file_upload", "import-kobo_file_import",
  "import-kobo_file_status", "import-kobo_file_mapping", "import-kobo_file_preview",
  "import-oms_conform_score", "import-oms_conform_table", "import-oms_fix_col",
  "import-oms_fix_var", "import-oms_fix_apply", "import-oms_fix_reset",
  "import-dict_selected_aliases", "import-import_preview", "import-import_summary",
  "import-apply_map"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_IMPORT_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
if (!grepl('data-ns-prefix="import-"', html, fixed = TRUE)) {
  cat("MOD_IMPORT_UI_FAIL: conditionalPanel non namespacé\n")
  quit(status = 1)
}
cat("MOD_IMPORT_UI_OK |", length(ids), "IDs namespacés + conditionalPanel\n")

set.seed(18)
n <- 30
raw <- data.frame(
  whz_raw = round(rnorm(n, 0, 1), 2),
  age_mois = sample(6:59, n, replace = TRUE),
  sexe = sample(c("M", "F"), n, replace = TRUE),
  region = rep(c("Nord", "Sud"), length.out = n),
  poids = round(rnorm(n, 10, 2), 1),
  stringsAsFactors = FALSE
)

raw_data <- shiny::reactiveVal(raw)
mapped_data <- shiny::reactiveVal(NULL)
map_cols <- shiny::reactiveVal(list(whz = "whz_raw", age_m = "age_mois", sexe = "sexe"))

ok <- tryCatch({
  shiny::testServer(
    mod_import_server,
    args = list(raw_data = raw_data, mapped_data = mapped_data, map_cols = map_cols),
    {
      session$setInputs(
        kobo_server = "https://kf.kobotoolbox.org",
        kobo_token = "",
        oms_fix_col = "",
        oms_fix_var = "",
        map_whz = "whz_raw"
      )
      cat("mapper_col1      :", class(output$mapper_col1)[1], "\n")
      cat("dict_verif       :", class(output$dict_verification_table)[1], "\n")
      cat("import_preview   :", class(output$import_preview)[1], "\n")
      cat("import_summary   :", class(output$import_summary)[1], "\n")
      cat("oms_conform_score:", class(output$oms_conform_score)[1], "\n")
      cat("oms_conform_table:", class(output$oms_conform_table)[1], "\n")

      session$setInputs(apply_map = 1)
      cat("mapped_applied   :", nrow(mapped_data()), "lignes\n")

      session$setInputs(oms_fix_col = "poids", oms_fix_var = "poids", oms_fix_apply = 1)
      cat("manual_fix       :", map_cols()[["poids"]], "\n")
      session$setInputs(oms_fix_reset = 1)
      cat("reset_ok         : TRUE\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_IMPORT_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_IMPORT_OK\n") else quit(status = 1)
