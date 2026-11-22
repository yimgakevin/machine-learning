# Charge tous les onglets UI extraits (app_parts/ui/tabs/).
.tab_files <- sort(list.files("app_parts/ui/tabs", pattern = "\\.R$", full.names = TRUE))
invisible(lapply(.tab_files, source, local = FALSE))
rm(.tab_files)
