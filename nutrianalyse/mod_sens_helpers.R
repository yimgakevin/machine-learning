# Helper partagé par les modules SENS pour les sélecteurs générés côté serveur.
# Contrairement à sens_select() (scope racine, encore utilisé par les onglets
# non convertis), cette version applique explicitement le namespace du module.
sens_select_ns <- function(ns, id, label, detect_key, d, allow_empty = TRUE) {
  all_cols <- names(d)
  det <- auto_detect(d)
  sel <- det[[detect_key]]
  choices <- if (allow_empty) c("—" = "", all_cols) else all_cols
  selectInput(ns(id), label, choices = choices, selected = sel)
}
