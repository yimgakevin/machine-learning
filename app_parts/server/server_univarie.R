# Module Analyse univariée — serveur namespacé (mod_uni.R)
mod_uni_server(
  "uni",
  df_f = df_f,
  vars_cat = vars_cat,
  vars_cat_pie = vars_cat_pie,
  vars_num = vars_num,
  gpal = gpal,
  gprimary = gprimary,
  gsecondary = gsecondary,
  global_palette = reactive(input$global_palette)
)
