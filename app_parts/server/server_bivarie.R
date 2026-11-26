# Module Analyse bivariée — serveur namespacé (mod_biv.R)
mod_biv_server(
  "biv",
  df_f = df_f,
  vars_cat = vars_cat,
  vars_num = vars_num,
  gpal = gpal,
  global_palette = reactive(input$global_palette)
)
