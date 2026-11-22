# Liste les appels tabItem() de app_parts/ui/ui_body.R avec leurs bornes exactes.
# Sortie TSV : nom <TAB> ligne_debut <TAB> ligne_fin
exprs <- parse("app_parts/ui/ui_body.R", keep.source = TRUE)

find_call <- function(x, fn) {
  if (!is.call(x)) return(NULL)
  if (identical(x[[1]], as.name(fn))) return(x)
  for (i in seq_along(x)) {
    if (i == 1) next
    hit <- find_call(x[[i]], fn)
    if (!is.null(hit)) return(hit)
  }
  NULL
}

tab_items <- NULL
for (e in exprs) {
  tab_items <- find_call(e, "tabItems")
  if (!is.null(tab_items)) break
}
if (is.null(tab_items)) stop("tabItems() introuvable")

args <- as.list(tab_items)[-1]
names <- vapply(args, function(a) {
  if (is.call(a) && identical(a[[1]], as.name("tabItem")) && !is.null(a[["tabName"]])) {
    as.character(a[["tabName"]])
  } else {
    NA_character_
  }
}, character(1))
names <- names[!is.na(names)]

pd <- getParseData(exprs)
syms <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "tabItem", ]
ranges <- lapply(seq_len(nrow(syms)), function(i) {
  fn_expr <- pd[pd$id == syms$parent[i], ]
  call_expr <- pd[pd$id == fn_expr$parent[1], ]
  c(call_expr$line1[1], call_expr$line2[1])
})

if (length(names) != length(ranges)) {
  stop(sprintf("Incohérence : %d noms pour %d appels", length(names), length(ranges)))
}

for (i in seq_along(names)) {
  cat(sprintf("%s\t%d\t%d\n", names[i], ranges[[i]][1], ranges[[i]][2]))
}
