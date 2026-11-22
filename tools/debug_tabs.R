e <- parse("app_parts/ui/ui_body.R", keep.source = TRUE)
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
ti <- find_call(e[[1]], "tabItems")
args <- as.list(ti)[-1]
cat("nb args:", length(args), "\n")
for (i in seq_along(args)) {
  a <- args[[i]]
  head <- if (is.call(a)) deparse(a[[1]]) else paste0("<", class(a), ">")
  nm <- if (is.call(a) && !is.null(a[["tabName"]])) as.character(a[["tabName"]]) else ""
  cat(sprintf("%2d  %-20s %s\n", i, head, nm))
}
