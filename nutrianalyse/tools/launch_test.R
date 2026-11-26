# Test de démarrage HTTP réel : lance l'app dans un processus fils,
# attend qu'elle réponde, puis l'arrête.
suppressPackageStartupMessages(library(callr))

port <- 8765L
p <- r_bg(
  function(port) {
    shiny::runApp(".", port = port, host = "127.0.0.1", launch.browser = FALSE)
  },
  args = list(port),
  stdout = "|", stderr = "|"
)
on.exit(try(p$kill(), silent = TRUE), add = TRUE)

http_ok <- function(url) {
  tryCatch(
    {
      con <- url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}

ok <- FALSE
for (i in seq_len(60)) {
  Sys.sleep(2)
  if (http_ok(sprintf("http://127.0.0.1:%d/", port))) {
    ok <- TRUE
    break
  }
  if (!p$is_alive()) break
}

if (ok) {
  cat("HTTP_OK | l'application repond sur le port", port, "\n")
} else {
  cat("HTTP_FAIL\n--- stdout ---\n")
  cat(p$read_output())
  cat("\n--- stderr ---\n")
  cat(p$read_error())
}

p$kill()
if (!ok) quit(status = 1)
