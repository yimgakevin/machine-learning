#!/usr/bin/env Rscript
# ============================================================================
# tools/manage_users.R — Gestion des utilisateurs hors du code source
# ============================================================================
# Le fichier config/users.rds n'est jamais versionné (voir .gitignore).
# Les mots de passe sont hachés avec sodium (argon2).
# Tous les utilisateurs disposent des mêmes droits : il n'y a plus de rôles.
#
# Usage :
#   Rscript tools/manage_users.R list
#   Rscript tools/manage_users.R add <user> <nom...>
#   Rscript tools/manage_users.R passwd <user>
#   Rscript tools/manage_users.R remove <user>
#
# Le mot de passe est lu depuis NUTRI_NEW_PASSWORD, ou demandé interactivement.
# ============================================================================

suppressPackageStartupMessages(library(sodium))

args <- commandArgs(trailingOnly = TRUE)
users_file <- Sys.getenv("NUTRI_USERS_FILE", unset = "config/users.rds")
user_cols <- c("user", "password", "name")

usage <- function() {
  cat(
    "Usage :\n",
    "  Rscript tools/manage_users.R list\n",
    "  Rscript tools/manage_users.R add <user> <nom...>\n",
    "  Rscript tools/manage_users.R passwd <user>\n",
    "  Rscript tools/manage_users.R remove <user>\n\n",
    "Mot de passe : variable NUTRI_NEW_PASSWORD ou saisie interactive.\n",
    "Fichier : ", users_file, "\n",
    sep = ""
  )
  quit(status = 1)
}

read_users <- function() {
  if (!file.exists(users_file)) {
    return(data.frame(
      user = character(), password = character(), name = character(),
      stringsAsFactors = FALSE
    ))
  }
  d <- readRDS(users_file)
  missing <- setdiff(user_cols, names(d))
  if (length(missing) > 0) {
    stop("Fichier utilisateurs invalide : colonnes manquantes ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  # Ignore une éventuelle ancienne colonne role.
  as.data.frame(d[, user_cols, drop = FALSE], stringsAsFactors = FALSE)
}

write_users <- function(d) {
  dir.create(dirname(users_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(d[, user_cols, drop = FALSE], users_file)
  try(Sys.chmod(users_file, mode = "0600"), silent = TRUE)
  cat("✅ ", nrow(d), " utilisateur(s) enregistré(s) dans ", users_file, "\n", sep = "")
}

ask_password <- function() {
  p <- Sys.getenv("NUTRI_NEW_PASSWORD", unset = "")
  if (nzchar(p)) return(p)
  if (requireNamespace("getPass", quietly = TRUE)) {
    return(getPass::getPass("Mot de passe (12 caractères minimum) : "))
  }
  readline("Mot de passe (12 caractères minimum, visible) : ")
}

check_password <- function(p) {
  if (!nzchar(p)) stop("Mot de passe vide.", call. = FALSE)
  if (nchar(p) < 12) stop("Mot de passe trop court (12 caractères minimum).", call. = FALSE)
  invisible(TRUE)
}

cmd <- if (length(args) == 0) "help" else args[[1]]

if (cmd == "list") {
  d <- read_users()
  if (nrow(d) == 0) {
    cat("Aucun fichier ", users_file, " — mode démonstration actif.\n", sep = "")
    cat("Créez le premier compte avec : Rscript tools/manage_users.R add <user> <nom>\n")
  } else {
    cat("Utilisateurs (", users_file, ") :\n", sep = "")
    print(d[, c("user", "name")], row.names = FALSE)
  }
  quit(status = 0)
}

if (cmd == "add") {
  if (length(args) < 2) usage()
  user <- args[[2]]
  name <- if (length(args) >= 3) paste(args[3:length(args)], collapse = " ") else user
  d <- read_users()
  if (user %in% d$user) stop("L'utilisateur '", user, "' existe déjà.", call. = FALSE)
  pwd <- ask_password()
  check_password(pwd)
  d <- rbind(d, data.frame(
    user = user, password = sodium::password_store(pwd), name = name,
    stringsAsFactors = FALSE
  ))
  write_users(d)
  quit(status = 0)
}

if (cmd == "passwd") {
  if (length(args) < 2) usage()
  user <- args[[2]]
  d <- read_users()
  if (!user %in% d$user) stop("Utilisateur inconnu : ", user, call. = FALSE)
  pwd <- ask_password()
  check_password(pwd)
  d$password[d$user == user] <- sodium::password_store(pwd)
  write_users(d)
  quit(status = 0)
}

if (cmd == "remove") {
  if (length(args) < 2) usage()
  user <- args[[2]]
  d <- read_users()
  if (!user %in% d$user) stop("Utilisateur inconnu : ", user, call. = FALSE)
  d <- d[d$user != user, , drop = FALSE]
  write_users(d)
  quit(status = 0)
}

usage()
