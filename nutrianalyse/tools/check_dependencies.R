#!/usr/bin/env Rscript
# Vérifie que tous les packages utilisés dans le projet sont déclarés dans
# DESCRIPTION (Imports, Suggests ou LinkingTo).
#
# Sortie :
#   CHECK_DEPS_OK      : tout est déclaré
#   CHECK_DEPS_FAILED  : au moins un package utilisé n'est pas déclaré
#                        (code de sortie 1, utilisé par la CI)

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("Le package renv est requis pour vérifier les dépendances.")
}

deps <- renv::dependencies(quiet = TRUE)
used <- sort(unique(deps$Package[!is.na(deps$Package)]))

# Les packages de base de R n'ont pas à être déclarés dans DESCRIPTION.
base_packages <- rownames(installed.packages(priority = "base"))
used <- setdiff(used, base_packages)

desc <- read.dcf("DESCRIPTION")
fields <- intersect(c("Depends", "Imports", "Suggests", "LinkingTo"), colnames(desc))
declared <- unique(trimws(unlist(strsplit(paste(desc[1, fields], collapse = ","), ","))))
declared <- sub("\\s*\\(.*", "", declared)
declared <- declared[nzchar(declared) & declared != "R"]

missing <- setdiff(used, declared)
unused <- setdiff(declared, used)

if (length(unused) > 0) {
  message(
    "Information : packages déclarés mais non détectés : ",
    paste(unused, collapse = ", ")
  )
}

if (length(missing) > 0) {
  cat("CHECK_DEPS_FAILED\n")
  cat("Packages utilisés mais absents de DESCRIPTION :\n")
  print(missing)
  quit(status = 1)
}

cat("CHECK_DEPS_OK |", length(used), "packages utilisés, tous déclarés.\n")
