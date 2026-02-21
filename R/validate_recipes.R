#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(yaml)
  library(glue)
  library(cli)
  library(here)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

recipes_dir <- here("recettes")
yaml_files <- list.files(recipes_dir, pattern = "\\.ya?ml$", recursive = TRUE, full.names = TRUE)
yaml_files <- yaml_files[!grepl("template\\.ya?ml$", yaml_files, ignore.case = TRUE)]
yaml_files <- yaml_files[!grepl("url_imports/", yaml_files, fixed = TRUE)]
yaml_files <- yaml_files[!grepl("url_imports_web/", yaml_files, fixed = TRUE)]
yaml_files <- yaml_files[!grepl("url_imports_youtube/", yaml_files, fixed = TRUE)]

required_root <- c("nom", "nom_court", "source", "portions", "preparation")
issues <- character()
nom_courts <- character()

for (f in yaml_files) {
  rel <- gsub(paste0("^", normalizePath(recipes_dir), "/?"), "", normalizePath(f))
  y <- tryCatch(read_yaml(f), error = function(e) e)
  if (inherits(y, "error")) {
    issues <- c(issues, glue("{rel}: YAML invalide ({conditionMessage(y)})"))
    next
  }

  miss <- required_root[!required_root %in% names(y)]
  if (length(miss) > 0) {
    issues <- c(issues, glue("{rel}: champs manquants -> {paste(miss, collapse = ', ')}"))
  }

  if (!is.null(y$nom_court) && nzchar(as.character(y$nom_court))) {
    nom_courts <- c(nom_courts, as.character(y$nom_court))
  } else {
    issues <- c(issues, glue("{rel}: nom_court vide."))
  }

  p <- suppressWarnings(as.numeric(y$portions))
  if (!is.finite(p) || p <= 0) {
    issues <- c(issues, glue("{rel}: portions doit être un nombre > 0."))
  }

  prep <- y$preparation
  if (!is.list(prep) || length(prep) == 0) {
    issues <- c(issues, glue("{rel}: preparation doit contenir au moins une section."))
    next
  }

  for (si in seq_along(prep)) {
    section <- prep[[si]]
    sname <- as.character(section$section %||% "")
    if (!nzchar(trimws(sname))) {
      issues <- c(issues, glue("{rel}: section #{si} sans nom."))
    }
    steps <- section$etapes
    if (!is.list(steps) || length(steps) == 0) {
      issues <- c(issues, glue("{rel}: section #{si} sans étapes."))
      next
    }

    for (ti in seq_along(steps)) {
      step <- steps[[ti]]
      txt <- as.character(step$etape %||% "")
      if (!nzchar(trimws(txt))) {
        issues <- c(issues, glue("{rel}: section #{si}, étape #{ti} vide."))
      }
      ings <- step$ingredients
      if (is.list(ings)) {
        for (ii in seq_along(ings)) {
          ing <- ings[[ii]]
          if (is.null(ing$nom) || !nzchar(trimws(as.character(ing$nom)))) {
            issues <- c(issues, glue("{rel}: section #{si}, étape #{ti}, ingrédient #{ii} sans nom."))
          }
          if (!is.null(ing$qte) && !is.numeric(ing$qte)) {
            issues <- c(issues, glue("{rel}: section #{si}, étape #{ti}, ingrédient #{ii}, qte non numérique."))
          }
        }
      }
    }
  }
}

dupes <- unique(nom_courts[duplicated(nom_courts)])
if (length(dupes) > 0) {
  issues <- c(issues, glue("nom_court dupliqués: {paste(dupes, collapse = ', ')}"))
}

if (length(issues) > 0) {
  cli_h1("Validation des recettes - erreurs détectées")
  for (m in issues) cli_alert_danger(m)
  quit(status = 1)
}

cli_h1("Validation des recettes")
cli_alert_success(glue("{length(yaml_files)} fichiers YAML valides."))
