collect_validation_issues <- function(yaml_files, required_root = c("nom", "nom_court", "source", "portions", "preparation")) {
  issues <- character()
  nom_courts <- character()

  recipes_dir <- "recettes"
  if (length(yaml_files) > 0) {
    recipes_dir <- dirname(dirname(yaml_files[[1]]))
  }

  for (f in yaml_files) {
    rel <- gsub(paste0("^", normalizePath(recipes_dir), "/?"), "", normalizePath(f))
    y <- tryCatch(yaml::read_yaml(f), error = function(e) e)
    if (inherits(y, "error")) {
      issues <- c(issues, glue::glue("{rel}: YAML invalide ({conditionMessage(y)})"))
      next
    }

    miss <- required_root[!required_root %in% names(y)]
    if (length(miss) > 0) {
      issues <- c(issues, glue::glue("{rel}: champs manquants -> {paste(miss, collapse = ', ')}"))
    }

    if (!is.null(y$nom_court) && nzchar(as.character(y$nom_court))) {
      nom_courts <- c(nom_courts, as.character(y$nom_court))
    } else {
      issues <- c(issues, glue::glue("{rel}: nom_court vide."))
    }

    p <- suppressWarnings(as.numeric(y$portions))
    if (!is.finite(p) || p <= 0) {
      issues <- c(issues, glue::glue("{rel}: portions doit etre un nombre > 0."))
    }

    prep <- y$preparation
    if (!is.list(prep) || length(prep) == 0) {
      issues <- c(issues, glue::glue("{rel}: preparation doit contenir au moins une section."))
      next
    }

    for (si in seq_along(prep)) {
      section <- prep[[si]]
      sname <- as.character(section$section %||% "")
      if (!nzchar(trimws(sname))) {
        issues <- c(issues, glue::glue("{rel}: section #{si} sans nom."))
      }

      steps <- section$etapes
      if (!is.list(steps) || length(steps) == 0) {
        issues <- c(issues, glue::glue("{rel}: section #{si} sans etapes."))
        next
      }

      for (ti in seq_along(steps)) {
        step <- steps[[ti]]
        txt <- as.character(step$etape %||% "")
        if (!nzchar(trimws(txt))) {
          issues <- c(issues, glue::glue("{rel}: section #{si}, etape #{ti} vide."))
        }
        ings <- step$ingredients
        if (is.list(ings)) {
          for (ii in seq_along(ings)) {
            ing <- ings[[ii]]
            if (is.null(ing$nom) || !nzchar(trimws(as.character(ing$nom)))) {
              issues <- c(issues, glue::glue("{rel}: section #{si}, etape #{ti}, ingredient #{ii} sans nom."))
            }
            if (!is.null(ing$qte) && !is.numeric(ing$qte)) {
              issues <- c(issues, glue::glue("{rel}: section #{si}, etape #{ti}, ingredient #{ii}, qte non numerique."))
            }
          }
        }
      }
    }
  }

  dupes <- unique(nom_courts[duplicated(nom_courts)])
  if (length(dupes) > 0) {
    issues <- c(issues, glue::glue("nom_court dupliques: {paste(dupes, collapse = ', ')}"))
  }

  issues
}

#' Validate recipe YAML files.
#'
#' GitHub Actions entrypoint for recipe structural validation.
#'
#' @param recipes_dir Directory containing recipe YAML files.
#' @return Vector of validated YAML file paths invisibly.
#' @export
gha_validate_recipes <- function(recipes_dir = "recettes") {
  yaml_files <- list.files(recipes_dir, pattern = "\\.ya?ml$", recursive = TRUE, full.names = TRUE)
  yaml_files <- yaml_files[!grepl("template\\.ya?ml$", yaml_files, ignore.case = TRUE)]
  yaml_files <- yaml_files[!grepl("url_imports/", yaml_files, fixed = TRUE)]
  yaml_files <- yaml_files[!grepl("url_imports_web/", yaml_files, fixed = TRUE)]
  yaml_files <- yaml_files[!grepl("url_imports_youtube/", yaml_files, fixed = TRUE)]

  issues <- collect_validation_issues(yaml_files)

  if (length(issues) > 0) {
    cli::cli_h1("Validation des recettes - erreurs detectees")
    for (m in issues) cli::cli_alert_danger(m)
    stop("Validation echouee.", call. = FALSE)
  }

  cli::cli_h1("Validation des recettes")
  cli::cli_alert_success(glue::glue("{length(yaml_files)} fichiers YAML valides."))
  invisible(yaml_files)
}
