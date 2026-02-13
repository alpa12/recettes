# validate_yamls.R
# Validation stricte des YAML recettes: qte doit être NUMÉRIQUE.
#
# Exécution:
#   Rscript recettes/ajouter_recette/validate_yamls.R
# ou:
#   source("recettes/ajouter_recette/validate_yamls.R"); validate_all("recettes")

suppressPackageStartupMessages({
  library(yaml)
  library(fs)
  library(stringr)
})

is_scalar_numeric <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x)
}

validate_qte_numeric <- function(recipe, file = NA_character_) {
  issues <- list()

  add_issue <- function(path, value, msg) {
    issues[[length(issues) + 1]] <<- list(
      file = file,
      path = path,
      value = if (is.null(value)) "NULL" else as.character(value),
      message = msg
    )
  }

  prep <- recipe$preparation
  if (is.null(prep) || !is.list(prep)) return(issues)

  for (si in seq_along(prep)) {
    sec <- prep[[si]]
    etapes <- sec$etapes
    if (is.null(etapes) || !is.list(etapes)) next

    for (ei in seq_along(etapes)) {
      et <- etapes[[ei]]
      ing <- et$ingredients
      if (is.null(ing) || !is.list(ing)) next

      for (ii in seq_along(ing)) {
        one <- ing[[ii]]
        p <- sprintf("preparation[%d].etapes[%d].ingredients[%d].qte", si - 1, ei - 1, ii - 1)
        qte <- one$qte

        if (is.null(qte)) {
          add_issue(p, qte, "qte manquant — attendu: nombre")
        } else if (!is_scalar_numeric(qte)) {
          add_issue(p, qte, "qte non numérique — corriger (ex: 0.5 au lieu de 1/2)")
        }
      }
    }
  }

  issues
}

validate_required <- function(recipe, file = NA_character_) {
  required <- c("nom", "nom_court", "source", "portions")
  issues <- list()

  for (k in required) {
    if (is.null(recipe[[k]]) || (is.character(recipe[[k]]) && recipe[[k]] == "")) {
      issues[[length(issues) + 1]] <- list(
        file = file, path = k, value = if (is.null(recipe[[k]])) "NULL" else as.character(recipe[[k]]),
        message = sprintf("champ requis manquant: %s", k)
      )
    }
  }
  issues
}

validate_recipe_file <- function(path) {
  recipe <- yaml.load_file(path)
  c(
    validate_required(recipe, file = path),
    validate_qte_numeric(recipe, file = path)
  )
}

validate_all <- function(root = "recettes") {
  files <- dir_ls(root, recurse = TRUE, glob = "*.yaml")
  files <- files[!str_detect(files, "template\\.yaml$")]

  all_issues <- list()
  for (f in files) {
    all_issues <- c(all_issues, validate_recipe_file(f))
  }

  if (length(all_issues) == 0) {
    message("✅ Aucune erreur trouvée.")
    return(invisible(all_issues))
  }

  df <- do.call(rbind, lapply(all_issues, as.data.frame))
  rownames(df) <- NULL
  print(df)

  message(sprintf("❌ %d problème(s) trouvé(s).", nrow(df)))
  invisible(all_issues)
}

if (!interactive()) {
  validate_all("recettes")
}
