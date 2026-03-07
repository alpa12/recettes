# Validation stricte des YAML recettes.
#
# Exécution:
#   Rscript recettes/ajouter_recette/validate_yamls.R
# ou:
#   source("recettes/ajouter_recette/validate_yamls.R"); validate_all("recettes")

suppressPackageStartupMessages({
  library(fs)
  library(stringr)
  library(yaml)
})

source("livrec/R/utils.R")
source("livrec/R/recipe_validation.R")
source("livrec/R/gha_common.R")

validate_recipe_file <- function(path) {
  recipe <- yaml.load_file(path)
  validate_recipe_data(recipe, file = path)
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

  report <- format_recipe_validation_report(
    all_issues,
    heading = "❌ Des YAML ne respectent pas le format attendu par le site"
  )
  cat(report, sep = "\n")
  cat("\n", sep = "")
  message(sprintf("Résumé: %d problème(s) à corriger dans %d fichier(s).", length(all_issues), length(unique(vapply(all_issues, `[[`, character(1), "file")))))
  invisible(all_issues)
}

if (!interactive()) {
  issues <- validate_all("recettes")
  if (length(issues) > 0) quit(status = 1)
}
