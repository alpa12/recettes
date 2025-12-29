#' Convert a recipe YAML file to a Quarto (.qmd) recipe
#'
#' @param yaml_path Path to the input YAML file
#' @param qmd_path Path to the output .qmd file.
#'  If NULL, same name as yaml with .qmd extension.
#'
#' @details
#' This function reads a structured recipe YAML file and renders it into
#' a human-readable Quarto Markdown recipe. It assumes UTF-8 encoding and
#' a schema compatible with the example provided.
#'
#' @importFrom yaml read_yaml
#' @importFrom fs path_ext_set
#' @importFrom stringr str_trim
#' @export
yaml_recipe_to_qmd <- function(yaml_path, qmd_path = NULL) {
  stopifnot(file.exists(yaml_path))

  if (is.null(qmd_path)) {
    qmd_path <- fs::path_ext_set(yaml_path, "qmd")
  }

  recipe <- yaml::read_yaml(yaml_path)

  lines <- character()

  # ---- Title ----
  lines <- c(lines, paste0("# ", recipe$nom), "")

  # ---- Ingredients ----
  lines <- c(lines, "## Ingrédients", "")

  for (section in recipe$ingredients) {
    lines <- c(lines, paste0("### ", section$section), "")

    for (ing in section$ingredients) {
      if (is.null(ing$qte) || is.na(ing$qte)) {
        item <- ing$nom
      } else {
        item <- paste(ing$qte, ing$uni, ing$nom)
      }
      lines <- c(lines, paste0("- ", stringr::str_trim(item)))
    }

    lines <- c(lines, "")
  }

  # ---- Preparation ----
  lines <- c(lines, "## Préparation", "")

  prep <- recipe$preparation

  for (i in seq_along(prep)) {
    section_name <- prep[[i]]$section
    steps <- prep[[i]]$etapes

    lines <- c(lines, paste0("### ", section_name), "")

    for (step in steps) {
      lines <- c(lines, paste0("- ", step), "")
    }
  }

  # ---- Notes / Comments ----
  if (!is.null(recipe$commentaires) && length(recipe$commentaires) > 0) {
    lines <- c(lines, "## Notes", "")

    for (comment in recipe$commentaires) {
      lines <- c(lines, paste0("- ", comment))
    }
  }

  # ---- Write file ----
  writeLines(enc2utf8(lines), qmd_path)

  invisible(qmd_path)
}
