#' Convert a recipe YAML file to a Quarto (.qmd) recipe using the new template
#'
#' @param yaml_path Path to the input YAML file
#' @param qmd_path Path to the output .qmd file.
#'   If NULL, same name as yaml with .qmd extension.
#'
#' @details
#' This function reads a structured recipe YAML file following the new template
#' and renders it into a human-readable Quarto Markdown recipe. It assumes UTF-8
#' encoding and a schema compatible with the updated YAML structure.
#'
#' @importFrom yaml read_yaml
#' @importFrom fs path_ext_set path_file
#' @importFrom stringr str_trim
#' @export
yaml_recipe_to_qmd <- function(yaml_path, qmd_path = NULL) {
  stopifnot(file.exists(yaml_path))

  if (is.null(qmd_path)) {
    qmd_path <- fs::path_ext_set(yaml_path, "qmd")
  }

  recipe <- yaml::read_yaml(yaml_path)
  lines <- character()

  # ---- YAML metadata (Quarto front-matter) ----
  image_name <- fs::path_ext_set(fs::path_file(yaml_path), "jpg")

  lines <- c(
    lines,
    "---",
    paste0("title: ", recipe$nom_court),
    paste0("image: ", image_name)
  )

  # ---- Categories ----
  if (!is.null(recipe$categories)) {
    cats <- recipe$categories
    if (!is.character(cats)) cats <- unlist(cats, use.names = FALSE)
    lines <- c(lines, "categories:")
    lines <- c(lines, paste0("  - ", cats))
  }

  lines <- c(lines, "---", "")

  # ---- Title ----
  lines <- c(lines, paste0("# ", recipe$nom), "")

  # ---- Ingredients ----
  lines <- c(lines, "## Ingrédients", "")

  if (!is.null(recipe$ingredients)) {
    for (section in recipe$ingredients) {
      lines <- c(lines, paste0("### ", section$section), "")

      for (ing in section$ingredients) {
        # Format quantity if present
        if (is.null(ing$qte) || is.na(ing$qte)) {
          item <- ing$nom
        } else {
          item <- paste(ing$qte, ing$uni, ing$nom)
        }
        # Include equipment if present
        if (!is.null(ing$equipement)) {
          item <- paste(item, "(Équipement: ", ing$equipement, ")", sep = "")
        }
        lines <- c(lines, paste0("- ", stringr::str_trim(item)))
      }

      # List section-level equipment if present
      if (!is.null(section$equipements)) {
        lines <- c(lines, "")
        lines <- c(lines, "Équipements nécessaires:")
        for (eq in section$equipements) {
          lines <- c(lines, paste0("- ", eq))
        }
      }

      lines <- c(lines, "")
    }
  }

  # ---- Preparation ----
  lines <- c(lines, "## Préparation", "")

  if (!is.null(recipe$preparation)) {
    for (section in recipe$preparation) {
      lines <- c(lines, paste0("### ", section$section), "")

      if (!is.null(section$etapes)) {
        for (step in section$etapes) {
          lines <- c(lines, paste0("- ", step), "")
        }
      }
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
