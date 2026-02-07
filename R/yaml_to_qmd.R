#' Convert a recipe YAML file to a Quarto (.qmd) recipe
#'
#' @param yaml_path Path to the input YAML file
#' @param qmd_path Path to the output .qmd file.
#'   If NULL, same name as yaml with .qmd extension.
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

  # ---- Quarto front-matter ----
  image_name <- fs::path_ext_set(fs::path_file(yaml_path), "jpg")
  lines <- c(lines, "---",
             paste0("title: ", recipe$nom_court),
             paste0("image: ", image_name))

  if (!is.null(recipe$categories)) {
    cats <- unlist(recipe$categories, use.names = FALSE)
    lines <- c(lines, "categories:", paste0("  - ", cats))
  }
  lines <- c(lines, "---", "")

  # ---- Title ----
  lines <- c(lines, paste0("# ", recipe$nom), "")

  # ---- Ingredients (grouped by section) ----
  lines <- c(lines, "## Ingrédients", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "")

    # Collect ingredients for this section
    ing_list <- list()
    for (step in section$etapes) {
      if (!is.null(step$ingredients)) {
        for (ing in step$ingredients) {
          key <- paste(ing$nom, ing$uni)
          if (!key %in% names(ing_list)) {
            ing_list[[key]] <- paste(ing$qte, ing$uni, ing$nom)
          }
        }
      }
    }
    # Write ingredients in order
    for (item in ing_list) {
      lines <- c(lines, paste0("- ", stringr::str_trim(item)))
    }
    lines <- c(lines, "")
  }

  # ---- Equipment (grouped by section) ----
  lines <- c(lines, "## Équipements", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "")

    equip_list <- character()
    for (step in section$etapes) {
      if (!is.null(step$equipements)) {
        equip_list <- c(equip_list, step$equipements)
      }
    }
    equip_list <- unique(equip_list)  # remove duplicates

    for (eq in equip_list) {
      lines <- c(lines, paste0("- ", stringr::str_trim(eq)))
    }
    lines <- c(lines, "")
  }

  # ---- Preparation steps ----
  lines <- c(lines, "## Préparation", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "")
    for (i in seq_along(section$etapes)) {
      step_text <- section$etapes[[i]]$etape
      lines <- c(lines, paste0(i, ". ", step_text))
    }
    lines <- c(lines, "")
  }

  # ---- Comments / Notes ----
  if (!is.null(recipe$commentaires) && length(recipe$commentaires) > 0) {
    lines <- c(lines, "## Notes", "")
    for (comment in recipe$commentaires) {
      lines <- c(lines, paste0("- ", comment))
    }
  }

  # ---- Write to file ----
  writeLines(enc2utf8(lines), qmd_path)
  invisible(qmd_path)
}
