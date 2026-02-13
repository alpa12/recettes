# ============================================================
# yaml_to_qmds.R
# - Convertit toutes les recettes YAML -> QMD
# - Ajoute un bouton "Modifier cette recette" sur chaque page
# ============================================================

#' Convert a recipe YAML file to a Quarto (.qmd) recipe
#'
#' @param yaml_path Path to the input YAML file
#' @param qmd_path Path to the output .qmd file.
#'   If NULL, same name as yaml with .qmd extension.
#' @param edit_page_href Relative href (from the recipe HTML page) to the edit form page.
#' @param edit_yaml_param Relative path (from the edit form page) to the YAML file to edit.
#'
#' @importFrom yaml read_yaml
#' @importFrom fs path_ext_set path_file
#' @importFrom stringr str_trim
#' @export
yaml_recipe_to_qmd <- function(
  yaml_path,
  qmd_path = NULL,
  edit_page_href = "ajouter_recette/index.html",
  edit_yaml_param = NULL
) {
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

  # ---- Edit button ----
  # The edit form page is at: recettes/ajouter_recette/index.qmd (=> index.html).
  # Recipe pages are at: recettes/<slug>.html
  #
  # From recettes/<slug>.html to the edit form page:
  #   "ajouter_recette/index.html"
  #
  # From recettes/ajouter_recette/index.html to the YAML file in recettes/:
  #   "../<slug>.yaml"
  if (is.null(edit_yaml_param)) {
    edit_yaml_param <- paste0("../", fs::path_file(yaml_path))
  }

  # URL-encode the param safely (keeps "/" but encodes spaces, accents, etc.)
  encoded_param <- utils::URLencode(edit_yaml_param, reserved = TRUE)
  edit_href <- paste0(edit_page_href, "?yaml=", encoded_param)

  lines <- c(
    lines,
    '<div class="text-end mb-3">',
    paste0('  <a class="btn btn-outline-secondary btn-sm" href="', edit_href, '">✏️ Modifier cette recette</a>'),
    "</div>",
    ""
  )

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

# ============================================================
# Convert all YAML recipes to QMD
# - Assumes:
#   - YAML recipes live under: recettes/*.yaml
#   - QMD recipes live under:  recettes/*.qmd
#   - The form page is:         recettes/ajouter_recette/index.qmd
# ============================================================

yaml_to_qmds <- function(recipes_dir = "recettes") {
  yaml_files <- list.files(
    recipes_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE
  )

  # Exclude template.yaml (and any other non-recipe yaml you might have)
  yaml_files <- yaml_files[!grepl("template\\.ya?ml$", basename(yaml_files), ignore.case = TRUE)]

  for (yaml_path in yaml_files) {
    # Only convert top-level recipe yamls (skip subfolders like recettes/ajouter_recette/**)
    # If you DO store recipe yamls in nested folders later, remove this filter.
    if (dirname(yaml_path) != recipes_dir) next

    yaml_recipe_to_qmd(
      yaml_path = yaml_path,
      qmd_path = fs::path_ext_set(yaml_path, "qmd"),
      edit_page_href = "ajouter_recette/index.html",
      edit_yaml_param = paste0("../", fs::path_file(yaml_path))
    )
  }

  invisible(yaml_files)
}
