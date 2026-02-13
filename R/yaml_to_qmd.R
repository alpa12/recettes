# yaml_to_qmds.R
# ------------------------------------------------------------------------------
# Utilities to convert recipe YAML files into Quarto (.qmd) pages.
# Adds an "Edit" button on every recipe page that links to an edit form page
# with the YAML path as a query parameter (static-site friendly).
#
# Comments upgrade:
# - recipe$commentaires can be:
#   (A) old format: character vector
#   (B) new format: list of dict with optional fields:
#       commentaire (string), evaluation (int 1..5), auteur (string), date (YYYY-MM-DD)
#   Backward compatibility:
#       note -> evaluation, nom -> auteur
#
# In the generated recipe page, we display:
# - Average stars (from available evaluations)
# - Total comment count
# - Count of comments written by "Alexandre Parent" and "Élodie Bourgeois"
#   (case-insensitive, accents-insensitive best-effort)
# ------------------------------------------------------------------------------

EDIT_PAGE_HREF <- "ajouter_recette/"
EDIT_PARAM_NAME <- "yaml"

#' @importFrom yaml read_yaml
#' @importFrom fs path_ext_set path_file path_rel
#' @importFrom stringr str_trim str_to_lower
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
  
  image_line <- if (!is.null(recipe$image_guid) && nzchar(as.character(recipe$image_guid))) {
    paste0("image: /images/", recipe$image_guid, ".jpg")
  } else {
    ""
  }
lines <- c(
    lines,
    "---",
    paste0("title: ", recipe$nom),
    image_line
  )

  if (!is.null(recipe$categories)) {
    cats <- unlist(recipe$categories, use.names = FALSE)
    lines <- c(lines, "categories:", paste0("  - ", cats))
  }
  lines <- c(lines, "---", "")

  # ---- Edit button ----
  yaml_rel_to_root <- fs::path_rel(yaml_path, start = ".")
  yaml_rel_to_root <- gsub("\\\\", "/", yaml_rel_to_root)
  if (!startsWith(yaml_rel_to_root, "/")) yaml_rel_to_root <- paste0("/", yaml_rel_to_root)

  yaml_qp <- utils::URLencode(yaml_rel_to_root, reserved = TRUE)
  edit_href <- paste0("../", EDIT_PAGE_HREF, "?", EDIT_PARAM_NAME, "=", yaml_qp)

  lines <- c(
    lines,
    paste0("[✏️ Modifier cette recette](", edit_href, "){.btn .btn-outline-primary .btn-sm}"),
    ""
  )

  # ---- Ingredients (grouped by section) ----
  lines <- c(lines, "## Ingrédients", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "")

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
    equip_list <- unique(equip_list)

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
  comments_norm <- normalize_comments(recipe$commentaires)

  if (length(comments_norm) > 0) {
    lines <- c(lines, "## Notes", "")

    # Summary: stars + counts
    avg <- mean(vapply(comments_norm, function(x) if (!is.null(x$evaluation)) x$evaluation else NA_real_, numeric(1)), na.rm = TRUE)
    if (is.nan(avg)) avg <- NA_real_
    stars <- if (is.na(avg)) "" else stars_string(avg)

    n_total <- length(comments_norm)
    n_alex <- count_by_author(comments_norm, "Alexandre Parent")
    n_elodie <- count_by_author(comments_norm, "Élodie Bourgeois")

    summary_parts <- c()
    if (stars != "") summary_parts <- c(summary_parts, paste0(stars, " (moyenne ", format(avg, digits = 2), "/5)"))
    summary_parts <- c(summary_parts, paste0(n_total, " commentaire(s)"))
    summary_parts <- c(summary_parts, paste0("Alexandre Parent: ", n_alex))
    summary_parts <- c(summary_parts, paste0("Élodie Bourgeois: ", n_elodie))

    lines <- c(lines, paste0("_", paste(summary_parts, collapse = " · "), "_"), "")

    # Each comment
    for (cmt in comments_norm) {
      lines <- c(lines, paste0("- ", format_comment_line(cmt)))
    }
  }

  writeLines(enc2utf8(lines), qmd_path)
  invisible(qmd_path)
}

# --- Comment helpers ----------------------------------------------------------

normalize_comments <- function(commentaires) {
  if (is.null(commentaires) || length(commentaires) == 0) return(list())

  # Old format: vector of strings
  if (is.character(commentaires)) {
    return(lapply(commentaires, function(s) list(commentaire = s, evaluation = NULL, auteur = NULL, date = NULL)))
  }

  # Some YAML parsers may give a list with unnamed entries
  if (is.list(commentaires)) {
    out <- list()
    for (i in seq_along(commentaires)) {
      x <- commentaires[[i]]
      if (is.character(x)) {
        out[[length(out) + 1]] <- list(commentaire = x, evaluation = NULL, auteur = NULL, date = NULL)
      } else if (is.list(x)) {
        raw_evaluation <- if (!is.null(x$evaluation)) x$evaluation else x$note
        raw_author <- if (!is.null(x$auteur)) x$auteur else x$nom
        out[[length(out) + 1]] <- list(
          commentaire = if (!is.null(x$commentaire)) x$commentaire else NULL,
          evaluation = if (!is.null(raw_evaluation)) as.numeric(raw_evaluation) else NULL,
          auteur = if (!is.null(raw_author)) raw_author else NULL,
          date = if (!is.null(x$date)) x$date else NULL
        )
      }
    }
    return(out)
  }

  list()
}

stars_string <- function(avg) {
  # round to nearest integer for display
  n <- round(avg)
  n <- max(0, min(5, n))
  paste0(strrep("★", n), strrep("☆", 5 - n))
}

strip_accents <- function(x) {
  # best effort: convert to ASCII
  iconv(x, from = "", to = "ASCII//TRANSLIT")
}

norm_name <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return("")
  x <- strip_accents(x)
  x <- stringr::str_to_lower(x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

count_by_author <- function(comments, author) {
  a <- norm_name(author)
  sum(vapply(comments, function(cmt) norm_name(cmt$auteur) == a, logical(1)))
}

format_comment_line <- function(cmt) {
  parts <- c()

  if (!is.null(cmt$evaluation) && !is.na(cmt$evaluation)) {
    ev <- as.integer(cmt$evaluation)
    if (!is.na(ev) && ev >= 1 && ev <= 5) {
      parts <- c(parts, paste0(strrep("★", ev), strrep("☆", 5 - ev)))
    }
  }

  if (!is.null(cmt$auteur) && cmt$auteur != "") parts <- c(parts, cmt$auteur)
  if (!is.null(cmt$date) && cmt$date != "") parts <- c(parts, cmt$date)

  prefix <- if (length(parts) > 0) paste0("[", paste(parts, collapse = " · "), "] ") else ""
  txt <- if (!is.null(cmt$commentaire) && cmt$commentaire != "") cmt$commentaire else "(sans texte)"
  paste0(prefix, txt)
}

#' Regenerate all recipe QMDs from YAMLs
#' @export
regenerate_recipe_qmds <- function(recipes_dir = "recettes", pattern = "\\.ya?ml$") {
  yaml_files <- list.files(recipes_dir, pattern = pattern, full.names = TRUE)
  yaml_files <- yaml_files[!grepl("template\\.ya?ml$", yaml_files, ignore.case = TRUE)]

  for (y in yaml_files) {
    yaml_recipe_to_qmd(y)
  }

  invisible(yaml_files)
}
