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

escape_html <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

`%||%` <- function(a, b) if (is.null(a)) b else a

fmt_number <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  n <- suppressWarnings(as.numeric(x))
  if (is.na(n)) return(as.character(x))
  if (abs(n - round(n)) < 1e-9) return(as.character(as.integer(round(n))))
  s <- format(round(n, 2), nsmall = 2, trim = TRUE, scientific = FALSE)
  s <- sub("0+$", "", s)
  sub("\\.$", "", s)
}

render_ingredient_li <- function(ing) {
  q_raw <- ing$qte %||% ""
  q_num <- suppressWarnings(as.numeric(q_raw))
  q_is_num <- !is.na(q_num)
  q_label <- escape_html(fmt_number(q_raw))
  uni <- escape_html(stringr::str_trim(as.character(ing$uni %||% "")))
  nom <- escape_html(stringr::str_trim(as.character(ing$nom %||% "")))

  if (q_is_num) {
    paste0(
      "<li class=\"recipe-ingredient\">",
      "<span class=\"ingredient-qte\" data-base=\"", q_num, "\">", q_label, "</span>",
      if (nzchar(uni)) paste0(" <span class=\"ingredient-uni\">", uni, "</span>") else "",
      if (nzchar(nom)) paste0(" <span class=\"ingredient-nom\">", nom, "</span>") else "",
      "</li>"
    )
  } else {
    paste0(
      "<li class=\"recipe-ingredient\">",
      if (nzchar(q_label)) q_label else "",
      if (nzchar(uni)) paste0(" ", uni) else "",
      if (nzchar(nom)) paste0(" ", nom) else "",
      "</li>"
    )
  }
}

render_ingredient_inline <- function(ing) {
  q_raw <- ing$qte %||% ""
  q_num <- suppressWarnings(as.numeric(q_raw))
  q_is_num <- !is.na(q_num)
  q_label <- escape_html(fmt_number(q_raw))
  uni <- escape_html(stringr::str_trim(as.character(ing$uni %||% "")))
  nom <- escape_html(stringr::str_trim(as.character(ing$nom %||% "")))

  if (q_is_num) {
    paste0(
      "<span class=\"ingredient-qte\" data-base=\"", q_num, "\">", q_label, "</span>",
      if (nzchar(uni)) paste0(" <span class=\"ingredient-uni\">", uni, "</span>") else "",
      if (nzchar(nom)) paste0(" <span class=\"ingredient-nom\">", nom, "</span>") else ""
    )
  } else {
    paste0(
      if (nzchar(q_label)) q_label else "",
      if (nzchar(uni)) paste0(" ", uni) else "",
      if (nzchar(nom)) paste0(" ", nom) else ""
    )
  }
}

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

  # ---- Quick facts and tools ----
  facts <- character()
  if (!is.null(recipe$source) && nzchar(as.character(recipe$source))) {
    facts <- c(facts, paste0("<div class=\"recipe-fact recipe-fact-source\"><span>Source</span><strong>", escape_html(recipe$source), "</strong></div>"))
  }
  if (!is.null(recipe$se_congele)) {
    txt <- if (isTRUE(recipe$se_congele)) "Oui" else "Non"
    facts <- c(facts, paste0("<div class=\"recipe-fact recipe-fact-compact\"><span>Se congèle</span><strong>", txt, "</strong></div>"))
  }
  t <- if (is.list(recipe$temps)) recipe$temps else list()
  prep <- if (!is.null(t$preparation) && nzchar(fmt_number(t$preparation))) paste0(fmt_number(t$preparation), " min") else "-"
  cook <- if (!is.null(t$cuisson) && nzchar(fmt_number(t$cuisson))) paste0(fmt_number(t$cuisson), " min") else "-"
  cool <- if (!is.null(t$refrigeration) && nzchar(fmt_number(t$refrigeration))) paste0(fmt_number(t$refrigeration), " min") else "-"
  facts <- c(
    facts,
    paste0(
      "<div class=\"recipe-fact\"><span>Temps</span><strong>",
      "Préparation: ", prep, "<br>",
      "Cuisson: ", cook, "<br>",
      "Réfrigération: ", cool,
      "</strong></div>"
    )
  )

  lines <- c(
    lines,
    "```{=html}",
    "<div class=\"recipe-toolbar\">",
    paste0("<a href=\"", edit_href, "\" class=\"btn btn-outline-primary btn-sm\">✏️ Modifier cette recette</a>"),
    "<div class=\"recipe-toolbar-actions\">",
    "<button type=\"button\" class=\"btn btn-outline-secondary btn-sm\" onclick=\"window.print()\">🖨️ Imprimer</button>",
    "<button type=\"button\" class=\"btn btn-outline-secondary btn-sm\" onclick=\"navigator.clipboard && navigator.clipboard.writeText(window.location.href)\">🔗 Copier le lien</button>",
    "</div>",
    "</div>",
    if (length(facts) > 0) paste0("<div class=\"recipe-facts-grid\">", paste(facts, collapse = ""), "</div>") else "",
    "```",
    ""
  )

  base_portions <- suppressWarnings(as.numeric(recipe$portions))
  has_scaler <- !is.null(base_portions) && !is.na(base_portions) && base_portions > 0
  has_step_images <- any(vapply(recipe$preparation %||% list(), function(section) {
    any(vapply(section$etapes %||% list(), function(step) {
      !is.null(step$image_guid) && nzchar(as.character(step$image_guid))
    }, logical(1)))
  }, logical(1)))

  if (has_scaler) {
    lines <- c(
      lines,
      "```{=html}",
      paste0(
        "<div class=\"recipe-servings-control\">",
        "<label for=\"servings-input\" class=\"form-label mb-1\">Ajuster les portions</label>",
        "<div class=\"d-flex gap-2 align-items-center\">",
        "<input id=\"servings-input\" class=\"form-control form-control-sm\" type=\"number\" min=\"1\" step=\"1\" value=\"", base_portions, "\" style=\"max-width: 110px;\">",
        "<button id=\"servings-reset\" class=\"btn btn-outline-secondary btn-sm\" type=\"button\">Réinitialiser</button>",
        "<span class=\"text-muted small\">Base: ", base_portions, "</span>",
        "</div>",
        "</div>"
      ),
      "```",
      ""
    )
  }

  if (has_step_images) {
    lines <- c(
      lines,
      "```{=html}",
      "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/glightbox/dist/css/glightbox.min.css\">",
      "<script src=\"https://cdn.jsdelivr.net/npm/glightbox/dist/js/glightbox.min.js\"></script>",
      "```",
      ""
    )
  }

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
            ing_list[[key]] <- ing
          }
        }
      }
    }
    lines <- c(lines, "```{=html}", "<ul class=\"recipe-ingredients\">")
    for (item in ing_list) lines <- c(lines, render_ingredient_li(item))
    lines <- c(lines, "</ul>", "```")
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

  # ---- Preparation steps (2-column cooking layout) ----
  lines <- c(lines, "## Préparation", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "", "```{=html}", "<div class=\"recipe-prep-grid\">")

    for (i in seq_along(section$etapes)) {
      step <- section$etapes[[i]]
      step_text <- escape_html(step$etape %||% "")

      ing_html <- ""
      if (is.list(step$ingredients) && length(step$ingredients) > 0) {
        ing_lines <- vapply(step$ingredients, render_ingredient_li, character(1))
        ing_html <- paste0(
          "<div class=\"recipe-prep-ingredients\">",
          "<div class=\"recipe-prep-label\">Ingrédients</div>",
          "<ul class=\"recipe-ingredients\">",
          paste(ing_lines, collapse = ""),
          "</ul>",
          "</div>"
        )
      } else {
        ing_html <- "<div class=\"recipe-prep-ingredients\"><div class=\"recipe-prep-label\">Ingrédients</div><div class=\"text-muted small\">Aucun ingrédient spécifique.</div></div>"
      }

      img_html <- ""
      if (!is.null(step$image_guid) && nzchar(as.character(step$image_guid))) {
        src <- paste0("/images/", step$image_guid, ".jpg")
        img_html <- paste0(
          "<a href=\"", src, "\" class=\"glightbox recipe-step-image-link\" data-gallery=\"recipe-steps\">",
          "<img src=\"", src, "\" alt=\"Image étape ", i, "\" class=\"recipe-step-thumb\">",
          "</a>"
        )
      }

      lines <- c(
        lines,
        paste0(
          "<article class=\"recipe-prep-step\">",
          ing_html,
          "<div class=\"recipe-prep-instruction\">",
          "<div class=\"recipe-prep-stepno\">Étape ", i, "</div>",
          "<p>", step_text, "</p>",
          img_html,
          "</div>",
          "</article>"
        )
      )
    }

    lines <- c(lines, "</div>", "```", "")
  }

  if (has_scaler) {
    lines <- c(
      lines,
      "```{=html}",
      paste0(
        "<script>",
        "(function(){",
        "const input=document.getElementById('servings-input');",
        "const reset=document.getElementById('servings-reset');",
        "if(!input) return;",
        "const base=", base_portions, ";",
        "const format=(n)=>{const r=Math.round(n*100)/100; return (Math.abs(r-Math.round(r))<1e-9)?String(Math.round(r)):String(r).replace('.',',');};",
        "const update=()=>{",
        "const current=parseFloat(input.value);",
        "if(!Number.isFinite(current)||current<=0) return;",
        "const ratio=current/base;",
        "document.querySelectorAll('.ingredient-qte[data-base]').forEach((el)=>{",
        "const b=parseFloat(el.getAttribute('data-base'));",
        "if(Number.isFinite(b)){el.textContent=format(b*ratio);} });",
        "};",
        "input.addEventListener('input', update);",
        "if(reset){reset.addEventListener('click', ()=>{input.value=String(base); update();});}",
        "update();",
        "})();",
        "</script>"
      ),
      "```",
      ""
    )
  }

  if (has_step_images) {
    lines <- c(
      lines,
      "```{=html}",
      "<script>document.addEventListener('DOMContentLoaded', function(){ if (window.GLightbox) { window.GLightbox({ selector: '.glightbox', touchNavigation: true, loop: true }); } });</script>",
      "```",
      ""
    )
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
