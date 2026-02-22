as_character_vec <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.character(x)) return(trimws(x))
  out <- unlist(x, recursive = TRUE, use.names = FALSE)
  out <- as.character(out)
  trimws(out[nzchar(trimws(out))])
}

is_noise_line <- function(x) {
  s <- tolower(iconv(clean_line(x), from = "", to = "ASCII//TRANSLIT"))
  if (!nzchar(s)) return(TRUE)
  if (nchar(s) < 3) return(TRUE)
  grepl(
    "^(menu|navigation|accueil|connexion|compte|profil|rechercher|search|newsletter|politique|cookies|publicite|sponsored|voir plus|lire aussi)$",
    s,
    perl = TRUE
  )
}

split_clean_lines <- function(text) {
  lines <- unlist(strsplit(as.character(text %||% ""), "\n", fixed = TRUE), use.names = FALSE)
  lines <- vapply(lines, clean_line, character(1))
  lines <- lines[nzchar(lines)]
  unique(lines)
}

guess_portions <- function(x) {
  if (is.null(x)) return(NULL)
  v <- as.character(x)
  if (length(v) == 0) return(NULL)
  v <- paste(v, collapse = " ")
  m <- regexec("(\\d+)\\s*(portions?|personnes?|servings?)", tolower(v), perl = TRUE)
  p <- regmatches(tolower(v), m)[[1]]
  if (length(p) >= 2) {
    out <- suppressWarnings(as.numeric(p[2]))
    if (is.finite(out) && out > 0) return(as.integer(round(out)))
  }
  m2 <- regexec("\\b(\\d{1,2})\\b", v, perl = TRUE)
  p2 <- regmatches(v, m2)[[1]]
  if (length(p2) >= 2) {
    out <- suppressWarnings(as.numeric(p2[2]))
    if (is.finite(out) && out > 0 && out <= 30) return(as.integer(round(out)))
  }
  NULL
}

extract_recipe_page_context <- function(page) {
  selectors <- c(
    "article", "main", "[itemtype*='Recipe']", ".wprm-recipe-container",
    ".recipe", ".entry-content", ".post-content", ".single-post"
  )

  blocks <- character()
  for (sel in selectors) {
    nodes <- rvest::html_elements(page, sel)
    if (length(nodes) == 0) next
    txt <- vapply(nodes, function(n) rvest::html_text2(n, preserve_nbsp = FALSE), character(1))
    txt <- txt[nzchar(trimws(txt))]
    blocks <- c(blocks, txt)
  }

  if (length(blocks) == 0) blocks <- rvest::html_text2(page, preserve_nbsp = FALSE)
  blocks <- unique(blocks)

  score_block <- function(txt) {
    t <- tolower(iconv(txt, from = "", to = "ASCII//TRANSLIT"))
    score <- 0
    if (grepl("ingredient", t)) score <- score + 3
    if (grepl("preparation|instructions?|etapes?|methode", t)) score <- score + 3
    if (grepl("portion|servings?|rendement", t)) score <- score + 1
    if (nchar(t) > 1000) score <- score + 1
    if (nchar(t) > 80000) score <- score - 2
    score
  }

  ord <- order(vapply(blocks, score_block, numeric(1)), decreasing = TRUE)
  blocks <- blocks[ord]
  focused_text <- paste(head(blocks, 3), collapse = "\n\n")
  focused_lines <- split_clean_lines(focused_text)

  lowered <- tolower(iconv(focused_lines, from = "", to = "ASCII//TRANSLIT"))
  ing_idx <- which(grepl("^ingredients?$|\\bingredients?\\b", lowered))[1]
  prep_idx <- which(grepl("^preparation$|^etapes?$|^instructions?$|\\bpreparation\\b|\\betapes?\\b|\\binstructions?\\b", lowered))[1]
  stop_idx <- which(grepl("^nutrition$|^notes?$|^commentaires?$|^astuces?$|^faq$", lowered))[1]

  ingredient_scope <- focused_lines
  if (!is.na(ing_idx)) {
    end_idx <- c(prep_idx, stop_idx, length(focused_lines) + 1)
    end_idx <- end_idx[!is.na(end_idx) & end_idx > ing_idx]
    ingredient_scope <- focused_lines[ing_idx:(min(end_idx) - 1)]
  }

  instruction_scope <- focused_lines
  if (!is.na(prep_idx)) {
    end_idx <- c(stop_idx, length(focused_lines) + 1)
    end_idx <- end_idx[!is.na(end_idx) & end_idx > prep_idx]
    instruction_scope <- focused_lines[prep_idx:(min(end_idx) - 1)]
  }

  ingredient_lines <- ingredient_scope[
    !vapply(ingredient_scope, is_noise_line, logical(1)) &
      !grepl("ingredient|preparation|instruction|etape", tolower(iconv(ingredient_scope, from = "", to = "ASCII//TRANSLIT"))) &
      nchar(ingredient_scope) <= 140 &
      (grepl("^(?:-|\\*|\u2022)\\s*", ingredient_scope, perl = TRUE) |
         grepl("^\\d", ingredient_scope) |
         grepl("\\b(c\\.|tasse|ml|g|kg|lb|oz|pincee|gousse|boite|cuill)", tolower(iconv(ingredient_scope, from = "", to = "ASCII//TRANSLIT"))))
  ]
  ingredient_lines <- unique(ingredient_lines)

  instruction_lines <- instruction_scope[
    !vapply(instruction_scope, is_noise_line, logical(1)) &
      !grepl("^ingredients?$", tolower(iconv(instruction_scope, from = "", to = "ASCII//TRANSLIT"))) &
      nchar(instruction_scope) >= 20 &
      nchar(instruction_scope) <= 400
  ]
  instruction_lines <- unique(instruction_lines)

  title_guess <- tryCatch(
    clean_line(rvest::html_text2(rvest::html_element(page, "h1"), preserve_nbsp = FALSE)),
    error = function(e) ""
  )

  list(
    title_guess = title_guess,
    portions_guess = guess_portions(focused_lines),
    ingredient_lines = ingredient_lines,
    instruction_lines = instruction_lines,
    focused_text = focused_text
  )
}

duration_to_minutes <- function(x) {
  v <- trimws(as.character(x %||% ""))
  if (!nzchar(v)) return(NULL)
  m <- regmatches(v, regexec("^PT(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?$", v, perl = TRUE))[[1]]
  if (length(m) == 0) return(NULL)
  hh <- if (is.na(m[2]) || !nzchar(m[2])) 0 else as.numeric(m[2])
  mm <- if (is.na(m[3]) || !nzchar(m[3])) 0 else as.numeric(m[3])
  ss <- if (is.na(m[4]) || !nzchar(m[4])) 0 else as.numeric(m[4])
  as.integer(round(hh * 60 + mm + ss / 60))
}

extract_instruction_lines <- function(instr) {
  if (is.null(instr)) return(character(0))
  if (is.character(instr)) {
    v <- trimws(instr)
    return(v[nzchar(v)])
  }
  if (!is.list(instr)) return(character(0))

  out <- character(0)
  for (item in instr) {
    if (is.character(item)) {
      out <- c(out, trimws(item))
      next
    }
    if (!is.list(item)) next
    text <- item$text %||% item$name
    if (!is.null(text)) out <- c(out, trimws(as.character(text)))
    if (!is.null(item$itemListElement)) out <- c(out, extract_instruction_lines(item$itemListElement))
  }

  out <- out[nzchar(out)]
  unique(out)
}

extract_recipe_json_ld <- function(page) {
  script_nodes <- rvest::html_elements(page, "script[type='application/ld+json']")
  if (length(script_nodes) == 0) return(NULL)

  get_recipe_nodes <- function(node) {
    if (is.null(node)) return(list())
    if (is.list(node) && !is.null(node[["@graph"]])) {
      out <- list()
      for (item in node[["@graph"]]) out <- c(out, get_recipe_nodes(item))
      return(out)
    }
    if (is.list(node) && !is.null(node[["@type"]])) {
      types <- tolower(as_character_vec(node[["@type"]]))
      if ("recipe" %in% types) return(list(node))
    }
    if (is.list(node)) {
      out <- list()
      for (item in node) out <- c(out, get_recipe_nodes(item))
      return(out)
    }
    list()
  }

  for (txt in rvest::html_text(script_nodes)) {
    parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) next
    recipes <- get_recipe_nodes(parsed)
    if (length(recipes) > 0) return(recipes[[1]])
  }

  NULL
}

extract_structured_recipe <- function(page) {
  recipe <- extract_recipe_json_ld(page)
  if (is.null(recipe)) return(NULL)

  list(
    name = as.character(recipe$name %||% ""),
    description = as.character(recipe$description %||% ""),
    recipeYield = as_character_vec(recipe$recipeYield),
    prep_minutes = duration_to_minutes(recipe$prepTime),
    cook_minutes = duration_to_minutes(recipe$cookTime),
    total_minutes = duration_to_minutes(recipe$totalTime),
    category = as_character_vec(recipe$recipeCategory),
    cuisine = as_character_vec(recipe$recipeCuisine),
    ingredients = as_character_vec(recipe$recipeIngredient),
    instructions = extract_instruction_lines(recipe$recipeInstructions)
  )
}

#' Import a recipe from a generic URL YAML request.
#'
#' GitHub Actions entrypoint for URL-based recipe imports.
#'
#' @param url_file Path to YAML request file (usually from `RECIPE_URL_FILE`).
#' @return Generated recipe YAML file path invisibly.
#' @export
gha_import_recipe_from_url <- function(url_file = Sys.getenv("RECIPE_URL_FILE")) {
  if (!file.exists(url_file)) stop("Fichier URL introuvable: ", url_file)

  url_data <- yaml::read_yaml(url_file)
  recipe_url <- as.character(url_data$url %||% "")
  submitted_by <- url_data$submitted_by %||% "Import automatique"

  cat("Import de la recette depuis:", recipe_url, "\n")
  page <- tryCatch(rvest::read_html(recipe_url), error = function(e) stop("Erreur lors du telechargement de l'URL: ", e$message))

  structured_recipe <- extract_structured_recipe(page)
  page_context <- extract_recipe_page_context(page)

  candidate_ingredient_lines <- unique(c(
    structured_recipe$ingredients %||% character(0),
    page_context$ingredient_lines %||% character(0)
  ))
  candidate_instruction_lines <- unique(c(
    structured_recipe$instructions %||% character(0),
    page_context$instruction_lines %||% character(0)
  ))

  page_text <- trimws(as.character(page_context$focused_text %||% ""))
  if (!nzchar(page_text)) page_text <- trimws(rvest::html_text2(page, preserve_nbsp = FALSE))
  if (nchar(page_text) > 50000) page_text <- substr(page_text, 1, 50000)

  template <- yaml::read_yaml("recettes/template.yaml")
  template_example <- yaml::as.yaml(template)

  context_yaml <- yaml::as.yaml(list(
    structured_recipe = structured_recipe %||% list(note = "Aucune donnee structuree detectee"),
    titre_page = page_context$title_guess %||% NULL,
    portions_candidates = page_context$portions_guess %||% NULL,
    ingredients_candidates = head(candidate_ingredient_lines, 120),
    etapes_candidates = head(candidate_instruction_lines, 120)
  ))

  prompt <- gha_build_prompt(
    context_title = "Page web",
    source_url = recipe_url,
    template_example = template_example,
    context_yaml = context_yaml,
    body_text = page_text
  )

  chat <- gha_new_chat()
  response <- chat$chat(prompt)
  recipe_data <- tryCatch(
    gha_parse_llm_yaml(response),
    error = function(e) stop("YAML invalide genere par le LLM: ", e$message)
  )

  recipe_data <- gha_finalize_recipe(
    recipe_data = recipe_data,
    source_url = recipe_url,
    submitted_by = submitted_by,
    fallback_title = structured_recipe$name %||% page_context$title_guess %||% "Recette importee",
    portions_text = paste(
      structured_recipe$recipeYield %||% "",
      page_context$portions_guess %||% "",
      candidate_instruction_lines,
      collapse = " "
    ),
    ingredient_lines = candidate_ingredient_lines,
    instruction_lines = candidate_instruction_lines
  )

  recipe_category <- gha_classify_recipe_category(chat)
  filename_base <- slugify(recipe_data$nom_court)
  if (!nzchar(filename_base)) filename_base <- paste0("recette-web-", as.integer(Sys.time()))

  yaml_file <- gha_write_recipe_and_qmd(recipe_data, recipe_category, filename_base)

  cat("Import termine. Fichier YAML genere:", yaml_file, "\n")
  invisible(yaml_file)
}
