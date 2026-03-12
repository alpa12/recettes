as_character_vec <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.character(x)) return(trimws(x))
  out <- unlist(x, recursive = TRUE, use.names = FALSE)
  out <- as.character(out)
  trimws(out[nzchar(trimws(out))])
}

is_noise_line <- function(x) {
  s <- tolower(iconv(clean_line(x), from = "", to = "ASCII//TRANSLIT"))
  if (is.na(s)) s <- tolower(clean_line(x))
  if (!nzchar(s)) return(TRUE)
  if (nchar(s) < 3) return(TRUE)
  grepl(
    "^(menu|navigation|accueil|connexion|compte|profil|rechercher|search|newsletter|politique|cookies|publicite|sponsored|voir plus|lire aussi)$",
    s,
    perl = TRUE
  )
}

split_clean_lines <- function(text) {
  lines <- unlist(strsplit(as.character(text %||% ""), "[\\r\\n]+", perl = TRUE), use.names = FALSE)
  lines <- vapply(lines, clean_line, character(1))
  lines <- lines[nzchar(lines)]
  unique(lines)
}

extract_recipe_text_context <- function(text) {
  lines <- split_clean_lines(text)
  lowered <- tolower(iconv(lines, from = "", to = "ASCII//TRANSLIT"))
  lowered[is.na(lowered)] <- tolower(lines[is.na(lowered)])

  ing_idx <- which(grepl("^ingredients?$|\\bingredients?\\b", lowered))[1]
  prep_idx <- which(grepl("^preparation$|^etapes?$|^instructions?$|\\bpreparation\\b|\\betapes?\\b|\\binstructions?\\b|\\bmethode\\b", lowered))[1]
  stop_idx <- which(grepl("^nutrition$|^notes?$|^commentaires?$|^astuces?$|^faq$", lowered))[1]

  ingredient_scope <- lines
  if (!is.na(ing_idx)) {
    end_idx <- c(prep_idx, stop_idx, length(lines) + 1)
    end_idx <- end_idx[!is.na(end_idx) & end_idx > ing_idx]
    ingredient_scope <- lines[ing_idx:(min(end_idx) - 1)]
  }

  instruction_scope <- lines
  if (!is.na(prep_idx)) {
    end_idx <- c(stop_idx, length(lines) + 1)
    end_idx <- end_idx[!is.na(end_idx) & end_idx > prep_idx]
    instruction_scope <- lines[prep_idx:(min(end_idx) - 1)]
  }

  ascii_ingredient_scope <- tolower(iconv(ingredient_scope, from = "", to = "ASCII//TRANSLIT"))
  ascii_ingredient_scope[is.na(ascii_ingredient_scope)] <- tolower(ingredient_scope[is.na(ascii_ingredient_scope)])
  ascii_instruction_scope <- tolower(iconv(instruction_scope, from = "", to = "ASCII//TRANSLIT"))
  ascii_instruction_scope[is.na(ascii_instruction_scope)] <- tolower(instruction_scope[is.na(ascii_instruction_scope)])

  ingredient_lines <- ingredient_scope[
    !vapply(ingredient_scope, is_noise_line, logical(1)) &
      !grepl("ingredient|preparation|instruction|etape", ascii_ingredient_scope) &
      nchar(ingredient_scope) <= 140 &
      (grepl("^(?:-|\\*|\u2022)\\s*", ingredient_scope, perl = TRUE) |
         grepl("^\\d", ingredient_scope) |
         grepl("\\b(c\\.|tasse|ml|g|kg|lb|oz|pincee|gousse|boite|cuill|tofu|sauce|huile|sucre|sel|poivre)\\b", ascii_ingredient_scope))
  ]
  ingredient_lines <- unique(ingredient_lines)

  instruction_lines <- instruction_scope[
    !vapply(instruction_scope, is_noise_line, logical(1)) &
      !grepl("^ingredients?$", ascii_instruction_scope) &
      nchar(instruction_scope) >= 20 &
      nchar(instruction_scope) <= 400
  ]
  instruction_lines <- unique(instruction_lines)

  title_guess <- ""
  if (length(lines) > 0) title_guess <- lines[[1]]
  if (!nzchar(title_guess)) {
    m <- which(grepl("^title\\s*:", lowered))[1]
    if (!is.na(m)) title_guess <- sub("^title\\s*:\\s*", "", lines[[m]], perl = TRUE)
  }

  list(
    title_guess = clean_line(title_guess),
    portions_guess = guess_portions(lines),
    ingredient_lines = ingredient_lines,
    instruction_lines = instruction_lines,
    focused_text = paste(head(lines, 1200), collapse = "\n")
  )
}

fetch_jina_markdown <- function(url) {
  safe_url <- sub("^https?://", "", as.character(url %||% ""))
  if (!nzchar(safe_url)) return("")
  jina_url <- paste0("https://r.jina.ai/http://", safe_url)
  txt <- tryCatch(
    {
      resp <- httr2::request(jina_url) |>
        httr2::req_timeout(20) |>
        httr2::req_perform()
      httr2::resp_body_string(resp)
    },
    error = function(e) ""
  )
  txt <- as.character(txt %||% "")
  if (!nzchar(txt)) return("")
  # Drop jina metadata header if present.
  txt <- sub("^.*?Markdown Content:\\s*", "", txt, perl = TRUE)
  trimws(txt)
}

read_html_safe <- function(url) {
  tryCatch(rvest::read_html(url), error = function(e) NULL)
}

find_alternate_recipe_urls <- function(page, source_url) {
  hrefs <- tryCatch(
    rvest::html_attr(rvest::html_elements(page, "a[href]"), "href"),
    error = function(e) character(0)
  )
  hrefs <- hrefs[nzchar(trimws(hrefs))]
  if (length(hrefs) == 0) return(character(0))

  abs_hrefs <- unique(vapply(hrefs, function(h) {
    out <- tryCatch(xml2::url_absolute(h, source_url), error = function(e) "")
    as.character(out %||% "")
  }, character(1)))
  abs_hrefs <- abs_hrefs[nzchar(abs_hrefs)]
  abs_hrefs <- abs_hrefs[grepl("/recipe/", abs_hrefs, fixed = TRUE)]
  abs_hrefs <- abs_hrefs[!grepl("#respond$|/fw-recipe-taxonomy-slug/|facebook.com/sharer|twitter.com/home|pinterest.com/pin", abs_hrefs)]
  abs_hrefs <- abs_hrefs[abs_hrefs != source_url]
  if (length(abs_hrefs) == 0) return(character(0))

  # Prefer explicit alternate canonical recipe host if available.
  preferred <- abs_hrefs[grepl("thebuddhistchef\\.com/recipe/", abs_hrefs)]
  unique(c(preferred, abs_hrefs))
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
    ".entry-ingredient", ".entry-instruction", ".entry-description", ".hs-recipe.single",
    "[itemtype*='Recipe']", ".wprm-recipe-container", ".wprm-recipe",
    ".recipe", ".entry-content", ".post-content", ".single-post",
    "main", "article"
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
    if (is.na(t)) t <- tolower(as.character(txt %||% ""))
    score <- 0
    if (grepl("ingredient", t)) score <- score + 3
    if (grepl("preparation|instructions?|etapes?|methode", t)) score <- score + 3
    if (grepl("etapes a suivre|ingredients", t)) score <- score + 2
    if (grepl("portion|servings?|rendement", t)) score <- score + 1
    if (grepl("commentaire|repondre|cancel reply|comment-form", t)) score <- score - 4
    if (nchar(t) > 1000) score <- score + 1
    if (nchar(t) > 80000) score <- score - 2
    score
  }

  ord <- order(vapply(blocks, score_block, numeric(1)), decreasing = TRUE)
  blocks <- blocks[ord]
  focused_text <- paste(head(blocks, 3), collapse = "\n\n")
  focused_lines <- split_clean_lines(focused_text)

  lowered <- tolower(iconv(focused_lines, from = "", to = "ASCII//TRANSLIT"))
  lowered[is.na(lowered)] <- tolower(focused_lines[is.na(lowered)])
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
      !grepl("ingredient|preparation|instruction|etape", ifelse(is.na(tolower(iconv(ingredient_scope, from = "", to = "ASCII//TRANSLIT"))), tolower(ingredient_scope), tolower(iconv(ingredient_scope, from = "", to = "ASCII//TRANSLIT")))) &
      nchar(ingredient_scope) <= 140 &
      (grepl("^(?:-|\\*|\u2022)\\s*", ingredient_scope, perl = TRUE) |
         grepl("^\\d", ingredient_scope) |
         grepl("\\b(c\\.|tasse|ml|g|kg|lb|oz|pincee|gousse|boite|cuill)", ifelse(is.na(tolower(iconv(ingredient_scope, from = "", to = "ASCII//TRANSLIT"))), tolower(ingredient_scope), tolower(iconv(ingredient_scope, from = "", to = "ASCII//TRANSLIT")))))
  ]
  ingredient_lines <- unique(ingredient_lines)

  instruction_lines <- instruction_scope[
    !vapply(instruction_scope, is_noise_line, logical(1)) &
      !grepl("^ingredients?$", ifelse(is.na(tolower(iconv(instruction_scope, from = "", to = "ASCII//TRANSLIT"))), tolower(instruction_scope), tolower(iconv(instruction_scope, from = "", to = "ASCII//TRANSLIT")))) &
      nchar(instruction_scope) >= 20 &
      nchar(instruction_scope) <= 400
  ]
  instruction_lines <- unique(instruction_lines)

  title_guess <- tryCatch(
    clean_line(rvest::html_text2(rvest::html_element(page, "h1"), preserve_nbsp = FALSE)),
    error = function(e) ""
  )

  direct_ing_nodes <- rvest::html_elements(page, ".entry-ingredient td, .entry-ingredient li, .entry-ingredient p")
  direct_instruction_nodes <- rvest::html_elements(page, ".entry-instruction dd, .entry-instruction li, .entry-instruction p")
  direct_ingredient_lines <- unique(vapply(direct_ing_nodes, function(n) clean_line(rvest::html_text2(n, preserve_nbsp = FALSE)), character(1)))
  direct_instruction_lines <- unique(vapply(direct_instruction_nodes, function(n) clean_line(rvest::html_text2(n, preserve_nbsp = FALSE)), character(1)))

  direct_ingredient_lines <- direct_ingredient_lines[
    nzchar(direct_ingredient_lines) &
      nchar(direct_ingredient_lines) <= 160 &
      !grepl("^ingredients?$", tolower(iconv(direct_ingredient_lines, from = "", to = "ASCII//TRANSLIT")))
  ]
  direct_instruction_lines <- direct_instruction_lines[
    nzchar(direct_instruction_lines) &
      nchar(direct_instruction_lines) >= 20 &
      nchar(direct_instruction_lines) <= 420 &
      !grepl("^etapes? a suivre$|^instructions?$", tolower(iconv(direct_instruction_lines, from = "", to = "ASCII//TRANSLIT")))
  ]

  if (length(direct_ingredient_lines) >= 2) {
    ingredient_lines <- direct_ingredient_lines
  } else {
    ingredient_lines <- unique(c(direct_ingredient_lines, ingredient_lines))
  }
  if (length(direct_instruction_lines) >= 2) {
    instruction_lines <- direct_instruction_lines
  } else {
    instruction_lines <- unique(c(direct_instruction_lines, instruction_lines))
  }

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
    if (is.list(node) && !is.null(node[["@type"]])) {
      types <- tolower(as_character_vec(node[["@type"]]))
      if ("recipe" %in% types) return(list(node))
    }
    if (is.list(node) && !is.null(node[["@graph"]])) {
      out <- list()
      for (item in node[["@graph"]]) out <- c(out, get_recipe_nodes(item))
      if (length(out) > 0) return(out)
    }
    if (is.list(node)) {
      out <- list()
      nms <- names(node)
      if (is.null(nms)) {
        for (item in node) out <- c(out, get_recipe_nodes(item))
      } else {
        for (nm in nms) {
          if (identical(nm, "@graph")) next
          out <- c(out, get_recipe_nodes(node[[nm]]))
        }
      }
      return(out)
    }
    list()
  }

  for (txt in rvest::html_text(script_nodes)) {
    parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) next
    recipes <- get_recipe_nodes(parsed)
    if (length(recipes) == 0) next

    score_recipe_node <- function(node) {
      if (is.null(node) || !is.list(node)) return(-Inf)
      ing <- as_character_vec(node$recipeIngredient)
      instr <- extract_instruction_lines(node$recipeInstructions)
      name <- clean_line(node$name %||% "")
      score <- 0
      score <- score + min(length(ing), 40) * 3
      score <- score + min(length(instr), 40) * 3
      if (nzchar(name)) score <- score + 2
      if (!is.null(node$recipeYield)) score <- score + 1
      score
    }

    scores <- vapply(recipes, score_recipe_node, numeric(1))
    best_idx <- which.max(scores)
    if (length(best_idx) == 1 && is.finite(scores[best_idx])) {
      return(recipes[[best_idx]])
    }
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

  weak_direct_extraction <- (
    length(structured_recipe$ingredients %||% character(0)) < 2 &&
      length(structured_recipe$instructions %||% character(0)) < 3 &&
      (length(candidate_ingredient_lines) < 3 || !nzchar(page_context$title_guess %||% ""))
  )
  if (isTRUE(weak_direct_extraction)) {
    cat("Extraction directe pauvre; tentative fallback URL alternative...\n")
    alt_urls <- head(find_alternate_recipe_urls(page, recipe_url), 3)
    for (alt in alt_urls) {
      alt_page <- read_html_safe(alt)
      if (is.null(alt_page)) next
      alt_structured <- extract_structured_recipe(alt_page)
      alt_context <- extract_recipe_page_context(alt_page)

      candidate_ingredient_lines <- unique(c(
        candidate_ingredient_lines,
        alt_structured$ingredients %||% character(0),
        alt_context$ingredient_lines %||% character(0)
      ))
      candidate_instruction_lines <- unique(c(
        candidate_instruction_lines,
        alt_structured$instructions %||% character(0),
        alt_context$instruction_lines %||% character(0)
      ))

      if (!nzchar(page_context$title_guess %||% "")) {
        page_context$title_guess <- alt_structured$name %||% alt_context$title_guess %||% ""
      }
      if (is.null(page_context$portions_guess)) {
        page_context$portions_guess <- guess_portions(c(alt_structured$recipeYield %||% character(0), alt_context$focused_text %||% ""))
      }
      if (nchar(page_context$focused_text %||% "") < 2000 && nzchar(alt_context$focused_text %||% "")) {
        page_context$focused_text <- alt_context$focused_text
      }
    }

    still_weak <- (
      length(candidate_ingredient_lines) < 3 &&
        length(candidate_instruction_lines) < 3
    )
    if (isTRUE(still_weak)) {
      cat("Fallback URL alternative insuffisant; tentative fallback texte (r.jina.ai)...\n")
      jina_text <- fetch_jina_markdown(recipe_url)
      if (nzchar(jina_text)) {
        jina_context <- extract_recipe_text_context(jina_text)
        candidate_ingredient_lines <- unique(c(candidate_ingredient_lines, jina_context$ingredient_lines %||% character(0)))
        candidate_instruction_lines <- unique(c(candidate_instruction_lines, jina_context$instruction_lines %||% character(0)))
        if (!nzchar(page_context$title_guess %||% "")) page_context$title_guess <- jina_context$title_guess %||% ""
        if (is.null(page_context$portions_guess)) page_context$portions_guess <- jina_context$portions_guess %||% NULL
        if (nchar(page_context$focused_text %||% "") < 2000) {
          page_context$focused_text <- jina_context$focused_text %||% jina_text
        }
      }
    }
  }

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
