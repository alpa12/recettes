gha_ensure_gemini_api_key <- function() {
  current_key <- Sys.getenv("GEMINI_API_KEY", unset = "")
  if (nzchar(current_key)) return(invisible(TRUE))

  renv_paths <- c(".Renviron", "~/.Renviron")
  for (path in renv_paths) {
    expanded <- path.expand(path)
    if (!file.exists(expanded)) next
    tryCatch(readRenviron(expanded), error = function(e) NULL)
    if (nzchar(Sys.getenv("GEMINI_API_KEY", unset = ""))) return(invisible(TRUE))
  }

  stop("GEMINI_API_KEY introuvable. Definis la variable d'environnement ou ajoute-la dans .Renviron.")
}

gha_new_chat <- function() {
  gha_ensure_gemini_api_key()
  ellmer::chat_google_gemini(
    system_prompt = paste(
      "Tu es un expert en extraction de recettes.",
      "Tu dois repondre uniquement avec du YAML valide lorsque demande."
    )
  )
}

gha_prompt_rules <- function(source_url) {
  paste(
    "REGLES IMPORTANTES:",
    "1. Reponds UNIQUEMENT avec du YAML valide, sans texte additionnel.",
    "2. N'utilise pas de backticks.",
    "3. Respecte EXACTEMENT la structure du template fourni.",
    "4. Si une information manque, utilise une valeur raisonnable, sans laisser preparation vide.",
    "5. source doit etre", source_url, ".",
    "6. nom_court doit etre un slug simple.",
    "7. Chaque etape pertinente doit inclure un tableau ingredients.",
    "8. qte doit etre numerique (ex: 0.5, pas 1/2).",
    "9. Regle stricte anti-repetition: dans une etape, mets UNIQUEMENT les ingredients introduits pour la premiere fois a cette etape.",
    "10. Si un ingredient reapparait dans une etape ulterieure, ne le repete pas dans ingredients de cette etape.",
    sep = "\n"
  )
}

gha_build_prompt <- function(context_title, source_url, template_example, context_yaml, body_text) {
  glue::glue(
"Tu es un assistant qui extrait des recettes.

CONTEXTE: {context_title}
SOURCE URL: {source_url}

DONNEES D'AIDE:
{context_yaml}

CONTENU:
---
{body_text}
---

Genere un YAML valide avec EXACTEMENT la meme structure que ce template:
{template_example}

{gha_prompt_rules(source_url)}")
}

gha_parse_llm_yaml <- function(response_text) {
  yaml_content <- clean_yaml_response(response_text)
  yaml::yaml.load(yaml_content)
}

gha_classify_recipe_category <- function(chat, fallback = "repas") {
  category_raw <- chat$chat("Dans quelle categorie classerais-tu cette recette? Reponds en un seul mot: accompagnements, repas ou desserts.")
  category_raw <- tolower(trimws(as.character(category_raw %||% "")))
  category_map <- c(
    "accompagnement" = "accompagnements",
    "accompagnements" = "accompagnements",
    "repas" = "repas",
    "dessert" = "desserts",
    "desserts" = "desserts"
  )
  category_map[[category_raw]] %||% fallback
}

enforce_new_ingredients_by_step <- function(recipe_data) {
  prep <- recipe_data$preparation
  if (!is.list(prep) || length(prep) == 0) return(recipe_data)

  seen <- character()
  for (si in seq_along(prep)) {
    steps <- prep[[si]]$etapes
    if (!is.list(steps)) next

    for (ti in seq_along(steps)) {
      ings <- steps[[ti]]$ingredients
      if (!is.list(ings) || length(ings) == 0) {
        recipe_data$preparation[[si]]$etapes[[ti]]$ingredients <- list()
        next
      }

      kept <- list()
      for (ing in ings) {
        nm <- slugify(ing$nom %||% "")
        if (!nzchar(nm)) next
        if (nm %in% seen) next
        seen <- c(seen, nm)
        kept[[length(kept) + 1L]] <- ing
      }

      recipe_data$preparation[[si]]$etapes[[ti]]$ingredients <- kept
    }
  }

  recipe_data
}

apply_recipe_defaults <- function(recipe_data, source_url, fallback_title, portions_text) {
  if (!nzchar(clean_line(recipe_data$nom %||% ""))) {
    recipe_data$nom <- clean_line(fallback_title %||% "Recette importee")
  }

  if (!nzchar(clean_line(recipe_data$nom_court %||% ""))) {
    recipe_data$nom_court <- slugify(recipe_data$nom)
  }

  if (!nzchar(clean_line(recipe_data$source %||% ""))) {
    recipe_data$source <- source_url
  }

  portions_value <- suppressWarnings(as.numeric(recipe_data$portions %||% NA_real_))
  if (!(length(portions_value) == 1 && is.finite(portions_value) && portions_value > 0)) {
    recipe_data$portions <- extract_portions_from_text(portions_text) %||% 4
  }

  if (is.null(recipe_data$commentaires)) recipe_data$commentaires <- list()
  recipe_data
}

inject_fallback_preparation <- function(recipe_data, instruction_lines) {
  if (has_any_steps(recipe_data)) return(recipe_data)

  lines <- unique(vapply(instruction_lines %||% character(0), clean_line, character(1)))
  lines <- lines[nzchar(lines)]
  lines <- lines[nchar(lines) >= 20 & nchar(lines) <= 280]
  if (length(lines) == 0) lines <- "Preparer les ingredients et cuire selon la recette."

  recipe_data$preparation <- list(
    list(
      section = "Preparation",
      etapes = lapply(lines, function(txt) list(etape = txt, ingredients = list()))
    )
  )
  recipe_data
}

inject_fallback_ingredients <- function(recipe_data, ingredient_lines) {
  if (length(ingredient_lines) == 0) return(recipe_data)
  if (has_any_step_ingredients(recipe_data)) return(recipe_data)

  if (!is.list(recipe_data$preparation) || length(recipe_data$preparation) == 0) return(recipe_data)
  if (!is.list(recipe_data$preparation[[1]]$etapes) || length(recipe_data$preparation[[1]]$etapes) == 0) return(recipe_data)

  parsed <- lapply(ingredient_lines, ingredient_line_to_obj)
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (length(parsed) == 0) return(recipe_data)

  recipe_data$preparation[[1]]$etapes[[1]]$ingredients <- parsed
  recipe_data
}

gha_finalize_recipe <- function(recipe_data, source_url, submitted_by, fallback_title, portions_text, ingredient_lines, instruction_lines) {
  recipe_data <- inject_fallback_preparation(recipe_data, instruction_lines)
  recipe_data <- inject_fallback_ingredients(recipe_data, ingredient_lines)
  recipe_data <- apply_recipe_defaults(recipe_data, source_url, fallback_title, portions_text)
  recipe_data <- enforce_new_ingredients_by_step(recipe_data)
  recipe_data$soumis_par <- submitted_by
  recipe_data
}

gha_write_recipe_and_qmd <- function(recipe_data, recipe_category, filename_base) {
  yaml_file <- glue::glue("recettes/{recipe_category}/{filename_base}.yaml")
  fs::dir_create(dirname(yaml_file), recurse = TRUE)
  yaml::write_yaml(recipe_data, yaml_file)
  yaml_recipe_to_qmd(yaml_path = yaml_file)
  yaml_file
}
