#!/usr/bin/env Rscript

library(yaml)
library(httr)
library(rvest)
library(jsonlite)
library(glue)
library(ellmer)
library(gargle)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

as_character_vec <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.character(x)) return(trimws(x))
  out <- unlist(x, recursive = TRUE, use.names = FALSE)
  out <- as.character(out)
  trimws(out[nzchar(trimws(out))])
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
    parsed <- tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next
    recipes <- get_recipe_nodes(parsed)
    if (length(recipes) > 0) return(recipes[[1]])
  }

  NULL
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
    if (!is.null(item$itemListElement)) {
      out <- c(out, extract_instruction_lines(item$itemListElement))
    }
  }

  out <- out[nzchar(out)]
  unique(out)
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

parse_numeric_token <- function(token) {
  t <- trimws(token)
  if (!nzchar(t)) return(NULL)
  t <- gsub(",", ".", t)
  if (grepl("^[0-9]+$", t)) return(as.numeric(t))
  if (grepl("^[0-9]+\\.[0-9]+$", t)) return(as.numeric(t))
  if (grepl("^[0-9]+/[0-9]+$", t)) {
    p <- strsplit(t, "/", fixed = TRUE)[[1]]
    d <- as.numeric(p[2])
    if (!is.na(d) && d != 0) return(as.numeric(p[1]) / d)
  }
  NULL
}

extract_leading_quantity <- function(line) {
  s <- trimws(line)
  if (!nzchar(s)) return(list(qte = NULL, remainder = ""))
  parts <- strsplit(s, "\\s+")[[1]]
  if (length(parts) == 0) return(list(qte = NULL, remainder = s))

  q1 <- parse_numeric_token(parts[1])
  if (!is.null(q1)) {
    if (length(parts) >= 2) {
      q2 <- parse_numeric_token(parts[2])
      if (!is.null(q2) && grepl("/", parts[2])) {
        rest <- paste(parts[-c(1, 2)], collapse = " ")
        return(list(qte = q1 + q2, remainder = trimws(rest)))
      }
    }
    rest <- paste(parts[-1], collapse = " ")
    return(list(qte = q1, remainder = trimws(rest)))
  }

  list(qte = NULL, remainder = s)
}

extract_unit_and_name <- function(text) {
  s <- trimws(text)
  if (!nzchar(s)) return(list(uni = "unite", nom = ""))

  unit_patterns <- c(
    "^c\\.\\s*a\\s*soupe\\b",
    "^c\\.\\s*a\\s*the\\b",
    "^tasses?\\b",
    "^ml\\b",
    "^l\\b",
    "^g\\b",
    "^kg\\b",
    "^lb\\b",
    "^oz\\b",
    "^gousses?\\b",
    "^pincees?\\b",
    "^boites?\\b",
    "^conserves?\\b",
    "^branches?\\b",
    "^paquets?\\b",
    "^tranches?\\b"
  )

  lowered <- tolower(iconv(s, from = "", to = "ASCII//TRANSLIT"))
  for (pat in unit_patterns) {
    m <- regexpr(pat, lowered, perl = TRUE)
    if (m[1] == 1) {
      len <- attr(m, "match.length")
      uni <- substr(s, 1, len)
      nom <- trimws(substr(s, len + 1, nchar(s)))
      if (!nzchar(nom)) nom <- s
      return(list(uni = uni, nom = nom))
    }
  }

  list(uni = "unite", nom = s)
}

ingredient_line_to_obj <- function(line) {
  raw <- trimws(gsub("^[\\-\\*\\u2022\\s]+", "", as.character(line %||% "")))
  raw <- gsub("\\s+", " ", raw)
  if (!nzchar(raw)) return(NULL)

  qty <- extract_leading_quantity(raw)
  qte <- qty$qte %||% 1
  unit_and_name <- extract_unit_and_name(qty$remainder %||% raw)
  nom <- unit_and_name$nom
  if (!nzchar(nom)) nom <- raw

  list(
    nom = nom,
    qte = as.numeric(round(qte, 3)),
    uni = unit_and_name$uni
  )
}

has_any_step_ingredients <- function(recipe_data) {
  prep <- recipe_data$preparation
  if (!is.list(prep)) return(FALSE)
  for (sec in prep) {
    steps <- sec$etapes
    if (!is.list(steps)) next
    for (st in steps) {
      if (is.list(st$ingredients) && length(st$ingredients) > 0) return(TRUE)
    }
  }
  FALSE
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

# Lire le fichier URL
url_file <- Sys.getenv("RECIPE_URL_FILE")
if (!file.exists(url_file)) {
  stop("Fichier URL introuvable: ", url_file)
}

url_data <- yaml::read_yaml(url_file)
recipe_url <- url_data$url
submitted_by <- url_data$submitted_by %||% "Import automatique"

cat("📥 Import de la recette depuis:", recipe_url, "\n")

# Scraper la page web
cat("🌐 Téléchargement de la page...\n")
page <- tryCatch({
  rvest::read_html(recipe_url)
}, error = function(e) {
  stop("Erreur lors du téléchargement de l'URL: ", e$message)
})

# Extraire les données structurées (JSON-LD Recipe) quand disponibles
structured_recipe <- extract_structured_recipe(page)
if (!is.null(structured_recipe)) {
  cat("✅ Données structurées Recipe détectées (JSON-LD)\n")
  cat("   - ingrédients:", length(structured_recipe$ingredients), "\n")
  cat("   - étapes:", length(structured_recipe$instructions), "\n")
} else {
  cat("⚠️ Aucune donnée Recipe JSON-LD détectée, fallback texte brut.\n")
}

# Extraire le contenu texte de la page
page_text <- page |> 
  html_text() |> 
  trimws()

# Tronquer si trop long (pour éviter de dépasser les limites du LLM)
if (nchar(page_text) > 50000) {
  page_text <- substr(page_text, 1, 50000)
  cat("⚠️ Contenu tronqué à 50000 caractères\n")
}

# Charger le template
template <- yaml::read_yaml("recettes/template.yaml")

cat("📋 Template chargé avec structure:\n")
str(template, max.level = 2)

# Convertir le template en exemple YAML pour le prompt (avec indentation correcte)
template_example <- yaml::as.yaml(template)

# Créer le prompt pour le LLM de manière dynamique basé sur le template
prompt <- glue::glue('
Tu es un assistant qui extrait des recettes depuis des pages web.

DONNÉES STRUCTURÉES EXTRAITES (prioritaires si présentes):
{yaml::as.yaml(structured_recipe %||% list(note = "Aucune donnée structurée détectée"))}

Voici le contenu d\'une page web contenant une recette :

---
{page_text}
---

SOURCE URL: {recipe_url}

Extrais les informations de cette recette et génère un fichier YAML avec EXACTEMENT la même structure que cet exemple de template :

{template_example}

RÈGLES IMPORTANTES:
1. Réponds UNIQUEMENT avec le YAML valide, sans texte d\'introduction ni conclusion
2. Ne mets PAS le YAML entre des triple backticks  
3. Commence directement par le premier champ du template
4. Respecte EXACTEMENT la même structure que le template fourni ci-dessus
5. Remplace les valeurs d\'exemple du template par les vraies informations extraites de la recette
6. Les valeurs numériques doivent être des nombres entiers sans guillemets
7. Utilise des tirets (-) pour les listes avec indentation correcte (2 espaces)
8. Si une information est manquante, utilise null ou une valeur par défaut raisonnable
9. Pour nom_court: enlève les accents, articles, et garde maximum 5 mots
10. Pour la source: utilise {recipe_url}
11. Le champ soumis_par sera ajouté automatiquement après, ne l\'inclus PAS dans ta réponse
12. IMPORTANT: n\'oublie PAS les ingrédients. Chaque étape pertinente doit avoir un tableau `ingredients` non vide.
13. Dans chaque ingrédient: `qte` doit être un nombre (ex: 0.5), `uni` un texte court, `nom` le nom de l\'ingrédient.

Extrais maintenant les informations de la recette et génère le YAML complet.
')

# Appeler le LLM avec ellmer (GitHub Copilot)
cat("🤖 Extraction des informations avec GitHub Copilot...\n")

chat <- chat_google_gemini(
  system_prompt = paste(
    "Tu es un expert en extraction de recettes.",
    "Tu réponds uniquement avec du YAML valide, sans texte additionnel, à moins qu'on demande explicitement autre chose."
  )
)

response <- chat$chat(prompt)
yaml_content <- response

cat("📝 Réponse du LLM reçue\n")

# Nettoyer la réponse (enlever les backticks si présents)
yaml_content <- gsub("^```yaml\\s*", "", yaml_content)
yaml_content <- gsub("^```\\s*", "", yaml_content)
yaml_content <- gsub("\\s*```$", "", yaml_content)
yaml_content <- trimws(yaml_content)

cat("📄 YAML nettoyé:\n")
cat(substr(yaml_content, 1, 500), "...\n")

# Valider et parser le YAML
recipe_data <- tryCatch({
  yaml::yaml.load(yaml_content)
}, error = function(e) {
  cat("❌ Erreur lors du parsing YAML:\n")
  cat(yaml_content, "\n")
  stop("YAML invalide généré par le LLM: ", e$message)
})

cat("✅ YAML valide généré\n")

# Fallback: si le LLM oublie les ingrédients, injecter la liste structurée dans la 1re étape.
recipe_data <- inject_fallback_ingredients(recipe_data, structured_recipe$ingredients %||% character(0))

# Ajouter le champ soumis_par
recipe_data$soumis_par <- submitted_by

# Demander la catégorie de la recette au LLM
recipe_category <- chat$chat("Dans quelle catégorie classerais-tu cette recette? Réponds en un seul mot. Choix : Accompagnements, Repas, Desserts.") |>
  trimws() |> 
  tolower()

category_map <- c(
  "accompagnement" = "accompagnements",
  "accompagnements" = "accompagnements",
  "repas" = "repas",
  "dessert" = "desserts",
  "desserts" = "desserts"
)
recipe_category <- category_map[[recipe_category]] %||% "repas"

# Générer le nom de fichier
filename_base <- gsub("[^a-z0-9]+", "-", tolower(recipe_data$nom_court))
filename_base <- gsub("^-|-$", "", filename_base)

yaml_file <- glue("recettes/{recipe_category}/{filename_base}.yaml")
fs::dir_create(dirname(yaml_file), recurse = TRUE)

# Sauvegarder le fichier YAML
cat("💾 Sauvegarde de", yaml_file, "\n")
yaml::write_yaml(recipe_data, yaml_file)

# Charger la fonction yaml_to_qmd depuis le projet
cat("📦 Chargement de la fonction yaml_to_qmd...\n")
source("R/yaml_to_qmd.R")

# Générer le fichier QMD avec la fonction existante
cat("💾 Génération du qmd avec yaml_to_qmd()...\n")
yaml_recipe_to_qmd(yaml_path = yaml_file)

cat("✅ Import terminé avec succès!\n")
cat("📄 Fichier yaml généré:\n")
cat("  -", yaml_file, "\n")
