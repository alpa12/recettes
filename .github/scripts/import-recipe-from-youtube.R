#!/usr/bin/env Rscript

library(yaml)
library(glue)
library(ellmer)
library(fs)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

clean_line <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[\r\t]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

is_youtube_url <- function(u) {
  p <- tryCatch(utils::URLencode(u), error = function(e) u)
  grepl("youtube\\.com|youtu\\.be", p, ignore.case = TRUE)
}

extract_video_id <- function(u) {
  s <- as.character(u %||% "")
  if (!nzchar(s)) return(NULL)

  patterns <- c(
    "youtu\\.be/([A-Za-z0-9_-]{11})",
    "youtube\\.com/watch\\?.*v=([A-Za-z0-9_-]{11})",
    "youtube\\.com/embed/([A-Za-z0-9_-]{11})",
    "youtube\\.com/shorts/([A-Za-z0-9_-]{11})"
  )
  for (pat in patterns) {
    m <- regexec(pat, s, perl = TRUE)
    r <- regmatches(s, m)[[1]]
    if (length(r) >= 2) return(r[2])
  }
  NULL
}

read_vtt_as_text <- function(path) {
  if (!file.exists(path)) return("")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- gsub("\ufeff", "", lines, fixed = TRUE)
  lines <- lines[!grepl("^WEBVTT", lines)]
  lines <- lines[!grepl("^NOTE", lines)]
  lines <- lines[!grepl("^[0-9]+$", lines)]
  lines <- lines[!grepl("-->", lines)]
  lines <- gsub("<[^>]+>", "", lines)
  lines <- gsub("&nbsp;", " ", lines, fixed = TRUE)
  lines <- vapply(lines, clean_line, character(1))
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) return("")

  # remove consecutive duplicates often present in auto-captions
  keep <- c(TRUE, lines[-1] != lines[-length(lines)])
  lines <- lines[keep]
  paste(lines, collapse = "\n")
}

fetch_youtube_transcript <- function(video_url, video_id) {
  tmp <- fs::path_temp(paste0("yt-", video_id))
  fs::dir_create(tmp, recurse = TRUE)
  outtmpl <- fs::path(tmp, paste0(video_id, ".%(ext)s"))

  args <- c(
    "--skip-download",
    "--write-auto-subs",
    "--write-subs",
    "--sub-format", "vtt",
    "--sub-langs", "fr.*,en.*",
    "-o", outtmpl,
    video_url
  )
  out <- system2("yt-dlp", args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("yt-dlp a échoué: ", paste(out, collapse = "\n"))
  }

  vtt_files <- list.files(tmp, pattern = "\\.vtt$", full.names = TRUE)
  if (length(vtt_files) == 0) {
    stop("Aucune transcription VTT trouvée pour cette vidéo.")
  }

  prefer <- vtt_files[grepl("\\.fr([.-]|$)", vtt_files)]
  chosen <- if (length(prefer) > 0) prefer[[1]] else vtt_files[[1]]

  txt <- read_vtt_as_text(chosen)
  if (!nzchar(clean_line(txt))) {
    stop("Transcription vide après nettoyage.")
  }
  txt
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
    rest <- paste(parts[-1], collapse = " ")
    return(list(qte = q1, remainder = trimws(rest)))
  }
  list(qte = NULL, remainder = s)
}

extract_unit_and_name <- function(text) {
  s <- trimws(text)
  if (!nzchar(s)) return(list(uni = "unite", nom = ""))
  unit_patterns <- c(
    "^c\\.\\s*a\\s*soupe\\b", "^c\\.\\s*a\\s*the\\b", "^tasses?\\b",
    "^ml\\b", "^l\\b", "^g\\b", "^kg\\b", "^lb\\b", "^oz\\b",
    "^gousses?\\b", "^pincees?\\b", "^boites?\\b", "^branches?\\b",
    "^paquets?\\b", "^tranches?\\b"
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
  raw <- clean_line(gsub("^[\\-\\*•\\s]+", "", as.character(line %||% "")))
  if (!nzchar(raw)) return(NULL)
  qty <- extract_leading_quantity(raw)
  qte <- qty$qte %||% 1
  unit_and_name <- extract_unit_and_name(qty$remainder %||% raw)
  list(nom = unit_and_name$nom %||% raw, qte = as.numeric(round(qte, 3)), uni = unit_and_name$uni)
}

has_any_step_ingredients <- function(recipe_data) {
  prep <- recipe_data$preparation
  if (!is.list(prep)) return(FALSE)
  for (sec in prep) {
    steps <- sec$etapes
    if (!is.list(steps)) next
    for (st in steps) if (is.list(st$ingredients) && length(st$ingredients) > 0) return(TRUE)
  }
  FALSE
}

has_any_steps <- function(recipe_data) {
  prep <- recipe_data$preparation
  if (!is.list(prep) || length(prep) == 0) return(FALSE)
  for (sec in prep) {
    if (is.list(sec$etapes) && length(sec$etapes) > 0) return(TRUE)
  }
  FALSE
}

inject_fallback_preparation <- function(recipe_data, transcript_text) {
  if (has_any_steps(recipe_data)) return(recipe_data)
  lines <- unlist(strsplit(transcript_text %||% "", "[\\.!?]\n?|\\n", perl = TRUE), use.names = FALSE)
  lines <- vapply(lines, clean_line, character(1))
  lines <- unique(lines[nchar(lines) >= 20 & nchar(lines) <= 220])
  lines <- lines[!grepl("abonne|like|comment|clique|chaine|sponsor", tolower(iconv(lines, from = "", to = "ASCII//TRANSLIT")))]
  if (length(lines) == 0) lines <- "Préparer les ingrédients et suivre les étapes décrites dans la vidéo."
  lines <- head(lines, 8)
  recipe_data$preparation <- list(
    list(
      section = "Préparation",
      etapes = lapply(lines, function(txt) list(etape = txt, ingredients = list()))
    )
  )
  recipe_data
}

inject_fallback_ingredients <- function(recipe_data, transcript_text) {
  if (has_any_step_ingredients(recipe_data)) return(recipe_data)
  lines <- unlist(strsplit(transcript_text %||% "", "\n", fixed = TRUE), use.names = FALSE)
  lines <- vapply(lines, clean_line, character(1))
  lines <- lines[nzchar(lines)]
  candidate <- lines[
    nchar(lines) <= 140 &
      (grepl("^\\d", lines) | grepl("\\b(c\\.|tasse|ml|g|kg|lb|oz|pincee|gousse|cuill)", tolower(iconv(lines, from = "", to = "ASCII//TRANSLIT"))))
  ]
  candidate <- unique(head(candidate, 20))
  if (length(candidate) == 0) return(recipe_data)
  parsed <- lapply(candidate, ingredient_line_to_obj)
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (length(parsed) == 0) return(recipe_data)
  if (!is.list(recipe_data$preparation) || length(recipe_data$preparation) == 0) {
    recipe_data <- inject_fallback_preparation(recipe_data, transcript_text)
  }
  if (!is.list(recipe_data$preparation[[1]]$etapes) || length(recipe_data$preparation[[1]]$etapes) == 0) {
    recipe_data <- inject_fallback_preparation(recipe_data, transcript_text)
  }
  recipe_data$preparation[[1]]$etapes[[1]]$ingredients <- parsed
  recipe_data
}

extract_portions_from_text <- function(txt) {
  s <- tolower(iconv(as.character(txt %||% ""), from = "", to = "ASCII//TRANSLIT"))
  m <- regexec("(\\d+)\\s*(portions?|personnes?|servings?)", s, perl = TRUE)
  r <- regmatches(s, m)[[1]]
  if (length(r) >= 2) {
    n <- suppressWarnings(as.numeric(r[2]))
    if (is.finite(n) && n > 0) return(as.integer(round(n)))
  }
  NULL
}

apply_recipe_defaults <- function(recipe_data, source_url, transcript_text) {
  if (!nzchar(clean_line(recipe_data$nom %||% ""))) recipe_data$nom <- "Recette importée de YouTube"
  if (!nzchar(clean_line(recipe_data$nom_court %||% ""))) {
    base <- tolower(iconv(clean_line(recipe_data$nom), from = "", to = "ASCII//TRANSLIT"))
    base <- gsub("[^a-z0-9]+", "-", base)
    base <- gsub("^-|-$", "", base)
    recipe_data$nom_court <- base
  }
  if (!nzchar(clean_line(recipe_data$source %||% ""))) recipe_data$source <- source_url
  p <- suppressWarnings(as.numeric(recipe_data$portions %||% NA_real_))
  if (!(length(p) == 1 && is.finite(p) && p > 0)) {
    recipe_data$portions <- extract_portions_from_text(transcript_text) %||% 4
  }
  if (is.null(recipe_data$commentaires)) recipe_data$commentaires <- list()
  recipe_data
}

clean_yaml_response <- function(x) {
  y <- as.character(x %||% "")
  y <- gsub("^```yaml\\s*", "", y)
  y <- gsub("^```\\s*", "", y)
  y <- gsub("\\s*```$", "", y)
  trimws(y)
}

# Read request file
url_file <- Sys.getenv("RECIPE_URL_FILE")
if (!file.exists(url_file)) stop("Fichier URL introuvable: ", url_file)

url_data <- yaml::read_yaml(url_file)
video_url <- as.character(url_data$url %||% "")
submitted_by <- url_data$submitted_by %||% "Import automatique"
if (!is_youtube_url(video_url)) stop("URL non-YouTube reçue: ", video_url)

video_id <- extract_video_id(video_url)
if (is.null(video_id)) stop("Impossible d'extraire l'identifiant de vidéo YouTube.")

cat("📥 Import de la recette depuis YouTube:", video_url, "\n")
cat("🎬 Video ID:", video_id, "\n")
cat("📝 Téléchargement de la transcription...\n")
transcript_text <- fetch_youtube_transcript(video_url, video_id)
cat("✅ Transcription récupérée (", nchar(transcript_text), " caractères)\n", sep = "")

if (nchar(transcript_text) > 70000) {
  transcript_text <- substr(transcript_text, 1, 70000)
  cat("⚠️ Transcription tronquée à 70000 caractères\n")
}

template <- yaml::read_yaml("recettes/template.yaml")
template_example <- yaml::as.yaml(template)

prompt <- glue::glue('
Tu es un assistant qui extrait des recettes depuis une transcription de vidéo YouTube.

SOURCE URL: {video_url}

TRANSCRIPTION:
---
{transcript_text}
---

Génère un YAML valide avec EXACTEMENT la même structure que ce template:
{template_example}

RÈGLES:
1. Réponds uniquement avec du YAML valide, sans texte supplémentaire.
2. Ne mets pas de backticks.
3. Si un détail manque, utilise une valeur raisonnable (mais évite de laisser preparation vide).
4. Inclure des étapes concrètes dans preparation.
5. Chaque étape pertinente doit inclure des ingrédients quand c est possible.
6. source doit être {video_url}.
7. nom_court en slug simple.
')

chat <- chat_google_gemini(
  system_prompt = paste(
    "Tu es un expert en extraction de recettes.",
    "Tu réponds uniquement avec du YAML valide."
  )
)

cat("🤖 Extraction des informations via Gemini...\n")
response <- chat$chat(prompt)
yaml_content <- clean_yaml_response(response)

recipe_data <- tryCatch(
  yaml::yaml.load(yaml_content),
  error = function(e) {
    cat("❌ YAML invalide:\n", substr(yaml_content, 1, 1200), "\n")
    stop("YAML invalide généré par le LLM: ", e$message)
  }
)

recipe_data <- inject_fallback_preparation(recipe_data, transcript_text)
recipe_data <- inject_fallback_ingredients(recipe_data, transcript_text)
recipe_data <- apply_recipe_defaults(recipe_data, video_url, transcript_text)
recipe_data$soumis_par <- submitted_by

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

filename_base <- gsub("[^a-z0-9]+", "-", tolower(recipe_data$nom_court))
filename_base <- gsub("^-|-$", "", filename_base)
if (!nzchar(filename_base)) filename_base <- paste0("recette-youtube-", video_id)

yaml_file <- glue("recettes/{recipe_category}/{filename_base}.yaml")
fs::dir_create(dirname(yaml_file), recurse = TRUE)

cat("💾 Sauvegarde de ", yaml_file, "\n", sep = "")
yaml::write_yaml(recipe_data, yaml_file)

source("R/yaml_to_qmd.R")
cat("💾 Génération du qmd avec yaml_to_qmd()...\n")
yaml_recipe_to_qmd(yaml_path = yaml_file)

cat("✅ Import YouTube terminé\n")
cat("📄 Fichier YAML généré: ", yaml_file, "\n", sep = "")
