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

  keep <- c(TRUE, lines[-1] != lines[-length(lines)])
  lines <- lines[keep]
  paste(lines, collapse = "\n")
}

fetch_youtube_transcript_python <- function(video_id) {
  if (nzchar(Sys.which("python3")) == 0) stop("python3 introuvable dans PATH.")

  out_file <- tempfile(pattern = paste0("yt-transcript-", video_id, "-"), fileext = ".txt")
  py_file <- tempfile(pattern = "yt-transcript-", fileext = ".py")
  py_code <- paste(
    "import sys",
    "from youtube_transcript_api import YouTubeTranscriptApi",
    "video_id = sys.argv[1]",
    "out_path = sys.argv[2]",
    "segments = []",
    "def _normalize(items):",
    "    out = []",
    "    for s in items or []:",
    "        if isinstance(s, dict):",
    "            txt = (s.get('text') or '').strip()",
    "        else:",
    "            txt = (getattr(s, 'text', '') or '').strip()",
    "        if txt:",
    "            out.append(txt)",
    "    return out",
    "try:",
    "    api = YouTubeTranscriptApi()",
    "    transcript_list = api.list(video_id)",
    "    transcript = None",
    "    for langs in (['fr', 'fr-CA', 'fr-FR'], ['en', 'en-US', 'en-GB']):",
    "        try:",
    "            transcript = transcript_list.find_transcript(langs)",
    "            break",
    "        except Exception:",
    "            try:",
    "                transcript = transcript_list.find_generated_transcript(langs)",
    "                break",
    "            except Exception:",
    "                pass",
    "    if transcript is None:",
    "        for t in transcript_list:",
    "            transcript = t",
    "            break",
    "    if transcript is not None:",
    "        fetched = transcript.fetch()",
    "        if hasattr(fetched, 'to_raw_data'):",
    "            segments = fetched.to_raw_data()",
    "        else:",
    "            segments = list(fetched)",
    "except Exception:",
    "    pass",
    "if not segments:",
    "    try:",
    "        segments = YouTubeTranscriptApi.get_transcript(video_id, languages=['fr', 'en'])",
    "    except Exception:",
    "        segments = []",
    "lines = _normalize(segments)",
    "if not lines:",
    "    raise RuntimeError('No transcript segments found')",
    "with open(out_path, 'w', encoding='utf-8') as f:",
    "    f.write('\\n'.join(lines))",
    sep = "\n"
  )
  writeLines(py_code, py_file, useBytes = TRUE)

  out <- system2("python3", args = c(py_file, video_id, out_file), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    if (any(grepl("No module named 'youtube_transcript_api'", out, fixed = TRUE))) {
      stop("python transcript API indisponible: module youtube_transcript_api manquant.")
    }
    stop("python transcript API a echoue: ", paste(out, collapse = "\n"))
  }
  if (!file.exists(out_file)) stop("python transcript API n'a pas produit de sortie.")

  txt <- paste(readLines(out_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  txt <- gsub("\r", "", txt, fixed = TRUE)
  txt <- gsub("\n{2,}", "\n", txt, perl = TRUE)
  txt <- trimws(txt)
  if (!nzchar(clean_line(txt))) stop("Transcription vide via python transcript API.")
  txt
}

fetch_youtube_transcript_ytdlp <- function(video_url, video_id) {
  tmp <- fs::path_temp(paste0("yt-", video_id))
  fs::dir_create(tmp, recurse = TRUE)
  outtmpl <- fs::path(tmp, paste0(video_id, ".%(ext)s"))

  if (nzchar(Sys.which("yt-dlp")) == 0) stop("yt-dlp introuvable dans PATH.")

  timeout_bin <- Sys.which("timeout")
  if (!nzchar(timeout_bin)) timeout_bin <- Sys.which("gtimeout")
  timeout_prefix <- if (nzchar(timeout_bin)) paste(shQuote(timeout_bin), "180") else ""

  cmd <- paste(
    timeout_prefix,
    "yt-dlp",
    "--skip-download",
    "--write-auto-subs",
    "--write-subs",
    "--no-progress",
    "--sub-format", "vtt",
    "--sub-langs", shQuote("fr.*,en.*"),
    "-o", shQuote(outtmpl),
    shQuote(video_url)
  )
  status <- suppressWarnings(system(cmd, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE))
  if (!is.null(status) && status != 0) stop("yt-dlp a echoue (code ", status, ").")

  vtt_files <- list.files(tmp, pattern = "\\.vtt$", full.names = TRUE)
  if (length(vtt_files) == 0) stop("Aucune transcription VTT trouvee.")

  prefer <- vtt_files[grepl("\\.fr([.-]|$)", vtt_files)]
  chosen <- if (length(prefer) > 0) prefer[[1]] else vtt_files[[1]]
  txt <- read_vtt_as_text(chosen)
  if (!nzchar(clean_line(txt))) stop("Transcription vide apres nettoyage.")
  txt
}

fetch_youtube_transcript_watchpage <- function(video_url, video_id) {
  html_lines <- tryCatch(readLines(video_url, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  if (length(html_lines) == 0) stop("Impossible de telecharger la page YouTube.")
  html <- paste(html_lines, collapse = "\n")

  m <- regexec('(?s)"captions":(\\{.*?\\}),"videoDetails"', html, perl = TRUE)
  parts <- regmatches(html, m)[[1]]
  if (length(parts) < 2) stop("Section captions introuvable.")

  captions_json <- parts[2]
  cap <- tryCatch(jsonlite::fromJSON(captions_json, simplifyVector = FALSE), error = function(e) NULL)
  tracks <- cap$playerCaptionsTracklistRenderer$captionTracks %||% list()
  if (!is.list(tracks) || length(tracks) == 0) stop("Aucune piste de sous-titres trouvee.")

  pick_track <- function(track_list, langs) {
    for (tr in track_list) {
      code <- tolower(as.character(tr$languageCode %||% ""))
      if (code %in% langs) return(tr)
    }
    NULL
  }

  track <- pick_track(tracks, c("fr", "fr-ca", "fr-fr"))
  if (is.null(track)) track <- pick_track(tracks, c("en", "en-us", "en-gb"))
  if (is.null(track)) track <- tracks[[1]]

  base_url <- as.character(track$baseUrl %||% "")
  base_url <- gsub("\\\\u0026", "&", base_url)
  if (!nzchar(base_url)) stop("baseUrl absent sur la piste de captions.")

  vtt_url <- if (grepl("[?&]fmt=", base_url)) {
    gsub("([?&]fmt=)[^&]+", "\\1vtt", base_url, perl = TRUE)
  } else {
    paste0(base_url, if (grepl("\\?", base_url)) "&fmt=vtt" else "?fmt=vtt")
  }

  vtt_lines <- tryCatch(readLines(vtt_url, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  if (length(vtt_lines) == 0) stop("Telechargement VTT impossible.")

  vtt_file <- tempfile(pattern = paste0("yt-vtt-", video_id, "-"), fileext = ".vtt")
  writeLines(vtt_lines, vtt_file, useBytes = TRUE)
  txt <- read_vtt_as_text(vtt_file)
  if (!nzchar(clean_line(txt))) stop("Captions VTT vides apres nettoyage.")
  txt
}

fetch_youtube_transcript <- function(video_url, video_id) {
  ytdlp_err <- NULL
  py_err <- NULL

  txt <- tryCatch(
    fetch_youtube_transcript_ytdlp(video_url, video_id),
    error = function(e) {
      ytdlp_err <<- conditionMessage(e)
      NULL
    }
  )
  if (nzchar(clean_line(txt %||% ""))) return(txt)

  txt <- tryCatch(
    fetch_youtube_transcript_python(video_id),
    error = function(e) {
      py_err <<- conditionMessage(e)
      NULL
    }
  )
  if (nzchar(clean_line(txt %||% ""))) return(txt)

  tryCatch(
    fetch_youtube_transcript_watchpage(video_url, video_id),
    error = function(e) {
      stop(
        "Impossible de recuperer une transcription. ",
        "Details: yt-dlp=[", ytdlp_err %||% "n/a", "], python=[", py_err %||% "n/a", "], captions=[", conditionMessage(e), "]."
      )
    }
  )
}

fetch_youtube_metadata_context <- function(video_url, video_id) {
  if (nzchar(Sys.which("yt-dlp")) == 0) stop("yt-dlp introuvable dans PATH.")

  out <- system2(
    "yt-dlp",
    args = c("--skip-download", "--dump-single-json", "--no-warnings", "--no-progress", video_url),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("yt-dlp metadata a echoue: ", paste(out, collapse = "\n"))
  }

  raw_txt <- paste(out, collapse = "\n")
  start_pos <- regexpr("\\{", raw_txt, perl = TRUE)[1]
  end_pos <- tail(gregexpr("\\}", raw_txt, perl = TRUE)[[1]], 1)
  if (is.na(start_pos) || is.na(end_pos) || start_pos <= 0 || end_pos <= 0 || end_pos <= start_pos) {
    stop("Impossible d'extraire le JSON des metadonnees yt-dlp.")
  }
  json_txt <- substr(raw_txt, start_pos, end_pos)
  info <- tryCatch(
    jsonlite::fromJSON(json_txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(info)) stop("Impossible de parser les metadonnees yt-dlp.")

  title <- clean_line(as.character(info$title %||% ""))
  description <- clean_line(as.character(info$description %||% ""))
  if (!nzchar(description)) description <- clean_line(as.character(info$fulltitle %||% ""))

  chapter_lines <- character(0)
  if (is.list(info$chapters) && length(info$chapters) > 0) {
    chapter_lines <- vapply(
      info$chapters,
      function(ch) clean_line(as.character(ch$title %||% "")),
      character(1)
    )
    chapter_lines <- chapter_lines[nzchar(chapter_lines)]
  }

  tags <- character(0)
  if (is.character(info$tags)) {
    tags <- clean_line(paste(info$tags, collapse = ", "))
  }

  context_lines <- c(
    if (nzchar(title)) paste("Titre:", title),
    if (nzchar(description)) c("Description:", description),
    if (length(chapter_lines) > 0) c("Chapitres:", chapter_lines),
    if (nzchar(tags)) paste("Tags:", tags)
  )
  context_lines <- context_lines[nzchar(vapply(context_lines, clean_line, character(1)))]
  context_text <- paste(context_lines, collapse = "\n")
  if (!nzchar(clean_line(context_text))) {
    stop("Metadonnees YouTube vides.")
  }

  list(
    text = context_text,
    title = title
  )
}

#' Import a recipe from a YouTube URL YAML request.
#'
#' GitHub Actions entrypoint for YouTube-based recipe imports.
#'
#' @param url_file Path to YAML request file (usually from `RECIPE_URL_FILE`).
#' @return Generated recipe YAML file path invisibly.
#' @export
gha_import_recipe_from_youtube <- function(url_file = Sys.getenv("RECIPE_URL_FILE")) {
  if (!file.exists(url_file)) stop("Fichier URL introuvable: ", url_file)

  url_data <- yaml::read_yaml(url_file)
  video_url <- as.character(url_data$url %||% "")
  submitted_by <- url_data$submitted_by %||% "Import automatique"
  if (!is_youtube_url(video_url)) stop("URL non-YouTube recue: ", video_url)

  video_id <- extract_video_id(video_url)
  if (is.null(video_id)) stop("Impossible d'extraire l'identifiant de video YouTube.")

  cat("Import YouTube:", video_url, "\n")
  transcript_source <- "transcription"
  fallback_title <- "Recette importee de YouTube"
  transcript_text <- tryCatch(
    fetch_youtube_transcript(video_url, video_id),
    error = function(e) {
      cat("Transcription indisponible, fallback metadonnees YouTube:", conditionMessage(e), "\n")
      md <- fetch_youtube_metadata_context(video_url, video_id)
      transcript_source <<- "metadonnees"
      if (nzchar(md$title %||% "")) fallback_title <<- md$title
      md$text
    }
  )
  if (nchar(transcript_text) > 70000) transcript_text <- substr(transcript_text, 1, 70000)

  transcript_lines <- split_clean_lines(transcript_text)
  candidate_ingredient_lines <- transcript_lines[
    nchar(transcript_lines) <= 140 &
      (grepl("^\\d", transcript_lines) |
         grepl("\\b(c\\.|tasse|ml|g|kg|lb|oz|pincee|gousse|cuill)", tolower(iconv(transcript_lines, from = "", to = "ASCII//TRANSLIT"))))
  ]
  candidate_instruction_lines <- transcript_lines[nchar(transcript_lines) >= 20 & nchar(transcript_lines) <= 280]

  template <- yaml::read_yaml("recettes/template.yaml")
  template_example <- yaml::as.yaml(template)

  context_yaml <- yaml::as.yaml(list(
    ingredients_candidates = head(candidate_ingredient_lines, 120),
    etapes_candidates = head(candidate_instruction_lines, 120),
    transcription_longueur = nchar(transcript_text),
    source_contenu = transcript_source
  ))

  prompt <- gha_build_prompt(
    context_title = if (transcript_source == "transcription") "Transcription YouTube" else "Metadonnees YouTube",
    source_url = video_url,
    template_example = template_example,
    context_yaml = context_yaml,
    body_text = transcript_text
  )

  chat <- gha_new_chat()
  response <- chat$chat(prompt)
  recipe_data <- tryCatch(
    gha_parse_llm_yaml(response),
    error = function(e) stop("YAML invalide genere par le LLM: ", e$message)
  )

  recipe_data <- gha_finalize_recipe(
    recipe_data = recipe_data,
    source_url = video_url,
    submitted_by = submitted_by,
    fallback_title = fallback_title,
    portions_text = transcript_text,
    ingredient_lines = candidate_ingredient_lines,
    instruction_lines = candidate_instruction_lines
  )

  recipe_category <- gha_classify_recipe_category(chat)
  filename_base <- slugify(recipe_data$nom_court)
  if (!nzchar(filename_base)) filename_base <- paste0("recette-youtube-", video_id)

  yaml_file <- gha_write_recipe_and_qmd(recipe_data, recipe_category, filename_base)

  cat("Import YouTube termine. Fichier YAML genere:", yaml_file, "\n")
  invisible(yaml_file)
}
