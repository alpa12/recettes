`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

clean_line <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[\\r\\t]", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

clean_yaml_response <- function(x) {
  y <- as.character(x %||% "")
  y <- gsub("^```yaml\\s*", "", y)
  y <- gsub("^```\\s*", "", y)
  y <- gsub("\\s*```$", "", y)
  trimws(y)
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
      if (!is.null(q2) && grepl("/", parts[2], fixed = TRUE)) {
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
    "^c\\.\\s*a\\s*soupe\\b", "^c\\.\\s*a\\s*the\\b", "^tasses?\\b",
    "^ml\\b", "^l\\b", "^g\\b", "^kg\\b", "^lb\\b", "^oz\\b",
    "^gousses?\\b", "^pincees?\\b", "^boites?\\b", "^conserves?\\b",
    "^branches?\\b", "^paquets?\\b", "^tranches?\\b"
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
  raw <- clean_line(gsub("^[\\-\\*\u2022\\s]+", "", as.character(line %||% "")))
  if (!nzchar(raw)) return(NULL)

  qty <- extract_leading_quantity(raw)
  qte <- qty$qte %||% 1
  unit_and_name <- extract_unit_and_name(qty$remainder %||% raw)

  list(
    nom = unit_and_name$nom %||% raw,
    qte = as.numeric(round(qte, 3)),
    uni = unit_and_name$uni
  )
}

has_any_steps <- function(recipe_data) {
  prep <- recipe_data$preparation
  if (!is.list(prep) || length(prep) == 0) return(FALSE)
  for (sec in prep) {
    if (is.list(sec$etapes) && length(sec$etapes) > 0) return(TRUE)
  }
  FALSE
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

slugify <- function(x) {
  base <- clean_line(x)
  base <- if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_trans_general(base, "Latin-ASCII")
  } else {
    iconv(base, from = "", to = "ASCII//TRANSLIT", sub = "")
  }
  base <- tolower(base)
  base <- gsub("[^a-z0-9]+", "-", base)
  base <- gsub("^-|-$", "", base)
  base
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
