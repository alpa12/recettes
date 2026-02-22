# Helpers for recipe comments rendering in QMD output.

normalize_comments <- function(commentaires) {
  if (is.null(commentaires) || length(commentaires) == 0) return(list())

  if (is.character(commentaires)) {
    return(lapply(commentaires, function(s) {
      list(commentaire = s, evaluation = NULL, auteur = NULL, date = NULL)
    }))
  }

  if (is.list(commentaires)) {
    out <- list()
    for (i in seq_along(commentaires)) {
      x <- commentaires[[i]]
      if (is.character(x)) {
        out[[length(out) + 1L]] <- list(commentaire = x, evaluation = NULL, auteur = NULL, date = NULL)
      } else if (is.list(x)) {
        raw_evaluation <- if (!is.null(x$evaluation)) x$evaluation else x$note
        raw_author <- if (!is.null(x$auteur)) x$auteur else x$nom
        out[[length(out) + 1L]] <- list(
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
  n <- round(avg)
  n <- max(0, min(5, n))
  paste0(strrep("\u2605", n), strrep("\u2606", 5 - n))
}

strip_accents <- function(x) {
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
      parts <- c(parts, paste0(strrep("\u2605", ev), strrep("\u2606", 5 - ev)))
    }
  }

  if (!is.null(cmt$auteur) && cmt$auteur != "") parts <- c(parts, cmt$auteur)
  if (!is.null(cmt$date) && cmt$date != "") parts <- c(parts, cmt$date)

  prefix <- if (length(parts) > 0) paste0("[", paste(parts, collapse = " \u00b7 "), "] ") else ""
  txt <- if (!is.null(cmt$commentaire) && cmt$commentaire != "") cmt$commentaire else "(sans texte)"
  paste0(prefix, txt)
}
