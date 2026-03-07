ALLOWED_RANGEE <- c(
  "Fruits et legumes",
  "Viandes et substituts",
  "Produits laitiers et oeufs",
  "Epicerie",
  "Conserves et sauces"
)

ALLOWED_MASS_UNITS <- c("g", "kg", "lbs", "onces")
ALLOWED_VOLUME_UNITS <- c("ml", "c. à thé", "c. à soupe", "tasse")

normalize_validation_text <- function(x) {
  txt <- clean_line(x)
  txt <- tolower(stringi::stri_trans_general(txt, "Latin-ASCII"))
  txt <- gsub("[^a-z0-9]+", " ", txt)
  trimws(gsub("\\s+", " ", txt))
}

canonical_rangee <- function(x) {
  txt <- normalize_validation_text(x)
  map <- c(
    "fruits et legumes" = "Fruits et legumes",
    "fruit et legume" = "Fruits et legumes",
    "fruits legumes" = "Fruits et legumes",
    "viandes et substituts" = "Viandes et substituts",
    "viande et substituts" = "Viandes et substituts",
    "produits laitiers et oeufs" = "Produits laitiers et oeufs",
    "produit laitier et oeufs" = "Produits laitiers et oeufs",
    "epicerie" = "Epicerie",
    "epiceries" = "Epicerie",
    "conserves et sauces" = "Conserves et sauces",
    "conserve et sauces" = "Conserves et sauces"
  )
  hit <- unname(map[txt])
  if (length(hit) == 1 && !is.na(hit)) hit else clean_line(x)
}

canonical_count_unit <- function(unit) {
  txt <- normalize_validation_text(unit)
  map <- c(
    "unite" = "unité",
    "unites" = "unité",
    "unit" = "unité",
    "units" = "unité",
    "gousse" = "gousse",
    "gousses" = "gousse",
    "branche" = "branche",
    "branches" = "branche",
    "boite" = "boîte",
    "boites" = "boîte",
    "conserve" = "conserve",
    "conserves" = "conserve",
    "tranche" = "tranche",
    "tranches" = "tranche",
    "paquet" = "paquet",
    "paquets" = "paquet",
    "bloc" = "bloc",
    "blocs" = "bloc",
    "oeuf" = "oeuf",
    "oeufs" = "oeuf"
  )
  hit <- unname(map[txt])
  if (length(hit) == 1 && !is.na(hit)) hit else clean_line(unit)
}

canonical_default_unit <- function(unit) {
  raw <- clean_line(unit)
  if (!nzchar(raw)) return("")
  if (exists("canonical_import_mass_unit", mode = "function")) {
    mass <- canonical_import_mass_unit(raw)
    if (mass %in% ALLOWED_MASS_UNITS) return(mass)
  }
  if (exists("canonical_import_volume_unit", mode = "function")) {
    volume <- canonical_import_volume_unit(raw)
    if (volume %in% ALLOWED_VOLUME_UNITS) return(volume)
  }
  canonical_count_unit(raw)
}

coerce_recipe_numeric <- function(x) {
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) return(as.numeric(x))
  txt <- clean_line(x %||% "")
  if (!nzchar(txt)) return(NA_real_)
  direct <- suppressWarnings(as.numeric(txt))
  if (is.finite(direct)) return(direct)

  parts <- strsplit(gsub(",", ".", txt, fixed = TRUE), "\\s+")[[1]]
  if (length(parts) == 2) {
    first <- parse_numeric_token(parts[[1]])
    second <- parse_numeric_token(parts[[2]])
    if (is.finite(first) && is.finite(second) && grepl("/", parts[[2]], fixed = TRUE)) {
      return(first + second)
    }
  }

  parsed <- parse_numeric_token(txt)
  if (is.null(parsed)) NA_real_ else as.numeric(parsed)
}

new_validation_issue <- function(path, value, message, fix = NULL, code = "invalid") {
  list(
    path = path,
    value = if (is.null(value)) "NULL" else as.character(value),
    message = message,
    fix = fix %||% "",
    code = code
  )
}

validate_recipe_required_fields <- function(recipe, file = NA_character_) {
  required <- c("nom", "nom_court", "source", "portions")
  issues <- list()

  for (k in required) {
    value <- recipe[[k]]
    if (is.null(value) || !nzchar(clean_line(value))) {
      issues[[length(issues) + 1L]] <- c(
        list(file = file),
        new_validation_issue(
          path = k,
          value = value,
          message = sprintf("Champ requis manquant: %s.", k),
          fix = sprintf("Renseigner `%s` avec une valeur non vide.", k),
          code = "missing_required"
        )
      )
    }
  }

  issues
}

validate_recipe_ingredients <- function(recipe, file = NA_character_) {
  issues <- list()
  prep <- recipe$preparation
  if (is.null(prep) || !is.list(prep)) return(issues)

  for (si in seq_along(prep)) {
    sec <- prep[[si]]
    etapes <- sec$etapes
    if (is.null(etapes) || !is.list(etapes)) next

    for (ei in seq_along(etapes)) {
      et <- etapes[[ei]]
      ing <- et$ingredients
      if (is.null(ing) || !is.list(ing)) next

      seen_names <- character()
      for (ii in seq_along(ing)) {
        one <- ing[[ii]]
        prefix <- sprintf("preparation[%d].etapes[%d].ingredients[%d]", si - 1, ei - 1, ii - 1)
        nom <- clean_line(one$nom %||% "")
        qte_value <- one$qte
        qte <- coerce_recipe_numeric(qte_value)
        uni <- clean_line(one$uni %||% "")
        uni_canonical <- canonical_default_unit(uni)
        rangee <- clean_line(one$rangee %||% "")
        rangee_canonical <- canonical_rangee(rangee)

        if (!nzchar(nom)) {
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = paste0(prefix, ".nom"),
              value = one$nom,
              message = "Nom d’ingrédient manquant.",
              fix = "Renseigner `nom` avec un libellé simple, idéalement neutre et au singulier.",
              code = "missing_name"
            )
          )
        } else {
          name_key <- slugify(nom)
          if (nzchar(name_key) && name_key %in% seen_names) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".nom"),
                value = one$nom,
                message = "Ingrédient dupliqué dans la même étape.",
                fix = "Ne garder l’ingrédient qu’à sa première apparition dans l’étape ou fusionner les doublons.",
                code = "duplicate_in_step"
              )
            )
          }
          seen_names <- c(seen_names, name_key)
        }

        if (!is.finite(qte)) {
          fix_text <- "Mettre une quantité numérique dans `qte`."
          if (is.character(qte_value) && grepl("/", qte_value, fixed = TRUE)) {
            fix_text <- sprintf("Remplacer `%s` par sa valeur décimale dans `qte` (ex: `0.5`).", qte_value)
          }
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = paste0(prefix, ".qte"),
              value = qte_value,
              message = "Quantité par défaut invalide ou absente.",
              fix = fix_text,
              code = "invalid_qte"
            )
          )
        } else if (qte <= 0) {
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = paste0(prefix, ".qte"),
              value = qte_value,
              message = "Quantité par défaut invalide: elle doit être strictement positive.",
              fix = "Utiliser une valeur numérique supérieure à 0 dans `qte`.",
              code = "non_positive_qte"
            )
          )
        }

        if (!nzchar(uni)) {
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = paste0(prefix, ".uni"),
              value = one$uni,
              message = "Unité par défaut manquante.",
              fix = "Renseigner `uni` avec l’unité affichée par défaut sur le site.",
              code = "missing_unit"
            )
          )
        } else if (!identical(uni, uni_canonical)) {
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = paste0(prefix, ".uni"),
              value = one$uni,
              message = "Unité par défaut non canonique.",
              fix = sprintf("Remplacer `%s` par `%s`.", uni, uni_canonical),
              code = "non_canonical_unit"
            )
          )
        }

        qte_masse_raw <- one$qte_masse
        qte_masse <- coerce_recipe_numeric(qte_masse_raw)
        uni_masse <- clean_line(one$uni_masse %||% "")
        if (nzchar(uni_masse) || !is.null(qte_masse_raw)) {
          if (!is.finite(qte_masse)) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".qte_masse"),
                value = qte_masse_raw,
                message = "Quantité de masse invalide.",
                fix = "Utiliser une valeur numérique dans `qte_masse`, ou supprimer complètement les champs masse.",
                code = "invalid_mass_qty"
              )
            )
          }
          canonical_mass <- if (nzchar(uni_masse) && exists("canonical_import_mass_unit", mode = "function")) canonical_import_mass_unit(uni_masse) else uni_masse
          if (!nzchar(uni_masse)) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".uni_masse"),
                value = one$uni_masse,
                message = "Unité de masse manquante alors que `qte_masse` est fournie.",
                fix = sprintf("Renseigner `uni_masse` avec une valeur permise: %s.", paste(ALLOWED_MASS_UNITS, collapse = ", ")),
                code = "missing_mass_unit"
              )
            )
          } else if (!(canonical_mass %in% ALLOWED_MASS_UNITS)) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".uni_masse"),
                value = one$uni_masse,
                message = "Unité de masse invalide.",
                fix = sprintf("Utiliser une unité permise pour `uni_masse`: %s.", paste(ALLOWED_MASS_UNITS, collapse = ", ")),
                code = "invalid_mass_unit"
              )
            )
          } else if (!identical(uni_masse, canonical_mass)) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".uni_masse"),
                value = one$uni_masse,
                message = "Unité de masse non canonique.",
                fix = sprintf("Remplacer `%s` par `%s`.", uni_masse, canonical_mass),
                code = "non_canonical_mass_unit"
              )
            )
          }
        }

        qte_volume_raw <- one$qte_volume
        qte_volume <- coerce_recipe_numeric(qte_volume_raw)
        uni_volume <- clean_line(one$uni_volume %||% "")
        if (nzchar(uni_volume) || !is.null(qte_volume_raw)) {
          if (!is.finite(qte_volume)) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".qte_volume"),
                value = qte_volume_raw,
                message = "Quantité de volume invalide.",
                fix = "Utiliser une valeur numérique dans `qte_volume`, ou supprimer complètement les champs volume.",
                code = "invalid_volume_qty"
              )
            )
          }
          canonical_volume <- if (nzchar(uni_volume) && exists("canonical_import_volume_unit", mode = "function")) canonical_import_volume_unit(uni_volume) else uni_volume
          if (!nzchar(uni_volume)) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".uni_volume"),
                value = one$uni_volume,
                message = "Unité de volume manquante alors que `qte_volume` est fournie.",
                fix = sprintf("Renseigner `uni_volume` avec une valeur permise: %s.", paste(ALLOWED_VOLUME_UNITS, collapse = ", ")),
                code = "missing_volume_unit"
              )
            )
          } else if (!(canonical_volume %in% ALLOWED_VOLUME_UNITS)) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".uni_volume"),
                value = one$uni_volume,
                message = "Unité de volume invalide.",
                fix = sprintf("Utiliser une unité permise pour `uni_volume`: %s.", paste(ALLOWED_VOLUME_UNITS, collapse = ", ")),
                code = "invalid_volume_unit"
              )
            )
          } else if (!identical(uni_volume, canonical_volume)) {
            issues[[length(issues) + 1L]] <- c(
              list(file = file),
              new_validation_issue(
                path = paste0(prefix, ".uni_volume"),
                value = one$uni_volume,
                message = "Unité de volume non canonique.",
                fix = sprintf("Remplacer `%s` par `%s`.", uni_volume, canonical_volume),
                code = "non_canonical_volume_unit"
              )
            )
          }
        }

        if (identical(uni_canonical, "unité") && (nzchar(uni_masse) || nzchar(uni_volume) || !is.null(qte_masse_raw) || !is.null(qte_volume_raw))) {
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = prefix,
              value = one$nom,
              message = "Un ingrédient compté avec `uni: unité` ne doit pas aussi définir des champs masse/volume, sauf cas très exceptionnel.",
              fix = "Supprimer `qte_masse`/`uni_masse` et `qte_volume`/`uni_volume`, ou remplacer `uni` par une vraie unité de masse/volume si l’ingrédient n’est pas un simple décompte.",
              code = "unit_count_conflict"
            )
          )
        }

        if (!nzchar(rangee)) {
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = paste0(prefix, ".rangee"),
              value = one$rangee,
              message = "Rayon d’épicerie manquant.",
              fix = sprintf("Renseigner `rangee` avec une valeur permise: %s.", paste(ALLOWED_RANGEE, collapse = ", ")),
              code = "missing_rangee"
            )
          )
        } else if (!(rangee_canonical %in% ALLOWED_RANGEE)) {
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = paste0(prefix, ".rangee"),
              value = one$rangee,
              message = "Rayon d’épicerie invalide.",
              fix = sprintf("Utiliser une valeur permise pour `rangee`: %s.", paste(ALLOWED_RANGEE, collapse = ", ")),
              code = "invalid_rangee"
            )
          )
        } else if (!identical(rangee, rangee_canonical)) {
          issues[[length(issues) + 1L]] <- c(
            list(file = file),
            new_validation_issue(
              path = paste0(prefix, ".rangee"),
              value = one$rangee,
              message = "Rayon d’épicerie non canonique.",
              fix = sprintf("Remplacer `%s` par `%s`.", rangee, rangee_canonical),
              code = "non_canonical_rangee"
            )
          )
        }
      }
    }
  }

  issues
}

validate_recipe_data <- function(recipe, file = NA_character_) {
  c(
    validate_recipe_required_fields(recipe, file = file),
    validate_recipe_ingredients(recipe, file = file)
  )
}

format_recipe_validation_report <- function(issues, heading = "YAML invalide") {
  if (length(issues) == 0) return(sprintf("%s\nAucun problème détecté.", heading))

  grouped <- split(issues, vapply(issues, function(x) x$file %||% "<objet en mémoire>", character(1)))
  lines <- c(
    heading,
    sprintf("%d problème(s) détecté(s).", length(issues)),
    "Chaque entrée ci-dessous indique le champ fautif, la valeur lue et l’action attendue."
  )

  for (file in names(grouped)) {
    lines <- c(lines, "", sprintf("Fichier: %s", file))
    for (issue in grouped[[file]]) {
      lines <- c(
        lines,
        sprintf("- %s", issue$path),
        sprintf("  Valeur lue: %s", issue$value %||% "NULL"),
        sprintf("  Problème: %s", issue$message),
        sprintf("  Correction: %s", issue$fix %||% "Aucune suggestion disponible.")
      )
    }
  }

  paste(lines, collapse = "\n")
}

signal_recipe_validation_errors <- function(issues, heading = "YAML invalide") {
  if (length(issues) == 0) return(invisible(TRUE))
  stop(format_recipe_validation_report(issues, heading = heading), call. = FALSE)
}

autofix_recipe_ingredient <- function(ing) {
  if (!is.list(ing)) return(ing)

  ing$nom <- clean_line(ing$nom %||% "")
  ing$uni <- canonical_default_unit(ing$uni %||% "")
  ing$rangee <- canonical_rangee(ing$rangee %||% "")

  qte <- coerce_recipe_numeric(ing$qte)
  qte_masse <- coerce_recipe_numeric(ing$qte_masse)
  qte_volume <- coerce_recipe_numeric(ing$qte_volume)

  ing$qte <- if (is.finite(qte)) qte else NULL
  ing$qte_masse <- if (is.finite(qte_masse)) qte_masse else NULL
  ing$qte_volume <- if (is.finite(qte_volume)) qte_volume else NULL

  ing$uni_masse <- if (nzchar(clean_line(ing$uni_masse %||% "")) && exists("canonical_import_mass_unit", mode = "function")) {
    canonical_import_mass_unit(ing$uni_masse)
  } else {
    clean_line(ing$uni_masse %||% "")
  }
  ing$uni_volume <- if (nzchar(clean_line(ing$uni_volume %||% "")) && exists("canonical_import_volume_unit", mode = "function")) {
    canonical_import_volume_unit(ing$uni_volume)
  } else {
    clean_line(ing$uni_volume %||% "")
  }
  if (!nzchar(ing$uni_masse %||% "")) ing$uni_masse <- NULL
  if (!nzchar(ing$uni_volume %||% "")) ing$uni_volume <- NULL

  if (!is.finite(ing$qte %||% NA_real_)) {
    if (is.finite(ing$qte_masse %||% NA_real_) && nzchar(ing$uni_masse %||% "")) {
      ing$qte <- ing$qte_masse
      ing$uni <- ing$uni_masse
    } else if (is.finite(ing$qte_volume %||% NA_real_) && nzchar(ing$uni_volume %||% "")) {
      ing$qte <- ing$qte_volume
      ing$uni <- ing$uni_volume
    }
  }

  if (!nzchar(ing$uni %||% "")) {
    if (is.finite(ing$qte_masse %||% NA_real_) && nzchar(ing$uni_masse %||% "")) {
      ing$uni <- ing$uni_masse
    } else if (is.finite(ing$qte_volume %||% NA_real_) && nzchar(ing$uni_volume %||% "")) {
      ing$uni <- ing$uni_volume
    }
  }

  if (is.finite(ing$qte %||% NA_real_) && nzchar(ing$uni %||% "")) {
    if (!is.finite(ing$qte_masse %||% NA_real_) && nzchar(ing$uni_masse %||% "")) {
      if ((ing$uni_masse %||% "") %in% ALLOWED_MASS_UNITS && identical(canonical_default_unit(ing$uni), ing$uni_masse)) {
        ing$qte_masse <- ing$qte
      }
    }
    if (!is.finite(ing$qte_volume %||% NA_real_) && nzchar(ing$uni_volume %||% "")) {
      if ((ing$uni_volume %||% "") %in% ALLOWED_VOLUME_UNITS && identical(canonical_default_unit(ing$uni), ing$uni_volume)) {
        ing$qte_volume <- ing$qte
      }
    }
  }

  if (identical(ing$uni %||% "", "unité")) {
    ing$qte_masse <- NULL
    ing$uni_masse <- NULL
    ing$qte_volume <- NULL
    ing$uni_volume <- NULL
  }

  if (!nzchar(ing$rangee %||% "")) ing$rangee <- "Epicerie"
  ing
}

autofix_recipe_data <- function(recipe) {
  if (!is.list(recipe)) return(recipe)

  if (!is.null(recipe$nom)) recipe$nom <- clean_line(recipe$nom)
  if (!is.null(recipe$nom_court)) recipe$nom_court <- clean_line(recipe$nom_court)
  if (!is.null(recipe$source)) recipe$source <- clean_line(recipe$source)
  portions <- coerce_recipe_numeric(recipe$portions)
  if (is.finite(portions) && portions > 0) recipe$portions <- portions

  prep <- recipe$preparation
  if (!is.list(prep)) return(recipe)

  for (si in seq_along(prep)) {
    steps <- prep[[si]]$etapes
    if (!is.list(steps)) next
    for (ei in seq_along(steps)) {
      ings <- steps[[ei]]$ingredients
      if (!is.list(ings)) {
        recipe$preparation[[si]]$etapes[[ei]]$ingredients <- list()
        next
      }
      recipe$preparation[[si]]$etapes[[ei]]$ingredients <- lapply(ings, autofix_recipe_ingredient)
    }
  }

  recipe
}
