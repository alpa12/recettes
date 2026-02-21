NUTRITION_NUTRIENTS <- data.frame(
  key = c(
    "energy_kcal", "protein_g", "fat_g", "saturated_fat_g", "carbs_g", "sugars_g",
    "fiber_g", "sodium_mg", "potassium_mg", "calcium_mg", "iron_mg", "magnesium_mg",
    "zinc_mg", "selenium_ug", "vitamin_c_mg", "vitamin_b12_ug", "vitamin_a_rae_ug",
    "vitamin_d_ug"
  ),
  label = c(
    "Calories", "Protéines", "Lipides", "Gras saturés", "Glucides", "Sucres",
    "Fibres", "Sodium", "Potassium", "Calcium", "Fer", "Magnésium",
    "Zinc", "Sélénium", "Vitamine C", "Vitamine B12", "Vitamine A (EAR)",
    "Vitamine D"
  ),
  unit = c(
    "kcal", "g", "g", "g", "g", "g",
    "g", "mg", "mg", "mg", "mg", "mg",
    "mg", "ug", "mg", "ug", "ug", "ug"
  ),
  decimals = c(
    0, 1, 1, 1, 1, 1,
    1, 0, 0, 0, 1, 0,
    1, 1, 1, 1, 0, 1
  ),
  stringsAsFactors = FALSE
)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

normalize_text <- function(x) {
  x <- tolower(as.character(x %||% ""))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("['`^~\"]", "", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

normalize_unit <- function(unit) {
  u <- normalize_text(unit)
  if (is.na(u) || !nzchar(u)) return("")

  map <- c(
    "g" = "g",
    "gramme" = "g",
    "grammes" = "g",
    "ml" = "ml",
    "millilitre" = "ml",
    "millilitres" = "ml",
    "t" = "cup",
    "tasse" = "cup",
    "c a soupe" = "tbsp",
    "c a table" = "tbsp",
    "c a the" = "tsp",
    "lbs" = "lb",
    "lb" = "lb",
    "once" = "oz",
    "onces" = "oz",
    "gousse" = "clove",
    "gousses" = "clove",
    "pincee" = "pinch",
    "unite" = "unit",
    "unites" = "unit",
    "oignon" = "green_onion_bunch",
    "au gout" = "to_taste"
  )

  m <- unname(map[u])
  if (length(m) == 1 && !is.na(m)) return(m)
  u
}

load_nutrition_db <- local({
  cache <- NULL
  resolve_data_file <- function(path) {
    candidates <- c(path, file.path("..", path))
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0) return(NA_character_)
    hit[[1]]
  }

  function() {
    if (!is.null(cache)) return(cache)

    foods_path <- resolve_data_file("data/nutrition/foods.csv")
    aliases_path <- resolve_data_file("data/nutrition/aliases.csv")
    cnf_path <- resolve_data_file("data/nutrition/foods_cnf.csv")

    if (is.na(foods_path) || is.na(aliases_path)) {
      cache <<- list(
        foods = data.frame(food_id = character(), stringsAsFactors = FALSE),
        aliases = data.frame(alias = character(), food_id = character(), alias_norm = character(), stringsAsFactors = FALSE)
      )
      return(cache)
    }

    foods <- read.csv(
      foods_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (!is.na(cnf_path) && file.exists(cnf_path)) {
      foods_cnf <- read.csv(
        cnf_path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      if ("food_id" %in% names(foods_cnf) && nrow(foods_cnf) > 0) {
        foods <- foods[!(foods$food_id %in% foods_cnf$food_id), , drop = FALSE]
        foods <- rbind(foods, foods_cnf)
      }
    }
    if (!("source" %in% names(foods))) foods$source <- "unknown"
    if (!("source_ref" %in% names(foods))) foods$source_ref <- ""
    aliases <- read.csv(
      aliases_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    aliases$alias_norm <- normalize_text(aliases$alias)
    aliases$food_id <- trimws(aliases$food_id)

    cache <<- list(
      foods = foods,
      aliases = aliases
    )
    cache
  }
})

lookup_food_id <- function(name, db) {
  n <- normalize_text(name)
  if (!nzchar(n)) return(NA_character_)

  m <- db$aliases$food_id[db$aliases$alias_norm == n]
  if (length(m) > 0) return(m[[1]])

  n_compact <- gsub(" ", "", n, fixed = TRUE)
  m_compact <- db$aliases$food_id[gsub(" ", "", db$aliases$alias_norm, fixed = TRUE) == n_compact]
  if (length(m_compact) > 0) return(m_compact[[1]])

  # Fallback simple singularization.
  if (grepl("s$", n)) {
    n2 <- sub("s$", "", n)
    m2 <- db$aliases$food_id[db$aliases$alias_norm == n2]
    if (length(m2) > 0) return(m2[[1]])
  }

  NA_character_
}

food_density_g_ml <- function(food_id) {
  dens <- c(
    "maple_syrup" = 1.33,
    "honey" = 1.42,
    "olive_oil" = 0.91,
    "canola_oil" = 0.92,
    "vegetable_oil" = 0.92,
    "milk_2pct" = 1.03,
    "cream_15" = 1.01,
    "worcestershire" = 1.22,
    "soy_sauce_reduced" = 1.17,
    "hoisin_sauce" = 1.20,
    "pineapple_juice" = 1.03,
    "tomato_juice" = 1.03,
    "apple_cider_vinegar" = 1.01,
    "balsamic_vinegar" = 1.05,
    "rice_vinegar" = 1.01,
    "red_wine_vinegar" = 1.01,
    "water" = 1.00,
    "brown_sugar" = 0.85,
    "sugar_granulated" = 0.85,
    "powdered_sugar" = 0.56,
    "hot_chocolate_mix" = 0.43,
    "cornstarch" = 0.54,
    "cocoa_powder" = 0.42,
    "flour_all_purpose" = 0.53,
    "graham_crumbs" = 0.40,
    "basil_dried" = 0.35,
    "oregano_dried" = 0.30,
    "black_pepper" = 0.52,
    "cayenne" = 0.52,
    "mustard_dry" = 0.55
  )

  d <- unname(dens[food_id])
  if (length(d) == 1 && !is.na(d)) return(as.numeric(d))
  1
}

unit_weight_g <- function(food_id) {
  w <- c(
    "egg_whole" = 50,
    "egg_yolk" = 17,
    "egg_white" = 33,
    "caramel_soft" = 8,
    "clove" = 0.2,
    "bay_leaf" = 0.2,
    "rosemary" = 1,
    "potato" = 170,
    "sweet_potato" = 200,
    "bell_pepper_green" = 120,
    "onion" = 110,
    "carrot" = 61,
    "mushroom_white" = 227, # barquette courante
    "green_onion" = 15,
    "shallot" = 40,
    "pearl_onion" = 15,
    "garlic" = 3,
    "celery" = 40,
    "pork_tenderloin" = 454,
    "pineapple_canned_in_juice" = 398,
    "tomato_canned" = 796,
    "tomato_juice" = 540,
    "tomato_paste" = 156
  )

  ww <- unname(w[food_id])
  if (length(ww) == 1 && !is.na(ww)) return(as.numeric(ww))
  NA_real_
}

as_grams <- function(qty, unit, food_id) {
  if (is.null(qty) || length(qty) == 0 || is.na(qty)) {
    return(list(ok = FALSE, grams = NA_real_, reason = "quantite_non_numerique"))
  }
  if (!is.finite(qty) || qty <= 0) {
    return(list(ok = FALSE, grams = NA_real_, reason = "quantite_invalide"))
  }

  u <- normalize_unit(unit)

  if (u == "to_taste") {
    return(list(ok = FALSE, grams = NA_real_, reason = "unite_non_calculable"))
  }
  if (u == "g") {
    return(list(ok = TRUE, grams = qty, reason = "ok"))
  }
  if (u == "ml") {
    return(list(ok = TRUE, grams = qty * food_density_g_ml(food_id), reason = "ok"))
  }
  if (u == "cup") {
    return(list(ok = TRUE, grams = qty * 250 * food_density_g_ml(food_id), reason = "ok"))
  }
  if (u == "tbsp") {
    return(list(ok = TRUE, grams = qty * 15 * food_density_g_ml(food_id), reason = "ok"))
  }
  if (u == "tsp") {
    return(list(ok = TRUE, grams = qty * 5 * food_density_g_ml(food_id), reason = "ok"))
  }
  if (u == "lb") {
    return(list(ok = TRUE, grams = qty * 453.59237, reason = "ok"))
  }
  if (u == "oz") {
    return(list(ok = TRUE, grams = qty * 28.34952, reason = "ok"))
  }
  if (u == "pinch") {
    return(list(ok = TRUE, grams = qty * 0.36, reason = "ok"))
  }
  if (u == "clove") {
    if (food_id == "garlic") return(list(ok = TRUE, grams = qty * 3, reason = "ok"))
    return(list(ok = FALSE, grams = NA_real_, reason = "unite_non_supportee"))
  }
  if (u == "green_onion_bunch") {
    if (food_id == "green_onion") return(list(ok = TRUE, grams = qty * 15, reason = "ok"))
    return(list(ok = FALSE, grams = NA_real_, reason = "unite_non_supportee"))
  }
  if (u == "unit") {
    ww <- unit_weight_g(food_id)
    if (!is.na(ww)) return(list(ok = TRUE, grams = qty * ww, reason = "ok"))
    return(list(ok = FALSE, grams = NA_real_, reason = "poids_unitaire_manquant"))
  }

  list(ok = FALSE, grams = NA_real_, reason = "unite_non_supportee")
}

format_number_fr <- function(x, decimals = 1L) {
  if (is.na(x)) return("-")
  fmt <- formatC(round(x, decimals), format = "f", digits = decimals, decimal.mark = ",")
  if (decimals > 0) {
    fmt <- sub(",?0+$", "", fmt)
    fmt <- sub(",$", "", fmt)
  }
  fmt
}

nutrition_value_with_unit <- function(value, unit, decimals) {
  paste0(format_number_fr(value, decimals), " ", unit)
}

escape_html_local <- function(x) {
  if (exists("escape_html", mode = "function")) return(escape_html(x))
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

#' Build HTML nutrition table for a recipe.
#' @param nutrition_result Output of [calc_recipe_nutrition()].
#' @param portions Numeric portion count.
#' @return A single HTML string.
#' @export
build_nutrition_table_html <- function(nutrition_result, portions = NA_real_) {
  totals <- nutrition_result$total
  per_portion <- nutrition_result$per_portion
  unresolved <- nutrition_result$unresolved
  source_usage <- nutrition_result$source_usage %||% data.frame(
    source = character(),
    n = integer(),
    stringsAsFactors = FALSE
  )

  rows <- character()
  for (i in seq_len(nrow(NUTRITION_NUTRIENTS))) {
    key <- NUTRITION_NUTRIENTS$key[i]
    label <- NUTRITION_NUTRIENTS$label[i]
    unit <- NUTRITION_NUTRIENTS$unit[i]
    dec <- NUTRITION_NUTRIENTS$decimals[i]

    total_val <- as.numeric(totals[[key]])
    per_val <- as.numeric(per_portion[[key]])

    rows <- c(
      rows,
      paste0(
        "<tr>",
        "<th scope=\"row\">", escape_html_local(label), "</th>",
        "<td class=\"recipe-nutrition-total\" data-base-total=\"", format(total_val, scientific = FALSE),
        "\" data-decimals=\"", dec, "\" data-unit=\"", escape_html_local(unit), "\">",
        nutrition_value_with_unit(total_val, unit, dec),
        "</td>",
        "<td class=\"recipe-nutrition-per-portion\" data-base-total=\"", format(total_val, scientific = FALSE),
        "\" data-decimals=\"", dec, "\" data-unit=\"", escape_html_local(unit), "\">",
        nutrition_value_with_unit(per_val, unit, dec),
        "</td>",
        "</tr>"
      )
    )
  }

  status <- paste0(
    "<p class=\"recipe-nutrition-meta\">Estimations basées sur les ingrédients saisis. ",
    if (is.finite(portions) && portions > 0) {
      paste0("Portions de base: ", format_number_fr(portions, 0), ".")
    } else {
      "Portions non définies."
    },
    "</p>"
  )

  src_html <- ""
  if (nrow(source_usage) > 0) {
    src_items <- vapply(seq_len(nrow(source_usage)), function(i) {
      paste0(escape_html_local(source_usage$source[i]), ": ", source_usage$n[i])
    }, character(1))
    src_html <- paste0(
      "<p class=\"recipe-nutrition-meta\">Sources nutrition (ingrédients calculés): ",
      paste(src_items, collapse = " · "),
      ".</p>"
    )
  }

  unresolved_html <- ""
  if (nrow(unresolved) > 0) {
    items <- vapply(seq_len(nrow(unresolved)), function(i) {
      nm <- escape_html_local(unresolved$ingredient[i])
      rs <- escape_html_local(unresolved$reason[i])
      paste0("<li><code>", nm, "</code> (", rs, ")</li>")
    }, character(1))
    unresolved_html <- paste0(
      "<details class=\"recipe-nutrition-missing\">",
      "<summary>Ingrédients non comptabilisés (", nrow(unresolved), ")</summary>",
      "<ul>", paste(items, collapse = ""), "</ul>",
      "</details>"
    )
  }

  main_cards <- list(
    list(label = "Calories", key = "energy_kcal", unit = "kcal", dec = 0),
    list(label = "Protéines", key = "protein_g", unit = "g", dec = 1),
    list(label = "Glucides", key = "carbs_g", unit = "g", dec = 1),
    list(label = "Lipides", key = "fat_g", unit = "g", dec = 1)
  )

  cards_html <- paste(
    vapply(main_cards, function(card) {
      val <- as.numeric(per_portion[[card$key]])
      paste0(
        "<div class=\"recipe-nutrition-card\">",
        "<span>", escape_html_local(card$label), "</span>",
        "<strong>", nutrition_value_with_unit(val, card$unit, card$dec), "</strong>",
        "</div>"
      )
    }, character(1)),
    collapse = ""
  )

  paste0(
    "<div class=\"recipe-nutrition-block\">",
    "<div class=\"recipe-nutrition-cards\">", cards_html, "</div>",
    "<details class=\"recipe-nutrition-details\">",
    "<summary class=\"recipe-nutrition-toggle\">Afficher le détail complet</summary>",
    status,
    src_html,
    "<table class=\"recipe-nutrition-table table table-sm\">",
    "<thead><tr><th>Nutriment</th><th>Par recette</th><th>Par portion</th></tr></thead>",
    "<tbody>", paste(rows, collapse = ""), "</tbody>",
    "</table>",
    unresolved_html,
    "</details>",
    "</div>"
  )
}

#' Compute nutrition totals and per-portion values for a recipe.
#' @param recipe Parsed recipe list from YAML.
#' @return A list with totals, per-portion values and unresolved ingredients.
#' @export
calc_recipe_nutrition <- function(recipe) {
  db <- load_nutrition_db()
  foods <- db$foods
  nutrient_keys <- NUTRITION_NUTRIENTS$key

  total <- setNames(as.list(rep(0, length(nutrient_keys))), nutrient_keys)
  unresolved <- data.frame(
    ingredient = character(),
    unit = character(),
    qty = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
  source_hits <- character()

  prep <- recipe$preparation %||% list()
  if (!is.list(prep)) prep <- list()

  for (section in prep) {
    steps <- section$etapes %||% list()
    if (!is.list(steps)) next

    for (step in steps) {
      ings <- step$ingredients %||% list()
      if (!is.list(ings)) next

      for (ing in ings) {
        nm <- as.character(ing$nom %||% "")
        unit <- as.character(ing$uni %||% "")
        qty <- suppressWarnings(as.numeric(ing$qte))

        food_id <- lookup_food_id(nm, db)
        if (is.na(food_id)) {
          unresolved <- rbind(
            unresolved,
            data.frame(
              ingredient = nm,
              unit = unit,
              qty = as.character(ing$qte %||% ""),
              reason = "ingredient_non_mappe",
              stringsAsFactors = FALSE
            )
          )
          next
        }

        conv <- as_grams(qty, unit, food_id)
        if (!isTRUE(conv$ok)) {
          unresolved <- rbind(
            unresolved,
            data.frame(
              ingredient = nm,
              unit = unit,
              qty = as.character(ing$qte %||% ""),
              reason = conv$reason,
              stringsAsFactors = FALSE
            )
          )
          next
        }

        row <- foods[foods$food_id == food_id, , drop = FALSE]
        if (nrow(row) < 1) {
          unresolved <- rbind(
            unresolved,
            data.frame(
              ingredient = nm,
              unit = unit,
              qty = as.character(ing$qte %||% ""),
              reason = "food_id_introuvable",
              stringsAsFactors = FALSE
            )
          )
          next
        }

        factor <- conv$grams / 100
        source_hits <- c(source_hits, as.character(row$source[1] %||% "unknown"))
        for (k in nutrient_keys) {
          val <- suppressWarnings(as.numeric(row[[k]][1]))
          if (!is.finite(val)) val <- 0
          total[[k]] <- as.numeric(total[[k]]) + (val * factor)
        }
      }
    }
  }

  portions <- suppressWarnings(as.numeric(recipe$portions))
  if (!is.finite(portions) || portions <= 0) portions <- NA_real_
  divisor <- if (is.finite(portions) && portions > 0) portions else 1

  per_portion <- total
  for (k in nutrient_keys) {
    per_portion[[k]] <- as.numeric(total[[k]]) / divisor
  }

  list(
    total = total,
    per_portion = per_portion,
    unresolved = unique(unresolved),
    portions = portions,
    source_usage = {
      s <- as.data.frame(table(source_hits), stringsAsFactors = FALSE)
      if (nrow(s) == 0) {
        data.frame(source = character(), n = integer(), stringsAsFactors = FALSE)
      } else {
        names(s) <- c("source", "n")
        s <- s[s$source != "", , drop = FALSE]
        s[order(-s$n), , drop = FALSE]
      }
    }
  )
}
