# yaml_to_qmds.R
# ------------------------------------------------------------------------------
# Utilities to convert recipe YAML files into Quarto (.qmd) pages.
# Adds an "Edit" button on every recipe page that links to an edit form page
# with the YAML path as a query parameter (static-site friendly).
#
# Comments upgrade:
# - recipe$commentaires can be:
#   (A) old format: character vector
#   (B) new format: list of dict with optional fields:
#       commentaire (string), evaluation (0.5..5 by 0.5), auteur (string), date (YYYY-MM-DD)
#   Backward compatibility:
#       note -> evaluation, nom -> auteur
#
# In the generated recipe page, we display:
# - Average stars (from available evaluations)
# - Total comment count
# - Count of comments written by "Alexandre Parent" and "\u00c9lodie Bourgeois"
#   (case-insensitive, accents-insensitive best-effort)
# ------------------------------------------------------------------------------

EDIT_PAGE_HREF <- "ajouter_recette/"
EDIT_PARAM_NAME <- "yaml"

escape_html <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

fmt_number <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  n <- suppressWarnings(as.numeric(x))
  if (is.na(n)) return(as.character(x))
  if (abs(n - round(n)) < 1e-9) return(as.character(as.integer(round(n))))
  s <- format(round(n, 2), nsmall = 2, trim = TRUE, scientific = FALSE)
  s <- sub("0+$", "", s)
  sub("\\.$", "", s)
}

normalize_measure_text <- function(x) {
  txt <- tolower(as.character(x %||% ""))
  txt <- stringi::stri_trans_general(txt, "Latin-ASCII")
  txt <- gsub("[^a-z0-9]+", " ", txt)
  trimws(gsub("\\s+", " ", txt))
}

is_count_unit <- function(unit) {
  normalize_measure_text(unit) %in% c("unite", "unites", "unit", "units")
}

display_measure_unit <- function(unit) {
  if (is_count_unit(unit)) return("")
  stringr::str_trim(as.character(unit %||% ""))
}

pluralize_french_token <- function(token) {
  prefix <- stringi::stri_extract_first_regex(token, "^[^\\p{L}]*")
  suffix <- stringi::stri_extract_last_regex(token, "[^\\p{L}]*$")
  if (is.na(prefix)) prefix <- ""
  if (is.na(suffix)) suffix <- ""
  core <- stringi::stri_replace_first_regex(token, "^[^\\p{L}]*", "")
  core <- stringi::stri_replace_last_regex(core, "[^\\p{L}]*$", "")
  if (!nzchar(core)) return(token)
  core_norm <- normalize_measure_text(core)
  if (!nzchar(core_norm)) return(token)
  if (core_norm %in% c("de", "du", "des", "et", "ou", "en", "a", "au", "aux", "avec", "sans", "pour", "sur", "sous")) return(token)
  if (grepl("^[a-z]{1,2}$", sub("'.*$", "", core_norm)) && grepl("'", core)) return(token)
  if (grepl("ment$", core_norm)) return(token)
  if (grepl("(s|x|z)$", core_norm)) return(token)
  plural <- if (grepl("al$", core_norm)) {
    sub("al$", "aux", core, ignore.case = TRUE)
  } else if (grepl("(eau|au|eu)$", core_norm)) {
    paste0(core, "x")
  } else {
    paste0(core, "s")
  }
  paste0(prefix, plural, suffix)
}

display_ingredient_name <- function(name, qty, unit) {
  nm <- stringr::str_trim(as.character(name %||% ""))
  if (!nzchar(nm) || !is_count_unit(unit)) return(nm)
  q <- suppressWarnings(as.numeric(qty))
  if (!is.finite(q) || abs(q - 1) < 1e-9) return(nm)
  parts <- strsplit(nm, "\\s+")[[1]]
  out <- character(length(parts))
  skip_next <- FALSE
  for (i in seq_along(parts)) {
    token <- parts[[i]]
    token_norm <- normalize_measure_text(token)
    token_prefix_norm <- normalize_measure_text(sub("['’].*$", "", token))
    if (skip_next) {
      out[[i]] <- token
      skip_next <- FALSE
      next
    }
    if (token_norm %in% c("de", "du", "des")) {
      out[[i]] <- token
      skip_next <- TRUE
      next
    }
    if (token_prefix_norm %in% c("d", "l")) {
      out[[i]] <- token
      next
    }
    out[[i]] <- pluralize_french_token(token)
  }
  paste(out, collapse = " ")
}

mass_unit_factor_g <- function(unit) {
  u <- normalize_measure_text(unit)
  map <- c(
    "g" = 1, "gramme" = 1, "grammes" = 1, "gram" = 1, "grams" = 1,
    "kg" = 1000, "kilogramme" = 1000, "kilogrammes" = 1000,
    "lb" = 453.59237, "lbs" = 453.59237,
    "oz" = 28.34952, "once" = 28.34952, "onces" = 28.34952
  )
  v <- unname(map[u])
  if (length(v) == 1 && !is.na(v)) as.numeric(v) else NA_real_
}

volume_unit_factor_ml <- function(unit) {
  u <- normalize_measure_text(unit)
  map <- c(
    "ml" = 1, "millilitre" = 1, "millilitres" = 1,
    "l" = 1000, "litre" = 1000, "litres" = 1000,
    "t" = 250, "tasse" = 250, "tasses" = 250,
    "c a soupe" = 15, "cuillere a soupe" = 15, "cuilleres a soupe" = 15, "c a table" = 15, "c s" = 15, "c.s." = 15,
    "c a the" = 5, "cuillere a the" = 5, "cuilleres a the" = 5, "c t" = 5, "c.t." = 5
  )
  v <- unname(map[u])
  if (length(v) == 1 && !is.na(v)) as.numeric(v) else NA_real_
}

canonical_mass_unit <- function(unit) {
  u <- normalize_measure_text(unit)
  if (u %in% c("lb", "lbs")) return("lbs")
  if (u %in% c("oz", "once", "onces")) return("onces")
  if (u %in% c("g", "gramme", "grammes", "gram", "grams", "kg", "kilogramme", "kilogrammes")) return("g")
  "g"
}

canonical_volume_unit <- function(unit) {
  u <- normalize_measure_text(unit)
  if (u %in% c("ml", "millilitre", "millilitres", "l", "litre", "litres")) return("ml")
  if (u %in% c("t", "tasse", "tasses")) return("tasse")
  if (u %in% c("c a soupe", "cuillere a soupe", "cuilleres a soupe", "c a table", "c s", "c.s.")) return("c. à soupe")
  if (u %in% c("c a the", "cuillere a the", "cuilleres a the", "c t", "c.t.")) return("c. à thé")
  "ml"
}

convert_from_g <- function(value_g, unit) {
  f <- mass_unit_factor_g(unit)
  if (!is.finite(value_g) || !is.finite(f) || f <= 0) return(NA_real_)
  value_g / f
}

convert_from_ml <- function(value_ml, unit) {
  f <- volume_unit_factor_ml(unit)
  if (!is.finite(value_ml) || !is.finite(f) || f <= 0) return(NA_real_)
  value_ml / f
}

ingredient_density_table <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    paths <- c(
      "data/ingredient_density_qc_20_25c.csv",
      "../data/ingredient_density_qc_20_25c.csv"
    )
    hit <- paths[file.exists(paths)]
    if (length(hit) == 0) {
      cache <<- data.frame()
      return(cache)
    }
    tbl <- utils::read.csv(hit[[1]], stringsAsFactors = FALSE, check.names = FALSE)
    if (!("ingredient_key" %in% names(tbl)) || !("density_g_ml" %in% names(tbl))) {
      cache <<- data.frame()
      return(cache)
    }
    tbl$ingredient_key_norm <- normalize_measure_text(tbl$ingredient_key)
    tbl$aliases <- as.character(tbl$aliases %||% "")
    cache <<- tbl
    cache
  }
})

lookup_density_g_ml <- function(ingredient_name, density_tbl = ingredient_density_table()) {
  if (!is.data.frame(density_tbl) || nrow(density_tbl) == 0) return(NA_real_)
  name_norm <- normalize_measure_text(ingredient_name)
  if (!nzchar(name_norm)) return(NA_real_)

  exact <- density_tbl$density_g_ml[density_tbl$ingredient_key_norm == name_norm]
  if (length(exact) > 0) return(suppressWarnings(as.numeric(exact[[1]])))

  for (i in seq_len(nrow(density_tbl))) {
    aliases <- unlist(strsplit(as.character(density_tbl$aliases[i] %||% ""), "\\|", fixed = FALSE))
    aliases <- normalize_measure_text(aliases)
    aliases <- aliases[nzchar(aliases)]
    if (length(aliases) == 0) next
    name_padded <- paste0(" ", name_norm, " ")
    if (any(vapply(aliases, function(a) grepl(paste0(" ", a, " "), name_padded, fixed = TRUE), logical(1)))) {
      return(suppressWarnings(as.numeric(density_tbl$density_g_ml[i])))
    }
  }
  NA_real_
}

ingredient_measurement <- function(ing, density_tbl = ingredient_density_table()) {
  q_legacy <- suppressWarnings(as.numeric(ing$qte %||% NA_real_))
  u_legacy <- stringr::str_trim(as.character(ing$uni %||% ""))
  q_mass <- suppressWarnings(as.numeric(ing$qte_masse %||% NA_real_))
  u_mass <- as.character(ing$uni_masse %||% "")
  q_volume <- suppressWarnings(as.numeric(ing$qte_volume %||% NA_real_))
  u_volume <- as.character(ing$uni_volume %||% "")

  legacy_mass_factor <- mass_unit_factor_g(u_legacy)
  legacy_volume_factor <- volume_unit_factor_ml(u_legacy)
  count_unit <- is_count_unit(u_legacy)
  default_kind <- if (is.finite(q_legacy) && is.finite(legacy_mass_factor)) {
    "mass"
  } else if (is.finite(q_legacy) && is.finite(legacy_volume_factor)) {
    "volume"
  } else {
    "other"
  }

  explicit_mass_g <- if (is.finite(q_mass) && is.finite(mass_unit_factor_g(u_mass))) q_mass * mass_unit_factor_g(u_mass) else NA_real_
  explicit_volume_ml <- if (is.finite(q_volume) && is.finite(volume_unit_factor_ml(u_volume))) q_volume * volume_unit_factor_ml(u_volume) else NA_real_
  has_explicit_mass <- is.finite(explicit_mass_g)
  has_explicit_volume <- is.finite(explicit_volume_ml)
  density <- lookup_density_g_ml(ing$nom %||% "", density_tbl)

  base_mass_g <- explicit_mass_g
  base_volume_ml <- explicit_volume_ml
  mass_source <- if (has_explicit_mass) "explicit" else "none"
  volume_source <- if (has_explicit_volume) "explicit" else "none"

  if (!is.finite(base_mass_g) && identical(default_kind, "mass")) {
    base_mass_g <- q_legacy * legacy_mass_factor
    mass_source <- "default"
  }
  if (!is.finite(base_volume_ml) && identical(default_kind, "volume")) {
    base_volume_ml <- q_legacy * legacy_volume_factor
    volume_source <- "default"
  }
  if (!is.finite(base_mass_g) && is.finite(base_volume_ml) && is.finite(density) && density > 0) {
    base_mass_g <- base_volume_ml * density
    mass_source <- "derived"
  }
  if (!is.finite(base_volume_ml) && is.finite(base_mass_g) && is.finite(density) && density > 0) {
    base_volume_ml <- base_mass_g / density
    volume_source <- "derived"
  }

  explicit_mass_unit <- if (nzchar(stringr::str_trim(u_mass))) canonical_mass_unit(u_mass) else if (identical(default_kind, "mass")) canonical_mass_unit(u_legacy) else "g"
  explicit_volume_unit <- if (nzchar(stringr::str_trim(u_volume))) canonical_volume_unit(u_volume) else if (identical(default_kind, "volume")) canonical_volume_unit(u_legacy) else "ml"

  can_toggle <- !count_unit && (is.finite(base_mass_g) || is.finite(base_volume_ml))

  list(
    base_mass_g = base_mass_g,
    base_volume_ml = base_volume_ml,
    has_explicit_mass = has_explicit_mass,
    has_explicit_volume = has_explicit_volume,
    mass_source = mass_source,
    volume_source = volume_source,
    unit_mass = explicit_mass_unit,
    unit_volume = explicit_volume_unit,
    explicit_mass_unit = explicit_mass_unit,
    explicit_volume_unit = explicit_volume_unit,
    can_toggle = can_toggle,
    default_mode = "default",
    default_kind = default_kind,
    count_unit = count_unit,
    q_legacy = q_legacy,
    u_legacy = u_legacy
  )
}

ingredient_display_for_mode <- function(meta, mode = "default") {
  if (identical(mode, "default")) {
    return(list(qty = meta$q_legacy %||% NA_real_, unit = meta$u_legacy %||% "", derived = FALSE))
  }
  if (identical(mode, "mass") && is.finite(meta$base_mass_g)) {
    unit <- meta$explicit_mass_unit %||% "g"
    qty <- convert_from_g(meta$base_mass_g, unit)
    if (!is.finite(qty)) {
      unit <- "g"
      qty <- meta$base_mass_g
    }
    return(list(qty = qty, unit = unit, derived = identical(meta$mass_source, "derived")))
  }
  if (identical(mode, "volume") && is.finite(meta$base_volume_ml)) {
    unit <- meta$explicit_volume_unit %||% "ml"
    qty <- convert_from_ml(meta$base_volume_ml, unit)
    if (!is.finite(qty)) {
      unit <- "ml"
      qty <- meta$base_volume_ml
    }
    return(list(qty = qty, unit = unit, derived = identical(meta$volume_source, "derived")))
  }
  list(qty = meta$q_legacy %||% NA_real_, unit = meta$u_legacy %||% "", derived = FALSE)
}

render_ingredient_li <- function(ing, list_kind = "generic", density_tbl = ingredient_density_table()) {
  nom_raw <- stringr::str_trim(as.character(ing$nom %||% ""))
  rangee_raw <- stringr::str_trim(as.character(ing$rangee %||% ing$rayon %||% ""))
  nom <- escape_html(nom_raw)
  rangee <- escape_html(rangee_raw)
  meta <- ingredient_measurement(ing, density_tbl = density_tbl)
  shown <- ingredient_display_for_mode(meta, mode = meta$default_mode)
  visible_name <- escape_html(display_ingredient_name(nom_raw, shown$qty, shown$unit))

  q_label <- escape_html(fmt_number(shown$qty))
  uni <- escape_html(display_measure_unit(shown$unit))
  derived_cls <- if (isTRUE(shown$derived)) " ingredient-approx" else ""

  q_attr <- if (is.finite(suppressWarnings(as.numeric(shown$qty)))) paste0(" data-base=\"", suppressWarnings(as.numeric(shown$qty)), "\"") else ""
  content <- paste0(
    "<span class=\"ingredient-qte", derived_cls, "\"", q_attr, ">", q_label, "</span>",
    if (nzchar(uni)) paste0(" <span class=\"ingredient-uni\">", uni, "</span>") else "",
    if (nzchar(visible_name)) paste0(" <span class=\"ingredient-nom\">", visible_name, "</span>") else ""
  )

  paste0(
    "<li class=\"recipe-ingredient\" data-list-kind=\"", list_kind,
    "\" data-rangee=\"", rangee,
    "\" data-ingredient-name=\"", nom,
    "\" data-base-ingredient-name=\"", nom,
    "\" data-ingredient-unit=\"", uni,
    "\" data-default-qty=\"", if (is.finite(meta$q_legacy)) meta$q_legacy else "",
    "\" data-default-unit=\"", escape_html(meta$u_legacy %||% ""),
    "\" data-display-default-unit=\"", escape_html(display_measure_unit(meta$u_legacy %||% "")),
    "\" data-default-kind=\"", escape_html(meta$default_kind %||% "other"),
    "\" data-measure-enabled=\"", if (meta$can_toggle) "1" else "0",
    "\" data-base-mass-g=\"", if (is.finite(meta$base_mass_g)) meta$base_mass_g else "",
    "\" data-base-volume-ml=\"", if (is.finite(meta$base_volume_ml)) meta$base_volume_ml else "",
    "\" data-unit-mass=\"", escape_html(meta$unit_mass %||% "g"),
    "\" data-unit-volume=\"", escape_html(meta$unit_volume %||% "ml"),
    "\" data-explicit-unit-mass=\"", escape_html(meta$explicit_mass_unit %||% "g"),
    "\" data-explicit-unit-volume=\"", escape_html(meta$explicit_volume_unit %||% "ml"),
    "\" data-explicit-mass=\"", if (isTRUE(meta$has_explicit_mass)) "1" else "0",
    "\" data-explicit-volume=\"", if (isTRUE(meta$has_explicit_volume)) "1" else "0",
    "\" data-mass-source=\"", escape_html(meta$mass_source %||% "none"),
    "\" data-volume-source=\"", escape_html(meta$volume_source %||% "none"),
    "\" data-default-mode=\"", meta$default_mode, "\">",
    "<label class=\"recipe-check-row\">",
    "<input type=\"checkbox\" class=\"recipe-check recipe-ingredient-check\">",
    "<span class=\"ingredient-label\">", content, "</span>",
    "</label>",
    "</li>"
  )
}

render_ingredient_inline <- function(ing, density_tbl = ingredient_density_table()) {
  li <- render_ingredient_li(ing, list_kind = "inline", density_tbl = density_tbl)
  sub("^<li[^>]*><label", "<label", sub("</label></li>$", "</label>", li))
}

build_fact_box <- function(label, value, href = NULL) {
  value_html <- if (!is.null(href) && nzchar(as.character(href))) {
    paste0(
      "<a href=\"", escape_html(as.character(href)),
      "\" target=\"_blank\" rel=\"noopener noreferrer\">",
      escape_html(value),
      "</a>"
    )
  } else {
    escape_html(value)
  }
  paste0(
    "<div class=\"recipe-fact\">",
    "<span>", escape_html(label), "</span>",
    "<strong>", value_html, "</strong>",
    "</div>"
  )
}

source_label_and_href <- function(source_raw) {
  src <- stringr::str_trim(as.character(source_raw %||% ""))
  if (!nzchar(src)) return(list(label = "", href = NULL))
  if (!grepl("^https?://", src, ignore.case = TRUE)) {
    return(list(label = src, href = NULL))
  }
  host <- sub("^https?://([^/]+).*$", "\\1", src, ignore.case = TRUE)
  host <- sub("^www\\.", "", host, ignore.case = TRUE)
  if (!nzchar(host)) host <- src
  list(label = host, href = src)
}

extract_step_timers <- function(step_text) {
  txt <- as.character(step_text %||% "")
  if (!nzchar(txt)) return(list())
  m <- gregexpr("(\\d+)\\s*(h|heure|heures|hr|hrs|min|minute|minutes)", txt, perl = TRUE, ignore.case = TRUE)
  vals <- regmatches(txt, m)[[1]]
  if (length(vals) == 1 && vals[1] == "-1") return(list())
  out <- list()
  for (v in vals) {
    mm <- regexec("(\\d+)\\s*(h|heure|heures|hr|hrs|min|minute|minutes)", v, perl = TRUE, ignore.case = TRUE)
    parts <- regmatches(v, mm)[[1]]
    if (length(parts) < 3) next
    n <- suppressWarnings(as.numeric(parts[2]))
    u <- tolower(parts[3])
    if (!is.finite(n)) next
    sec <- if (u %in% c("h", "heure", "heures", "hr", "hrs")) n * 3600 else n * 60
    if (sec > 0) out[[length(out) + 1]] <- list(label = trimws(v), seconds = as.integer(sec))
  }
  out
}

#' Convert one recipe YAML file to Quarto (`.qmd`).
#'
#' @param yaml_path Path to source YAML recipe file.
#' @param qmd_path Optional output `.qmd` path. Defaults to same path with
#'   extension replaced by `.qmd`.
#' @return Output `.qmd` path invisibly.
#' @importFrom yaml read_yaml
#' @importFrom fs path_ext_set path_file path_rel
#' @importFrom stringr str_trim str_to_lower
#' @export
yaml_recipe_to_qmd <- function(yaml_path, qmd_path = NULL) {
  stopifnot(file.exists(yaml_path))

  if (is.null(qmd_path)) {
    qmd_path <- fs::path_ext_set(yaml_path, "qmd")
  }

  recipe <- yaml::read_yaml(yaml_path)
  density_tbl <- ingredient_density_table()
  lines <- character()

  # ---- Quarto front-matter ----
  image_line <- if (!is.null(recipe$image_guid) && nzchar(as.character(recipe$image_guid))) {
    guid <- as.character(recipe$image_guid)
    img_path <- paste0("/images/", guid, ".jpg")
    paste0("image: ", img_path)
  } else {
    ""
  }
  lines <- c(
    lines,
    "---",
    paste0("title: ", recipe$nom),
    image_line
  )

  if (!is.null(recipe$categories)) {
    cats <- unlist(recipe$categories, use.names = FALSE)
    cats <- stringr::str_trim(as.character(cats))
    cats <- cats[nzchar(cats)]
    if (length(cats) > 0) {
      lines <- c(lines, "categories:", paste0("  - ", cats))
    }
  }
  lines <- c(lines, "---", "")

  # ---- Edit button ----
  yaml_rel_to_root <- fs::path_rel(yaml_path, start = ".")
  yaml_rel_to_root <- gsub("\\\\", "/", yaml_rel_to_root)
  if (!startsWith(yaml_rel_to_root, "/")) yaml_rel_to_root <- paste0("/", yaml_rel_to_root)
  recipe_url <- sub("\\.ya?ml$", ".qmd", yaml_rel_to_root, ignore.case = TRUE)

  yaml_qp <- utils::URLencode(yaml_rel_to_root, reserved = TRUE)
  edit_href <- paste0("../", EDIT_PAGE_HREF, "?", EDIT_PARAM_NAME, "=", yaml_qp)
  quick_comment_href <- paste0(edit_href, "&quick=comment")
  base_portions <- suppressWarnings(as.numeric(recipe$portions %||% NA_real_))
  if (length(base_portions) != 1 || !is.finite(base_portions) || base_portions <= 0) {
    base_portions <- NULL
  }

  cart_ingredients <- list()
  for (section in recipe$preparation %||% list()) {
    for (step in section$etapes %||% list()) {
      for (ing in step$ingredients %||% list()) {
        nom <- as.character(ing$nom %||% "")
        if (!nzchar(stringr::str_trim(nom))) next
        meta <- ingredient_measurement(ing, density_tbl = density_tbl)
        cart_ingredients[[length(cart_ingredients) + 1]] <- list(
          nom = nom,
          uni = as.character(ing$uni %||% ""),
          qte = if (is.finite(suppressWarnings(as.numeric(ing$qte %||% NA_real_)))) as.numeric(ing$qte) else ing$qte,
          rangee = as.character(ing$rangee %||% ing$rayon %||% "")
        )
      }
    }
  }
  cart_payload <- list(
    id = recipe_url,
    title = as.character(recipe$nom %||% ""),
    url = recipe_url,
    portions_base = base_portions,
    portions_target = base_portions,
    ingredients = cart_ingredients
  )
  cart_json <- jsonlite::toJSON(cart_payload, auto_unbox = TRUE, null = "null")
  cart_json <- gsub("</", "<\\\\/", cart_json, fixed = TRUE)

  # ---- Quick facts and tools ----
  facts <- character()
  if (!is.null(recipe$source) && nzchar(as.character(recipe$source))) {
    src <- source_label_and_href(recipe$source)
    facts <- c(facts, build_fact_box("Source", src$label, href = src$href))
  }
  if (!is.null(recipe$se_congele)) {
    txt <- if (isTRUE(recipe$se_congele)) "Oui" else "Non"
    facts <- c(facts, build_fact_box("Se cong\u00e8le", txt))
  }
  t <- if (is.list(recipe$temps)) recipe$temps else list()
  prep <- if (!is.null(t$preparation) && nzchar(fmt_number(t$preparation))) paste0(fmt_number(t$preparation), " min") else "-"
  cook <- if (!is.null(t$cuisson) && nzchar(fmt_number(t$cuisson))) paste0(fmt_number(t$cuisson), " min") else "-"
  facts <- c(facts, build_fact_box("Temps pr\u00e9paration", prep))
  facts <- c(facts, build_fact_box("Temps cuisson", cook))
  if (!is.null(t$refrigeration) && nzchar(fmt_number(t$refrigeration))) {
    cool <- paste0(fmt_number(t$refrigeration), " min")
    facts <- c(facts, build_fact_box("Temps r\u00e9frig\u00e9ration", cool))
  }
  cover_image_html <- ""
  if (!is.null(recipe$image_guid) && nzchar(as.character(recipe$image_guid))) {
    guid <- trimws(as.character(recipe$image_guid))
    if (nzchar(guid) && !tolower(guid) %in% c("na", "null", "~")) {
      cover_src <- paste0("/images/", guid, ".jpg")
      cover_alt <- escape_html(paste0("Photo de couverture - ", as.character(recipe$nom %||% "Recette")))
      cover_image_html <- paste0(
        "<figure class=\"recipe-cover\">",
        "<img class=\"recipe-cover-image\" src=\"", cover_src, "\" alt=\"", cover_alt, "\" loading=\"eager\" decoding=\"async\">",
        "</figure>"
      )
    }
  }

  lines <- c(
    lines,
    "```{=html}",
    "<div class=\"recipe-toolbar\">",
    "<div class=\"recipe-toolbar-main\">",
    paste0("<a href=\"", edit_href, "\" class=\"btn btn-outline-primary btn-sm\">\u270f\ufe0f Modifier</a>"),
    paste0("<a href=\"", quick_comment_href, "\" class=\"btn btn-outline-primary btn-sm\">\ud83d\udcac + Commentaire</a>"),
    "</div>",
    "<div class=\"recipe-toolbar-actions\">",
    "<button id=\"recipe-cart-toggle\" type=\"button\" class=\"btn btn-outline-success btn-sm\">\ud83d\uded2 Mettre au panier</button>",
    "<button id=\"recipe-reading-mode\" type=\"button\" class=\"btn btn-outline-secondary btn-sm\">\U0001f373 Mode cuisine</button>",
    "</div>",
    "</div>",
    if (length(facts) > 0) paste0("<div class=\"recipe-facts-grid\">", paste(facts, collapse = ""), "</div>") else "",
    cover_image_html,
    "```",
    ""
  )

  lines <- c(
    lines,
    "```{=html}",
    paste0("<script id=\"recipe-cart-data\" type=\"application/json\">", cart_json, "</script>"),
    "<script src=\"/includes/recipe-cart.js\"></script>",
    "```",
    ""
  )

  meta_badges <- character()
  if (!is.null(recipe$difficulte) && nzchar(as.character(recipe$difficulte))) {
    meta_badges <- c(meta_badges, paste0("<span class=\"recipe-meta-badge\">Difficult\u00e9: ", escape_html(recipe$difficulte), "</span>"))
  }
  if (!is.null(recipe$cout) && nzchar(as.character(recipe$cout))) {
    meta_badges <- c(meta_badges, paste0("<span class=\"recipe-meta-badge\">Co\u00fbt: ", escape_html(recipe$cout), "</span>"))
  }
  if (is.list(recipe$allergenes) && length(recipe$allergenes) > 0) {
    allg <- paste(escape_html(unlist(recipe$allergenes, use.names = FALSE)), collapse = ", ")
    meta_badges <- c(meta_badges, paste0("<span class=\"recipe-meta-badge\">Allerg\u00e8nes: ", allg, "</span>"))
  }
  if (length(meta_badges) > 0) {
    lines <- c(
      lines,
      "```{=html}",
      paste0("<div class=\"recipe-meta-badges\">", paste(meta_badges, collapse = ""), "</div>"),
      "```",
      ""
    )
  }

  has_scaler <- !is.null(base_portions) && !is.na(base_portions) && base_portions > 0
  has_measure_toggle <- any(vapply(recipe$preparation %||% list(), function(section) {
    any(vapply(section$etapes %||% list(), function(step) {
      any(vapply(step$ingredients %||% list(), function(ing) {
        meta <- ingredient_measurement(ing, density_tbl = density_tbl)
        is.finite(meta$base_mass_g) || is.finite(meta$base_volume_ml)
      }, logical(1)))
    }, logical(1)))
  }, logical(1)))
  has_step_images <- any(vapply(recipe$preparation %||% list(), function(section) {
    any(vapply(section$etapes %||% list(), function(step) {
      !is.null(step$image_guid) && nzchar(as.character(step$image_guid))
    }, logical(1)))
  }, logical(1)))

  if (has_scaler || has_measure_toggle) {
    portions_html <- ""
    if (has_scaler) {
      portions_html <- paste0(
        "<label for=\"servings-input\" class=\"form-label mb-1\">Ajuster les portions</label>",
        "<div class=\"d-flex gap-2 align-items-center mb-2\">",
        "<input id=\"servings-input\" class=\"form-control form-control-sm\" type=\"number\" min=\"1\" step=\"1\" value=\"", base_portions, "\" style=\"max-width: 110px;\">",
        "<button id=\"servings-reset\" class=\"btn btn-outline-secondary btn-sm\" type=\"button\">R\u00e9initialiser</button>",
        "<span class=\"text-muted small\">Base: ", base_portions, "</span>",
        "</div>"
      )
    }
    mode_html <- ""
    if (has_measure_toggle) {
      mode_html <- paste0(
        "<div class=\"recipe-measure-mode\">",
        "<span class=\"form-label mb-1 d-block\">Affichage des quantit\u00e9s</span>",
        "<div class=\"btn-group btn-group-sm\" role=\"group\" aria-label=\"Mode de mesure\">",
        "<button id=\"measure-mode-default\" class=\"btn btn-outline-secondary\" type=\"button\" data-mode=\"default\">Par d\u00e9faut</button>",
        "<button id=\"measure-mode-volume\" class=\"btn btn-outline-secondary\" type=\"button\" data-mode=\"volume\">Volume</button>",
        "<button id=\"measure-mode-mass\" class=\"btn btn-outline-secondary\" type=\"button\" data-mode=\"mass\">Masse</button>",
        "</div>",
        "<div class=\"recipe-measure-submodes mt-2\">",
        "<div id=\"volume-submodes\" class=\"btn-group btn-group-sm\" role=\"group\" aria-label=\"Format volume\">",
        "<button id=\"volume-unit-kitchen\" class=\"btn btn-outline-secondary\" type=\"button\">Cuill\u00e8res / tasse</button>",
        "<button id=\"volume-unit-ml\" class=\"btn btn-outline-secondary\" type=\"button\">mL</button>",
        "</div>",
        "<div id=\"mass-submodes\" class=\"btn-group btn-group-sm ms-2\" role=\"group\" aria-label=\"Format masse\">",
        "<button id=\"mass-unit-g\" class=\"btn btn-outline-secondary\" type=\"button\">g</button>",
        "<button id=\"mass-unit-lbs\" class=\"btn btn-outline-secondary\" type=\"button\">lbs</button>",
        "</div>",
        "</div>",
        "<span id=\"measure-mode-help\" class=\"text-muted small d-block mt-1\">Les quantit\u00e9s converties sont affich\u00e9es dans une couleur diff\u00e9rente.</span>",
        "</div>"
      )
    }
    lines <- c(
      lines,
      "```{=html}",
      paste0(
        "<div class=\"recipe-servings-control\">",
        portions_html,
        mode_html,
        "</div>"
      ),
      "```",
      ""
    )
  }

  # ---- Nutrition ----
  nutrition_html <- NULL
  if (exists("calc_recipe_nutrition", mode = "function") &&
      exists("build_nutrition_table_html", mode = "function")) {
    nutrition_res <- tryCatch(
      calc_recipe_nutrition(recipe),
      error = function(e) NULL
    )
    if (!is.null(nutrition_res)) {
      nutrition_html <- tryCatch(
        build_nutrition_table_html(nutrition_res, portions = base_portions),
        error = function(e) NULL
      )
    }
  }

  if (!is.null(nutrition_html) && nzchar(nutrition_html)) {
    lines <- c(
      lines,
      "```{=html}",
      "<section class=\"recipe-nutrition-section\">",
      "<h2>Valeurs nutritives</h2>",
      nutrition_html,
      "</section>",
      "```",
      ""
    )
  }

  if (has_step_images) {
    lines <- c(
      lines,
      "```{=html}",
      "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/glightbox/dist/css/glightbox.min.css\">",
      "<script src=\"https://cdn.jsdelivr.net/npm/glightbox/dist/js/glightbox.min.js\"></script>",
      "```",
      ""
    )
  }

  lines <- c(
    lines,
    "```{=html}",
    "<script>(function(){",
    "const key='recipe_reading_mode';",
    "let wakeLock=null;",
    "const apply=(on)=>{document.body.classList.toggle('reading-mode', on);};",
    "const requestWake=async()=>{try{if('wakeLock' in navigator){wakeLock=await navigator.wakeLock.request('screen');}}catch(e){}};",
    "const releaseWake=async()=>{try{if(wakeLock){await wakeLock.release(); wakeLock=null;}}catch(e){}};",
    "document.addEventListener('DOMContentLoaded', ()=>{",
    "const btn=document.getElementById('recipe-reading-mode');",
    "const saved=localStorage.getItem(key)==='1'; apply(saved); if(saved) requestWake();",
    "if(!btn) return;",
    "const refresh=()=>{btn.classList.toggle('active', document.body.classList.contains('reading-mode'));}; refresh();",
    "btn.addEventListener('click', async()=>{const next=!document.body.classList.contains('reading-mode'); apply(next); localStorage.setItem(key, next?'1':'0'); refresh(); if(next){await requestWake();} else {await releaseWake();}});",
    "});",
    "})();</script>",
    "```",
    ""
  )

  # ---- Ingredients (grouped by section) ----
  lines <- c(lines, "## Ingr\u00e9dients", "")
  lines <- c(
    lines,
    "```{=html}",
    "<div class=\"recipe-grocery-actions\">",
    "<button id=\"copy-grocery-list\" type=\"button\" class=\"btn btn-outline-secondary btn-sm\">\U0001f4cb G\u00e9n\u00e9rer la liste d'\u00e9picerie</button>",
    "<button id=\"reset-recipe-checks\" type=\"button\" class=\"btn btn-outline-secondary btn-sm\">\u21ba R\u00e9initialiser les cases</button>",
    "<span id=\"grocery-copy-feedback\" class=\"text-muted small\"></span>",
    "</div>",
    "```",
    ""
  )

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "")

    ing_list <- list()
    for (step in section$etapes) {
      if (!is.null(step$ingredients)) {
        for (ing in step$ingredients) {
          key <- paste(ing$nom, ing$uni)
          if (!key %in% names(ing_list)) {
            ing_list[[key]] <- ing
          }
        }
      }
    }
    lines <- c(lines, "```{=html}", "<ul class=\"recipe-ingredients recipe-grocery-list\">")
    for (item in ing_list) lines <- c(lines, render_ingredient_li(item, list_kind = "grocery", density_tbl = density_tbl))
    lines <- c(lines, "</ul>", "```")
    lines <- c(lines, "")
  }

  # ---- Equipment (grouped by section) ----
  lines <- c(lines, "## \u00c9quipements", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "")

    equip_list <- character()
    for (step in section$etapes) {
      if (!is.null(step$equipements)) {
        equip_list <- c(equip_list, step$equipements)
      }
    }
    equip_list <- unique(equip_list)

    for (eq in equip_list) {
      lines <- c(lines, paste0("- ", stringr::str_trim(eq)))
    }
    lines <- c(lines, "")
  }

  # ---- Preparation steps (2-column cooking layout) ----
  lines <- c(lines, "## Pr\u00e9paration", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "", "```{=html}", "<div class=\"recipe-prep-grid\">")

    for (i in seq_along(section$etapes)) {
      step <- section$etapes[[i]]
      step_text <- escape_html(step$etape %||% "")

      ing_html <- ""
      if (is.list(step$ingredients) && length(step$ingredients) > 0) {
        ing_lines <- vapply(step$ingredients, function(x) render_ingredient_li(x, list_kind = "step", density_tbl = density_tbl), character(1))
        ing_html <- paste0(
          "<div class=\"recipe-prep-ingredients\">",
          "<div class=\"recipe-prep-label\">Ingr\u00e9dients</div>",
          "<ul class=\"recipe-ingredients recipe-step-ingredients\">",
          paste(ing_lines, collapse = ""),
          "</ul>",
          "</div>"
        )
      } else {
        ing_html <- "<div class=\"recipe-prep-ingredients\"><div class=\"recipe-prep-label\">Ingr\u00e9dients</div><div class=\"text-muted small\">Aucun ingr\u00e9dient sp\u00e9cifique.</div></div>"
      }

      img_html <- ""
      if (!is.null(step$image_guid) && nzchar(as.character(step$image_guid))) {
        src <- paste0("/images/", step$image_guid, ".jpg")
        img_html <- paste0(
          "<a href=\"", src, "\" class=\"glightbox recipe-step-image-link\" data-gallery=\"recipe-steps\">",
          "<img src=\"", src, "\" alt=\"Image \u00e9tape ", i, "\" class=\"recipe-step-thumb\">",
          "</a>"
        )
      }
      timers <- extract_step_timers(step$etape)
      timer_html <- ""
      if (length(timers) > 0) {
        timer_buttons <- vapply(timers, function(t) {
          paste0("<button type=\"button\" class=\"btn btn-outline-secondary btn-sm recipe-timer-btn\" data-seconds=\"", t$seconds, "\">\u23f1\ufe0f ", escape_html(t$label), "</button>")
        }, character(1))
        timer_html <- paste0("<div class=\"recipe-step-timers\">", paste(timer_buttons, collapse = ""), "</div>")
      }

      lines <- c(
        lines,
        paste0(
          "<article class=\"recipe-prep-step\">",
          ing_html,
          "<div class=\"recipe-prep-instruction\">",
          "<div class=\"recipe-prep-stepno\"><label class=\"recipe-check-row\"><input type=\"checkbox\" class=\"recipe-check recipe-step-check\"><span>\u00c9tape ", i, "</span></label></div>",
          "<p>", step_text, "</p>",
          timer_html,
          img_html,
          "</div>",
          "</article>"
        )
      )
    }

    lines <- c(lines, "</div>", "```", "")
  }

  if (has_scaler || has_measure_toggle) {
    lines <- c(
      lines,
      "```{=html}",
      paste0(
        "<script>",
        "(function(){",
        "const input=document.getElementById('servings-input');",
        "const reset=document.getElementById('servings-reset');",
        "const btnDefault=document.getElementById('measure-mode-default');",
        "const btnVolume=document.getElementById('measure-mode-volume');",
        "const btnMass=document.getElementById('measure-mode-mass');",
        "const btnVolKitchen=document.getElementById('volume-unit-kitchen');",
        "const btnVolMl=document.getElementById('volume-unit-ml');",
        "const btnMassG=document.getElementById('mass-unit-g');",
        "const btnMassLbs=document.getElementById('mass-unit-lbs');",
        "const volumeSubmodes=document.getElementById('volume-submodes');",
        "const massSubmodes=document.getElementById('mass-submodes');",
        "const measureHelp=document.getElementById('measure-mode-help');",
        "const hasScaler=!!input;",
        "const base=", if (has_scaler) base_portions else "1", ";",
        "const modeKey='recipe_measure_mode:'+window.location.pathname;",
        "const volumeUnitKey='recipe_volume_unit_mode:'+window.location.pathname;",
        "const massUnitKey='recipe_mass_unit_mode:'+window.location.pathname;",
        "const savedMode=localStorage.getItem(modeKey);",
        "let mode=(savedMode==='mass'||savedMode==='volume'||savedMode==='default')?savedMode:'default';",
        "let volumeUnitMode=(localStorage.getItem(volumeUnitKey)==='ml')?'ml':'kitchen';",
        "let massUnitMode=(localStorage.getItem(massUnitKey)==='lbs')?'lbs':'g';",
        "const format=(n)=>{const r=Math.round(n*100)/100; return (Math.abs(r-Math.round(r))<1e-9)?String(Math.round(r)):String(r).replace('.',',');};",
        "const formatFraction=(n)=>{const whole=Math.floor(n+1e-9); const frac=n-whole; const choices=[[0.25,'1/4'],[1/3,'1/3'],[0.5,'1/2'],[2/3,'2/3'],[0.75,'3/4']]; let hit=''; for(const [v,label] of choices){ if(Math.abs(frac-v)<0.04){ hit=label; break; } } if(!hit) return null; if(whole<1) return hit; return `${whole} ${hit}`;};",
        "const normalize=(u)=>String(u||'').toLowerCase().normalize('NFD').replace(/[\\u0300-\\u036f]/g,'').replace(/[^a-z0-9]+/g,' ').trim();",
        "const isCountUnit=(u)=>{const n=normalize(u); return n==='unite'||n==='unites'||n==='unit'||n==='units';};",
        "const displayUnit=(u)=>isCountUnit(u)?'':String(u||'');",
        "const pluralizeMeasureUnit=(unit,qty)=>{const q=Number(qty); if(!Number.isFinite(q)) return displayUnit(unit); if(unit==='tasse') return Math.abs(q-1)<1e-9?'tasse':'tasses'; if(unit==='c. à soupe') return Math.abs(q-1)<1e-9?'c. à soupe':'c. à soupe'; if(unit==='c. à thé') return Math.abs(q-1)<1e-9?'c. à thé':'c. à thé'; if(unit==='ml') return 'ml'; if(unit==='g') return 'g'; if(unit==='onces') return Math.abs(q-1)<1e-9?'once':'onces'; return displayUnit(unit);};",
        "const pluralizeToken=(token)=>{const core=token.replace(/^[^\\p{L}]*/u,'').replace(/[^\\p{L}]*$/u,''); if(!core) return token; const n=normalize(core); if(!n||['de','du','des','et','ou','en','a','au','aux','avec','sans','pour','sur','sous'].includes(n)||/ment$/.test(n)||/[sxz]$/.test(n)) return token; let plural=core; if(/al$/.test(n)) plural=core.replace(/al$/i,'aux'); else if(/(eau|au|eu)$/.test(n)) plural=core+'x'; else plural=core+'s'; return token.replace(core, plural);};",
        "const displayName=(name,qty,unit)=>{const raw=String(name||'').trim(); if(!raw||!isCountUnit(unit)) return raw; const q=Number(qty); if(!Number.isFinite(q)||Math.abs(q-1)<1e-9) return raw; const parts=raw.split(/\\s+/); const out=[]; let skipNext=false; for(const token of parts){const tokenNorm=normalize(token); const prefixNorm=normalize(token.replace(/[’'].*/,'')); if(skipNext){out.push(token); skipNext=false; continue;} if(['de','du','des'].includes(tokenNorm)){out.push(token); skipNext=true; continue;} if(['d','l'].includes(prefixNorm)){out.push(token); continue;} out.push(pluralizeToken(token));} return out.join(' ');};",
        "const massFactor=(unit)=>{const u=normalize(unit); const m={'g':1,'gramme':1,'grammes':1,'gram':1,'grams':1,'kg':1000,'kilogramme':1000,'kilogrammes':1000,'lb':453.59237,'lbs':453.59237,'oz':28.34952,'once':28.34952,'onces':28.34952}; return Object.prototype.hasOwnProperty.call(m,u)?m[u]:NaN;};",
        "const volumeFactor=(unit)=>{const u=normalize(unit); const m={'ml':1,'millilitre':1,'millilitres':1,'l':1000,'litre':1000,'litres':1000,'t':250,'tasse':250,'tasses':250,'c a soupe':15,'cuillere a soupe':15,'cuilleres a soupe':15,'c a table':15,'c s':15,'c.s.':15,'c a the':5,'cuillere a the':5,'cuilleres a the':5,'c t':5,'c.t.':5}; return Object.prototype.hasOwnProperty.call(m,u)?m[u]:NaN;};",
        "const chooseKitchenDisplay=(volumeMl)=>{if(!Number.isFinite(volumeMl)) return {qty:NaN,unit:'c. à thé'}; const cups=volumeMl/250; const cupFractions=[0.25,1/3,0.5,2/3,0.75,1,1.25,1.5,1.75,2,2.5,3,4]; const nearCup=cupFractions.find(v=>Math.abs(cups-v)<0.045); const tbsp=volumeMl/15; if((Number.isFinite(nearCup)&&nearCup>=0.25)||tbsp>=3.75) return {qty:Number.isFinite(nearCup)?nearCup:cups,unit:'tasse'}; if(tbsp>=0.75) return {qty:tbsp,unit:'c. à soupe'}; return {qty:volumeMl/5,unit:'c. à thé'};};",
        "const formatMeasure=(qty,unit)=>{if(unit==='tasse'){const frac=formatFraction(qty); if(frac){return {qtyLabel:frac,unitLabel:qty<1?'de tasse':pluralizeMeasureUnit(unit,qty)};}} return {qtyLabel:format(qty),unitLabel:pluralizeMeasureUnit(unit,qty)};};",
        "const chooseImperialMassUnit=(massG,explicit)=>{const e=normalize(explicit); if(e==='once'||e==='onces'||e==='oz') return 'onces'; if(e==='lb'||e==='lbs') return 'lbs'; if(!Number.isFinite(massG)) return 'lbs'; return massG<453.59237?'onces':'lbs';};",
        "const setDerived=(qNode,isDerived)=>{if(qNode) qNode.classList.toggle('ingredient-approx',!!isDerived);};",
        "const updateIngredientForMode=(li,ratio)=>{const qNode=li.querySelector('.ingredient-qte'); const uNode=li.querySelector('.ingredient-uni'); const nNode=li.querySelector('.ingredient-nom'); if(!qNode) return false; const defaultQty=parseFloat(li.dataset.defaultQty||''); const defaultUnit=li.dataset.defaultUnit||''; const baseName=li.dataset.baseIngredientName||li.dataset.ingredientName||''; const baseMass=parseFloat(li.dataset.baseMassG||''); const baseVolume=parseFloat(li.dataset.baseVolumeMl||''); const explicitUnitMass=li.dataset.explicitUnitMass||'g'; const massSource=li.dataset.massSource||'none'; const volumeSource=li.dataset.volumeSource||'none'; let qty=NaN; let unit=defaultUnit; let derived=false; if(mode==='mass'&&Number.isFinite(baseMass)){unit=(massUnitMode==='lbs')?chooseImperialMassUnit(baseMass*ratio,explicitUnitMass):'g'; const f=massFactor(unit); qty=Number.isFinite(f)&&f>0?(baseMass*ratio)/f:baseMass*ratio; derived=massSource==='derived';} else if(mode==='volume'&&Number.isFinite(baseVolume)){if(volumeUnitMode==='ml'){unit='ml'; qty=baseVolume*ratio;} else {const display=chooseKitchenDisplay(baseVolume*ratio); unit=display.unit; qty=display.qty;} derived=volumeSource==='derived';} else if(Number.isFinite(defaultQty)){qty=defaultQty*ratio; unit=defaultUnit; derived=false;} else {li.removeAttribute('data-live-updated'); return false;} const measure=formatMeasure(qty,unit); qNode.textContent=measure.qtyLabel; if(uNode){uNode.textContent=measure.unitLabel;} if(nNode){nNode.textContent=displayName(baseName, qty, unit);} li.setAttribute('data-ingredient-unit', measure.unitLabel); li.setAttribute('data-ingredient-name', displayName(baseName, qty, unit)); li.setAttribute('data-live-updated','1'); setDerived(qNode,derived); return true;};",
        "const applyModeButtons=()=>{if(btnDefault) btnDefault.classList.toggle('active', mode==='default'); if(btnVolume) btnVolume.classList.toggle('active', mode==='volume'); if(btnMass) btnMass.classList.toggle('active', mode==='mass'); if(btnVolKitchen) btnVolKitchen.classList.toggle('active', volumeUnitMode==='kitchen'); if(btnVolMl) btnVolMl.classList.toggle('active', volumeUnitMode==='ml'); if(btnMassG) btnMassG.classList.toggle('active', massUnitMode==='g'); if(btnMassLbs) btnMassLbs.classList.toggle('active', massUnitMode==='lbs'); const isVolume=mode==='volume'; const isMass=mode==='mass'; if(volumeSubmodes){volumeSubmodes.classList.toggle('recipe-measure-group-hidden', !isVolume); volumeSubmodes.querySelectorAll('button').forEach((btn)=>{btn.disabled=!isVolume;});} if(massSubmodes){massSubmodes.classList.toggle('recipe-measure-group-hidden', !isMass); massSubmodes.querySelectorAll('button').forEach((btn)=>{btn.disabled=!isMass;});} if(measureHelp){measureHelp.textContent=isVolume?'Les fractions de tasse sont privilégiées pour les grands volumes.':(isMass?'Choisissez entre grammes et livres selon votre préférence.':'Affiche les quantités telles qu’écrites dans la recette.');}};",
        "const update=()=>{",
        "const current=hasScaler?parseFloat(input.value):base;",
        "const ratio=(hasScaler&&Number.isFinite(current)&&current>0)?(current/base):1;",
        "document.querySelectorAll('.recipe-ingredient').forEach((li)=>{if(!updateIngredientForMode(li,ratio)){li.removeAttribute('data-live-updated');}});",
        "document.querySelectorAll('.ingredient-qte[data-base]').forEach((el)=>{",
        "if(el.closest('.recipe-ingredient[data-live-updated=\"1\"]')) return;",
        "const b=parseFloat(el.getAttribute('data-base'));",
        "if(Number.isFinite(b)){el.textContent=format(b*ratio);} });",
        "document.querySelectorAll('.recipe-nutrition-total[data-base-total]').forEach((el)=>{",
        "const baseTotal=parseFloat(el.getAttribute('data-base-total'));",
        "const unit=el.getAttribute('data-unit')||'';",
        "const dec=parseInt(el.getAttribute('data-decimals')||'1',10);",
        "if(Number.isFinite(baseTotal)){",
        "const v=baseTotal*ratio;",
        "let t=(Math.round(v*Math.pow(10,dec))/Math.pow(10,dec)).toFixed(Math.max(0,dec));",
        "if(dec>0){t=t.replace(/\\.0+$/,'').replace(/(\\.[0-9]*?)0+$/,'$1');}",
        "t=t.replace('.',',');",
        "el.textContent=t+' '+unit;",
        "}",
        "});",
        "document.querySelectorAll('.recipe-nutrition-per-portion[data-base-total]').forEach((el)=>{",
        "const baseTotal=parseFloat(el.getAttribute('data-base-total'));",
        "const unit=el.getAttribute('data-unit')||'';",
        "const dec=parseInt(el.getAttribute('data-decimals')||'1',10);",
        "if(Number.isFinite(baseTotal)){",
        "const v=baseTotal/base;",
        "let t=(Math.round(v*Math.pow(10,dec))/Math.pow(10,dec)).toFixed(Math.max(0,dec));",
        "if(dec>0){t=t.replace(/\\.0+$/,'').replace(/(\\.[0-9]*?)0+$/,'$1');}",
        "t=t.replace('.',',');",
        "el.textContent=t+' '+unit;",
        "}",
        "});",
        "};",
        "if(input){input.addEventListener('input', update);} ",
        "if(reset&&input){reset.addEventListener('click', ()=>{input.value=String(base); update();});}",
        "if(btnDefault){btnDefault.addEventListener('click', ()=>{mode='default'; localStorage.setItem(modeKey,'default'); applyModeButtons(); update();});}",
        "if(btnVolume){btnVolume.addEventListener('click', ()=>{mode='volume'; localStorage.setItem(modeKey,'volume'); applyModeButtons(); update();});}",
        "if(btnMass){btnMass.addEventListener('click', ()=>{mode='mass'; localStorage.setItem(modeKey,'mass'); applyModeButtons(); update();});}",
        "if(btnVolKitchen){btnVolKitchen.addEventListener('click', ()=>{volumeUnitMode='kitchen'; localStorage.setItem(volumeUnitKey,'kitchen'); applyModeButtons(); update();});}",
        "if(btnVolMl){btnVolMl.addEventListener('click', ()=>{volumeUnitMode='ml'; localStorage.setItem(volumeUnitKey,'ml'); applyModeButtons(); update();});}",
        "if(btnMassG){btnMassG.addEventListener('click', ()=>{massUnitMode='g'; localStorage.setItem(massUnitKey,'g'); applyModeButtons(); update();});}",
        "if(btnMassLbs){btnMassLbs.addEventListener('click', ()=>{massUnitMode='lbs'; localStorage.setItem(massUnitKey,'lbs'); applyModeButtons(); update();});}",
        "applyModeButtons();",
        "update();",
        "})();",
        "</script>"
      ),
      "```",
      ""
    )
  }

  if (has_step_images) {
    lines <- c(
      lines,
      "```{=html}",
      "<script>document.addEventListener('DOMContentLoaded', function(){ if (window.GLightbox) { window.GLightbox({ selector: '.glightbox', touchNavigation: true, loop: true }); } });</script>",
      "```",
      ""
    )
  }

  lines <- c(
    lines,
    "```{=html}",
    "<div id=\"recipe-timer-dock\" class=\"recipe-timer-dock d-none\"><strong>Minuteur</strong> <span id=\"recipe-timer-label\"></span> <span id=\"recipe-timer-remaining\"></span> <button id=\"recipe-timer-stop\" class=\"btn btn-sm btn-outline-light\" type=\"button\">Arr\u00eater</button></div>",
    "<script>(function(){",
    "let timer=null, target=0, audioCtx=null, alarmInterval=null, alarmTimeout=null;",
    "const pad=(n)=>String(n).padStart(2,'0');",
    "const fmt=(s)=>{const h=Math.floor(s/3600), m=Math.floor((s%3600)/60), sec=s%60; return h>0?`${h}:${pad(m)}:${pad(sec)}`:`${m}:${pad(sec)}`;};",
    "const ensureAudio=async()=>{const Ctx=window.AudioContext||window.webkitAudioContext; if(!Ctx) return null; if(!audioCtx) audioCtx=new Ctx(); if(audioCtx.state==='suspended'){try{await audioCtx.resume();}catch(e){}} return audioCtx;};",
    "const stopAlarm=()=>{if(alarmInterval){clearInterval(alarmInterval); alarmInterval=null;} if(alarmTimeout){clearTimeout(alarmTimeout); alarmTimeout=null;}};",
    "const playBeep=(ctx,freq)=>{const t=ctx.currentTime+0.01; const osc=ctx.createOscillator(); const gain=ctx.createGain(); osc.type='sawtooth'; osc.frequency.value=freq; gain.gain.setValueAtTime(0.0001,t); gain.gain.exponentialRampToValueAtTime(0.42,t+0.015); gain.gain.exponentialRampToValueAtTime(0.0001,t+0.19); osc.connect(gain); gain.connect(ctx.destination); osc.start(t); osc.stop(t+0.2);};",
    "const startAlarm=async()=>{const ctx=await ensureAudio(); if(!ctx) return; stopAlarm(); const notes=[1760,1480,2093,1760,2349,1760]; let i=0; const ring=()=>{playBeep(ctx,notes[i%notes.length]); i+=1;}; ring(); alarmInterval=setInterval(ring,250); alarmTimeout=setTimeout(stopAlarm,15000);};",
    "const dock=document.getElementById('recipe-timer-dock'); const rem=document.getElementById('recipe-timer-remaining'); const lab=document.getElementById('recipe-timer-label'); const stop=document.getElementById('recipe-timer-stop');",
    "const hide=()=>{if(timer){clearInterval(timer); timer=null;} stopAlarm(); if(dock) dock.classList.add('d-none');};",
    "document.addEventListener('click',async(e)=>{const b=e.target.closest('.recipe-timer-btn'); if(!b) return; const secs=parseInt(b.dataset.seconds||'0',10); if(!secs||secs<1) return; await ensureAudio(); stopAlarm(); if(timer) clearInterval(timer); target=Date.now()+secs*1000; if(lab) lab.textContent=b.textContent.replace('\u23f1\ufe0f','').trim(); if(dock) dock.classList.remove('d-none'); timer=setInterval(()=>{const left=Math.max(0, Math.round((target-Date.now())/1000)); if(rem) rem.textContent=fmt(left); if(left<=0){if(timer){clearInterval(timer); timer=null;} if(rem) rem.textContent='Termin\u00e9!'; if(dock) dock.classList.remove('d-none'); startAlarm();}},250);});",
    "if(stop) stop.addEventListener('click', hide);",
    "})();</script>",
    "```",
    ""
  )

  lines <- c(
    lines,
    "```{=html}",
    "<script>(function(){",
    "const pageKey='recipe_check_state:'+window.location.pathname;",
    "const save=()=>{const state={}; document.querySelectorAll('.recipe-check').forEach((cb,i)=>{state[i]=cb.checked?1:0;}); localStorage.setItem(pageKey, JSON.stringify(state));};",
    "const load=()=>{try{const s=JSON.parse(localStorage.getItem(pageKey)||'{}'); document.querySelectorAll('.recipe-check').forEach((cb,i)=>{cb.checked=!!s[i];});}catch(e){}};",
    "const reset=()=>{document.querySelectorAll('.recipe-check').forEach(cb=>cb.checked=false); save();};",
    "const textFromLi=(li)=>{const label=li.querySelector('.ingredient-label'); return label?label.innerText.replace(/\\s+/g,' ').trim():li.innerText.replace(/\\s+/g,' ').trim();};",
    "const copyGroceries=async()=>{const items=[...document.querySelectorAll('.recipe-grocery-list .recipe-ingredient')].filter(li=>!(li.querySelector('.recipe-ingredient-check')||{}).checked).map(textFromLi).filter(Boolean); const txt=items.length?items.map(x=>'- '+x).join('\\n'):'(Aucun ingr\u00e9dient restant)'; const fb=document.getElementById('grocery-copy-feedback'); try{if(navigator.clipboard&&navigator.clipboard.writeText){await navigator.clipboard.writeText(txt);} else {window.prompt('Copie la liste:', txt);} if(fb) fb.textContent='Liste copi\u00e9e.';}catch(e){window.prompt('Copie la liste:', txt); if(fb) fb.textContent='Liste pr\u00eate \u00e0 copier.';}};",
    "document.addEventListener('DOMContentLoaded',()=>{load(); document.querySelectorAll('.recipe-check').forEach(cb=>cb.addEventListener('change',save)); const c=document.getElementById('copy-grocery-list'); if(c) c.addEventListener('click',copyGroceries); const r=document.getElementById('reset-recipe-checks'); if(r) r.addEventListener('click',reset);});",
    "})();</script>",
    "```",
    ""
  )

  # ---- Comments / Notes ----
  comments_norm <- normalize_comments(recipe$commentaires)

  if (length(comments_norm) > 0) {
    lines <- c(lines, "## Notes", "")

    # Summary: stars + counts
    avg <- mean(vapply(comments_norm, function(x) if (!is.null(x$evaluation)) x$evaluation else NA_real_, numeric(1)), na.rm = TRUE)
    if (is.nan(avg)) avg <- NA_real_
    stars <- if (is.na(avg)) "" else stars_string(avg)

    n_total <- length(comments_norm)
    n_alex <- count_by_author(comments_norm, "Alexandre Parent")
    n_elodie <- count_by_author(comments_norm, "\u00c9lodie Bourgeois")

    summary_parts <- c()
    if (stars != "") summary_parts <- c(summary_parts, paste0(stars, " (moyenne ", format(avg, digits = 2), "/5)"))
    summary_parts <- c(summary_parts, paste0(n_total, " commentaire(s)"))
    summary_parts <- c(summary_parts, paste0("Alexandre Parent: ", n_alex))
    summary_parts <- c(summary_parts, paste0("\u00c9lodie Bourgeois: ", n_elodie))

    lines <- c(lines, paste0("_", paste(summary_parts, collapse = " \u00b7 "), "_"), "")

    # Each comment
    for (cmt in comments_norm) {
      lines <- c(lines, paste0("- ", format_comment_line(cmt)))
    }
  }

  writeLines(enc2utf8(lines), qmd_path)
  invisible(qmd_path)
}

#' Regenerate all recipe QMDs from YAMLs
#'
#' @param recipes_dir Directory containing YAML recipes.
#' @param pattern File matching pattern for YAML recipes.
#' @return Vector of processed YAML file paths invisibly.
#' @export
regenerate_recipe_qmds <- function(recipes_dir = "recettes", pattern = "\\.ya?ml$") {
  yaml_files <- list.files(recipes_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  yaml_files <- yaml_files[!grepl("template\\.ya?ml$", yaml_files, ignore.case = TRUE)]

  for (y in yaml_files) {
    yaml_recipe_to_qmd(y)
  }

  invisible(yaml_files)
}
