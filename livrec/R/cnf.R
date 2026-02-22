#' Build CNF-based nutrition file.
#'
#' GitHub Actions entrypoint used to generate `data/nutrition/foods_cnf.csv`
#' from downloaded CNF CSV files and the local mapping file.
#'
#' @param cnf_dir Directory containing CNF raw CSV files.
#' @param map_path Path to mapping file linking local food ids to CNF ids.
#' @param base_path Path to base foods CSV file.
#' @param out_path Output path for generated CNF foods CSV.
#' @return The generated data frame invisibly.
#' @export
gha_build_cnf_foods <- function(
  cnf_dir = Sys.getenv("CNF_DIR", unset = "data/nutrition/cnf"),
  map_path = Sys.getenv("CNF_MAP_PATH", unset = "data/nutrition/cnf_food_map.csv"),
  base_path = Sys.getenv("BASE_FOODS_PATH", unset = "data/nutrition/foods.csv"),
  out_path = Sys.getenv("CNF_OUT_PATH", unset = "data/nutrition/foods_cnf.csv")
) {
  options(stringsAsFactors = FALSE)

  stop_if_missing <- function(path, label) {
    if (!file.exists(path)) stop(sprintf("%s introuvable: %s", label, path), call. = FALSE)
  }

  stop_if_missing(base_path, "Fichier de base")
  stop_if_missing(map_path, "Fichier de mapping CNF")
  stop_if_missing(file.path(cnf_dir, "FOOD NAME.csv"), "CNF FOOD NAME.csv")
  stop_if_missing(file.path(cnf_dir, "NUTRIENT AMOUNT.csv"), "CNF NUTRIENT AMOUNT.csv")

  foods_base <- read.csv(base_path, check.names = FALSE)
  map_tbl <- read.csv(map_path, check.names = FALSE)

  food_name <- read.csv(
    file.path(cnf_dir, "FOOD NAME.csv"),
    check.names = FALSE,
    fileEncoding = "latin1"
  )
  nut_amt <- read.csv(
    file.path(cnf_dir, "NUTRIENT AMOUNT.csv"),
    check.names = FALSE,
    fileEncoding = "latin1"
  )

  nutrient_ids <- c(
    energy_kcal = 208,
    protein_g = 203,
    fat_g = 204,
    saturated_fat_g = 606,
    carbs_g = 205,
    sugars_g = 269,
    fiber_g = 291,
    sodium_mg = 307,
    potassium_mg = 306,
    calcium_mg = 301,
    iron_mg = 303,
    magnesium_mg = 304,
    zinc_mg = 309,
    selenium_ug = 317,
    vitamin_c_mg = 401,
    vitamin_b12_ug = 418,
    vitamin_a_rae_ug = 814,
    vitamin_d_ug = 339
  )

  map_tbl$food_id <- trimws(map_tbl$food_id)
  map_tbl$cnf_food_id <- suppressWarnings(as.integer(map_tbl$cnf_food_id))
  map_tbl <- map_tbl[!is.na(map_tbl$cnf_food_id) & nzchar(map_tbl$food_id), , drop = FALSE]
  map_tbl <- unique(map_tbl[c("food_id", "cnf_food_id", "note")])

  out_rows <- list()
  n_missing_food <- 0L
  n_with_nutrients <- 0L

  for (i in seq_len(nrow(map_tbl))) {
    fid <- map_tbl$food_id[i]
    cnf_id <- map_tbl$cnf_food_id[i]

    base_row <- foods_base[foods_base$food_id == fid, , drop = FALSE]
    if (nrow(base_row) < 1) next
    base_row <- base_row[1, , drop = FALSE]

    cnf_row <- food_name[food_name$FoodID == cnf_id, , drop = FALSE]
    if (nrow(cnf_row) < 1) {
      n_missing_food <- n_missing_food + 1L
      next
    }

    amt <- nut_amt[nut_amt$FoodID == cnf_id, c("NutrientID", "NutrientValue"), drop = FALSE]
    if (nrow(amt) > 0) {
      amt <- amt[!duplicated(amt$NutrientID), , drop = FALSE]
    }

    row <- base_row
    got_one <- FALSE
    for (k in names(nutrient_ids)) {
      nid <- nutrient_ids[[k]]
      v <- amt$NutrientValue[amt$NutrientID == nid]
      if (length(v) > 0 && is.finite(v[1])) {
        row[[k]] <- as.numeric(v[1])
        got_one <- TRUE
      }
    }

    if (got_one) n_with_nutrients <- n_with_nutrients + 1L

    row$source <- "cnf_2015"
    row$source_ref <- sprintf(
      "CNF FoodID %s | %s",
      cnf_id,
      as.character(cnf_row$FoodDescription[1] %||% "")
    )
    out_rows[[length(out_rows) + 1L]] <- row
  }

  if (length(out_rows) == 0) {
    stop("Aucune ligne CNF generee. Verifie le mapping et les fichiers CNF.", call. = FALSE)
  }

  foods_cnf <- do.call(rbind, out_rows)
  foods_cnf <- foods_cnf[!duplicated(foods_cnf$food_id), , drop = FALSE]

  if (!("source" %in% names(foods_cnf))) foods_cnf$source <- "cnf_2015"
  if (!("source_ref" %in% names(foods_cnf))) foods_cnf$source_ref <- ""

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(foods_cnf, out_path, row.names = FALSE, na = "")

  cat(sprintf("CNF rows written: %d\n", nrow(foods_cnf)))
  cat(sprintf("Mapped foods with nutrient data: %d\n", n_with_nutrients))
  if (n_missing_food > 0) {
    cat(sprintf("Mapped foods missing in FOOD NAME.csv: %d\n", n_missing_food))
  }

  invisible(foods_cnf)
}
