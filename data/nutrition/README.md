# Nutrition Data Sources

This project supports two nutrition data layers:

1. `foods_cnf.csv` (optional, highest priority): verified Canadian Nutrient File (CNF) values.
2. `foods.csv` (fallback): local estimates used only when CNF data is missing.

## CNF-first behavior

If `data/nutrition/foods_cnf.csv` exists, rows in that file override `foods.csv` by `food_id`.

## Automatic build in CI

The GitHub Actions workflows download CNF and rebuild `foods_cnf.csv` automatically using:

- `livrec::gha_build_cnf_foods()`
- `data/nutrition/cnf_food_map.csv` (mapping from local `food_id` to CNF `FoodID`)

This means CNF raw files do not need to be committed to the repository.

## How to add verified CNF data

1. Download the official CNF files from Health Canada:
   - https://www.canada.ca/en/health-canada/services/food-nutrition/healthy-eating/nutrient-data/canadian-nutrient-file-2015-download-files.html
   - CNF search tool: https://food-nutrition.canada.ca/cnf-fce/index-eng.jsp
2. Build `data/nutrition/foods_cnf.csv` with the same columns as `foods_cnf.template.csv`.
3. Set `source=cnf_2015` and keep a precise `source_ref` per item (CNF page/table reference).

## Required columns

Use the exact header from `foods_cnf.template.csv`.

## Notes

- Values are expected per 100 g edible portion.
- Keep `food_id` stable; aliases in `aliases.csv` should map to these IDs.
- Any ingredient with no CNF row will continue using fallback data from `foods.csv`.
