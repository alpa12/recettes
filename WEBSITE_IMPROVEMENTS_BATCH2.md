# Website Improvements Batch 2

This file documents the second wave of improvements made to the recipe website.  
Each area was implemented in a dedicated commit for easier review and rollback.

## 1) Search Relevance

### What
- Added keyword support in schema: `mots_cles`.
- Added `keywords:` front-matter generation in recipe QMD files from:
  - `categories`
  - `mots_cles`

### Why
- Improves search discoverability and relevance for users looking by topic or intent.

### Files
- `recettes/template.yaml`
- `R/yaml_to_qmd.R`

### Commit
- `acf319d` - Add keyword metadata to improve recipe search relevance

## 2) Reading Mode (Cooking Mode)

### What
- Added toolbar toggle: `🍳 Mode cuisson`.
- Adds a focus-friendly reading state:
  - larger text and spacing
  - hides side TOC clutter
- Uses Wake Lock (when supported) to reduce screen sleep while cooking.

### Why
- Better in-kitchen readability and less friction while following steps.

### Files
- `R/yaml_to_qmd.R`
- `styles.css`

### Commit
- `fbb1015` - Add cooking reading mode toggle with focus-friendly layout

## 3) Smart Timers

### What
- Durations are parsed from step text (`min`, `h`, `heure(s)`).
- One-click timer buttons are added under steps.
- Added floating timer dock with countdown and stop action.

### Why
- Reduces context switching and manual timer setup while cooking.

### Files
- `R/yaml_to_qmd.R`
- `styles.css`

### Commit
- `ad77e47` - Add one-click smart timers parsed from preparation steps

## 4) Add-Recipe Form Quality Checks

### What
- Added pre-submit validation in form flow:
  - missing section names
  - empty step descriptions
  - duplicate ingredient names in a section
  - negative ingredient quantities
- Submission is blocked with clear feedback until corrected.

### Why
- Prevents low-quality or broken recipe data from entering the repo.

### Files
- `recettes/ajouter_recette/index.qmd`

### Commit
- `0f0b827` - Add form quality checks before recipe submission

## 5) Lightweight Metadata (Difficulty / Cost / Allergens)

### What
- Added optional schema fields:
  - `difficulte`
  - `cout`
  - `allergenes`
- Added badge rendering on recipe pages.
- Included metadata values into search keywords.

### Why
- Helps readers quickly evaluate recipe suitability.

### Files
- `recettes/template.yaml`
- `R/yaml_to_qmd.R`
- `styles.css`

### Commit
- `1916bb7` - Add optional recipe metadata badges for difficulty cost and allergens

## 6) Listing Image Thumbnails

### What
- Added thumbnail generation logic in QMD generation script.
- If `magick` is available:
  - builds compressed thumbnails in `images/thumbs/`
  - uses thumbnails for recipe front image when present
- Fallback: use original image if no thumbnail exists.

### Why
- Reduces page weight and improves listing performance.

### Files
- `livrec/R/generate_qmds.R`
- `livrec/R/yaml_to_qmd.R`

### Commit
- `83940ca` - Generate lightweight thumbnails for recipe listing images

## 7) YAML Consistency Tooling + CI

### What
- Added validation script to enforce recipe YAML quality.
- Added CI workflow to run validation on PR/push.

### Why
- Catches schema/data issues before merge.

### Files
- `livrec/R/validate.R`
- `.github/workflows/validate_recipes.yaml`

### Commit
- `982031a` - Add YAML consistency validator and CI workflow

## 8) Accessibility Pass

### What
- Added site-wide keyboard skip link to main content.
- Added stronger focus-visible styles for interactive controls.
- Hooked skip-link include in Quarto config.

### Why
- Better keyboard navigation and accessibility compliance.

### Files
- `_quarto.yml`
- `includes/skip-link.html`
- `styles.css`

### Commit
- `91c44da` - Improve accessibility with skip link and stronger focus states

## Additional related adjustments in this period

- Recipe facts card layout tuning, source normalization, and dark-mode card border fix were also completed in separate commits before/alongside this batch.
