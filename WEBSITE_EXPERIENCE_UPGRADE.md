# Website Experience Upgrade

This document explains the major UX improvements implemented for the recipe website, why they matter, and where they are implemented.

## Goals

- Make recipe pages easier and faster to use while cooking.
- Improve readability and visual quality across desktop and mobile.
- Improve the authoring experience when adding/editing recipes.

## 1) Reading Experience Improvements

### What changed

- Added a quick action toolbar on each recipe page:
  - Print button
  - Copy-link button
- Added a quick-facts block on each recipe:
  - Source
  - Portions
  - Freezer-friendly (se congèle)
  - Time summary (prep/cook/chill when available)
- Added ingredient scaling by portions:
  - Numeric ingredient quantities are now rendered with metadata.
  - Users can adjust serving count and ingredient quantities update live.
- Added support to display preparation-step images (`image_guid`) directly under the relevant step.

### Why it helps

- Print and share are high-frequency actions for normal users.
- Quick facts reduce scanning effort before starting a recipe.
- Live scaling removes manual arithmetic errors.
- Step images improve confidence and reduce mistakes during cooking.

### Implemented in

- `R/yaml_to_qmd.R`
  - richer generated recipe structure
  - ingredient metadata rendering
  - servings scaling script
  - step-image rendering
- Regenerated recipe pages (`recettes/**/*.qmd`) from YAML

## 2) Writing Experience Improvements

### What changed

- Added draft tools to the add-recipe page:
  - Auto-save draft in local storage while typing
  - Manual "Enregistrer le brouillon"
  - Manual "Effacer le brouillon"
  - Auto-load draft when opening the form (for new recipe mode)
- Draft tools are hidden automatically in edit mode (`?yaml=...`) to avoid conflicts.

### Why it helps

- Long recipe forms are easy to lose due to refresh/navigation.
- Auto-save drastically reduces frustration and accidental data loss.
- Manual controls give confidence and transparency.

### Implemented in

- `recettes/ajouter_recette/index.qmd`
  - draft controls UI
  - draft save/load/clear logic
  - status messaging

## 3) Visual Design Overhaul

### What changed

- Added a cohesive visual system using CSS tokens:
  - spacing, borders, shadows, accent color
  - improved typographic hierarchy
- Improved listing cards:
  - rounded cards
  - subtle gradients
  - hover lift effect
- Improved recipe page component styling:
  - toolbar
  - facts grid
  - servings control
  - ingredient cards
  - step images
- Added homepage hero section and stronger category page intros.

### Why it helps

- Better visual hierarchy improves scanability.
- Cleaner cards and spacing make content feel more trustworthy and modern.
- The site feels more intentional and polished while staying recipe-first.

### Implemented in

- `styles.css`
- `index.qmd`
- `recettes/repas/index.qmd`
- `recettes/desserts/index.qmd`
- `recettes/accompagnements/index.qmd`

## Notes on Compatibility

- Existing YAMLs remain compatible.
- Ingredient scaling only affects numeric quantities; non-numeric quantities remain unchanged.
- Step images are optional (`image_guid` nullable).

## How to Regenerate Pages

```bash
Rscript --vanilla R/generate_qmds.R
```

Then preview:

```bash
quarto preview
```
