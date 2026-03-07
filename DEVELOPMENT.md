# Development Notes

## Must Keep This File Updated

If you change behavior, workflow, architecture, branch policy, submission flow, or important conventions, update this file in the same PR/commit series.
Do not leave this document stale.

## Branch Policy

- Primary long-lived branch: `main`.
- Do not use `dev` as base or target branch.
- Feature/fix branches are short-lived and merge into `main`.
- Recipe submission flows (manual form + URL import) must create PRs targeting `main`.

## R Environment Policy (`renv`)

The repository uses `renv` with `snapshot.type = explicit`.

Rules:
- Declare top-level dependencies in root `DESCRIPTION`.
- Keep package runtime dependencies in `livrec/DESCRIPTION`.
- Run `renv::snapshot()` after dependency changes.
- Validate with `renv::status()` before opening a PR.

## Repo Structure

- `livrec/`: reusable R package for generation, import, validation, nutrition, helpers.
- `recettes/**/*.yaml`: source recipe data.
- `recettes/**/*.qmd`: generated recipe pages.
- `recettes/ajouter_recette/index.qmd`: client-side app for create/edit/import URL.
- `styles.css`: shared styling.
- `.github/workflows/`: CI + automation workflows.

## Submission Flow (Important)

- GitHub token and site password are injected from environment variables at render-time in `recettes/ajouter_recette/index.qmd`:
  - `GITHUB_PAT`
  - `MOT_DE_PASSE`
- Client JS then uses:
  - `window.GITHUB_PAT`
  - `window.RECIPE_PASSWORD`
- No GitHub token should be stored in tracked JS files.

If deployed environment shows `Bad credentials`, verify `GITHUB_PAT` is set correctly in deployment environment variables.

## Generation and Validation

- After changing recipe rendering/generation logic, regenerate recipe QMD pages.
- After significant edits, run:
  - `quarto render`
- Treat successful render as required validation before finalizing work.

## Recipe Quantity Conventions

- Every ingredient entry in recipe YAMLs must define default `qte` and `uni`.
- Ingredient names in YAML should stay in a neutral singular form when practical; site rendering handles simple count-based wording improvements for display.
- Optional alternate forms stay in `qte_masse`/`uni_masse` and `qte_volume`/`uni_volume`.
- The recipe UI supports three quantity display modes:
  - default (`qte` + `uni`)
  - mass
  - volume
- Converted quantities inferred from the opposite measurement type should remain visually distinct.
- For count-based ingredients, prefer `uni: unité` with a neutral singular ingredient name when no more specific default unit is needed.
- Import prompts and import post-processing must preserve these conventions so generated YAMLs are valid without manual cleanup.

## Commit Hygiene

- Prefer small, focused commits (clean history).
- Split unrelated changes into separate commits.
- Avoid bundling refactors and behavior changes together unless necessary.

## Guidance for Future LLM/Coding Agents

- Read this file first, then inspect relevant files before editing.
- Do not introduce secrets in tracked files.
- Preserve user-facing French labels and existing UX conventions unless asked to change them.
- When changing a generated output format, also update its generator source.
- If you change conventions/processes, update this file immediately.

## Package Workflow

All recipe automation logic should live in `livrec/`.
Workflows and scripts should call exported `livrec::gha_*` entrypoints.
