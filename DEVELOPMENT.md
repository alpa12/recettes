# Development Notes

## R environment policy (`renv`)

The repository uses `renv` with `snapshot.type = explicit`.

Rules:
- Declare top-level dependencies in `DESCRIPTION` (project manifest).
- Keep package runtime dependencies in `livrec/DESCRIPTION`.
- Run `renv::snapshot()` after dependency changes.
- Validate with `renv::status()` before opening a PR.

## Package workflow

All recipe automation logic should live in `livrec/`.
Workflows and scripts should call exported `livrec::gha_*` entrypoints.
