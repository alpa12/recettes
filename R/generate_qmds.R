source("R/yaml_to_qmd.R")

recettes_dir <- here::here("recettes")

cli::cli_h1("Génération des fichiers QMD")

yaml_files <- list.files(
  path = recettes_dir,
  pattern = "\\.ya?ml$",
  recursive = TRUE,
  full.names = TRUE
)

yaml_files <- yaml_files[
  basename(yaml_files) != "template.yaml"
]

cli::cli_alert_info(glue::glue("Recettes YAML détectées : {length(yaml_files)}"))

overwritten <- 0
created <- 0

for (yaml in yaml_files) {
  qmd <- sub("\\.ya?ml$", ".qmd", yaml)

  if (file.exists(qmd)) {
    state <- "Écrasé"
    overwritten <- overwritten + 1
  } else {
    state <- "Créé  "
    created <- created + 1
  }

  cli::cli_alert(glue::glue("{state} : {fs::path_rel(qmd, start = recettes_dir)}"))

  yaml_recipe_to_qmd(
    yaml_path = yaml,
    qmd_path  = qmd
  )
}

cli::cli_h2("Résumé")
cli::cli_bullets(c(
  glue::glue("YAML traités : {length(yaml_files)}"),
  glue::glue("QMD écrasés : {overwritten}"),
  glue::glue("QMD créés : {created}")
))
