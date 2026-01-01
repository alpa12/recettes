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

created <- 0
modified <- 0
unchanged <- 0

for (yaml in yaml_files) {
  qmd <- sub("\\.ya?ml$", ".qmd", yaml)

  old_content <- NULL
  existed <- file.exists(qmd)

  if (existed) {
    old_content <- readLines(qmd, warn = FALSE)
  }

  yaml_recipe_to_qmd(
    yaml_path = yaml,
    qmd_path  = qmd
  )

  new_content <- readLines(qmd, warn = FALSE)

  if (!existed) {
    created <- created + 1
    state <- "Créé   "
  } else if (identical(old_content, new_content)) {
    unchanged <- unchanged + 1
    state <- "Inchangé"
  } else {
    modified <- modified + 1
    state <- "Modifié"
  }

  if (state != "Inchangé") {
    cli::cli_alert(glue::glue("{state} : {fs::path_rel(qmd, start = recettes_dir)}"))
  }
}

cli::cli_h2("Résumé")
cli::cli_bullets(c(
  glue::glue("YAML traités : {length(yaml_files)}"),
  glue::glue("QMD créés : {created}"),
  glue::glue("QMD modifiés : {modified}"),
  glue::glue("QMD inchangés : {unchanged}")
))
