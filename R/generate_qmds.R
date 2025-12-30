source("R/yaml_to_qmd.R")

recettes_dir <- here::here("recettes")

cli::cli_h1("Génération des fichiers QMD")


cli::cli_h2("Lister les YAML existants")
yaml_files <- list.files(
  path = recettes_dir,
  pattern = "\\.ya?ml$",
  recursive = TRUE,
  full.names = TRUE
)

# Retirer template.yaml de la liste, ce n'est pas une vraie recette
yaml_files <- yaml_files[
  basename(yaml_files) != "template.yaml"
]


cli::cli_h2("Supprimer les QMD existants correspondant aux YAML")
deleted_count <- 0

for (yaml in yaml_files) {
  qmd <- sub("\\.ya?ml$", ".qmd", yaml)

  if (file.exists(qmd)) {
    file.remove(qmd)
    deleted_count <- deleted_count + 1
  }
}


cli::cli_h2("Regénérer les fichiers QMD")
created_count <- 0

for (yaml in yaml_files) {
  qmd <- sub("\\.ya?ml$", ".qmd", yaml)

  yaml_recipe_to_qmd(
    yaml_path = yaml,
    qmd_path  = qmd
  )

  created_count <- created_count + 1
}


cli::cli_h2("Résumé")
cli::cli_bullets(c(
  glue::glue("YAML trouvés: {length(yaml_files)}"),
  glue::glue("QMD supprimés: {deleted_count}"),
  glue::glue("QMD créés: {created_count}")
))
