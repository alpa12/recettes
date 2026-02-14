source("R/yaml_to_qmd.R")

recettes_dir <- here::here("recettes")
images_dir <- here::here("images")
thumbs_dir <- fs::path(images_dir, "thumbs")

extract_image_guids <- function(recipe) {
  out <- character()
  if (!is.null(recipe$image_guid) && nzchar(as.character(recipe$image_guid))) {
    out <- c(out, as.character(recipe$image_guid))
  }
  prep <- recipe$preparation
  if (is.list(prep)) {
    for (section in prep) {
      steps <- section$etapes
      if (!is.list(steps)) next
      for (step in steps) {
        if (!is.null(step$image_guid) && nzchar(as.character(step$image_guid))) {
          out <- c(out, as.character(step$image_guid))
        }
      }
    }
  }
  unique(out)
}

generate_thumbnails <- function(yaml_files) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    cli::cli_alert_info("Package {.code magick} non installé: miniatures non générées.")
    return(invisible(0))
  }
  fs::dir_create(thumbs_dir, recurse = TRUE)
  made <- 0L
  seen <- character()

  for (yaml in yaml_files) {
    recipe <- yaml::read_yaml(yaml)
    guids <- extract_image_guids(recipe)
    for (g in guids) {
      if (g %in% seen) next
      seen <- c(seen, g)
      src <- fs::path(images_dir, paste0(g, ".jpg"))
      dst <- fs::path(thumbs_dir, paste0(g, ".jpg"))
      if (!file.exists(src)) next
      if (file.exists(dst) && file.info(dst)$mtime >= file.info(src)$mtime) next
      img <- magick::image_read(src)
      img <- magick::image_resize(img, "1200x1200>")
      img <- magick::image_strip(img)
      magick::image_write(img, path = dst, format = "jpeg", quality = 72)
      made <- made + 1L
    }
  }

  cli::cli_alert_info(glue::glue("Miniatures générées/mises à jour : {made}"))
  invisible(made)
}

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

generate_thumbnails(yaml_files)

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
