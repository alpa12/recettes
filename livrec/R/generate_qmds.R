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

generate_thumbnails <- function(yaml_files, images_dir = "images", thumbs_dir = file.path(images_dir, "thumbs")) {
  magick_bin <- Sys.which("magick")
  convert_bin <- Sys.which("convert")
  bin <- if (nzchar(magick_bin)) "magick" else if (nzchar(convert_bin)) "convert" else ""
  if (!nzchar(bin)) {
    cli::cli_alert_info("ImageMagick CLI indisponible (magick/convert): miniatures non generees.")
    return(invisible(0L))
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

      src <- file.path(images_dir, paste0(g, ".jpg"))
      dst <- file.path(thumbs_dir, paste0(g, ".jpg"))
      if (!file.exists(src)) next
      if (file.exists(dst) && file.info(dst)$mtime >= file.info(src)$mtime) next

      args <- if (identical(bin, "magick")) {
        c(src, "-resize", "1200x1200>", "-strip", "-quality", "72", dst)
      } else {
        c(src, "-resize", "1200x1200>", "-strip", "-quality", "72", dst)
      }
      status <- suppressWarnings(system2(bin, args = args, stdout = TRUE, stderr = TRUE))
      if (!is.null(attr(status, "status")) && attr(status, "status") != 0) {
        cli::cli_alert_warning(glue::glue("Echec miniature pour {basename(src)}"))
        next
      }
      made <- made + 1L
    }
  }

  cli::cli_alert_info(glue::glue("Miniatures generees/mises a jour : {made}"))
  invisible(made)
}

find_recipe_yaml_files <- function(recipes_dir = "recettes") {
  yaml_files <- list.files(
    path = recipes_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )
  yaml_files <- yaml_files[basename(yaml_files) != "template.yaml"]
  yaml_files <- yaml_files[!grepl("url_imports/", yaml_files, fixed = TRUE)]
  yaml_files <- yaml_files[!grepl("url_imports_web/", yaml_files, fixed = TRUE)]
  yaml_files <- yaml_files[!grepl("url_imports_youtube/", yaml_files, fixed = TRUE)]
  yaml_files
}

#' GitHub Actions entrypoint: generate QMD files from recipe YAMLs.
#' @export
gha_generate_qmds <- function(recipes_dir = "recettes", images_dir = "images") {
  cli::cli_h1("Generation des fichiers QMD")

  yaml_files <- find_recipe_yaml_files(recipes_dir)
  cli::cli_alert_info(glue::glue("Recettes YAML detectees : {length(yaml_files)}"))

  generate_thumbnails(
    yaml_files = yaml_files,
    images_dir = images_dir,
    thumbs_dir = file.path(images_dir, "thumbs")
  )

  created <- 0L
  modified <- 0L
  unchanged <- 0L

  for (yaml in yaml_files) {
    qmd <- sub("\\.ya?ml$", ".qmd", yaml)

    old_content <- NULL
    existed <- file.exists(qmd)
    if (existed) old_content <- readLines(qmd, warn = FALSE)

    yaml_recipe_to_qmd(yaml_path = yaml, qmd_path = qmd)
    new_content <- readLines(qmd, warn = FALSE)

    if (!existed) {
      created <- created + 1L
      state <- "Cree"
    } else if (identical(old_content, new_content)) {
      unchanged <- unchanged + 1L
      state <- "Inchange"
    } else {
      modified <- modified + 1L
      state <- "Modifie"
    }

    if (state != "Inchange") {
      cli::cli_alert(glue::glue("{state} : {fs::path_rel(qmd, start = recipes_dir)}"))
    }
  }

  cli::cli_h2("Resume")
  cli::cli_bullets(c(
    glue::glue("YAML traites : {length(yaml_files)}"),
    glue::glue("QMD crees : {created}"),
    glue::glue("QMD modifies : {modified}"),
    glue::glue("QMD inchanges : {unchanged}")
  ))

  invisible(list(
    yaml_files = yaml_files,
    created = created,
    modified = modified,
    unchanged = unchanged
  ))
}
