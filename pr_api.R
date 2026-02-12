library(plumber)
library(gh)
library(glue)
library(yaml)
library(gert)

#* @apiTitle Recettes PR API

#* Create Pull Request for a new recipe
#* @post /create-pr
function(req, res) {

  token <- Sys.getenv("GITHUB_PAT")
  if (token == "") {
    res$status <- 500
    return(list(error = "GITHUB_PAT not configured"))
  }

  body <- jsonlite::fromJSON(req$postBody)
  yaml_content <- body$yaml
  nom_court <- body$nom_court

  repo <- "alpa12/recettes"
  base_branch <- "dev"

  tmp <- tempfile()
  dir.create(tmp)

  # Clone repo
  repo_path <- file.path(tmp, "repo")

  gert::git_clone(
    url = glue("https://{token}@github.com/{repo}.git"),
    path = repo_path
  )

  setwd(repo_path)

  gert::git_checkout(base_branch)

  # Collision handling
  existing_files <- gert::git_ls()
  filename <- glue("recettes/{nom_court}.yaml")

  suffix <- 1
  original_name <- nom_court

  while (filename %in% existing_files$path) {
    nom_court <- glue("{original_name}-{suffix}")
    filename <- glue("recettes/{nom_court}.yaml")
    suffix <- suffix + 1
  }

  branch_name <- glue("recette/{nom_court}")

  gert::git_branch_create(branch_name)
  gert::git_checkout(branch_name)

  # Write file
  writeLines(yaml_content, filename)

  gert::git_add(filename)

  gert::git_commit(
    message = glue("Ajout recette {nom_court}")
  )

  gert::git_push(remote = "origin", refspec = branch_name)

  # Create PR
  pr <- gh::gh(
    "POST /repos/{owner}/{repo}/pulls",
    owner = "alpa12",
    repo = "recettes",
    title = glue("Ajout recette {nom_court}"),
    head = branch_name,
    base = base_branch,
    .token = token
  )

  list(
    success = TRUE,
    pr_url = pr$html_url
  )
}
