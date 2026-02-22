test_that("yaml_recipe_to_qmd generates output file", {
  td <- tempdir()
  y <- file.path(td, "recipe.yaml")
  q <- file.path(td, "recipe.qmd")

  recipe <- list(
    nom = "Test",
    nom_court = "test",
    source = "unit-test",
    portions = 2,
    preparation = list(
      list(
        section = "Preparation",
        etapes = list(
          list(
            etape = "Melanger.",
            ingredients = list(list(nom = "eau", qte = 1, uni = "tasse")),
            equipements = list("bol")
          )
        )
      )
    ),
    commentaires = list()
  )

  yaml::write_yaml(recipe, y)
  livrec::yaml_recipe_to_qmd(y, q)

  expect_true(file.exists(q))
  content <- paste(readLines(q, warn = FALSE), collapse = "\n")
  expect_true(grepl("## Ingrédients", content, fixed = TRUE))
})
