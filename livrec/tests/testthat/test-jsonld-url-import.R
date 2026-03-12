test_that("extract_recipe_json_ld keeps top-level Recipe when @graph is also present", {
  payload <- list(
    "@context" = "https://schema.org",
    "@type" = "Recipe",
    name = "Top level recipe",
    recipeIngredient = list("1 tasse farine"),
    recipeInstructions = list("Melanger."),
    "@graph" = list(
      list("@type" = "WebPage", name = "Metadata node")
    )
  )

  html <- paste0(
    "<html><head><script type='application/ld+json'>",
    jsonlite::toJSON(payload, auto_unbox = TRUE),
    "</script></head></html>"
  )

  page <- xml2::read_html(html)
  recipe <- livrec:::extract_recipe_json_ld(page)

  expect_false(is.null(recipe))
  expect_equal(recipe$name, "Top level recipe")
})

test_that("extract_recipe_json_ld still finds Recipe in @graph", {
  payload <- list(
    "@context" = "https://schema.org",
    "@graph" = list(
      list("@type" = "WebPage", name = "Metadata node"),
      list(
        "@type" = "Recipe",
        name = "Graph recipe",
        recipeIngredient = list("2 oeufs"),
        recipeInstructions = list("Battre les oeufs.")
      )
    )
  )

  html <- paste0(
    "<html><head><script type='application/ld+json'>",
    jsonlite::toJSON(payload, auto_unbox = TRUE),
    "</script></head></html>"
  )

  page <- xml2::read_html(html)
  recipe <- livrec:::extract_recipe_json_ld(page)

  expect_false(is.null(recipe))
  expect_equal(recipe$name, "Graph recipe")
})
