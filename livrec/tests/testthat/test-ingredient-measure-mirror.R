test_that("autofix_recipe_ingredient mirrors default mass quantity", {
  ing <- list(
    nom = "Sucre",
    qte = 120,
    uni = "g",
    qte_masse = NULL,
    uni_masse = NULL,
    qte_volume = NULL,
    uni_volume = NULL,
    rangee = "Epicerie"
  )

  out <- livrec:::autofix_recipe_ingredient(ing)
  expect_equal(out$qte_masse, 120)
  expect_equal(out$uni_masse, "g")
})

test_that("autofix_recipe_ingredient mirrors default volume quantity", {
  ing <- list(
    nom = "Lait",
    qte = 250,
    uni = "ml",
    qte_masse = NULL,
    uni_masse = NULL,
    qte_volume = NULL,
    uni_volume = NULL,
    rangee = "Produits laitiers et oeufs"
  )

  out <- livrec:::autofix_recipe_ingredient(ing)
  expect_equal(out$qte_volume, 250)
  expect_equal(out$uni_volume, "ml")
})

test_that("autofix_recipe_ingredient keeps count units without mass/volume mirrors", {
  ing <- list(
    nom = "Oeuf",
    qte = 2,
    uni = "unité",
    qte_masse = 100,
    uni_masse = "g",
    qte_volume = 100,
    uni_volume = "ml",
    rangee = "Produits laitiers et oeufs"
  )

  out <- livrec:::autofix_recipe_ingredient(ing)
  expect_null(out$qte_masse)
  expect_null(out$uni_masse)
  expect_null(out$qte_volume)
  expect_null(out$uni_volume)
})
