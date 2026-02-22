test_that("enforce_new_ingredients_by_step keeps only first introduction", {
  recipe <- list(
    preparation = list(
      list(
        section = "Prep",
        etapes = list(
          list(etape = "Step 1", ingredients = list(list(nom = "longe de porc", qte = 1, uni = "kg"))),
          list(etape = "Step 2", ingredients = list(
            list(nom = "longe de porc", qte = 1, uni = "kg"),
            list(nom = "porto", qte = 0.5, uni = "tasse")
          ))
        )
      )
    )
  )

  out <- livrec:::enforce_new_ingredients_by_step(recipe)
  expect_length(out$preparation[[1]]$etapes[[1]]$ingredients, 1)
  expect_length(out$preparation[[1]]$etapes[[2]]$ingredients, 1)
  expect_equal(out$preparation[[1]]$etapes[[2]]$ingredients[[1]]$nom, "porto")
})
