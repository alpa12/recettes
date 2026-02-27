test_that("slugify normalizes accents and spaces", {
  expect_equal(livrec:::slugify("Médaillons de porc!!"), "medaillons-de-porc")
})

test_that("extract_leading_quantity parses mixed fraction", {
  q <- livrec:::extract_leading_quantity("1 1/2 tasse de lait")
  expect_equal(q$qte, 1.5)
  expect_equal(q$remainder, "tasse de lait")
})
