test_that("my_knn function works fine", {
  expect_is(my_knn_cv(my_gapminder, my_gapminder$continent, 1, 5), "list")
})
