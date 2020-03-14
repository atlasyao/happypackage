test_that("my knn function works fine", {
  expect_is(my_knn_cv <- my_knn_cv(iris, iris$Species, 1, 5), "list")
})
