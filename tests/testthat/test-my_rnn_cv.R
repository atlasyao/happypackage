data("my_iris")
test_that("my knn function works fine", {
  expect_is(my_knn_cv(my_iris[, -5], my_iris$Species, 5, 5), "list")
})
