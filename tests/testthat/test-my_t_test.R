test_that("my t test function works fine", {
  expect_is(my_t_test(x = c(1:200), alternative = "less", mu = 25), "list")
})
