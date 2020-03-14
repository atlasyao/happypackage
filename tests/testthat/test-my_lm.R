test_that("my lm function works fine", {
  expect_is(my_lm(pop ~ gdpPercap, data = my_gapminder), "table")
})
