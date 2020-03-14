test_that("my lm function works fine", {
  expect_is(my_lm(my_fml = pop ~ gdpPercap, my_data = my_gapminder), "table")
})
