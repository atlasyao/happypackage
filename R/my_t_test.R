my_t_test <- function(x, alternative, mu) {
  # send an error message if the alternative is unrecognizable
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("Alternative Error! Please choose from: two.sided, less, or greater.")
  }

  # store relevant details of x
  # store x mean
  x_mean <- mean(x)
  # store x standard deviation
  x_sd <- sd(x)
  # store x standard error
  x_se <- x_sd / sqrt(length(x))
  # store the degree of freedom
  x_dof <- length(x) - 1
  # store the test statistics value
  test_stat <- (x_mean - mu) / x_se

  # get the area under the curve for a t-distribution using function "pt()" after specifing the alternative hypothesis
  if (alternative == "two.sided") {
    if (test_stat < 0) {
      # test_stat < 0, calculate the lower tail then double it
      prob <- pt(test_stat, df = x_dof, lower.tail = T) * 2
    } else {
      # test_stat > 0, calculate the upper tail then double it
      prob <- pt(test_stat, df = x_dof, lower.tail = F) * 2
    }
  } else if (alternative == "greater") {
    # alternative is "greater", calculate the upper tail
    prob <- pt(test_stat, df = x_dof, lower.tail = F)
  } else {
    # alternative is "less", calculate the lower tail
    prob <- pt(test_stat, df = x_dof, lower.tail = T)
  }

  # store the results in a list
  results <- list("test_stat" = test_stat,
                  "df" = x_dof,
                  "alternative" = alternative,
                  "p_val" = prob)
  # return results
  results
}
