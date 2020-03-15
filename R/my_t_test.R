#' My t-test function
#'
#' This function performs a one sample t-test in R.
#'
#' @param x a non-empty numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "two.sided", "greater" or "less".
#' @param mu a number indicating the true value of the mean
#' (or difference in means if you are performing a two sample test).
#'
#' @keywords inference
#'
#' @return the t test results of x:
#' a list includes,
#' \code{test_stat}, the value of the test statistic;
#' \code{df}, the degrees of freedom;
#' \code{alternative} a character string describing the alternative hypothesis;
#' \code{p_val}, the p-value for the test.
#'
#' @examples
#' my_t_test(x = c(1:200), alternative = "less", mu = 25)
#' my_t_test(c(1:100), "greater", 10)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # send error message when the second input is not equal to "two.sided" or
  # "less" or "greater"
  if(alternative != "two.sided" & alternative != "less" &
     alternative != "greater") {
    stop("The second function input must be \"two.sided\" or \"less\" or
         \"greater\"")
  }

  # calculate the mean of the the matrix
  x_mean <- mean(x)
  # calculate the standard deviation of the matrix
  x_sd <- sd(x)
  # calculat the sample size
  x_length <- length(x)
  # get the test statistic of one sample t-test
  test_stat <- (x_mean - mu) / (x_sd / sqrt(x_length))
  # get degree of freedom
  x_dof <- x_length - 1

  # calculate the p-value based on the input "alternative"
  if(alternative == "greater") {
    prob <- pt(test_stat, x_dof, lower.tail = FALSE)
  } else if(alternative == "less") {
    prob <- pt(test_stat, x_dof, lower.tail = TRUE)
  } else if(alternative == "two.sided") {
    # calculate p-value based on whether or not the test statistic is bigger
    # than
    if(test_stat < 0) {
      prob <- 2 * pt(test_stat, x_dof, lower.tail = TRUE)
    } else {
      prob <- 2 * pt(test_stat, x_dof, lower.tail = FALSE)
    }
  }
  # create a list for the 4 main components of a t-test
  my_list <- list("test_stat" = test_stat,
                  "df" = x_dof,
                  "alternative" = alternative,
                  "p_value" = prob)
  return(my_list)
}
