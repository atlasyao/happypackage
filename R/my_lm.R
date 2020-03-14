#' My linear model function
#'
#' This function fits a linear model in R.
#'
#' @param formula a \code{formula} class object, a symbolic description of the
#' model to be fitted.
#' @param data input data frame.
#'
#' @keywords prediction
#'
#' @return a summary table:
#' \code{Estimate}, the beta value;
#' \code{Std. Error}, the standard error;
#' \code{t value} the t value;
#' \code{Pr(>|t|)}, the p value.
#'
#' @examples
#' data(mtcars)
#' my_lm(mpg ~ hp + wt, data = mtcars)
#' data(my_gapminder)
#' my_lm(pop ~ gdpPercap, data = my_gapminder)
#'
#' @export
my_lm <- function(formula, data) {
  # model matrix x
  matrix_x <- model.matrix(formula, data)
  # model matrix y
  # find model frame of matrix y
  model_frame <- model.frame(formula, data)
  # model matrix y
  matrix_y <- model.response(model_frame)
  # model matrix x - T
  t_x <- t(matrix_x)
  # calculate beta
  beta <- solve(t_x %*% matrix_x) %*% t_x %*% matrix_y
  # find the row number of matrix x
  row_n <- nrow(matrix_x)
  # find the column number of matrix x
  col_n <- ncol(matrix_x)
  # calculate the degree of freedom
  dof <- row_n - col_n
  # calculate the variance
  variance <- sum(((matrix_y - matrix_x %*% beta)^2) / dof)
  #calculate the standard error
  se <- sqrt(diag(variance * solve(t_x %*% matrix_x)))
  # calculate the t value
  t_value <- (beta - 0) / se
  # calculate the p value
  prob <- pt(abs(t_value), dof, lower.tail = FALSE) * 2

  # generate the table summary
  my_table <- as.table(cbind(beta, se, t_value, prob))
  # give column names to the table
  colnames(my_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  # return the table summary
  my_table
}
