#' My Random Forest Cross-Validation
#'
#' This function performs a Random Forest Cross-Validation in R.
#'
#' @param k number of folds
#'
#' @keywords inference
#'
#' @return a numeric with the cross-validation error
#'
#' @examples
#' my_rf_cv(5)
#'
#' @import randomForest
#'
#' @export
my_rf_cv <- function(k) {
  # load the data
  dataset <- my_gapminder
  # get the total row number of the data
  n <- nrow(dataset)
  # randomly split data into k parts
  fold <- sample(rep(1:k, length = n))
  # create empty vector for storing mean squared error
  MSE <- rep(NA, k)
  for(i in 1:k) {
    # X_i
    data_train <- dataset[fold != i, ]
    # X_i^*
    data_test <- dataset[fold == i, ]
    # train the randomForest model with 50 trees
    MODEL <- randomForest(lifeExp ~ gdpPercap, data = data_train, ntree = 50)
    # store predictions
    pred = predict(MODEL, data_test[, -1])
    # store MSE
    MSE[i] <- mean((data_test$lifeExp - pred)^2)
  }
  # calculate the mean MSE
  avg_MSE <- mean(MSE)
  return(avg_MSE)
}
