#' my k-Nearest Neighbors Cross-Validation function
#'
#' This function performs a k-Nearest Neighbors Cross-Validation in R.
#'
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#'
#' @keywords prediction
#'
#' @return a list with objects:
#' \code{class}, a vector of the predicted class yi_hat for all observations;
#' \code{cv_err}, a numeric with the cross-validation misclassification error.
#'
#' @examples
#' my_knn_cv(my_gapminder, my_gapminder$continent, 1, 5)
#'
#' @import class magrittr stats
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  lifeExp <- train[[4]]
  gdpPercap <- train[[6]]
  measure <- data.frame(lifeExp, gdpPercap)
  set.seed(302)
  # get the total number of the data
  n <- nrow(train)
  # Randomly splicl at data into k parts
  inds <- sample(rep(1:k_cv, length = n))
  data_combine <- data.frame("x" = measure, "y" = cl, "split" = inds)
  # create empty matrix for storing misclassification rate
  error <- matrix(NA, k_cv, 1)
  for(i in 1:k_cv) {
    data_train <- data_combine %>%
      dplyr::filter(split != i)
    data_test <- data_combine %>%
      dplyr::filter(split == i)
    # select only the numeric column
    x_lifeExp_train <- data_train[[1]]
    x_gdpPercap_train <- data_train[[2]]
    data_train_knn <- data.frame(x_lifeExp_train, x_gdpPercap_train)

    x_lifeExp_test <- data_test[[1]]
    x_gdpPercap_test <- data_test[[2]]
    data_test_knn <- data.frame(x_lifeExp_test, x_gdpPercap_test)
    # extract 3th column of train data set and use it for knn later
    cl_train_column <- data_train[[3]]
    # extract 3th column of test data set to measure accuracy
    cl_test_column <- data_test[[3]]
    # Train our models
    knn_predict <- knn(train = data_train_knn, cl = cl_train_column,
                       test = data_test_knn, k = k_nn)
    # create confusion matrix
    tab <- table(knn_predict, cl_test_column)
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
    # calculate the misclassification rate
    false_rate <- (100 - accuracy(tab)) / 100
    # store the misclassification rate in the empty matrix
    error[i, 1] <- false_rate
  }
  # build model with full data
  knn_full <- knn(train = measure, cl = cl, test = measure, k = k_nn)
  # calculate the average misclassification rate
  cv_error <- colMeans(error)
  # create a list to the model as well as the average misclassification rate
  mylist <- list("class" = knn_full,
                 "cv_error" = cv_error)
  return(mylist)
}
