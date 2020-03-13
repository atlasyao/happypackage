#' my k-Nearest Neighbors Cross-Validation function
#'
#' This function performs a k-Nearest Neighbors Cross-Validation in R.
#'
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#'
#' @return a list with objects:
#' \code{class}, a vector of the predicted class yi_hat for all observations;
#' \code{cv_err}, a numeric with the cross-validation misclassification error.
#'
#' @examples
#' my_knn_cv <- my_knn_cv(iris, iris$Species, 1, 5)
#' my_knn_cv <- my_knn_cv(iris, iris$Species, 5, 5)
#'
#' @import class
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # get the total number of the data
  n <- nrow(data)
  # randomly split data into k parts
  fold <- sample(rep(1:k_cv, length = n))
  # create empty vector for storing error
  error_for_i <- c()
  for (i in 1 : k_cv) {
    # X_i
    data_train <- train[fold != i, ]
    # X_i^*
    data_test <-  train[fold == i, ]
    # Y_i
    cl_train <- cl[fold != i]
    # Y_i^*
    cl_test <-  cl[fold == i]
    knn_output <- knn(train = data_train, cl = cl_train, test  = data_test, k = k_nn)
    # calculate the misclassification error
    error_for_i[i] <- sum(knn_output != cl_test) / length(cl_test)
  }
  # build model
  my_class <- knn(train = train, cl = cl, test  = train, k = k_nn)
  # calculate the average misclassification rate
  my_error <- mean(error_for_i)
  # integrate the class and error to a list for output
  my_list <- list("class" = my_class, "cv_err" = my_error)
  return(my_list)
}
