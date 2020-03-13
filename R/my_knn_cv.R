my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # select only the numeric column
  measure <- train[,-5]
  set.seed(302)
  # get the total number of the data
  n <- nrow(data)
  # Randomly split data into k parts
  inds <- sample(rep(1:k_cv, length = n))
  data_combine <- data.frame("x" = measure, "y" = cl, "split" = inds)
  # create empty matrix for storing  misclassification rate
  error <- matrix(NA, k_cv, 1)
  for(i in 1:k_cv) {
    # X_i
    data_train <- data_combine %>% filter(split != i)
    # X_i^*
    data_test <- data_combine %>% filter(split == i)
    # select only the numeric column
    data_train_knn <- data_train %>%
      select(x.Sepal.Length, x.Sepal.Width, x.Petal.Length, x.Petal.Width)
    data_test_knn <- data_test %>%
      select(x.Sepal.Length, x.Sepal.Width, x.Petal.Length, x.Petal.Width)
    # extract 5th column of train data set and use it for knn later
    cl_train_column <- data_train[ ,5]
    # extract 5th column of test data set to measure accuracy
    cl_test_column <- data_test[ ,5]
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
}
