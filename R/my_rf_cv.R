my_rf_cv <- function(k) {
  dataset <- iris
  n <- nrow(dataset)
  inds <- sample(rep(1:k, length = n))
  dataset[, "split"] <- inds
  MSE_sum <- 0;
  pred_mat <- matrix(NA, n, 1)
  for(i in 1:k) {
    data_train <- dataset %>%
      filter(split != i)
    data_test <- dataset %>%
      filter(split == i)

    MODEL <- randomForest(Sepal.Length ~ Sepal.Width + Petal.Length +
                            Petal.Width, data = data_train, ntree = 100)

    pred_mat[inds == i, 1] = predict(MODEL, data_test[, -1])
  }
  MSE <- colMeans((pred_mat - dataset$Sepal.Length)^2)
  return(MSE)
}
