set.seed(1, sample.kind = "Rounding")

knn_train_data <- train_set %>%
  filter(str_starts(Downloads, "1")) %>%
  mutate(Price = replace_na(as.numeric(Price), 0),
         Content_Rating = as.numeric(factor(Content_Rating)),
         Category = as.numeric(factor(Category)),
         Offered_By = as.numeric(factor(Offered_By)),
         OS_Version_Required = as.numeric(factor(OS_Version_Required)),
         Last_Updated_On = as.numeric(as_date(Last_Updated_On, format = "%b %d %Y"))) %>%
  select(Reviews, Price, Rating, Category, Last_Updated_On, Content_Rating, Offered_By, OS_Version_Required) %>%
  as.matrix()

knn_test_data <- test_set %>%
  mutate(Price = replace_na(as.numeric(Price), 0),
         Content_Rating = as.numeric(factor(Content_Rating)),
         Category = as.numeric(factor(Category)),
         Offered_By = as.numeric(factor(Offered_By)),
         OS_Version_Required = as.numeric(factor(OS_Version_Required)),
         Last_Updated_On = as.numeric(as_date(Last_Updated_On, format = "%b %d %Y"))) %>%
  select(Reviews, Price, Rating, Category, Last_Updated_On, Content_Rating, Offered_By, OS_Version_Required) %>%
  as.matrix()

knn_y <- train_set %>%
  filter(str_starts(Downloads, "1")) %>%
  select(Downloads) %>%
  as.matrix() %>%
  factor()

knn_y_test <- test_set %>%
  select(Downloads) %>%
  as.matrix() %>%
  factor()

knn_accuracy <- sapply(seq(2, 8), function(col) {
  knn_x <- knn_train_data[, 1:col]
  knn_x_test <- knn_test_data[, 1:col]

  index <- sample(nrow(knn_x), 2000)
  train_knn <- train(knn_x[index,],
                  knn_y[index],
                  method = "knn",
                  tuneGrid = data.frame(k = seq(3, 200, 2)))
  fit_knn <- knn3(knn_x, knn_y, k = train_knn$bestTune$k)

  y_hat_knn <- predict(fit_knn, knn_x_test, type = "class")
  cm_knn <- confusionMatrix(y_hat_knn, knn_y_test)
  cm_knn$overall["Accuracy"]
})

best_knn_x <- knn_train_data[, 1:4]
best_knn_x_test <- knn_test_data[, 1:4]
best_index <- sample(nrow(best_knn_x), 2000)
best_train_knn <- train(best_knn_x[best_index,],
                knn_y[best_index],
                method = "knn",
                tuneGrid = data.frame(k = seq(3, 200, 2)))
best_fit_knn <- knn3(best_knn_x, knn_y, k = best_train_knn$bestTune$k)
best_p_hat_knn <- predict(best_fit_knn, best_knn_x_test, type = "prob")
best_y_hat_knn <- predict(best_fit_knn, best_knn_x_test, type = "class")



