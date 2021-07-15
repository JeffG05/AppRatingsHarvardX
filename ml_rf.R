set.seed(1, sample.kind = "Rounding")

rf_train_data <- train_set %>%
  filter(str_starts(Downloads, "1")) %>%
  mutate(Price = replace_na(as.numeric(Price), 0),
         Content_Rating = as.numeric(factor(Content_Rating)),
         Category = as.numeric(factor(Category)),
         Offered_By = as.numeric(factor(Offered_By)),
         OS_Version_Required = as.numeric(factor(OS_Version_Required)),
         Last_Updated_On = as.numeric(as_date(Last_Updated_On, format = "%b %d %Y"))) %>%
  select(Reviews, Price, Rating, Category, Last_Updated_On, Content_Rating, Offered_By, OS_Version_Required) %>%
  as.matrix()

rf_test_data <- test_set %>%
  mutate(Price = replace_na(as.numeric(Price), 0),
         Content_Rating = as.numeric(factor(Content_Rating)),
         Category = as.numeric(factor(Category)),
         Offered_By = as.numeric(factor(Offered_By)),
         OS_Version_Required = as.numeric(factor(OS_Version_Required)),
         Last_Updated_On = as.numeric(as_date(Last_Updated_On, format = "%b %d %Y"))) %>%
  select(Reviews, Price, Rating, Category, Last_Updated_On, Content_Rating, Offered_By, OS_Version_Required) %>%
  as.matrix()

rf_y <- train_set %>%
  filter(str_starts(Downloads, "1")) %>%
  select(Downloads) %>%
  as.matrix() %>%
  factor()

rf_y_test <- test_set %>%
  select(Downloads) %>%
  as.matrix() %>%
  factor()

rf_accuracy <- sapply(seq(2, 8), function(col) {
  rf_x <- rf_train_data[, 1:col]
  rf_x_test <- rf_test_data[, 1:col]

  train_rf <- train(rf_x,
                  rf_y,
                  method = "rf",
                  trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = data.frame(mtry = seq(5, 100, 5)),
                  nSamp = 5000)
  fit_rf <- randomForest(rf_x, rf_y, minNode = train_rf$bestTune$mtry)

  y_hat_rf <- predict(fit_rf, rf_x_test, type = "class")
  cm_rf <- confusionMatrix(y_hat_rf, rf_y_test)
  cm_rf$overall["Accuracy"]
})

best_rf_x <- rf_train_data[, 1:6]
best_rf_x_test <- rf_test_data[, 1:6]
best_train_rf <- train(best_rf_x,
                rf_y,
                method = "rf",
                trControl = trainControl(method = "cv", number = 5),
                tuneGrid = data.frame(mtry = seq(5, 100, 5)),
                nSamp = 5000)
best_fit_rf <- randomForest(best_rf_x, rf_y, minNode = best_train_rf$bestTune$mtry)
best_p_hat_rf <- predict(best_fit_rf, best_rf_x_test, type = "prob")
best_y_hat_rf <- predict(best_fit_rf, best_rf_x_test, type = "class")
