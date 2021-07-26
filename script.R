# Check if libraries are installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

# Load libraries
library(caret)
library(tidyverse)
library(lubridate)
library(randomForest)
library(knitr)

set.seed(1, sample.kind = "Rounding")

# Download the dataset
temp <- tempfile()
download.file("https://raw.githubusercontent.com/JeffG05/AppRatingsHarvardX/master/data.csv", temp)

# Convert to CSV
data_set <- read.csv(temp, header = TRUE) %>%
  group_by(Downloads) %>%
  filter(n() > 1) %>%
  ungroup()

# Select useful columns and convert to numeric values
data_set <- data_set %>%
  select(Reviews, Price, Rating, Category, Last_Updated_On, Content_Rating, Offered_By, OS_Version_Required, Downloads) %>%
  mutate(Last_Updated_On = as.numeric(as_date(Last_Updated_On, format = "%b %d %Y")),
         Offered_By = as.numeric(factor(Offered_By)),
         Category = as.numeric(factor(Category)),
         Content_Rating = as.numeric(factor(Content_Rating)),
         OS_Version_Required = as.numeric(factor(OS_Version_Required)),
         Price = replace_na(as.numeric(Price), 0))

# Split dataset into train, test and validation dataframes
data_ind <- createDataPartition(data_set$Downloads, p = 0.9, list = FALSE)
validation_set <- data_set %>% slice(-data_ind)
data_set <- data_set %>% slice(data_ind)

train_ind <- createDataPartition(data_set$Downloads, p = 0.9, list = FALSE)
test_set <- data_set %>% slice(-train_ind)
train_set <- data_set %>% slice(train_ind)

# Convert dataframes to matrices
validation_x <- validation_set %>% select(-Downloads) %>% as.matrix()
validation_y <- validation_set %>% select(Downloads) %>% as.matrix() %>% factor()

test_x <- test_set %>% select(-Downloads) %>% as.matrix()
test_y <- test_set %>% select(Downloads) %>% as.matrix() %>% factor()

train_x <- train_set %>% select(-Downloads) %>% as.matrix()
train_y <- train_set %>% select(Downloads) %>% as.matrix() %>% factor()

set.seed(1, sample.kind = "Rounding")

# MODEL 1: KNN & Random Forest

# Get accuracies for all KNN models
knn_accuracy <- sapply(seq(2, 8), function(col) {
  # Select columns to train model on
  knn_x <- train_x[, 1:col]
  knn_x_test <- test_x[, 1:col]

  # Tune parameters and train a final model
  index <- sample(nrow(knn_x), 2000)
  train_knn <- train(knn_x[index,],
                  train_y[index],
                  method = "knn",
                  tuneGrid = data.frame(k = seq(3, 200, 2)))
  fit_knn <- knn3(knn_x, train_y, k = train_knn$bestTune$k)

  # Test model on the test set and return the accuracy
  y_hat_knn <- predict(fit_knn, knn_x_test, type = "class")
  cm_knn <- confusionMatrix(y_hat_knn, test_y)
  cm_knn$overall["Accuracy"]
})

# Get accuracies for all Random Forest models
rf_accuracy <- sapply(seq(2, 8), function(col) {
  # Select columns to train model on
  rf_x <- train_x[, 1:col]
  rf_x_test <- test_x[, 1:col]

  # Tune parameters and train a final model
  train_rf <- train(rf_x,
                  train_y,
                  method = "rf",
                  trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = data.frame(mtry = seq(5, 100, 5)),
                  nSamp = 5000)
  fit_rf <- randomForest(rf_x, train_y, minNode = train_rf$bestTune$mtry)

  # Test model on the test set and return the accuracy
  y_hat_rf <- predict(fit_rf, rf_x_test, type = "class")
  cm_rf <- confusionMatrix(y_hat_rf, test_y)
  cm_rf$overall["Accuracy"]
})

# Put results in a table
model1_results <- data.frame(
  KNN = knn_accuracy,
  RF = rf_accuracy
)

# MODEL 2: Ensemble

# Select columns that performed best in KNN models
best_knn_x <- train_x[, 1:3]
best_knn_x_test <- test_x[, 1:3]

# Retrain the best KNN model
best_index <- sample(nrow(best_knn_x), 2000)
best_train_knn <- train(best_knn_x[best_index,],
                train_y[best_index],
                method = "knn",
                tuneGrid = data.frame(k = seq(3, 200, 2)))
best_fit_knn <- knn3(best_knn_x, train_y, k = best_train_knn$bestTune$k)

# Obtain probability values for the test set using the best KNN model
best_p_hat_knn <- predict(best_fit_knn, best_knn_x_test, type = "prob")

# Select columns that performed best in RF models
best_rf_x <- train_x[, 1:7]
best_rf_x_test <- test_x[, 1:7]

# Retrain the best RF model
best_train_rf <- train(best_rf_x,
                train_y,
                method = "rf",
                trControl = trainControl(method = "cv", number = 5),
                tuneGrid = data.frame(mtry = seq(5, 100, 5)),
                nSamp = 5000)
best_fit_rf <- randomForest(best_rf_x, train_y, minNode = best_train_rf$bestTune$mtry)

# Obtain probability values for the test set using the best RF model
best_p_hat_rf <- predict(best_fit_rf, best_rf_x_test, type = "prob")

# Obtain ensemble predictions using averaged KNN and RF probabilities
ensemble_p_hat <- (best_p_hat_knn + best_p_hat_rf) / 2
ensemble_max_cols <- max.col(ensemble_p_hat, ties.method = "random")
ensemble_y_hat <- colnames(ensemble_p_hat)[ensemble_max_cols] %>% factor()
cm_ensemble <- confusionMatrix(ensemble_y_hat, test_y)

# Put results in a table
ensemble_result <- data.frame(
  Accuracy = c(model1_results$KNN[2], model1_results$RF[6], cm_ensemble$overall["Accuracy"])
)
rownames(ensemble_result) <- c("Best KNN", "Best RF", "Ensemble")

# MODEL 3: Remove download bands

# Remove download bands from training dataset
train_x <- train_set %>%
  filter(str_starts(Downloads, "1")) %>%
  select(-Downloads) %>%
  as.matrix()
train_y <- train_set %>%
  filter(str_starts(Downloads, "1")) %>%
  select(Downloads) %>%
  as.matrix() %>%
  factor()

# Get accuracies for all KNN models
knn_accuracy <- sapply(seq(2, 8), function(col) {
  knn_x <- train_x[, 1:col]
  knn_x_test <- test_x[, 1:col]

  index <- sample(nrow(knn_x), 2000)
  train_knn <- train(knn_x[index,],
                  train_y[index],
                  method = "knn",
                  tuneGrid = data.frame(k = seq(3, 200, 2)))
  fit_knn <- knn3(knn_x, train_y, k = train_knn$bestTune$k)

  y_hat_knn <- predict(fit_knn, knn_x_test, type = "class")
  cm_knn <- confusionMatrix(y_hat_knn, test_y)
  cm_knn$overall["Accuracy"]
})

# Get accuracies for all RF models
rf_accuracy <- sapply(seq(2, 8), function(col) {
  rf_x <- train_x[, 1:col]
  rf_x_test <- test_x[, 1:col]

  train_rf <- train(rf_x,
                  train_y,
                  method = "rf",
                  trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = data.frame(mtry = seq(5, 100, 5)),
                  nSamp = 5000)
  fit_rf <- randomForest(rf_x, train_y, minNode = train_rf$bestTune$mtry)

  y_hat_rf <- predict(fit_rf, rf_x_test, type = "class")
  cm_rf <- confusionMatrix(y_hat_rf, test_y)
  cm_rf$overall["Accuracy"]
})

# FINAL MODEL ACCURACY

# Obtain final accuracy
best_y_hat <- predict(best_fit_rf, validation_x[, 1:7], type = "class")
cm_best <- confusionMatrix(best_y_hat, validation_y)
final_accuracy <- cm_best$overall["Accuracy"]

# Obtain final accuracy (one band error margin)
one_band_accuracy <- data.frame(pred = parse_number(str_replace(best_y_hat, "\\+", "")),
           act = parse_number(str_replace(validation_y, "\\+", ""))) %>%
    mutate(below = ifelse(str_starts(as.character(act), "1"), act / 2, act / 5),
           above = ifelse(str_starts(as.character(act), "1"), act * 5, act * 2)) %>%
    mutate(match = pred == below | pred == act | pred == above) %>%
    pull(match) %>%
    mean()