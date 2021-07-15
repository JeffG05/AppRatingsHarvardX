library(caret)
library(tidyverse)
library(lubridate)
library(randomForest)

set.seed(1, sample.kind = "Rounding")

temp <- tempfile()
download.file("https://raw.githubusercontent.com/JeffG05/AppRatingsHarvardX/master/data.csv", temp)

data_set <- read.csv(temp, header = TRUE) %>%
  group_by(Downloads) %>%
  filter(n() > 1) %>%
  ungroup()

data_ind <- createDataPartition(data_set$Downloads, p = 0.9, list = FALSE)
validation_set <- data_set %>% slice(-data_ind)
data_set <- data_set %>% slice(data_ind)

train_ind <- createDataPartition(data_set$Downloads, p = 0.9, list = FALSE)
test_set <- data_set %>% slice(-train_ind)
train_set <- data_set %>% slice(train_ind)

validation_x <- validation_set %>% select(-Downloads) %>% as.matrix()
validation_y <- validation_set %>% select(Downloads) %>% as.matrix()

test_x <- test_set %>% select(-Downloads) %>% as.matrix()
test_y <- test_set %>% select(Downloads) %>% as.matrix()

train_x <- train_set %>% select(-Downloads) %>% as.matrix()
train_y <- train_set %>% select(Downloads) %>% as.matrix()