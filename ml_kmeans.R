set.seed(1, sample.kind = "Rounding")

lm_train_data <- train_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", ""))) %>%
  select(Reviews, Downloads_Num)

lm_test_data <- test_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", ""))) %>%
  select(Reviews, Downloads_Num)

fit_lm <- lm(Downloads_Num ~ Reviews, data = lm_train_data)

y_hat_lm <- predict(fit_lm, lm_test_data)

possible_values <- train_set %>%
  pull(Downloads) %>%
  unique() %>%
  data.frame(Downloads = .) %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", "")))
y_hat_lm_banded <- sapply(y_hat_lm, function (prediction) {
  distances <- sapply(possible_values$Downloads_Num, function (possible) {
    abs(possible - prediction)
  })
  possible_values$Downloads[which.min(distances)]
})

data.frame(pred = y_hat_lm_banded, actual = test_set$Downloads)

cm_lm <- confusionMatrix(factor(y_hat_lm_banded), factor(test_set$Downloads))


