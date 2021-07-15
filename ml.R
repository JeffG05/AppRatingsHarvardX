results <- data.frame(
  knn = knn_accuracy,
  rf = rf_accuracy
)
rownames(results) <- sapply(seq(2, 8), function (x) {
  paste(ifelse(x==2, paste0(colnames(knn_train_data)[1], ","), "+"), colnames(knn_train_data)[x], collapse = "")
})


ensemble_p_hat <- (best_p_hat_rf + best_p_hat_knn) / 2
ensemble_y_hat <- colnames(ensemble_p_hat)[max.col(ensemble_p_hat, ties.method = "random")] %>% factor()
cm_ensemble <- confusionMatrix(ensemble_y_hat, rf_y_test)
cm_ensemble$overall["Accuracy"]