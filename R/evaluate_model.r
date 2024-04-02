# Model Evaluation --------------------------------------------------------
evaluate_model <- function(model, original_data, synthetic_data) {
  
  # Train set of data
  train_index <- sample(1:nrow(original_data), 0.7 * nrow(original_data)) # 70% for training
  train_data <- original_data[train_index, ]
  
  # Tests set of data
  test_data <- original_data[-train_index, ]
  
  # Model performance on original data
  predictions_original <- predict(model, newdata = test_data)
  mse_original <- mean((predictions_original - test_data$Outcome)^2)
  
  # Model performance on synthetic data
  model_synthetic <- lm(Outcome ~ ., data = synthetic_data[train_index, ])
  predictions_synthetic <- predict(model_synthetic, newdata = synthetic_data[-train_index, ])
  mse_synthetic <- mean((predictions_synthetic - synthetic_data$Outcome[-train_index])^2)
  
  # Comparation of MSEs to knowe which model performs better
  if (mse_original < mse_synthetic) {
    result <- "The model on original data performs better."
  } else if (mse_synthetic < mse_original) {
    result <- "The model on synthetic data performs better."
  } else {
    result <- "Both models perform similarly."
  }
  
  return(list(mse_original = mse_original, mse_synthetic = mse_synthetic, result = result))
}
# Correlations comparation (original vs synthetic data)
compare_correlations <- function(original_data, synthetic_data) {
  correlation_matrix_original <- cor(original_data[, -which(names(original_data) == "Outcome")])
  correlation_matrix_synthetic <- cor(synthetic_data[, -which(names(synthetic_data) == "Outcome")])
  
  return(list(correlation_original = correlation_matrix_original, correlation_synthetic = correlation_matrix_synthetic))
}
