
# Data Visualization ------------------------------------------------------

visualize_data <- function(original_data, synthetic_data) {
  par(mfrow = c(2, 4))
  for (col in colnames(original_data)) {
    hist(original_data[[col]], main = paste("Original -", col), xlab = col, ylim = c(0, 200))
    hist(synthetic_data[[col]], main = paste("Synthetic -", col), xlab = col, ylim = c(0, 200))
  }

  pairs(original_data[, -which(names(original_data) == "Outcome")], main = "Original Data Scatterplot Matrix")
  pairs(synthetic_data[, -which(names(synthetic_data) == "Outcome")], main = "Synthetic Data Scatterplot Matrix")

  correlation_comparison <- compare_correlations(original_data, synthetic_data)
  print("Correlation Matrix for Original Data:")
  print(correlation_comparison$correlation_original)
  print("Correlation Matrix for Synthetic Data:")
  print(correlation_comparison$correlation_synthetic)
}



