# Sythetic Data Generation ------------------------------------------------

generate_synthetic_data <- function(model, data, n_samples) {
  # Generation of independent variables with random values within the range of original data
  independent_vars <- data.frame(lapply(data[, -which(names(data) == "Outcome")], function(x) runif(n_samples, min = min(x), max = max(x))))

  # Prediction of the dependent variable based on the trained model and generated independent variables
  dependent_var <- predict(model, newdata = independent_vars)

  # Independent and dependent variables combination to form synthetic data
  synthetic_data <- cbind(independent_vars, Outcome = dependent_var)

  # Check if the synthetic data has the same dimensions as the original data
  if (ncol(synthetic_data) != ncol(data) || nrow(synthetic_data) != nrow(data)) {
    stop("Synthetic data does not have the same dimensions as original data.")
  }

  return(synthetic_data)
}
