

# DEPENDENCIES ------------------------------------------------------------

install_and_load_packages <- function() {
  if (!requireNamespace("synthpop", quietly = TRUE)) {
    install.packages("synthpop")
  }
  library(synthpop)
}


# LOAD AND DATA CLEANING --------------------------------------------------

# Read and clean data from a CSV 

read_and_clean_csv_data <- function(file_path) {
  data <- read.csv(file_path)
  # Clean the data
  cleaned_data <- clean_data(data)
  return(cleaned_data)

}

# Clean Data:
# We remove rows with null values, outliers, normalize data, 
# encode categorical variables, handle missing data, remove duplicates and validate data

clean_data <- function(data) {
  # Remove rows with null values
  cleaned_data <- na.omit(data)
  
  # We erase outliers
  cleaned_data <- remove_outliers(cleaned_data)
  
  # Normalization: 
  for (col in names(cleaned_data)[-which(names(cleaned_data) == "Outcome")]) {
    cleaned_data[[col]] <- (cleaned_data[[col]] - min(cleaned_data[[col]])) / (max(cleaned_data[[col]]) - min(cleaned_data[[col]]))
  }
  
  # Encoding categorical variables: One-hot encoding for categorical variables (in this case "Gender")
  if ("Gender" %in% colnames(cleaned_data)) {
    cleaned_data <- cbind(cleaned_data, model.matrix(~ Gender - 1, data = cleaned_data))
    cleaned_data <- cleaned_data[, -which(names(cleaned_data) == "Gender")]
  }
  
  # Handling missing data: Mean imputation for missing values
  for (col in names(cleaned_data)[-which(names(cleaned_data) == "Outcome")]) {
    cleaned_data[[col]][is.na(cleaned_data[[col]])] <- mean(cleaned_data[[col]], na.rm = TRUE)
  }
  
  # Duplicates removal: 
  cleaned_data <- unique(cleaned_data)
  
  # Data validation: We check for negative values in certain columns
  # For example, if Age should always be non-negative:
  if (any(cleaned_data$Age < 0)) {
    stop("Age column contains negative values.")
  }
  
  return(cleaned_data)
}



# Model Training ---------------------------------------------------------

train_linear_model <- function(data) {
  # Lineal Regression 
  formula <- as.formula(paste("Outcome ~", paste(names(data)[-which(names(data) == "Outcome")], collapse = " + ")))
  model <- lm(formula, data = data)
  return(model)
}


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


# Functions use case ---------------------------------------------------------------
install_and_load_packages()

# Here you need to replace "Your_Data.csv" with the path to your CSV file
data <- read_and_clean_csv_data("Your_Data.csv")

# Train the linear regression model
model <- train_linear_model(data)

# Generation of synthetic data based on the trained model
synthetic_data <- generate_synthetic_data(model, data, 1000) # Here we use 1000 samples as an example

# Evaluation of model performance
evaluation_result <- evaluate_model(model, data, synthetic_data)
print(paste("Mean Squared Error on original data:", evaluation_result$mse_original))
print(paste("Mean Squared Error on synthetic data:", evaluation_result$mse_synthetic))
print(evaluation_result$result)

# Visualization of data
visualize_data(data, synthetic_data)

