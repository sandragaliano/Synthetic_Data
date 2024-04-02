
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

renv::snapshot()
