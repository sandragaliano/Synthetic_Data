# Read and clean data from a CSV 

read_and_clean_csv_data <- function(file_path) {
  data <- read.csv(file_path)
  # Clean the data
  cleaned_data <- clean_data(data)
  return(cleaned_data)

}

# Clean Data:
# We remove rows with null values, normalize data, encode categorical variables, handle missing data, remove duplicates and validate data

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
