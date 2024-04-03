# Model Training ---------------------------------------------------------
train_linear_model <- function(data) {
  # Lineal Regression
  formula <- as.formula(paste("Outcome ~", paste(names(data)[-which(names(data) == "Outcome")], collapse = " + ")))
  model <- lm(formula, data = data)
  return(model)
}
