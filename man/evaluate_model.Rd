\name{evaluate_model}
\alias{evaluate_model}
\title{evaluate_model}
\usage{evaluate_model(model, data, synthetic_data)
\description{Evaluate_model Function assesses the performance of a given model using both original and synthetic data. It randomly splits the original data into training (70%) and testing (30%) sets. The model's predictions are evaluated on both original and synthetic test datasets, calculating mean squared error (MSE) for each. By comparing MSE values, the function determines which model performs better: the one trained on original data or the one trained on synthetic data.    Compare_correlations Function compares the correlation matrices between original and synthetic data. It calculates correlation matrices for both datasets, excluding the "Outcome" column.
}
\examples{
evaluation_result <- evaluate_model(model, data, synthetic_data)}
