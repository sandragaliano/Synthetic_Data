\name{generate_synthetic_data}
\alias{generate_synthetic_data}
\title{generate_synthetic_data}
\usage{synthetic_data <- generate_synthetic_data(model, data, n_samples)}
\description{This function automates the creation of synthetic datasets mirroring the original data.
Key steps include: generating random values for independent variables within the original data range, predicting dependent variable values using a trained model, and combining them to form synthetic datasets.    A crucial validation ensures the synthetic data dimensions match the original dataset. This function enables efficient exploration and validation of datasets without compromising privacy or security.}
\examples{ synthetic_data <- generate_synthetic_data(model, data, 768) # Here we use 768 because the size of the original dataset (data) is 768, but you can change it depending on your dataset size
}
