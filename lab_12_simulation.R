generate_data <- function(n, p) {
  covariates <- matrix(rnorm(n*p), nrow = n, ncol = p)
  responses <- rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

