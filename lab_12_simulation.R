generate_data <- function(n, p) {
  covariates <- matrix(rnorm(n*p), nrow = n, ncol = p)
  responses <- rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select <- function(covariates, responses, cutoff) {
  reg_m <- lm(responses ~ covariates)
  p_values <- summary(reg_m)$coefficients[,"Pr(>|t|)"][-1]
  index_p <- which(p_values <= cutoff)
  if (length(index_p) == 0) {
    return(vector())
  } else {
    new_reg <- lm(responses ~ covariates[, index_p])
    return (summary(new_reg)$coefficients[,"Pr(>|t|)"][-1])
  }
}