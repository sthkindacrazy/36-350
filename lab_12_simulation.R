library(tidyverse)
library(gridExtra)

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

# helper function which combines generate_data, and model_select
generate_and_select <- function(n, p, cutoff) {
  gen_data <- generate_data(n, p)
  p_vals <- model_select(gen_data$covariates, gen_data$responses, cutoff)
  return(p_vals)
}

run_simulation <- function(n_trials, n, p, cutoff) {
  collected_p <- replicate(n_trials, generate_and_select(n, p, cutoff))
  p_values <- as.numeric(unlist(collected_p))
  ggplot(data.frame(p_values)) + geom_histogram(aes(x = p_values)) + labs(x = "p values", title = paste("plot of", c(n, p, cutoff)))
}


gghist <- list()
index <- 1

for (n in c(100, 1000, 10000)) {
  for (p in c(10, 20, 50)) {
    gghist[[index]] <- run_simulation(1000, n, p, 0.05)
    index <- index + 1
  }
}

grid.arrange(grobs = gghist)


