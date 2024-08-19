# Set working directory
setwd("/home/kalil/Documents/Graduacao/FGV/IC/joint-models-in-stan")

# Load required library
library(rstan)

# Define parameters for integration
alfa <- -10
beta <- -10
gamma <- 0.1
t0 <- 1
t1 <- 2

# Define the integrand function
integrand <- function(u) {
  u^(gamma - 1) * exp(alfa * beta * u)
}

# Perform the integration
area <- integrate(integrand, t0, t1)

# Define data to pass to Stan model
data_list <- list(
  t0 = 1.0,
  t1 = 2.0,
  alfa = -10.0,
  beta = -10.0,
  gamma = 0.1
)

# Compile the Stan model
stan_model_obj <- stan_model("code/integrate_example.stan")

# Run the Stan model
fit <- sampling(stan_model_obj, data = data_list, iter = 1, chains = 1, algorithm = "Fixed_param")

# Print the results
print(fit)
