set.seed(25072023)

setwd("C:/Users/rodri/Documents/joint-models-in-stan/")
source("code/generate_data.R")
set_cmdstan_path("C:/Users/rodri/.cmdstan/cmdstan-2.36.0")
N<- 250
lambda <- 0.4
rho_s <- 0.7
cens_time <- 4
beta <- c(0,1,1,1)
gamma <- c(-1.5,0,2)
var_u <- c(0.5,1,0.25)
var_z <- 0.25
rho <- 0
n_rep_obs <- 0.5

print("Parameters chosen")

sim_data(N, 
         lambda, 
         rho_s, 
         cens_time, 
         beta, 
         gamma, 
         var_u, 
         var_z, 
         rho, 
         n_rep_obs)

print("Simulation runned")

load("data/joint_data.RData")

print("Loaded data")

long_model <- cmdstan_model("code/long_model.stan")
long_posterior_samples <- long_model$sample(data = joint_data,
                                            chains = 4, 
                                            parallel_chains = 4)
long_posterior_samples$summary(c("beta_1",
                                 "var_z",
                                 "var_u", 
                                 "rho"))
print("Long model fitted")

event_model <- cmdstan_model("code/event_model.stan")
event_posterior_samples <- event_model$sample(data = joint_data, 
                                              chains = 4, 
                                              parallel_chains = 4)
event_posterior_samples$summary(c("beta_21", 
                                  "lambda",
                                  "rho_s",
                                  "var_u3"))

print("Event model fitted")


joint_model <- cmdstan_model("code/joint_model.stan")

# very slow with more iterations:
joint_posterior_samples <- joint_model$sample(data = joint_data,
                                              chains = 4, 
                                              parallel_chains = 4,
                                              iter_warmup = 500,
                                              iter_sampling = 500)
                                            
print("Joint model fitted")

joint_posterior_samples$summary(c("beta_1", 
                                  "beta_21", 
                                  "gamma", 
                                  "lambda", 
                                  "rho_s", 
                                  "var_z", 
                                  "var_u", 
                                  "rho", 
                                  "var_u3"))


print(joint_posterior_samples$summary(c("beta_1", 
                                        "beta_21", 
                                        "gamma", 
                                        "lambda", 
                                        "rho_s", 
                                        "var_z", 
                                        "var_u", 
                                        "rho", 
                                        "var_u3")))