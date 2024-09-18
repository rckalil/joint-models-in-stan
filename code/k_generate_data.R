library("MASS") 
library("cmdstanr")

setwd(paste("/home/kalil/Documents/Graduacao/FGV/IC/",
            "joint-models-in-stan", sep = "/"))

sim_data <- function(N, 
                     lambda, 
                     rho_s, 
                     cens_time, 
                     beta, 
                     gamma, 
                     var_u, 
                     var_z, 
                     rho, 
                     n_rep_obs){
  
  times <- id <- long_out <- x_total <- vector()
  
  beta_11 <- beta[1]
  beta_12 <- beta[2]
  beta_13 <- beta[3]
  beta_21 <- beta[4]
  
  gamma_1 <- gamma[1] 
  gamma_2 <- gamma[2]
  gamma_3 <- gamma[3]
  
  mu_u1 <- 0
  mu_u2 <- 0
  mu_u <- c(mu_u1,mu_u2)
  var_u1 <- var_u[1]
  var_u2 <- var_u[2]
  var_u3 <- var_u[3]
  sigma <- matrix(c(var_u1, sqrt(var_u1*var_u2)*rho, 
                    sqrt(var_u1*var_u2)*rho, var_u2), 2)
  bvn_u <- mvrnorm(N, mu = mu_u, Sigma = sigma)
  u_1 <- bvn_u[,1]
  u_2 <- bvn_u[,2]
  u_3 <- rnorm(N, 0, sqrt(var_u3))
  
  x <- rnorm(N, 0, 1)
  
  ###################
  # Survival process
  ###################
  
  v <- runif(N)
  id_times <- c(1:N)
  
  for(i in 1:N){
    haz <- function(s) {
      lres <- log(lambda) + log(rho_s) + (rho_s-1)*log(s) + beta_21*x[i] +
        gamma_1*u_1[i] + gamma_2*u_2[i] + gamma_3*(u_1[i] + u_2[i]*s) +
        u_3[i]
      return(exp(lres))
    }
    
    cum_haz <- function(t) {
      res <- integrate(haz, 0, t)$value
      return(res)
    }
    
    sv <- function(t) (exp(-cum_haz(t)) - v[i])^2
    times[i] <- optim(1, sv, lower = 1e-6, upper = Inf, method = "L-BFGS-B")$par
  }
  
  status <- as.vector(times < cens_time)
  times <- as.vector(ifelse(status, times, cens_time))
  status <- as.numeric(status)
  
  ##############################
  # Longitudinal process  
  ##############################
  
  obs_times_out <- vector()
  for(i in 1:N){
    obs_times <- seq(0, times[i], by = n_rep_obs) 
    
    x_t <- rep(x[i], length(obs_times))
    
    x_total <- c(x_total, x_t)
    z = rnorm(length(obs_times), 0, sqrt(var_z))
    y_t <- beta_11 + beta_12*obs_times + beta_13*rep(x[i], length(obs_times)) + 
      rep(u_1[i], length(obs_times)) + rep(u_2[i], length(obs_times))*
      obs_times + z
    
    long_out <- c(long_out, y_t)
    id <- c(id, rep(i, length(obs_times)))
    obs_times_out <- c(obs_times_out, obs_times)
  }
  
  ##############################
  # Salvando os dados
  ##############################
  N <- length(id_times)                # number of subjects
  n_obs <- length(long_out)            # total number of observations
  x_total <- x_total                   # repeated x for longitudinal data
  obs_times <- obs_times_out           # visit times for repeated observations
  y <- long_out                        # longitudinal outcomes
  ind_unc_times <- which(status == 1)  # uncensored times indicator
  n_unc_times <- length(ind_unc_times) # number of uncensored times
  
  # Expandir `times` e `status` para o número de observações longitudinais
  times_expanded <- rep(times, times = table(id))  # Replicar `times` por sujeito
  status_expanded <- rep(status, times = table(id))  # Replicar `status` por sujeito
  
  # Salvando os dados longitudinais
  long_data <- data.frame(id = id,
                          y = y,
                          obs_times = obs_times)
  write.csv(long_data, file = "data/long_data.csv", row.names = FALSE)
  
  # Salvando os dados de eventos de sobrevivência
  event_data <- data.frame(id = id_times,
                           x = x,
                           times = times,
                           status = status)
  write.csv(event_data, file = "data/event_data.csv", row.names = FALSE)
  
  # Salvando os dados conjuntos
  joint_data <- data.frame(id = id,
                           y = y,
                           obs_times = obs_times,
                           x = x_total,
                           times = times_expanded,  # Usar a versão expandida
                           status = status_expanded)  # Usar a versão expandida
  write.csv(joint_data, file = "data/joint_data.csv", row.names = FALSE)
  
}
