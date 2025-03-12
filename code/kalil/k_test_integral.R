# Set working directory
setwd(paste("/home/kalil/Documents/Graduacao/FGV/IC/", "joint-models-in-stan", sep = "/"))

# Load required libraries
library("rstan")

# Define parameters for integration
t0 <- 0
t1 <- 1

anal_sol <- function(alfa, beta, gamma, t0, t1) {
  lgamma(gamma) - gamma * log(abs(alfa * beta)) + 
    pgamma(t1, shape = gamma,
           rate  = abs(alfa * beta),
           log.p = TRUE)
}


# Define the function to compare areas
compare_areas <- function(alfa, beta, gamma, t0, t1, stan_model_obj) {
  # Definir a função de integração para R
  integrand <- function(u) {
    (gamma - 1) * log(u) + alfa * beta * u
  }
  
  # Realizar a integração em R
  area <- tryCatch({
    integrate(integrand, t0, t1)
  }, error = function(e) {
    message("Erro na integração em R: ", e)
    return(NULL)
  })
  
  if (is.null(area)) return(NULL)
  
  R_area <- area$value
  R_error <- area$abs.error  # Erro absoluto da integração em R
  print(paste("Área calculada em R:", R_area))
  
  # Dados para o modelo Stan
  data_list <- list(
    t0 = t0,
    t1 = t1,
    alfa = alfa,
    beta = beta,
    gamma = gamma
  )
  
  # Executar o modelo Stan
  fit <- tryCatch({
    sampling(stan_model_obj, data = data_list, iter = 1, chains = 1, algorithm = "Fixed_param")
  }, error = function(e) {
    message("Erro na amostragem Stan: ", e)
    return(NULL)
  })
  
  if (is.null(fit)) return(NULL)
  
  # Extrair o resumo do objeto fit
  fit_summary <- summary(fit)
  
  # Acessar o valor médio da 'área' no resumo
  STAN_area <- fit_summary$summary["area", "mean"]
  
  # Solução analítica (integral entre 0 e 1)
  AS_area <- anal_sol(alfa, beta, gamma, t0, t1)
  
  # Retornar as áreas e erros para comparação
  return(list(R_area = R_area, R_error = R_error, STAN_area = STAN_area, AS_area = AS_area))
}


# Compile the Stan model once
stan_model_obj <- stan_model("code/k_integrate_example.stan")

# Generate values for alfa, beta, and gamma
alfa_values <- seq(-10, 10, length.out = 10)
beta_values <- seq(-10, 10, length.out = 10)
gamma_values <- seq(0.1, 10, length.out = 5)

# Initialize list to store results
areas_list <- list()

# Check how many lines are already in the file
output_file_path <- "r_area.txt"
if (file.exists(output_file_path)) {
  lines <- readLines(output_file_path)
  num_lines <- length(lines)
} else {
  num_lines <- 0
}

# Calculate the starting index based on the number of lines already written
start_index <- num_lines + 1
i <- 0

# Loop over all combinations of alfa, beta, and gamma
while (t1 <= 1) {
  for (alfa in alfa_values) {
    for (beta in beta_values) {
      for (gamma in gamma_values) {
        i <- i + 1
        #if (i < start_index) {
        #  next
        #}
        areas <- compare_areas(alfa, beta, gamma, t0, t1, stan_model_obj)
        
        if (is.null(areas)) {
          next  # Skip if there was an error in R or Stan
        }
        
        areas_list[[paste("alfa", alfa, "beta", beta, "gamma", gamma, sep = "_")]] <- areas
        output_file <- file(output_file_path, open = "a")
        
        last_key <- paste("alfa", alfa, "beta", beta, "gamma", gamma, sep = "_")
        R_area <- areas$R_area
        STAN_area <- areas$STAN_area
        difference <- R_area - STAN_area
        R_error <- areas$R_error
        STAN_error <- areas$STAN_error
        AS_area <- areas$AS_area
        
        # Write the key, R_area, STAN_area, difference, and errors to the file
        cat(paste(last_key, ", ", t0, ", ", t1, ", ", AS_area, ", ",  R_area, ", ", STAN_area, 
                  ", ", R_error, ", ", R_area-STAN_area, ", ", R_area-AS_area, ", ", STAN_area-AS_area, "\n"), file = output_file)
        
        close(output_file)
        gc()
      }
    }
  }
  t1 <- t1 + 1
}

