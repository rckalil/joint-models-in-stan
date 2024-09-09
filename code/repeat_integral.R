repeat {
  tryCatch({
    # Set working directory
    setwd(paste("/home/kalil/Documents/Graduacao/FGV/IC/", "joint-models-in-stan", sep = "/"))
    
    # Load required libraries
    library("rstan")
    
    # Define parameters for integration
    t0 <- 0
    t1 <- 1
    
    # Define the function to compare areas
    compare_areas <- function(alfa, beta, gamma, t0, t1) {
      # Define the integrand function
      integrand <- function(u) {
        (gamma - 1)*log(u) + alfa * beta * u
      }
      
      # Perform the integration in R
      area <- integrate(integrand, t0, t1)
      R_area <- area$value
      R_error <- area$abs.error  # Absolute error from R integration
      print(paste("R area:", R_area))
      
      # Define data to pass to Stan model
      data_list <- list(
        t0 = t0,
        t1 = t1,
        alfa = alfa,
        beta = beta,
        gamma = gamma
      )
      
      # Compile the Stan model
      stan_model_obj <- stan_model("code/k_integrate_example.stan")
      
      # Run the Stan model
      fit <- sampling(stan_model_obj, data = data_list, iter = 1, chains = 1, algorithm = "Fixed_param")
      
      # Extract the summary of the fit object
      fit_summary <- summary(fit)
      
      # Access the mean value of 'area' from the summary
      STAN_area <- fit_summary$summary["area", "mean"]
      STAN_error <- fit_summary$summary["area", "sd"]  # Standard deviation as a measure of uncertainty
      
      # Return the areas and errors for comparison
      return(list(R_area = R_area, R_error = R_error, STAN_area = STAN_area, STAN_error = STAN_error))
    }
    
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
    for (alfa in alfa_values) {
      for (beta in beta_values) {
        for (gamma in gamma_values) {
          i <- i + 1
          if (i < start_index) {
            next
          }
          areas <- compare_areas(alfa, beta, gamma, t0, t1)
          areas_list[[paste("alfa", alfa, "beta", beta, "gamma", gamma, sep = "_")]] <- areas
          output_file <- file(output_file_path, open = "a")
          
          last_key <- paste("alfa", alfa, "beta", beta, "gamma", gamma, sep = "_")
          R_area <- areas$R_area
          STAN_area <- areas$STAN_area
          difference <- R_area - STAN_area
          R_error <- areas$R_error
          STAN_error <- areas$STAN_error
          
          # Write the key, R_area, STAN_area, difference, and errors to the file
          cat(paste(last_key, "R:", R_area, "Stan:", STAN_area, "Difference:", difference,
                    "R_Error:", R_error, "Stan_Error:", STAN_error, "\n"), file = output_file)
          
          close(output_file)
        }
      }
    }
    
  }, error = function(e) {
    # Handle memory limit exceeded error
    if (grepl("cannot allocate vector of size", e$message)) {
      cat("Memory limit exceeded. Restarting script...\n")
      gc()  # Clear some memory before retrying
    } else {
      # If the error is something else, stop the loop
      stop("An unexpected error occurred: ", e$message)
    }
  })
  
  # Optional: Check for a stopping condition
  # if (some_condition) break;
}
