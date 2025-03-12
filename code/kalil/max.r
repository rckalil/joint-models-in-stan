## test numerical stability
gamma <- 5
alfa <- 10 #3.33 
beta <- -1.11  
integrand <- function(u) {
  u^(gamma - 1) * exp(alfa * beta * u)
}

integrand_ns <- function(u) {
  exp(
    (gamma - 1)* log(u) + alfa * beta * u
  )
}

anal_sol <- function(t){
  exp(
    lgamma(gamma) - gamma * log(abs(alfa * beta)) + 
      pgamma(t, shape = gamma,
             rate  = abs(alfa * beta),
             log.p = TRUE)
  )
}

uu <- 5

integrand(uu)
integrand_ns(uu)


## integral

integrate(integrand, 0, 1)
integrate(integrand_ns, 0, 1)
anal_sol(1)
