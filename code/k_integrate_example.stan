functions {
  // Define the integrand function
  real integrand(real u, real xc, real[] theta, real[] x, int[] x_int) {
    real alfa = theta[1];
    real beta = theta[2];
    real gamma = theta[3];
    return pow(u, gamma - 1) * exp(alfa * beta * u);
  }
}

data {
  real<lower=0> t0;       // Lower bound of integration
  real<lower=t0> t1;      // Upper bound of integration
  real alfa;              // Parameter alfa
  real beta;              // Parameter beta
  real gamma;             // Parameter gamma
}

transformed data {
  real theta[3];          // Parameters passed to the integrand
  theta[1] = alfa;
  theta[2] = beta;
  theta[3] = gamma;
}

generated quantities {
  real area;              // Area under the curve
  area = integrate_1d(integrand, t0, t1, theta, rep_array(0.0, 0), rep_array(0, 0), 1e-8);
}
