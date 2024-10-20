data {
  int<lower = 1> T;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations
  array[N, P] int<lower = 0> y;           // Polling data (counts per party)
  array[N] int<lower = 1, upper = H> house; // House indicator for each poll
  array[N] int<lower = 1, upper = T> date;  // Date indicator for each poll

  int<lower = 1> N_now;                   // Number of expected voters
}

parameters {
  matrix[P, T] beta;                     // Dynamic party effects over time
  matrix[P, H] gamma;                    // House effects (constant over time for each house)
}

model {
  // Priors for beta (dynamic main effects for each party)
  for (p in 1:P) {
    beta[p, 1] ~ normal(0, 1);           // Prior for initial state of beta
  }

  // Priors for gamma (house effects)
  for (h in 1:H) {
    gamma[, h] ~ normal(0, 1);             // Constant house effects
    sum(gamma[, h]) ~ normal(0, 0.01);
  }

  

  // Likelihood (Multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n = beta[ , date[n]] + gamma[ , house[n]];  // Linear predictor for softmax
    vector[P] pi_n = softmax(eta_n);                // Party probabilities

    y[n, ] ~ multinomial(pi_n);                       // Polling data likelihood
  }
}

generated quantities {
  array[P] int<lower = 0> y_now;
  vector[P] pi_now = softmax(beta[ , date[N]] + gamma[ , house[N]]);
  y_now = multinomial_rng(pi_now, N_now);
}
