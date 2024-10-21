data {
  int<lower = 1> T;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations
  array[N, P] int<lower = 0> y;           // Polling data (counts per party)
  array[N] int<lower = 1, upper = H> house; // House indicator for each poll
  array[N] int<lower = 1, upper = T> date;  // Date indicator for each poll
}

parameters {
  matrix[P, T] beta;                     // Dynamic party effects over time
  matrix[P, H] gamma;                    // House effects (constant over time for each house)
}

model {
  // Priors for beta (dynamic main effects for each party)
  for (p in 1:P) {
    beta[p, 1] ~ normal(0, 1);           // Prior for initial state of beta
    for (t in 2:T) {
      beta[p, t] ~ normal(beta[p, t - 1], 1);
    }
    gamma[p, ] ~ normal(0, 1);
    sum(gamma[p, ]) ~ normal(0, 0.01);
  }


  

  // Likelihood (Multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n = beta[ , date[n]] + gamma[ , house[n]];  // Linear predictor for softmax
    vector[P] pi_n = softmax(eta_n);                // Party probabilities
    y[n, ] ~ multinomial(pi_n);                       // Polling data likelihood
  }
}

generated quantities {
  array[T, P] int<lower = 0> y_rep;
  matrix[T, P] pi_rep;
  int n = 10000;
  for (t in 1:T) {
    y_rep[t, ] = multinomial_rng(softmax(beta[ , t]), n);
    for (p in 1:P) {
      pi_rep[t, p] = 1.0 * y_rep[t, p] / sum(y_rep[t, ]);
    }
  }
}
