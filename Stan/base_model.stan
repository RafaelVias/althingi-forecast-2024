data {
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations

  array[N, P] int<lower = 0> y;           // Polling data (counts per party)

  array[N] int<lower = 1, upper = H> house; // House indicator for each poll
  array[N] int<lower = 1, upper = D> date;  // Date indicator for each poll
  vector[D - 1] time_diff;
  real pred_y_time_diff;

  real<lower = 0> sigma_house_sum;
  int<lower = 1> n_pred;
}

parameters {
  vector[P] beta_0;                      // Party-specific initial effect
  matrix[P, D + 1] z_beta;                   // Standardized random walk innovations
  matrix[P, H - 1] gamma_raw;            // House effects (constant over time for each house)
  vector<lower = 0>[P] sigma;            // Party-specific random walk scale
}

transformed parameters {
  matrix[P, H] gamma;
  matrix[P, D + 1] beta;                     // Dynamic party effects over time
  for (p in 1:P) {
    gamma[p, 1] = 0;                   // Fix the first house effect of the election to zero
    gamma[p, 2:H] = gamma_raw[p];      // Free parameters for other houses
  }

  for (p in 1:P) {
    beta[p, 1] = beta_0[p];
    for (t in 2:D) {
      beta[p, t] = beta[p, t - 1] + z_beta[p, t] * sigma[p] * time_diff[t - 1];
    }

    beta[p, D + 1] = beta[p, D] + z_beta[p, D + 1] * sigma[p] * pred_y_time_diff;
  }
}

model {
  // Priors for beta (dynamic main effects for each party)
  for (p in 1:P) {
    gamma_raw[p, ] ~ normal(0, 1);      // House effects prior
    sum(gamma_raw[p, ]) ~ normal(0, sigma_house_sum * sqrt(H - 1)); // Soft sum-to-zero constraint to allow a little combined bias
  }

  beta_0 ~ normal(0, 1);
  to_vector(z_beta) ~ normal(0, 1);
  sigma ~ exponential(1);                 // Random walk scale prior

  // Likelihood (Multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n = beta[ , date[n]] + gamma[ , house[n]];  // Linear predictor for softmax
    y[n, ] ~ multinomial_logit(eta_n);                // Polling data likelihood
  }
}

generated quantities {
  array[D + 1, P] int<lower = 0> y_rep;
  for (d in 1:(D + 1)) {
    y_rep[d, ] = multinomial_logit_rng(beta[ , d], n_pred);
  }
}
