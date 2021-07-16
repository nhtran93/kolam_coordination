data {
  int<lower=0> N; // Individuals
  int<lower=0> C; // Number of caste
  int<lower=0> caste[N]; // caste
  int<lower=1> d; // Matrix Dimension
  vector[N] duration; // Duration of practice
  int<lower=0> y[N, d, d]; // transition counts as array
}

parameters {
  matrix[d, d] mu;
  matrix[d, d] z[N];
  real<lower=0> sigma;
  matrix[d, d] z_caste[C];
  real<lower=0> sigma_caste;
  matrix[d,d] beta_duration ;
}
transformed parameters {
  simplex[d] pi[N, d];
  vector[N] log_lik = rep_vector(0.0, N);
  
  for(ind in 1:N) {
    for(row in 1:d) {
      pi[ind, row] = softmax(to_vector(mu[row,] + beta_duration[row,]*duration[ind] + sigma*z[ind, row] + sigma_caste*z_caste[caste[ind], row]));
      log_lik[ind] += multinomial_lpmf(y[ind, row, ] | pi[ind, row]);
    }
  }
}
model {
  // priors
  target += std_normal_lpdf(to_vector(mu));
  target += gamma_lpdf(sigma | 2, 5);
  target += gamma_lpdf(sigma_caste | 2, 5);
  target += std_normal_lpdf(to_vector(beta_duration));
  for(ind in 1:N) target += std_normal_lpdf(to_vector(z[ind]));
  for(ind in 1:C) target += std_normal_lpdf(to_vector(z_caste[ind]));
  
  // likelihood
  target += sum(log_lik);
}

