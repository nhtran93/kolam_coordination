data {
  int<lower=0> N; // Individuals
  int<lower=1> d; // Matrix Dimension
  int<lower=0> y[N, d, d]; // transition counts as array
}

parameters {
  matrix[d, d] mu;
  matrix[d, d] z[N];
  real<lower=0> sigma[d];
}
transformed parameters {
  simplex[d] pi[N, d];
  vector[N] log_lik = rep_vector(0.0, N);
  
  for(ind in 1:N) {
    for(row in 1:d) {
      pi[ind, row] = softmax(to_vector(mu[row,] + sigma[row]*z[ind, row]));
      log_lik[ind] += multinomial_lpmf(y[ind, row, ] | pi[ind, row]);
    }
  }
}
model {
  // priors
  target += std_normal_lpdf(to_vector(mu));
  target += gamma_lpdf(to_vector(sigma) | 2, 5);
  for(ind in 1:N) target += std_normal_lpdf(to_vector(z[ind]));
  
  // likelihood
  target += sum(log_lik);
}





