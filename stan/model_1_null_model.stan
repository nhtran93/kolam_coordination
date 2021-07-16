data {
  int<lower=0> N; // Individuals
  int<lower=1> d; // Matrix Dimension
  int<lower=0> y[N, d, d]; // transition counts as array
}

parameters {
  simplex[d] pi[d];
}

transformed parameters {
  vector[N] log_lik = rep_vector(0.0, N);
  
  for(ind in 1:N) {
    for(row in 1:d) {
      log_lik[ind] += multinomial_lpmf(y[ind, row, ] | pi[row]);
    }
  }
}
model {
  for(i in 1:d){
    target += dirichlet_lpdf(pi[i] | rep_vector(1, d));
  }
  
  target += sum(log_lik);
}

