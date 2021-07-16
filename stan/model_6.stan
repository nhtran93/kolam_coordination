data {
  int<lower=0> N; // Individuals
  int<lower=0> cross[N, 3, 3]; // transition counts as array
  int<lower=0> ortho[N, 6, 6]; // transition counts as array
  int<lower=0> trans[N, 4, 4]; // transition counts as array
  int<lower=0> diag[N, 4, 4]; // transition counts as array
  int<lower=0> C; // Number of caste
  int<lower=0> caste[N]; // caste
  vector[N] duration; // Duration of practice
}

parameters {
  matrix[3, 3] mu_cross;
  matrix[6, 6] mu_ortho;
  matrix[4, 4] mu_trans;
  matrix[4, 4] mu_diag;
  matrix[3, 3] z_cross[N];
  matrix[6, 6] z_ortho[N];
  matrix[4, 4] z_trans[N];
  matrix[4, 4] z_diag[N];
  real<lower=0> sigma_cross[3];
  real<lower=0> sigma_ortho[6];
  real<lower=0> sigma_trans[4];
  real<lower=0> sigma_diag[4];
  matrix[3, 3] z_caste_cross[C];
  matrix[6, 6] z_caste_ortho[C];
  matrix[4, 4] z_caste_trans[C];
  matrix[4, 4] z_caste_diag[C];
  real<lower=0> sigma_caste_cross[3];
  real<lower=0> sigma_caste_ortho[6];
  real<lower=0> sigma_caste_trans[4];
  real<lower=0> sigma_caste_diag[4];
  matrix[3, 3] beta_duration_cross;
  matrix[6, 6] beta_duration_ortho;
  matrix[4, 4] beta_duration_trans;
  matrix[4, 4] beta_duration_diag;
}
transformed parameters {
  simplex[3] pi_cross[N, 3];
  simplex[6] pi_ortho[N, 6];
  simplex[4] pi_trans[N, 4];
  simplex[4] pi_diag[N, 4];
  vector[N] log_lik = rep_vector(0.0, N);
  
  for(ind in 1:N) {
    for(row in 1:3) {
      pi_cross[ind, row] = softmax(to_vector(mu_cross[row,] + beta_duration_cross[row,]*duration[ind] + sigma_cross[row]*z_cross[ind, row] + sigma_caste_cross[row]*z_caste_cross[caste[ind], row]));
      
      log_lik[ind] += multinomial_lpmf(cross[ind, row, ] | pi_cross[ind, row]);
    }
    
    for(row in 1:6) {
      pi_ortho[ind, row] = softmax(to_vector(mu_ortho[row,] + beta_duration_ortho[row,]*duration[ind] + sigma_ortho[row]*z_ortho[ind, row] + sigma_caste_ortho[row]*z_caste_ortho[caste[ind], row]));
      
      log_lik[ind] += multinomial_lpmf(ortho[ind, row, ] | pi_ortho[ind, row]);
    }
    
    for(row in 1:4) {
      pi_trans[ind, row] = softmax(to_vector(mu_trans[row,] + beta_duration_trans[row,]*duration[ind] + sigma_trans[row]*z_trans[ind, row] + sigma_caste_trans[row]*z_caste_trans[caste[ind], row]));
      pi_diag[ind, row] = softmax(to_vector(mu_diag[row,] + beta_duration_diag[row,]*duration[ind] + sigma_diag[row]*z_diag[ind, row] + sigma_caste_diag[row]*z_caste_diag[caste[ind], row]));
      
      log_lik[ind] += multinomial_lpmf(trans[ind, row, ] | pi_trans[ind, row]);
      log_lik[ind] += multinomial_lpmf(diag[ind, row, ] | pi_diag[ind, row]);
    }
  }
}
model {
  // priors
  for(row in 1:3) {
    for(col in 1:3) {
      // put low priors on transitioning ortho (1) <--> diag (3)
      if((row == 1 && col == 3) || (row == 3 && col == 1)) { 
        target += normal_lpdf(mu_cross[row, col] | -1000, 1);
      } else {
        target += std_normal_lpdf(mu_cross[row, col]);
      }
    }
  }
  target += std_normal_lpdf(to_vector(mu_ortho));
  target += std_normal_lpdf(to_vector(mu_trans));
  target += std_normal_lpdf(to_vector(mu_diag));
  target += gamma_lpdf(to_vector(sigma_cross) | 2, 5);
  target += gamma_lpdf(to_vector(sigma_ortho) | 2, 5);
  target += gamma_lpdf(to_vector(sigma_trans) | 2, 5);
  target += gamma_lpdf(to_vector(sigma_diag) | 2, 5);
  target += std_normal_lpdf(to_vector(beta_duration_cross));
  target += std_normal_lpdf(to_vector(beta_duration_ortho));
  target += std_normal_lpdf(to_vector(beta_duration_trans));
  target += std_normal_lpdf(to_vector(beta_duration_diag));
  target += gamma_lpdf(to_vector(sigma_caste_cross) | 2, 5);
  target += gamma_lpdf(to_vector(sigma_caste_ortho) | 2, 5);
  target += gamma_lpdf(to_vector(sigma_caste_trans) | 2, 5);
  target += gamma_lpdf(to_vector(sigma_caste_diag) | 2, 5);
  for(ind in 1:N){
    target += std_normal_lpdf(to_vector(z_cross[ind]));
    target += std_normal_lpdf(to_vector(z_ortho[ind]));
    target += std_normal_lpdf(to_vector(z_trans[ind]));
    target += std_normal_lpdf(to_vector(z_diag[ind]));
  }
  for(ind in 1:C){
    target += std_normal_lpdf(to_vector(z_caste_cross[ind]));
    target += std_normal_lpdf(to_vector(z_caste_ortho[ind]));
    target += std_normal_lpdf(to_vector(z_caste_trans[ind]));
    target += std_normal_lpdf(to_vector(z_caste_diag[ind]));
  }
  
  // likelihood
  target += sum(log_lik);
}





