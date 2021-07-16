rm(list = ls())
library(rstan)
library(rethinking)
library(loo)
load("data/data.RData")

cross <- lapply(artist_trans_count_matrices, function(x) {
  idx <- list(
    ortho = c('o1', 'o2', 'o3', 'o4', 'h3', 'op'),
    trans = c('t1', 't2', 't3', 't4'),
    diag  = c('d1', 'd2', 'd3', 'd4')
  )
  mat <- matrix(NA, 3, 3)
  colnames(mat) <- rownames(mat) <- names(idx)
  
  for(row in rownames(mat)) {
    for(col in colnames(mat)) {
      mat[row,col] <- sum(x[idx[[row]], idx[[col]]])
    }
  }
  
  mat
})

cross_kolam <- lapply(all_trans_count_matrices, function(x) {
  idx <- list(
    ortho = c('o1', 'o2', 'o3', 'o4', 'h3', 'op'),
    trans = c('t1', 't2', 't3', 't4'),
    diag  = c('d1', 'd2', 'd3', 'd4')
  )
  mat <- matrix(NA, 3, 3)
  colnames(mat) <- rownames(mat) <- names(idx)
  
  for(row in rownames(mat)) {
    for(col in colnames(mat)) {
      mat[row,col] <- sum(x[idx[[row]], idx[[col]]])
    }
  }
  
  mat
})


data_list <- list(
  N = length(unique(df$artist_id)),
  duration = (df[!duplicated(df$artist_id), "duration_practice"] - mean(df[!duplicated(df$artist_id), "duration_practice"]))/sd(df[!duplicated(df$artist_id), "duration_practice"]),
  cross = cross,
  ortho = lapply(artist_trans_count_matrices, function(x) x[c('o1', 'o2', 'o3', 'o4', 'h3', 'op'), c('o1', 'o2', 'o3', 'o4', 'h3', 'op')]),
  diag = lapply(artist_trans_count_matrices, function(x) x[c('d1', 'd2', 'd3', 'd4'), c('d1', 'd2', 'd3', 'd4')]),
  trans = lapply(artist_trans_count_matrices, function(x) x[c('t1', 't2', 't3', 't4'), c('t1', 't2', 't3', 't4')]),
  C = length(unique(df$jathi_standard_all)),
  caste = df[!duplicated(df$artist_id), "jathi_standard_all"],
  R = length(unique(df$neighborhood_cluster)),
  residence = df[!duplicated(df$artist_id), "neighborhood_cluster"],
  native = df[!duplicated(df$artist_id), "native"]
)

stanmodel <- stan_model(file = "stan/model_7_full_model.stan")

iter = 4000
Nchains = 4
Ncores = 4
samples <- sampling(stanmodel, data_list, seed = 123,
                    warmup = 1000, iter = iter, cores = Ncores,
                    chains = Nchains)
params <- extract.samples(samples)

save(samples, params,
     file = "samples/samples.RData")
