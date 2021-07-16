rm(list=ls())
library(rstan)
library(ggplot2)
library(patchwork)
library(loo)
library(rethinking)
library(DiagrammeR)
library(DiagrammeRsvg)
library(ggpubr)

load("samples/samples.RData")
params <- extract.samples(samples)

############## Population-level Transitions ############## 
cross_trans <- apply(params$mu_cross, 2:3, mean)
ortho_trans <- apply(params$mu_ortho, 2:3, mean)
diag_trans <- apply(params$mu_diag, 2:3, mean)
trans_trans <- apply(params$mu_trans, 2:3, mean)

cross_trans <- t(apply(cross_trans, 1, function(x) exp(x)/sum(exp(x))))
ortho_trans <- t(apply(ortho_trans, 1, function(x) exp(x)/sum(exp(x))))
diag_trans <- t(apply(diag_trans, 1, function(x) exp(x)/sum(exp(x))))
trans_trans <- t(apply(trans_trans, 1, function(x) exp(x)/sum(exp(x))))

colnames(cross_trans) <- rownames(cross_trans) <- c("orthogonal", "transitional", "diagonal")
colnames(ortho_trans) <- rownames(ortho_trans) <- c("o1", "o2", "o3", "o4", "h3", "p4")
colnames(trans_trans) <- rownames(trans_trans) <- c("t1", "t2", "t3", "t4")
colnames(diag_trans) <- rownames(diag_trans) <- c("d1", "d2", "d3", "d4")

nodes <- data.frame(id = 1:14, 
                    label = c("o1", "o2", "o3", "o4",
                              "d1", "d2", "d3", "d4",
                              "t1", "t2", "t3", "t4", "h3", "p4"),
                    x = c(1.1, 1.9, 1.1, 1.9,
                          5, 6, 5, 6,
                          3, 4, 3, 4,
                          0.5, 2.5)*2, 
                    y = c(5.41, 5.41, 3.59, 3.59,
                          5, 5, 4, 4,
                          2, 2, 1, 1,
                          4.5, 4.5)*2,
                    height = rep(0.4, 14)*2,
                    fontsize = rep(10, 14)*2, 
                    fillcolor = c(rep("#4292C6", 4), rep("#7FBC41", 4),
                                  rep("#F46D43", 4), rep("#4292C6", 2)),
                    #fillcolor = rep("#4292C6", 14),
                    stringsAsFactors = FALSE)
edges_ortho <- expand.grid(from = c(1:4, 13:14),  to = c(1:4, 13:14),
                           type = 'ortho', KEEP.OUT.ATTRS = FALSE)
edges_ortho$label_from <- nodes$label[edges_ortho$from]
edges_ortho$label_to   <- nodes$label[edges_ortho$to]
edges_ortho$value <- diag(ortho_trans[edges_ortho$label_from, edges_ortho$label_to])

edges_diag  <- expand.grid(from = 5:8,  to = 5:8,
                           type = 'diag', KEEP.OUT.ATTRS = FALSE)
edges_diag$label_from <- nodes$label[edges_diag$from]
edges_diag$label_to   <- nodes$label[edges_diag$to]
edges_diag$value <- diag(diag_trans[edges_diag$label_from, edges_diag$label_to])

edges_trans <- expand.grid(from = 9:12, to = 9:12,
                           type = 'trans', KEEP.OUT.ATTRS = FALSE)
edges_trans$label_from <- nodes$label[edges_trans$from]
edges_trans$label_to   <- nodes$label[edges_trans$to]
edges_trans$value <- diag(trans_trans[edges_trans$label_from, edges_trans$label_to])

edges <- expand.grid(from = 1:14,  to = 1:14,KEEP.OUT.ATTRS = FALSE)
edges$label_from <- nodes$label[edges$from]
edges$label_to   <- nodes$label[edges$to]
edges[edges$label_from %in% edges_ortho$label_from & edges$label_to %in% edges_ortho$label_to, "value"] <- edges_ortho$value*cross_trans["orthogonal", "orthogonal"]
edges[edges$label_from %in% edges_diag$label_from & edges$label_to %in% edges_diag$label_to, "value"] <- edges_diag$value*cross_trans["diagonal", "diagonal"]
edges[edges$label_from %in% edges_trans$label_from & edges$label_to %in% edges_trans$label_to, "value"] <- edges_trans$value*cross_trans["transitional", "transitional"]

additional_gestures <- edges[is.na(edges$value), ]
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "o"), "value"] <- cross_trans["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "h"), "value"] <- cross_trans["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "p"), "value"] <- cross_trans["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "o") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- cross_trans["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "h") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- cross_trans["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "p") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- cross_trans["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "o") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- cross_trans["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "h") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- cross_trans["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "p") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- cross_trans["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- cross_trans["diagonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- cross_trans["transitional", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "o"), "value"] <- cross_trans["transitional", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "h"), "value"] <- cross_trans["transitional", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "p"), "value"] <- cross_trans["transitional", "orthogonal"] * 1/6
edges[is.na(edges$value), "value"] <- additional_gestures$value

edges <- edges[edges$from != edges$to, ]
edges$color <- "#636363"
# make cross transitions better visiable
edges$penwidth <- 5 * edges$value 
edges$type <- NULL
edges$label <- gsub('0\\.', '.', format.pval(edges$value, digits = 1, eps = 0.01))
edges$fontsize <- 12

edges <- edges[- which(edges$value < 1/14), ] # remove all edges below 0.01
edges$label <- NULL # remove edge labels
edges$tailclip <- TRUE
edges$headclip <- TRUE

graph <- create_graph()
graph <- graph %>%
  add_nodes_from_table(nodes, label_col = label) %>%
  add_edges_from_table(edges, from_col = from, to_col = to,
                       from_to_map = id_external)
graph %>%
  render_graph()

graph %>%
  render_graph() %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg::rsvg_svg(file = "output/figure_SI.svg")



############## Two Example Individual-level Transitions ############
ind_cross <- list()
for (i in 1:dim(params$pi_cross)[2]) {
  ind_cross[[i]] <- apply(params$pi_cross[, i,,], 2:3, mean)
  colnames(ind_cross[[i]]) <- rownames(ind_cross[[i]]) <- c("orthogonal", "transitional", "diagonal")
}

ind_ortho <- list()
for (i in 1:dim(params$pi_ortho)[2]) {
  ind_ortho[[i]] <- apply(params$pi_ortho[, i,,], 2:3, mean)
  colnames(ind_ortho[[i]]) <- rownames(ind_ortho[[i]]) <- c("o1", "o2", "o3", "o4", "h3", "p4")
}

ind_diag <- list()
for (i in 1:dim(params$pi_diag)[2]) {
  ind_diag[[i]] <- apply(params$pi_diag[, i,,], 2:3, mean)
  colnames(ind_diag[[i]]) <- rownames(ind_diag[[i]]) <- c("d1", "d2", "d3", "d4")
}


ind_trans <- list()
for (i in 1:dim(params$pi_trans)[2]) {
  ind_trans[[i]] <- apply(params$pi_trans[, i,,], 2:3, mean)
  colnames(ind_trans[[i]]) <- rownames(ind_trans[[i]]) <- c("t1", "t2", "t3", "t4")
}

###### Artist 156
nodes <- data.frame(id = 1:14, 
                    label = c("o1", "o2", "o3", "o4",
                              "d1", "d2", "d3", "d4",
                              "t1", "t2", "t3", "t4", "h3", "p4"),
                    x = c(1.1, 1.9, 1.1, 1.9, 5, 6, 5, 6, 3, 4, 3, 4, 0.5, 2.5)*2, 
                    y = c(5.41, 5.41, 3.59, 3.59, 5, 5, 4, 4, 2, 2, 1, 1, 4.5, 4.5)*2,
                    height = rep(0.4, 14)*2,
                    fontsize = rep(10, 14)*2, 
                    fillcolor = c(rep("#4292C6", 4), rep("#7FBC41", 4),
                                  rep("#F46D43", 4), rep("#4292C6", 2)),
                    #fillcolor = rep("#4292C6", 14),
                    stringsAsFactors = FALSE)
i = 156

edges_ortho <- expand.grid(from = c(1:4, 13:14),  to = c(1:4, 13:14),
                           type = 'ortho', KEEP.OUT.ATTRS = FALSE)
edges_ortho$label_from <- nodes$label[edges_ortho$from]
edges_ortho$label_to   <- nodes$label[edges_ortho$to]
edges_ortho$value <- diag(ind_ortho[[i]][edges_ortho$label_from, edges_ortho$label_to])

edges_diag  <- expand.grid(from = 5:8,  to = 5:8,
                           type = 'diag', KEEP.OUT.ATTRS = FALSE)
edges_diag$label_from <- nodes$label[edges_diag$from]
edges_diag$label_to   <- nodes$label[edges_diag$to]
edges_diag$value <- diag(ind_diag[[i]][edges_diag$label_from, edges_diag$label_to])

edges_trans <- expand.grid(from = 9:12, to = 9:12,
                           type = 'trans', KEEP.OUT.ATTRS = FALSE)
edges_trans$label_from <- nodes$label[edges_trans$from]
edges_trans$label_to   <- nodes$label[edges_trans$to]
edges_trans$value <- diag(ind_trans[[i]][edges_trans$label_from, edges_trans$label_to])

edges <- expand.grid(from = 1:14,  to = 1:14,KEEP.OUT.ATTRS = FALSE)
edges$label_from <- nodes$label[edges$from]
edges$label_to   <- nodes$label[edges$to]
edges[edges$label_from %in% edges_ortho$label_from & edges$label_to %in% edges_ortho$label_to, "value"] <- edges_ortho$value*ind_cross[[i]]["orthogonal", "orthogonal"]
edges[edges$label_from %in% edges_diag$label_from & edges$label_to %in% edges_diag$label_to, "value"] <- edges_diag$value*ind_cross[[i]]["diagonal", "diagonal"]
edges[edges$label_from %in% edges_trans$label_from & edges$label_to %in% edges_trans$label_to, "value"] <- edges_trans$value*ind_cross[[i]]["transitional", "transitional"]

additional_gestures <- edges[is.na(edges$value), ]
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "o"), "value"] <- ind_cross[[i]]["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "h"), "value"] <- ind_cross[[i]]["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "p"), "value"] <- ind_cross[[i]]["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "o") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- ind_cross[[i]]["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "h") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- ind_cross[[i]]["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "p") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- ind_cross[[i]]["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "o") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- ind_cross[[i]]["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "h") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- ind_cross[[i]]["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "p") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- ind_cross[[i]]["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- ind_cross[[i]]["diagonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- ind_cross[[i]]["transitional", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "o"), "value"] <- ind_cross[[i]]["transitional", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "h"), "value"] <- ind_cross[[i]]["transitional", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "p"), "value"] <- ind_cross[[i]]["transitional", "orthogonal"] * 1/6
edges[is.na(edges$value), "value"] <- additional_gestures$value

edges <- edges[edges$from != edges$to, ]
edges$color <- "#636363"
# make cross transitions better visiable
edges$penwidth <- 5 * edges$value  + 1.2
edges$type <- NULL
edges$label <- gsub('0\\.', '.', format.pval(edges$value, digits = 1, eps = 0.01))
edges$fontsize <- 12

edges <- edges[- which(edges$value < 1/14), ] # remove all edges below 0.01
edges$label <- NULL # remove edge labels
edges$tailclip <- TRUE
edges$headclip <- TRUE

graph <- create_graph()
graph <- graph %>%
  add_nodes_from_table(nodes, label_col = label) %>%
  add_edges_from_table(edges, from_col = from, to_col = to,
                       from_to_map = id_external)
graph %>%
  render_graph()

graph %>%
  render_graph() %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg::rsvg_svg(file = "output/figure_2A_right.svg")

###### Artist 6
nodes <- data.frame(id = 1:14, 
                    label = c("o1", "o2", "o3", "o4",
                              "d1", "d2", "d3", "d4",
                              "t1", "t2", "t3", "t4", "h3", "p4"),
                    x = c(1.1, 1.9, 1.1, 1.9, 5, 6, 5, 6, 3, 4, 3, 4, 0.5, 2.5)*2, 
                    y = c(5.41, 5.41, 3.59, 3.59, 5, 5, 4, 4, 2, 2, 1, 1, 4.5, 4.5)*2,
                    height = rep(0.4, 14)*2,
                    fontsize = rep(10, 14)*2, 
                    fillcolor = c(rep("#4292C6", 4), rep("#7FBC41", 4),
                                  rep("#F46D43", 4), rep("#4292C6", 2)),
                    #fillcolor = rep("#4292C6", 14),
                    stringsAsFactors = FALSE)
i = 6

edges_ortho <- expand.grid(from = c(1:4, 13:14),  to = c(1:4, 13:14),
                           type = 'ortho', KEEP.OUT.ATTRS = FALSE)
edges_ortho$label_from <- nodes$label[edges_ortho$from]
edges_ortho$label_to   <- nodes$label[edges_ortho$to]
edges_ortho$value <- diag(ind_ortho[[i]][edges_ortho$label_from, edges_ortho$label_to])

edges_diag  <- expand.grid(from = 5:8,  to = 5:8,
                           type = 'diag', KEEP.OUT.ATTRS = FALSE)
edges_diag$label_from <- nodes$label[edges_diag$from]
edges_diag$label_to   <- nodes$label[edges_diag$to]
edges_diag$value <- diag(ind_diag[[i]][edges_diag$label_from, edges_diag$label_to])

edges_trans <- expand.grid(from = 9:12, to = 9:12,
                           type = 'trans', KEEP.OUT.ATTRS = FALSE)
edges_trans$label_from <- nodes$label[edges_trans$from]
edges_trans$label_to   <- nodes$label[edges_trans$to]
edges_trans$value <- diag(ind_trans[[i]][edges_trans$label_from, edges_trans$label_to])

edges <- expand.grid(from = 1:14,  to = 1:14,KEEP.OUT.ATTRS = FALSE)
edges$label_from <- nodes$label[edges$from]
edges$label_to   <- nodes$label[edges$to]
edges[edges$label_from %in% edges_ortho$label_from & edges$label_to %in% edges_ortho$label_to, "value"] <- edges_ortho$value*ind_cross[[i]]["orthogonal", "orthogonal"]
edges[edges$label_from %in% edges_diag$label_from & edges$label_to %in% edges_diag$label_to, "value"] <- edges_diag$value*ind_cross[[i]]["diagonal", "diagonal"]
edges[edges$label_from %in% edges_trans$label_from & edges$label_to %in% edges_trans$label_to, "value"] <- edges_trans$value*ind_cross[[i]]["transitional", "transitional"]

additional_gestures <- edges[is.na(edges$value), ]
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "o"), "value"] <- ind_cross[[i]]["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "h"), "value"] <- ind_cross[[i]]["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "p"), "value"] <- ind_cross[[i]]["diagonal", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "o") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- ind_cross[[i]]["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "h") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- ind_cross[[i]]["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "p") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- ind_cross[[i]]["orthogonal", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "o") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- ind_cross[[i]]["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "h") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- ind_cross[[i]]["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "p") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- ind_cross[[i]]["orthogonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "d") & grepl(additional_gestures$label_to, pattern = "t"), "value"] <- ind_cross[[i]]["diagonal", "transitional"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "d"), "value"] <- ind_cross[[i]]["transitional", "diagonal"] * 1/4
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "o"), "value"] <- ind_cross[[i]]["transitional", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "h"), "value"] <- ind_cross[[i]]["transitional", "orthogonal"] * 1/6
additional_gestures[grepl(additional_gestures$label_from, pattern = "t") & grepl(additional_gestures$label_to, pattern = "p"), "value"] <- ind_cross[[i]]["transitional", "orthogonal"] * 1/6
edges[is.na(edges$value), "value"] <- additional_gestures$value

edges <- edges[edges$from != edges$to, ]
edges$color <- "#636363"
edges$penwidth <- 5 * edges$value  + 1.2
edges$type <- NULL
edges$label <- gsub('0\\.', '.', format.pval(edges$value, digits = 1, eps = 0.01))
edges$fontsize <- 12

edges <- edges[- which(edges$value < 1/14), ] 
edges$label <- NULL # remove edge labels
edges$tailclip <- TRUE
edges$headclip <- TRUE

graph <- create_graph()
graph <- graph %>%
  add_nodes_from_table(nodes, label_col = label) %>%
  add_edges_from_table(edges, from_col = from, to_col = to,
                       from_to_map = id_external)
graph %>%
  render_graph()

graph %>%
  render_graph() %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg::rsvg_svg(file = "output/figure_2B_right.svg")


############## Plot the coefficients ############## 

beta_duration_cross <- round(exp(apply(params$beta_duration_cross, 2:3, mean)), 2)
plot(samples, pars = "beta_duration_cross")
beta_duration_trans <- round(exp(apply(params$beta_duration_trans, 2:3, mean)), 2)
plot(samples, pars = "beta_duration_ortho")
beta_duration_diag <- round(exp(apply(params$beta_duration_diag, 2:3, mean)), 2)
plot(samples, pars = "beta_duration_diag")
beta_duration_trans <- round(exp(apply(params$beta_duration_trans, 2:3, mean)), 2)
plot(samples, pars = "beta_duration_trans")

beta_native_cross <- round(exp(apply(params$beta_native_cross, 2:3, mean)), 2)
plot(samples, pars = "beta_native_cross")
beta_native_ortho <- round(exp(apply(params$beta_native_ortho, 2:3, mean)), 2)
plot(samples, pars = "beta_native_ortho")
beta_native_diag <- round(exp(apply(params$beta_native_diag, 2:3, mean)), 2)
plot(samples, pars = "beta_native_diag")
beta_native_trans <- round(exp(apply(params$beta_native_trans, 2:3, mean)), 2)
plot(samples, pars = "beta_native_trans")

###### Individual Variation
df_sigma_cross <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_cross[1, "transition"] <- "cross_1"
df_sigma_cross[1, "mean"] <- apply(params$sigma_cross, 2, mean)[1]
df_sigma_cross[1, "lower_HPDI"] <- round(apply(params$sigma_cross, 2, HPDI, 0.9)[1], 2)
df_sigma_cross[1, "upper_HPDI"] <- round(apply(params$sigma_cross, 2, HPDI, 0.9)[2], 2)

df_sigma_cross[2, "transition"] <- "cross_2"
df_sigma_cross[2, "mean"] <- apply(params$sigma_cross, 2, mean)[2]
df_sigma_cross[2, "lower_HPDI"] <- round(apply(params$sigma_cross, 2, HPDI, 0.9)[3], 2)
df_sigma_cross[2, "upper_HPDI"] <- round(apply(params$sigma_cross, 2, HPDI, 0.9)[4], 2)

df_sigma_cross[3, "transition"] <- "cross_3"
df_sigma_cross[3, "mean"] <- apply(params$sigma_cross, 2, mean)[3]
df_sigma_cross[3, "lower_HPDI"] <- round(apply(params$sigma_cross, 2, HPDI, 0.9)[5], 2)
df_sigma_cross[3, "upper_HPDI"] <- round(apply(params$sigma_cross, 2, HPDI, 0.9)[6], 2)


###### Caste Variation
df_sigma_caste_cross <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_caste_cross[1, "transition"] <- "cross_1"
df_sigma_caste_cross[1, "mean"] <- apply(params$sigma_caste_cross, 2, mean)[1]
df_sigma_caste_cross[1, "lower_HPDI"] <- round(apply(params$sigma_caste_cross, 2, HPDI, 0.9)[1], 2)
df_sigma_caste_cross[1, "upper_HPDI"] <- round(apply(params$sigma_caste_cross, 2, HPDI, 0.9)[2], 1)

df_sigma_caste_cross[2, "transition"] <- "cross_2"
df_sigma_caste_cross[2, "mean"] <- apply(params$sigma_caste_cross, 2, mean)[2]
df_sigma_caste_cross[2, "lower_HPDI"] <- round(apply(params$sigma_caste_cross, 2, HPDI, 0.9)[3], 2)
df_sigma_caste_cross[2, "upper_HPDI"] <- round(apply(params$sigma_caste_cross, 2, HPDI, 0.9)[4], 1)

df_sigma_caste_cross[3, "transition"] <- "cross_3"
df_sigma_caste_cross[3, "mean"] <- apply(params$sigma_caste_cross, 2, mean)[3]
df_sigma_caste_cross[3, "lower_HPDI"] <- round(apply(params$sigma_caste_cross, 2, HPDI, 0.9)[5], 2)
df_sigma_caste_cross[3, "upper_HPDI"] <- round(apply(params$sigma_caste_cross, 2, HPDI, 0.9)[6], 1)

###### Neighborhood Variation
df_sigma_residence_cross <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_residence_cross[1, "transition"] <- "cross_1"
df_sigma_residence_cross[1, "mean"] <- apply(params$sigma_residence_cross, 2, mean)[1]
df_sigma_residence_cross[1, "lower_HPDI"] <- round(apply(params$sigma_residence_cross, 2, HPDI, 0.9)[1], 2)
df_sigma_residence_cross[1, "upper_HPDI"] <- round(apply(params$sigma_residence_cross, 2, HPDI, 0.9)[2], 1)

df_sigma_residence_cross[2, "transition"] <- "cross_2"
df_sigma_residence_cross[2, "mean"] <- apply(params$sigma_residence_cross, 2, mean)[2]
df_sigma_residence_cross[2, "lower_HPDI"] <- round(apply(params$sigma_residence_cross, 2, HPDI, 0.9)[3], 2)
df_sigma_residence_cross[2, "upper_HPDI"] <- round(apply(params$sigma_residence_cross, 2, HPDI, 0.9)[4], 1)

df_sigma_residence_cross[3, "transition"] <- "cross_3"
df_sigma_residence_cross[3, "mean"] <- apply(params$sigma_residence_cross, 2, mean)[3]
df_sigma_residence_cross[3, "lower_HPDI"] <- round(apply(params$sigma_residence_cross, 2, HPDI, 0.9)[5], 2)
df_sigma_residence_cross[3, "upper_HPDI"] <- round(apply(params$sigma_residence_cross, 2, HPDI, 0.9)[6], 1)


df_sigma_cross$transition <- factor(df_sigma_cross$transition,
                                    levels = paste0('cross_', 1:3),
                                    labels = c("orthogonal", "transitional", "diagonal")
)
df_sigma_caste_cross$transition <- factor(df_sigma_caste_cross$transition,
                                          levels = paste0('cross_', 1:3),
                                          labels = c("orthogonal", "transitional", "diagonal")
)
df_sigma_residence_cross$transition <- factor(df_sigma_residence_cross$transition,
                                              levels = paste0('cross_', 1:3),
                                              labels = c("orthogonal", "transitional", "diagonal")
)
df_sigma_caste_cross$param <- "caste"
df_sigma_cross$param  <- "artist"
df_sigma_residence_cross$param  <- "neighborhood"

df_combined <- rbind(df_sigma_cross, df_sigma_caste_cross, df_sigma_residence_cross)
df_combined$param <- factor(df_combined$param,
                            levels = c("artist", "caste", "neighborhood")) 

coefficient_plots_cross <- ggplot(data = df_combined,
                                  aes(x = transition, y = mean, ymin = lower_HPDI, ymax = upper_HPDI)) +
  geom_pointrange(aes(color = param), size= 1, position = position_dodge(width = 1/2)) +
  geom_hline(yintercept = 0, lty = 2, color = "red") +  # add a dotted line at x=2 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_x_discrete(limits = rev) +
  ggtitle("A") +
  xlab("Transitions from") + ylab("sigma") +
  theme_bw() + # use a white background
  theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        panel.spacing = unit(2, "lines"),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 15)) +
  scale_color_brewer(palette="Paired")



###### Individual Variation
df_sigma_ortho <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_ortho[1, "transition"] <- "ortho_1"
df_sigma_ortho[1, "mean"] <- apply(params$sigma_ortho, 2, mean)[1]
df_sigma_ortho[1, "lower_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[1], 2)
df_sigma_ortho[1, "upper_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[2], 2)

df_sigma_ortho[2, "transition"] <- "ortho_2"
df_sigma_ortho[2, "mean"] <- apply(params$sigma_ortho, 2, mean)[2]
df_sigma_ortho[2, "lower_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[3], 2)
df_sigma_ortho[2, "upper_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[4], 2)

df_sigma_ortho[3, "transition"] <- "ortho_3"
df_sigma_ortho[3, "mean"] <- apply(params$sigma_ortho, 2, mean)[3]
df_sigma_ortho[3, "lower_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[5], 2)
df_sigma_ortho[3, "upper_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[6], 2)

df_sigma_ortho[4, "transition"] <- "ortho_4"
df_sigma_ortho[4, "mean"] <- apply(params$sigma_ortho, 2, mean)[4]
df_sigma_ortho[4, "lower_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[7], 2)
df_sigma_ortho[4, "upper_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[8], 2)

df_sigma_ortho[5, "transition"] <- "ortho_5"
df_sigma_ortho[5, "mean"] <- apply(params$sigma_ortho, 2, mean)[5]
df_sigma_ortho[5, "lower_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[9], 2)
df_sigma_ortho[5, "upper_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[10], 2)

df_sigma_ortho[6, "transition"] <- "ortho_6"
df_sigma_ortho[6, "mean"] <- apply(params$sigma_ortho, 2, mean)[6]
df_sigma_ortho[6, "lower_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[11], 2)
df_sigma_ortho[6, "upper_HPDI"] <- round(apply(params$sigma_ortho, 2, HPDI, 0.9)[12], 2)

###### Caste Variation
df_sigma_caste_ortho <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_caste_ortho[1, "transition"] <- "ortho_1"
df_sigma_caste_ortho[1, "mean"] <- apply(params$sigma_caste_ortho, 2, mean)[1]
df_sigma_caste_ortho[1, "lower_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[1], 2)
df_sigma_caste_ortho[1, "upper_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[2], 1)

df_sigma_caste_ortho[2, "transition"] <- "ortho_2"
df_sigma_caste_ortho[2, "mean"] <- apply(params$sigma_caste_ortho, 2, mean)[2]
df_sigma_caste_ortho[2, "lower_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[3], 2)
df_sigma_caste_ortho[2, "upper_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[4], 1)

df_sigma_caste_ortho[3, "transition"] <- "ortho_3"
df_sigma_caste_ortho[3, "mean"] <- apply(params$sigma_caste_ortho, 2, mean)[3]
df_sigma_caste_ortho[3, "lower_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[5], 2)
df_sigma_caste_ortho[3, "upper_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[6], 1)

df_sigma_caste_ortho[4, "transition"] <- "ortho_4"
df_sigma_caste_ortho[4, "mean"] <- apply(params$sigma_caste_ortho, 2, mean)[4]
df_sigma_caste_ortho[4, "lower_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[7], 2)
df_sigma_caste_ortho[4, "upper_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[8], 1)

df_sigma_caste_ortho[5, "transition"] <- "ortho_5"
df_sigma_caste_ortho[5, "mean"] <- apply(params$sigma_caste_ortho, 2, mean)[5]
df_sigma_caste_ortho[5, "lower_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[9], 2)
df_sigma_caste_ortho[5, "upper_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[10], 1)

df_sigma_caste_ortho[6, "transition"] <- "ortho_6"
df_sigma_caste_ortho[6, "mean"] <- apply(params$sigma_caste_ortho, 2, mean)[6]
df_sigma_caste_ortho[6, "lower_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[11], 2)
df_sigma_caste_ortho[6, "upper_HPDI"] <- round(apply(params$sigma_caste_ortho, 2, HPDI, 0.9)[12], 1)



###### Neighborhood Variation
df_sigma_residence_ortho <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_residence_ortho[1, "transition"] <- "ortho_1"
df_sigma_residence_ortho[1, "mean"] <- apply(params$sigma_residence_ortho, 2, mean)[1]
df_sigma_residence_ortho[1, "lower_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[1], 2)
df_sigma_residence_ortho[1, "upper_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[2], 1)

df_sigma_residence_ortho[2, "transition"] <- "ortho_2"
df_sigma_residence_ortho[2, "mean"] <- apply(params$sigma_residence_ortho, 2, mean)[2]
df_sigma_residence_ortho[2, "lower_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[3], 2)
df_sigma_residence_ortho[2, "upper_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[4], 1)

df_sigma_residence_ortho[3, "transition"] <- "ortho_3"
df_sigma_residence_ortho[3, "mean"] <- apply(params$sigma_residence_ortho, 2, mean)[3]
df_sigma_residence_ortho[3, "lower_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[5], 2)
df_sigma_residence_ortho[3, "upper_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[6], 1)

df_sigma_residence_ortho[4, "transition"] <- "ortho_4"
df_sigma_residence_ortho[4, "mean"] <- apply(params$sigma_residence_ortho, 2, mean)[4]
df_sigma_residence_ortho[4, "lower_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[7], 2)
df_sigma_residence_ortho[4, "upper_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[8], 1)

df_sigma_residence_ortho[5, "transition"] <- "ortho_5"
df_sigma_residence_ortho[5, "mean"] <- apply(params$sigma_residence_ortho, 2, mean)[5]
df_sigma_residence_ortho[5, "lower_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[9], 2)
df_sigma_residence_ortho[5, "upper_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[10], 1)

df_sigma_residence_ortho[6, "transition"] <- "ortho_6"
df_sigma_residence_ortho[6, "mean"] <- apply(params$sigma_residence_ortho, 2, mean)[6]
df_sigma_residence_ortho[6, "lower_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[11], 2)
df_sigma_residence_ortho[6, "upper_HPDI"] <- round(apply(params$sigma_residence_ortho, 2, HPDI, 0.9)[12], 1)


df_sigma_ortho$transition <- factor(df_sigma_ortho$transition,
                                    levels = paste0('ortho_', 1:6),
                                    labels = c(paste0('o', 1:4), "h3", "p4"))
df_sigma_caste_ortho$transition <- factor(df_sigma_caste_ortho$transition,
                                          levels = paste0('ortho_', 1:6),
                                          labels = c(paste0('o', 1:4), "h3", "p4"))
df_sigma_residence_ortho$transition <- factor(df_sigma_residence_ortho$transition,
                                              levels = paste0('ortho_', 1:6),
                                              labels = c(paste0('o', 1:4), "h3", "p4"))
df_sigma_caste_ortho$param <- "caste"
df_sigma_ortho$param  <- "artist"
df_sigma_residence_ortho$param  <- "neighborhood"

df_combined <- rbind(df_sigma_ortho, df_sigma_caste_ortho, df_sigma_residence_ortho)
df_combined$param <- factor(df_combined$param,
                            levels = c("artist", "caste", "neighborhood")) 

coefficient_plots_ortho <- ggplot(data = df_combined,
                                  aes(x = transition, y = mean, ymin = lower_HPDI, ymax = upper_HPDI)) +
  geom_pointrange(aes(color = param), size= 1, position = position_dodge(width = 1/2)) +
  geom_hline(yintercept = 0, lty = 2, color = "red") +  # add a dotted line at x=2 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_x_discrete(limits = rev) +
  xlab("") +
  ylab("sigma") +
  ggtitle("B") +
  theme_bw() + # use a white background
  theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        panel.spacing = unit(2, "lines"),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 15)) +
  scale_color_brewer(palette="Paired")


###### Individual Variation
df_sigma_diag <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_diag[1, "transition"] <- "diag_1"
df_sigma_diag[1, "mean"] <- apply(params$sigma_diag, 2, mean)[1]
df_sigma_diag[1, "lower_HPDI"] <- round(apply(params$sigma_diag, 2, HPDI, 0.9)[1], 2)
df_sigma_diag[1, "upper_HPDI"] <- round(apply(params$sigma_diag, 2, HPDI, 0.9)[2], 2)

df_sigma_diag[2, "transition"] <- "diag_2"
df_sigma_diag[2, "mean"] <- apply(params$sigma_diag, 2, mean)[2]
df_sigma_diag[2, "lower_HPDI"] <- round(apply(params$sigma_diag, 2, HPDI, 0.9)[3], 2)
df_sigma_diag[2, "upper_HPDI"] <- round(apply(params$sigma_diag, 2, HPDI, 0.9)[4], 2)

df_sigma_diag[3, "transition"] <- "diag_3"
df_sigma_diag[3, "mean"] <- apply(params$sigma_diag, 2, mean)[3]
df_sigma_diag[3, "lower_HPDI"] <- round(apply(params$sigma_diag, 2, HPDI, 0.9)[5], 2)
df_sigma_diag[3, "upper_HPDI"] <- round(apply(params$sigma_diag, 2, HPDI, 0.9)[6], 2)

df_sigma_diag[4, "transition"] <- "diag_4"
df_sigma_diag[4, "mean"] <- apply(params$sigma_diag, 2, mean)[4]
df_sigma_diag[4, "lower_HPDI"] <- round(apply(params$sigma_diag, 2, HPDI, 0.9)[7], 2)
df_sigma_diag[4, "upper_HPDI"] <- round(apply(params$sigma_diag, 2, HPDI, 0.9)[8], 2)

###### Caste Variation
df_sigma_caste_diag <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_caste_diag[1, "transition"] <- "diag_1"
df_sigma_caste_diag[1, "mean"] <- apply(params$sigma_caste_diag, 2, mean)[1]
df_sigma_caste_diag[1, "lower_HPDI"] <- round(apply(params$sigma_caste_diag, 2, HPDI, 0.9)[1], 2)
df_sigma_caste_diag[1, "upper_HPDI"] <- round(apply(params$sigma_caste_diag, 2, HPDI, 0.9)[2], 1)

df_sigma_caste_diag[2, "transition"] <- "diag_2"
df_sigma_caste_diag[2, "mean"] <- apply(params$sigma_caste_diag, 2, mean)[2]
df_sigma_caste_diag[2, "lower_HPDI"] <- round(apply(params$sigma_caste_diag, 2, HPDI, 0.9)[3], 2)
df_sigma_caste_diag[2, "upper_HPDI"] <- round(apply(params$sigma_caste_diag, 2, HPDI, 0.9)[4], 1)

df_sigma_caste_diag[3, "transition"] <- "diag_3"
df_sigma_caste_diag[3, "mean"] <- apply(params$sigma_caste_diag, 2, mean)[3]
df_sigma_caste_diag[3, "lower_HPDI"] <- round(apply(params$sigma_caste_diag, 2, HPDI, 0.9)[5], 2)
df_sigma_caste_diag[3, "upper_HPDI"] <- round(apply(params$sigma_caste_diag, 2, HPDI, 0.9)[6], 1)


df_sigma_caste_diag[4, "transition"] <- "diag_4"
df_sigma_caste_diag[4, "mean"] <- apply(params$sigma_caste_diag, 2, mean)[4]
df_sigma_caste_diag[4, "lower_HPDI"] <- round(apply(params$sigma_caste_diag, 2, HPDI, 0.9)[7], 2)
df_sigma_caste_diag[4, "upper_HPDI"] <- round(apply(params$sigma_caste_diag, 2, HPDI, 0.9)[8], 1)

###### Neighborhood Variation
df_sigma_residence_diag <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_residence_diag[1, "transition"] <- "diag_1"
df_sigma_residence_diag[1, "mean"] <- apply(params$sigma_residence_diag, 2, mean)[1]
df_sigma_residence_diag[1, "lower_HPDI"] <- round(apply(params$sigma_residence_diag, 2, HPDI, 0.9)[1], 2)
df_sigma_residence_diag[1, "upper_HPDI"] <- round(apply(params$sigma_residence_diag, 2, HPDI, 0.9)[2], 1)

df_sigma_residence_diag[2, "transition"] <- "diag_2"
df_sigma_residence_diag[2, "mean"] <- apply(params$sigma_residence_diag, 2, mean)[2]
df_sigma_residence_diag[2, "lower_HPDI"] <- round(apply(params$sigma_residence_diag, 2, HPDI, 0.9)[3], 2)
df_sigma_residence_diag[2, "upper_HPDI"] <- round(apply(params$sigma_residence_diag, 2, HPDI, 0.9)[4], 1)

df_sigma_residence_diag[3, "transition"] <- "diag_3"
df_sigma_residence_diag[3, "mean"] <- apply(params$sigma_residence_diag, 2, mean)[3]
df_sigma_residence_diag[3, "lower_HPDI"] <- round(apply(params$sigma_residence_diag, 2, HPDI, 0.9)[5], 2)
df_sigma_residence_diag[3, "upper_HPDI"] <- round(apply(params$sigma_residence_diag, 2, HPDI, 0.9)[6], 1)

df_sigma_residence_diag[4, "transition"] <- "diag_4"
df_sigma_residence_diag[4, "mean"] <- apply(params$sigma_residence_diag, 2, mean)[4]
df_sigma_residence_diag[4, "lower_HPDI"] <- round(apply(params$sigma_residence_diag, 2, HPDI, 0.9)[7], 2)
df_sigma_residence_diag[4, "upper_HPDI"] <- round(apply(params$sigma_residence_diag, 2, HPDI, 0.9)[8], 1)


df_sigma_diag$transition <- factor(df_sigma_diag$transition,
                                   levels = paste0("diag_", 1:4),
                                   labels = paste0("d", 1:4))
df_sigma_caste_diag$transition <- factor(df_sigma_caste_diag$transition,
                                         levels = paste0("diag_", 1:4),
                                         labels = paste0("d", 1:4))
df_sigma_residence_diag$transition <- factor(df_sigma_residence_diag$transition,
                                             levels = paste0("diag_", 1:4),
                                             labels = paste0("d", 1:4))
df_sigma_caste_diag$param <- "caste"
df_sigma_diag$param  <- "artist"
df_sigma_residence_diag$param  <- "neighborhood"

df_combined <- rbind(df_sigma_diag, df_sigma_caste_diag, df_sigma_residence_diag)
df_combined$param <- factor(df_combined$param,
                            levels = c("artist", "caste", "neighborhood")) 

coefficient_plots_diag <- ggplot(data = df_combined,
                                 aes(x = transition, y = mean, ymin = lower_HPDI, ymax = upper_HPDI)) +
  geom_pointrange(aes(color = param), size= 1, position = position_dodge(width = 1/2)) +
  geom_hline(yintercept = 0, lty = 2, color = "red") +  # add a dotted line at x=2 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_x_discrete(limits=rev) +
  xlab("Transitions from") + ylab("sigma") +
  theme_bw() + # use a white background
  ggtitle("C") +
  theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        panel.spacing = unit(2, "lines"),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 15)) +
  scale_color_brewer(palette="Paired")



###### Individual Variation
df_sigma_trans <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_trans[1, "transition"] <- "trans_1"
df_sigma_trans[1, "mean"] <- apply(params$sigma_trans, 2, mean)[1]
df_sigma_trans[1, "lower_HPDI"] <- round(apply(params$sigma_trans, 2, HPDI, 0.9)[1], 2)
df_sigma_trans[1, "upper_HPDI"] <- round(apply(params$sigma_trans, 2, HPDI, 0.9)[2], 2)

df_sigma_trans[2, "transition"] <- "trans_2"
df_sigma_trans[2, "mean"] <- apply(params$sigma_trans, 2, mean)[2]
df_sigma_trans[2, "lower_HPDI"] <- round(apply(params$sigma_trans, 2, HPDI, 0.9)[3], 2)
df_sigma_trans[2, "upper_HPDI"] <- round(apply(params$sigma_trans, 2, HPDI, 0.9)[4], 2)

df_sigma_trans[3, "transition"] <- "trans_3"
df_sigma_trans[3, "mean"] <- apply(params$sigma_trans, 2, mean)[3]
df_sigma_trans[3, "lower_HPDI"] <- round(apply(params$sigma_trans, 2, HPDI, 0.9)[5], 2)
df_sigma_trans[3, "upper_HPDI"] <- round(apply(params$sigma_trans, 2, HPDI, 0.9)[6], 2)

df_sigma_trans[4, "transition"] <- "trans_4"
df_sigma_trans[4, "mean"] <- apply(params$sigma_trans, 2, mean)[4]
df_sigma_trans[4, "lower_HPDI"] <- round(apply(params$sigma_trans, 2, HPDI, 0.9)[7], 2)
df_sigma_trans[4, "upper_HPDI"] <- round(apply(params$sigma_trans, 2, HPDI, 0.9)[8], 2)


###### Caste Variation
df_sigma_caste_trans <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_caste_trans[1, "transition"] <- "trans_1"
df_sigma_caste_trans[1, "mean"] <- apply(params$sigma_caste_trans, 2, mean)[1]
df_sigma_caste_trans[1, "lower_HPDI"] <- round(apply(params$sigma_caste_trans, 2, HPDI, 0.9)[1], 2)
df_sigma_caste_trans[1, "upper_HPDI"] <- round(apply(params$sigma_caste_trans, 2, HPDI, 0.9)[2], 1)

df_sigma_caste_trans[2, "transition"] <- "trans_2"
df_sigma_caste_trans[2, "mean"] <- apply(params$sigma_caste_trans, 2, mean)[2]
df_sigma_caste_trans[2, "lower_HPDI"] <- round(apply(params$sigma_caste_trans, 2, HPDI, 0.9)[3], 2)
df_sigma_caste_trans[2, "upper_HPDI"] <- round(apply(params$sigma_caste_trans, 2, HPDI, 0.9)[4], 1)

df_sigma_caste_trans[3, "transition"] <- "trans_3"
df_sigma_caste_trans[3, "mean"] <- apply(params$sigma_caste_trans, 2, mean)[3]
df_sigma_caste_trans[3, "lower_HPDI"] <- round(apply(params$sigma_caste_trans, 2, HPDI, 0.9)[5], 2)
df_sigma_caste_trans[3, "upper_HPDI"] <- round(apply(params$sigma_caste_trans, 2, HPDI, 0.9)[6], 1)

df_sigma_caste_trans[4, "transition"] <- "trans_4"
df_sigma_caste_trans[4, "mean"] <- apply(params$sigma_caste_trans, 2, mean)[4]
df_sigma_caste_trans[4, "lower_HPDI"] <- round(apply(params$sigma_caste_trans, 2, HPDI, 0.9)[7], 2)
df_sigma_caste_trans[4, "upper_HPDI"] <- round(apply(params$sigma_caste_trans, 2, HPDI, 0.9)[8], 1)

###### Neighborhood Variation
df_sigma_residence_trans <- data.frame(transition = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)

df_sigma_residence_trans[1, "transition"] <- "trans_1"
df_sigma_residence_trans[1, "mean"] <- apply(params$sigma_residence_trans, 2, mean)[1]
df_sigma_residence_trans[1, "lower_HPDI"] <- round(apply(params$sigma_residence_trans, 2, HPDI, 0.9)[1], 2)
df_sigma_residence_trans[1, "upper_HPDI"] <- round(apply(params$sigma_residence_trans, 2, HPDI, 0.9)[2], 1)

df_sigma_residence_trans[2, "transition"] <- "trans_2"
df_sigma_residence_trans[2, "mean"] <- apply(params$sigma_residence_trans, 2, mean)[2]
df_sigma_residence_trans[2, "lower_HPDI"] <- round(apply(params$sigma_residence_trans, 2, HPDI, 0.9)[3], 2)
df_sigma_residence_trans[2, "upper_HPDI"] <- round(apply(params$sigma_residence_trans, 2, HPDI, 0.9)[4], 1)

df_sigma_residence_trans[3, "transition"] <- "trans_3"
df_sigma_residence_trans[3, "mean"] <- apply(params$sigma_residence_trans, 2, mean)[3]
df_sigma_residence_trans[3, "lower_HPDI"] <- round(apply(params$sigma_residence_trans, 2, HPDI, 0.9)[5], 2)
df_sigma_residence_trans[3, "upper_HPDI"] <- round(apply(params$sigma_residence_trans, 2, HPDI, 0.9)[6], 1)

df_sigma_residence_trans[4, "transition"] <- "trans_4"
df_sigma_residence_trans[4, "mean"] <- apply(params$sigma_residence_trans, 2, mean)[4]
df_sigma_residence_trans[4, "lower_HPDI"] <- round(apply(params$sigma_residence_trans, 2, HPDI, 0.9)[7], 2)
df_sigma_residence_trans[4, "upper_HPDI"] <- round(apply(params$sigma_residence_trans, 2, HPDI, 0.9)[8], 1)


df_sigma_trans$transition <- factor(df_sigma_trans$transition,
                                    levels = paste0("trans_", 1:4),
                                    labels = paste0("t", 1:4))
df_sigma_caste_trans$transition <- factor(df_sigma_caste_trans$transition,
                                          levels = paste0("trans_", 1:4),
                                          labels = paste0("t", 1:4))
df_sigma_residence_trans$transition <- factor(df_sigma_residence_trans$transition,
                                              levels = paste0("trans_", 1:4),
                                              labels = paste0("t", 1:4))
df_sigma_caste_trans$param <- "caste"
df_sigma_trans$param  <- "artist"
df_sigma_residence_trans$param  <- "neighborhood"

df_combined <- rbind(df_sigma_trans, df_sigma_caste_trans, df_sigma_residence_trans)
df_combined$param <- factor(df_combined$param,
                            levels = c("artist", "caste", "neighborhood")) 

coefficient_plots_trans <- ggplot(data = df_combined,
                                  aes(x = transition, y = mean, ymin = lower_HPDI, ymax = upper_HPDI)) +
  geom_pointrange(aes(color = param), size= 1, position = position_dodge(width = 1/2)) +
  geom_hline(yintercept = 0, lty = 2, color = "red") +  # add a dotted line at x=2 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_x_discrete(limits = rev) +
  xlab("") +
  ylab("sigma") +
  theme_bw() + # use a white background
  ggtitle("D") +
  theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        panel.spacing = unit(2, "lines"),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 15)) +
  scale_color_brewer(palette="Paired")

cairo_ps(filename = "output/figure_3.eps",
         onefile = TRUE, fallback_resolution = 800, width = 10, height = 10)
(coefficient_plots_cross | coefficient_plots_ortho) / (coefficient_plots_diag | coefficient_plots_trans) +
  plot_layout(guides = 'collect')
dev.off()



########## Compute ICC ##########
load("samples/samples.RData")
params <- extract.samples(samples)

var_list_cross <- list(
  vars = list(
    id = array(dim=c(12000, 3, 3)),
    duration = array(dim=c(12000, 3, 3)),
    native = array(dim=c(12000, 3, 3)),
    caste = array(dim=c(12000, 3, 3)),
    residence = array(dim=c(12000, 3, 3)),
    total = array(dim=c(12000, 3, 3))
  ),
  icc = list(
    id = array(dim=c(12000, 3, 3)),
    duration = array(dim=c(12000, 3, 3)),
    native = array(dim=c(12000, 3, 3)),
    caste = array(dim=c(12000, 3, 3)),
    residence = array(dim=c(12000, 3, 3))
  )
)

# For each mcmc iteration, we compute the icc for each column and each row of the transition matrix
# icc is calculated as var_effect[i] / var_total, where var_total = sum of all var_effect[i]
for(iter in 1:12000) {
  for(row in 1:3) {
    for(col in 1:3) {
      # calculating predictions for ids, then computing the variance of the predictions
      prediction_id_cross <- params$z_cross[iter,,row,col]*params$sigma_cross[iter,row]
      var_list_cross$vars$id[iter,row,col] <- var(prediction_id_cross)
      
      # calculating predictions for caste, then computing the variance of the predictions
      prediction_caste_cross <- params$z_caste_cross[iter,,row,col]*params$sigma_caste_cross[iter,row]
      var_list_cross$vars$caste[iter,row,col] <- var(prediction_caste_cross)
      
      # calculating predictions for residene, then computing the variance of the predictions
      prediction_residence_cross <- params$z_residence_cross[iter,1:8,row,col]*params$sigma_residence_cross[iter,row]
      var_list_cross$vars$residence[iter,row,col] <- var(prediction_residence_cross)
      
      # calculating predictions for duration, then computing the variance of the predictions
      beta_duration_cross <- params$beta_duration_cross[iter,row,col]
      prediction_duration_cross <- beta_duration_cross*data_list$duration
      var_list_cross$vars$duration[iter,row,col] <- var(prediction_duration_cross)
      
      # calculating predictions for native, then computing the variance of the predictions
      beta_native_cross <- params$beta_native_cross[iter,row,col]
      prediction_native_cross <- beta_native_cross*data_list$native
      var_list_cross$vars$native[iter,row,col] <- var(prediction_native_cross)
      
      # calculate total variation
      var_list_cross$vars$total[iter,row,col] <- 
        var_list_cross$vars$id[iter,row,col] +
        var_list_cross$vars$caste[iter,row,col] +
        var_list_cross$vars$residence[iter,row,col] +
        var_list_cross$vars$duration[iter,row,col] +
        var_list_cross$vars$native[iter,row,col]
      
      # calculate icc as var_effect[i] / var_total
      var_list_cross$icc$id[iter,row,col] <- var_list_cross$vars$id[iter,row,col] / var_list_cross$vars$total[iter,row,col]
      var_list_cross$icc$caste[iter,row,col] <- var_list_cross$vars$caste[iter,row,col] / var_list_cross$vars$total[iter,row,col]
      var_list_cross$icc$residence[iter,row,col] <- var_list_cross$vars$residence[iter,row,col] / var_list_cross$vars$total[iter,row,col]
      var_list_cross$icc$duration[iter,row,col] <- var_list_cross$vars$duration[iter,row,col] / var_list_cross$vars$total[iter,row,col]
      var_list_cross$icc$native[iter,row,col] <- var_list_cross$vars$native[iter,row,col] / var_list_cross$vars$total[iter,row,col]
    }
  }
}

# average icc over mcmc iterations (we will get average icc matrix), then average over the matrix to get a single number
sapply(lapply(var_list_cross$icc, function(x) apply(x, 2:3, mean)), mean)
# id   duration     native      caste  residence    
# 0.62661464 0.24521600 0.05672598 0.03021677 0.04122661 


var_list_ortho <- list(
  vars = list(
    id = array(dim=c(12000, 3, 3)),
    duration = array(dim=c(12000, 3, 3)),
    native = array(dim=c(12000, 3, 3)),
    caste = array(dim=c(12000, 3, 3)),
    residence = array(dim=c(12000, 3, 3)),
    total = array(dim=c(12000, 3, 3))
  ),
  icc = list(
    id = array(dim=c(12000, 3, 3)),
    duration = array(dim=c(12000, 3, 3)),
    native = array(dim=c(12000, 3, 3)),
    caste = array(dim=c(12000, 3, 3)),
    residence = array(dim=c(12000, 3, 3))
  )
)
for(iter in 1:12000) {
  for(row in 1:3) {
    for(col in 1:3) {
      prediction_id_ortho <- params$z_ortho[iter,,row,col]*params$sigma_ortho[iter,row]
      var_list_ortho$vars$id[iter,row,col] <- var(prediction_id_ortho)
      
      prediction_caste_ortho <- params$z_caste_ortho[iter,,row,col]*params$sigma_caste_ortho[iter,row]
      var_list_ortho$vars$caste[iter,row,col] <- var(prediction_caste_ortho)
      
      prediction_residence_ortho <- params$z_residence_ortho[iter,1:8,row,col]*params$sigma_residence_ortho[iter,row]
      var_list_ortho$vars$residence[iter,row,col] <- var(prediction_residence_ortho)
      
      beta_duration_ortho <- params$beta_duration_ortho[iter,row,col]
      prediction_duration_ortho <- beta_duration_ortho*data_list$duration
      var_list_ortho$vars$duration[iter,row,col] <- var(prediction_duration_ortho)
      
      beta_native_ortho <- params$beta_native_ortho[iter,row,col]
      prediction_native_ortho <- beta_native_ortho*data_list$native
      var_list_ortho$vars$native[iter,row,col] <- var(prediction_native_ortho)
      
      var_list_ortho$vars$total[iter,row,col] <- 
        var_list_ortho$vars$id[iter,row,col] +
        var_list_ortho$vars$caste[iter,row,col] +
        var_list_ortho$vars$residence[iter,row,col] +
        var_list_ortho$vars$duration[iter,row,col] +
        var_list_ortho$vars$native[iter,row,col]
      
      var_list_ortho$icc$id[iter,row,col] <- var_list_ortho$vars$id[iter,row,col] / var_list_ortho$vars$total[iter,row,col]
      var_list_ortho$icc$caste[iter,row,col] <- var_list_ortho$vars$caste[iter,row,col] / var_list_ortho$vars$total[iter,row,col]
      var_list_ortho$icc$residence[iter,row,col] <- var_list_ortho$vars$residence[iter,row,col] / var_list_ortho$vars$total[iter,row,col]
      var_list_ortho$icc$duration[iter,row,col] <- var_list_ortho$vars$duration[iter,row,col] / var_list_ortho$vars$total[iter,row,col]
      var_list_ortho$icc$native[iter,row,col] <- var_list_ortho$vars$native[iter,row,col] / var_list_ortho$vars$total[iter,row,col]
    }
  }
}

sapply(lapply(var_list_ortho$icc, function(x) apply(x, 2:3, mean)), mean)
# id   duration     native      caste  residence 
# 0.62375275 0.19944162 0.05368658 0.02618884 0.09693021 


var_list_diag <- list(
  vars = list(
    id = array(dim=c(12000, 3, 3)),
    duration = array(dim=c(12000, 3, 3)),
    native = array(dim=c(12000, 3, 3)),
    caste = array(dim=c(12000, 3, 3)),
    residence = array(dim=c(12000, 3, 3)),
    total = array(dim=c(12000, 3, 3))
  ),
  icc = list(
    id = array(dim=c(12000, 3, 3)),
    duration = array(dim=c(12000, 3, 3)),
    native = array(dim=c(12000, 3, 3)),
    caste = array(dim=c(12000, 3, 3)),
    residence = array(dim=c(12000, 3, 3))
  )
)
for(iter in 1:12000) {
  for(row in 1:3) {
    for(col in 1:3) {
      prediction_id_diag <- params$z_diag[iter,,row,col]*params$sigma_diag[iter,row]
      var_list_diag$vars$id[iter,row,col] <- var(prediction_id_diag)
      
      prediction_caste_diag <- params$z_caste_diag[iter,,row,col]*params$sigma_caste_diag[iter,row]
      var_list_diag$vars$caste[iter,row,col] <- var(prediction_caste_diag)
      
      prediction_residence_diag <- params$z_residence_diag[iter,1:8,row,col]*params$sigma_residence_diag[iter,row]
      var_list_diag$vars$residence[iter,row,col] <- var(prediction_residence_diag)
      
      beta_duration_diag <- params$beta_duration_diag[iter,row,col]
      prediction_duration_diag <- beta_duration_diag*data_list$duration
      var_list_diag$vars$duration[iter,row,col] <- var(prediction_duration_diag)
      
      beta_native_diag <- params$beta_native_diag[iter,row,col]
      prediction_native_diag <- beta_native_diag*data_list$native
      var_list_diag$vars$native[iter,row,col] <- var(prediction_native_diag)
      
      var_list_diag$vars$total[iter,row,col] <- 
        var_list_diag$vars$id[iter,row,col] +
        var_list_diag$vars$caste[iter,row,col] +
        var_list_diag$vars$residence[iter,row,col] +
        var_list_diag$vars$duration[iter,row,col] +
        var_list_diag$vars$native[iter,row,col]
      
      var_list_diag$icc$id[iter,row,col] <- var_list_diag$vars$id[iter,row,col] / var_list_diag$vars$total[iter,row,col]
      var_list_diag$icc$caste[iter,row,col] <- var_list_diag$vars$caste[iter,row,col] / var_list_diag$vars$total[iter,row,col]
      var_list_diag$icc$residence[iter,row,col] <- var_list_diag$vars$residence[iter,row,col] / var_list_diag$vars$total[iter,row,col]
      var_list_diag$icc$duration[iter,row,col] <- var_list_diag$vars$duration[iter,row,col] / var_list_diag$vars$total[iter,row,col]
      var_list_diag$icc$native[iter,row,col] <- var_list_diag$vars$native[iter,row,col] / var_list_diag$vars$total[iter,row,col]
    }
  }
}

sapply(lapply(var_list_diag$icc, function(x) apply(x, 2:3, mean)), mean)
# id   duration     native      caste  residence 
# 0.83402586 0.06978352 0.01937164 0.04612038 0.03069861 

var_list_trans <- list(
  vars = list(
    id = array(dim=c(12000, 3, 3)),
    duration = array(dim=c(12000, 3, 3)),
    native = array(dim=c(12000, 3, 3)),
    caste = array(dim=c(12000, 3, 3)),
    residence = array(dim=c(12000, 3, 3)),
    total = array(dim=c(12000, 3, 3))
  ),
  icc = list(
    id = array(dim=c(12000, 3, 3)),
    duration = array(dim=c(12000, 3, 3)),
    native = array(dim=c(12000, 3, 3)),
    caste = array(dim=c(12000, 3, 3)),
    residence = array(dim=c(12000, 3, 3))
  )
)
for(iter in 1:12000) {
  for(row in 1:3) {
    for(col in 1:3) {
      prediction_id_trans <- params$z_trans[iter,,row,col]*params$sigma_trans[iter,row]
      var_list_trans$vars$id[iter,row,col] <- var(prediction_id_trans)
      
      prediction_caste_trans <- params$z_caste_trans[iter,,row,col]*params$sigma_caste_trans[iter,row]
      var_list_trans$vars$caste[iter,row,col] <- var(prediction_caste_trans)
      
      prediction_residence_trans <- params$z_residence_trans[iter,1:8,row,col]*params$sigma_residence_trans[iter,row]
      var_list_trans$vars$residence[iter,row,col] <- var(prediction_residence_trans)
      
      beta_duration_trans <- params$beta_duration_trans[iter,row,col]
      prediction_duration_trans <- beta_duration_trans*data_list$duration
      var_list_trans$vars$duration[iter,row,col] <- var(prediction_duration_trans)
      
      beta_native_trans <- params$beta_native_trans[iter,row,col]
      prediction_native_trans <- beta_native_trans*data_list$native
      var_list_trans$vars$native[iter,row,col] <- var(prediction_native_trans)
      
      var_list_trans$vars$total[iter,row,col] <- 
        var_list_trans$vars$id[iter,row,col] +
        var_list_trans$vars$caste[iter,row,col] +
        var_list_trans$vars$residence[iter,row,col] +
        var_list_trans$vars$duration[iter,row,col] +
        var_list_trans$vars$native[iter,row,col]
      
      var_list_trans$icc$id[iter,row,col] <- var_list_trans$vars$id[iter,row,col] / var_list_trans$vars$total[iter,row,col]
      var_list_trans$icc$caste[iter,row,col] <- var_list_trans$vars$caste[iter,row,col] / var_list_trans$vars$total[iter,row,col]
      var_list_trans$icc$residence[iter,row,col] <- var_list_trans$vars$residence[iter,row,col] / var_list_trans$vars$total[iter,row,col]
      var_list_trans$icc$duration[iter,row,col] <- var_list_trans$vars$duration[iter,row,col] / var_list_trans$vars$total[iter,row,col]
      var_list_trans$icc$native[iter,row,col] <- var_list_trans$vars$native[iter,row,col] / var_list_trans$vars$total[iter,row,col]
    }
  }
}

sapply(lapply(var_list_trans$icc, function(x) apply(x, 2:3, mean)), mean)

icc <- rbind(sapply(lapply(var_list_cross$icc, function(x) apply(x, 2:3, mean)), mean),
             sapply(lapply(var_list_ortho$icc, function(x) apply(x, 2:3, mean)), mean),
             sapply(lapply(var_list_diag$icc, function(x) apply(x, 2:3, mean)), mean),
             sapply(lapply(var_list_trans$icc, function(x) apply(x, 2:3, mean)), mean))
rownames(icc) <- c("across geometric spaces", "orthogonal", "diagonal", "transitional")








