rm(list=ls())
library(kolam)
library(DiagrammeR)
library(tidyr)
library(DiagrammeRsvg)

compute_trans_count_mat <- function(x, levels = NULL) {
  x <- c(x, x[[1]]) # it's a loop, so we connect the sequence
  if(is.null(levels)) levels <- unique(x)
  x <- factor(x, levels = levels)
  
  len  <- length(x)
  if(len < 2) return(matrix(0, nrow = length(levels(x)), ncol = length(levels(x))))
  
  from <- 1:(len-1)
  to   <- 2:len
  counts <- as.matrix(table(from = x[from], to = x[to], useNA = 'n'))
  
  return(counts)
}

############# Transition Network for kolam examples with same complexity #####
gestures <- c("h3", "op", 
              "d1", "d2", "d3", "d4", 
              "o1", "o2", "o3", "o4", 
              "t1", "t2", "t3", "t4")

### First example
kolam_1 <- loadKolam(read_yaml(file = "data/84d8aec.yaml"))
example_kolam <- Reduce('+', lapply(kolam_1$sequences, function(sequence) {
  sequence <- gsub('r|l', '', sequence)
  compute_trans_count_mat(sequence, gestures)
}))
rownames(example_kolam)[which(rownames(example_kolam) == "op")] <- "p4"
colnames(example_kolam)[which(colnames(example_kolam) == "op")] <- "p4"

idx <- list(
  orthogonal = c('o1', 'o2', 'o3', 'o4', 'h3', 'p4'),
  transitional = c('t1', 't2', 't3', 't4'),
  diagonal  = c('d1', 'd2', 'd3', 'd4')
)
mat <- matrix(NA, 3, 3)
colnames(mat) <- rownames(mat) <- names(idx)

for(row in rownames(mat)) {
  for(col in colnames(mat)) {
    mat[row,col] <- sum(example_kolam[idx[[row]], idx[[col]]])
  }
}

cross_example <- mat


postscript(file = "output/figure_1A_left.eps", width = 5, height = 5)
plotKolam(kolam_1,
          arrow = FALSE, loop_col = FALSE)
dev.off()

example_kolam_df <- as.data.frame(as.table(example_kolam))
colnames(example_kolam_df) <- c("label_from", "label_to", "value")

nodes <- data.frame(id = 1:8, 
                    label = c("o1", "o2", "o3", "o4", "p4",
                              "d4",
                              "t1", "t2"),
                    x = c(1, 2, 0.5, 1.5, 2.5,
                          6,
                          3, 4), 
                    y = c(5.41, 5.41, 3.59, 3.59, 4.5,
                          4.5,
                          2, 2),
                    height = rep(0.4, 8)*2,
                    fontsize = rep(10, 8)*2, 
                    fillcolor = c(rep("#4292C6", 5), rep("#7FBC41", 1),
                                  rep("#F46D43", 2)),
                    stringsAsFactors = FALSE)
edges <- expand.grid(label_from = nodes$label,  label_to = nodes$label,KEEP.OUT.ATTRS = FALSE)
edges <- left_join(edges, example_kolam_df, by= c("label_from", "label_to"))
edges <- edges[- which(edges$value == 0), ] # remove all edges below 0.01
edges$color <- "#636363"
edges$penwidth <- 1*edges$value
edges$type <- NULL
edges$headport <- c("we", rep(NA, nrow(edges)-1))
edges$tailport <- c("sw", rep(NA, nrow(edges)-1))
edges$label_from <- as.integer(edges$label_from)
edges$label_to <- as.integer(edges$label_to)

graph <- create_graph()
graph <- graph %>%
  add_nodes_from_table(nodes, label_col = label) %>%
  add_edges_from_table(edges, from_col = label_from, to_col = label_to,
                       from_to_map = id_external)
graph %>%
  render_graph()

graph %>%
  render_graph() %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg::rsvg_svg(file = "output/figure_1A_right.svg")

### Second example
kolam_2 <- loadKolam(read_yaml(file = "data/7333574.yaml"))
example_kolam <- Reduce('+', lapply(kolam_2$sequences, function(sequence) {
  sequence <- gsub('r|l', '', sequence)
  compute_trans_count_mat(sequence, gestures)
}))
rownames(example_kolam)[which(rownames(example_kolam) == "op")] <- "p4"
colnames(example_kolam)[which(colnames(example_kolam) == "op")] <- "p4"

idx <- list(
  orthogonal = c('o1', 'o2', 'o3', 'o4', 'h3', 'p4'),
  transitional = c('t1', 't2', 't3', 't4'),
  diagonal  = c('d1', 'd2', 'd3', 'd4')
)
mat <- matrix(NA, 3, 3)
colnames(mat) <- rownames(mat) <- names(idx)

for(row in rownames(mat)) {
  for(col in colnames(mat)) {
    mat[row,col] <- sum(example_kolam[idx[[row]], idx[[col]]])
  }
}

cross_example <- mat

postscript(file = "output/figure_1B_left.eps", width = 5, height = 5)
plotKolam(kolam_2,
          arrow = FALSE, loop_col = FALSE)
dev.off()

example_kolam_df <- as.data.frame(as.table(example_kolam))
colnames(example_kolam_df) <- c("label_from", "label_to", "value")


nodes <- data.frame(id = 1:7, 
                    label = c("o1", "o2", "o4",
                              "d1", "d4",
                              "t1", "t2"),
                    x = c(1, 2, 1,
                          5, 6,
                          3, 4), 
                    y = c(5.41, 5.41, 3.59,
                          4.5, 4.5,
                          2, 2),
                    height = rep(0.4, 7)*2,
                    fontsize = rep(10, 7)*2, 
                    fillcolor = c(rep("#4292C6", 3), rep("#7FBC41", 2),
                                  rep("#F46D43", 2)),
                    stringsAsFactors = FALSE)
edges <- expand.grid(label_from = nodes$label,  label_to = nodes$label,KEEP.OUT.ATTRS = FALSE)
edges <- left_join(edges, example_kolam_df, by= c("label_from", "label_to"))
edges <- edges[- which(edges$value == 0), ] # remove all edges below 0.01
edges$color <- "#636363"
edges$penwidth <- 0.4*edges$value
edges$type <- NULL
edges$headport <- c("we", rep(NA, nrow(edges)-1))
edges$tailport <- c("sw", rep(NA, nrow(edges)-1))
edges$label_from <- as.integer(edges$label_from)
edges$label_to <- as.integer(edges$label_to)

graph <- create_graph()
graph <- graph %>%
  add_nodes_from_table(nodes, label_col = label) %>%
  add_edges_from_table(edges, from_col = label_from, to_col = label_to,
                       from_to_map = id_external)
graph %>%
  render_graph()

graph %>%
  render_graph() %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg::rsvg_svg(file = "output/figure_1B_right.svg")

