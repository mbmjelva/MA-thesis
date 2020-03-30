### Tester find best tree ###

# Hmm, kommer ut alt for mye info, ikke bare ett tre

best <- find_best_tree(cf_neg, "causal")
saveRDS(best, file = "./R-script, analysis/Models/best_tree.rds")

best$best_tree

# Plotter
library("DiagrammeR")
tree.plot100 = plot(get_tree(cf_neg, 100))
tree.plot100$dependencies
get_tree
get_tree(cf_neg, 1)

get_tree(cf_neg, best$best_tree)

?get_tree
# Må kjøre disse kodene for å få det over til å funke 
## Koder herfra: https://gist.github.com/ginward/451043145ef914f57af5a7272cf02489
# Eller mer generelt fra denne tråden: https://github.com/grf-labs/grf/issues/281
r_loss <- function(Y, samples, W.orig = NULL, W.hat = NULL, Z.orig, Z.hat, M.hat = NULL, Tau.hat = NULL) {
  size <- length(samples)
  if (size == 1) {
    return(Y[samples[1]]^2)
  }
  if (is.null(W.orig)) {
    unscaled_spread <- sum((Y[samples] - mean(Y[samples]))^2)
    output <- unscaled_spread * (size^2)/((size - 1)^2)
  } else if (is.null(Z.orig)) {
    unscaled_spread <- sum((Y[samples] - M.hat[samples] - (W.orig[samples] - W.hat[samples])*mean(Tau.hat[samples]))^2)
    output <- unscaled_spread * (size^2)/((size - 1)^2)
  } else {
    unscaled_spread <- sum((Z.orig[samples] - Z.hat[samples])*(Y[samples] - M.hat[samples] - (W.orig[samples] - W.hat[samples])*mean(Tau.hat[samples]))^2)
    output <- unscaled_spread * (size^2)/((size - 1)^2)    
  }
  return(output)
}

get_r_loss <- function(Y, tree, index, cost = 0, prune_info, W.orig = NULL, W.hat = NULL, Z.orig = NULL, Z.hat=NULL, M.hat = NULL, Tau.hat = NULL) {
  node <- tree$nodes[[index]]
  if (node$is_leaf) {
    # If the node is a leaf, then we just calculate the r_loss and return
    prune_info[[index]]$is_pruned_leaf <- TRUE
    prune_info[[index]]$samples <- node$samples
    node_r_loss <- r_loss(Y, node$samples, W.orig, W.hat, M.hat, Tau.hat)
    return(list(node_r_loss = node_r_loss, prune_info = prune_info))
  } else {
    # If the node is not a leaf, first we get the samples and r_loss of the left child
    left_leaf <- get_r_loss(Y, tree, node$left_child, cost, prune_info, W.orig, W.hat, Z.orig, Z.hat, M.hat, Tau.hat)
    new_prune_info <- left_leaf$prune_info
    left_r_loss <- left_leaf$node_r_loss
    # Then we get samples and r_loss from the right child
    right_leaf <- get_r_loss(Y, tree, node$right_child, cost, new_prune_info, W.orig, W.hat, Z.orig, Z.hat, M.hat, Tau.hat)
    new_prune_info <- right_leaf$prune_info
    right_r_loss <- right_leaf$node_r_loss
    # Then we aggregate the samples and calculace the aggregated r_loss
    node_samples <- c(new_prune_info[[node$left_child]]$samples, new_prune_info[[node$right_child]]$samples)
    new_prune_info[[index]]$samples <- node_samples
    node_r_loss <- r_loss(Y, node_samples, W.orig, W.hat, Z.orig, Z.hat, M.hat, Tau.hat)
    # Compare the r_losses, and decide whether to prune, then return
    if (node_r_loss < (left_r_loss + right_r_loss + cost)) {
      new_prune_info[[index]]$is_pruned_leaf <- TRUE
      return(list(node_r_loss = node_r_loss, prune_info = new_prune_info))
    } else {
      new_prune_info[[index]]$is_pruned_leaf <- FALSE
      return(list(node_r_loss = left_r_loss + right_r_loss + cost,
                  prune_info = new_prune_info))
    }
  }
}




find_best_tree <- function(forest, type = c("regression", "causal", "instrumental"), cost = 0) {
  best_r_loss <- Inf
  best_tree <- 0
  best_prune_info <- list()
  Y <- forest$Y.orig
  type <- match.arg(type)
  if (type == "causal") {
    W.orig <- forest$W.orig
    W.hat <- forest$W.hat
    M.hat <- forest$Y.hat
    Tau.hat <- forest$predictions
  } else if (type == "instrumental"){
    W.orig <- forest$W.orig
    W.hat <- forest$W.hat
    Z.orig <- forest$Z.orig
    Z.hat <- forest$Z.hat
    M.hat <- forest$Y.hat
    Tau.hat <- forest$predictions    
  }
  nt <- forest$'_num_trees'
  nt <- floor(nt/20)
  for (t in 1:forest$'_num_trees') {
    if (t%%nt == 0) cat("tree:", t, "\n")
    t_tree <- grf::get_tree(forest, t)
    prune_info <- rep(list(list(is_pruned_leaf = FALSE, samples = c())),
                      length(t_tree$nodes))
    if (type == "regression") {
      t_tree <- get_r_loss(Y, t_tree, 1, cost, prune_info)
    } else if (type == "causal") {
      t_tree <- get_r_loss(Y, t_tree, 1, cost, prune_info, W.orig, W.hat, M.hat, Tau.hat)
    } else {
      t_tree <- get_r_loss(Y, t_tree, 1, cost, prune_info, W.orig, W.hat, Z.orig, Z.hat, M.hat, Tau.hat)
    }
    if (t_tree$node_r_loss < best_r_loss) {
      best_r_loss <- t_tree$node_r_loss
      best_tree <- t
      best_prune_info <- t_tree$prune_info
    }
  }
  return(list(best_tree = best_tree, best_r_loss = best_r_loss, best_prune_info = best_prune_info))
}


find_leaf <- function(x, tree, prune_info) {
  nodes <- tree$nodes
  
  # Begin at root
  n <- nodes[[1]]
  idx <- 1
  
  # Propagate down until hit leaf
  while(!prune_info[[idx]]$is_pruned_leaf) {
    if (x[n$split_variable] <= n$split_value) {
      idx <- n$left_child
    } else {
      idx <- n$right_child
    }
    n <- nodes[[idx]]
  }
  return(idx)
}


estimate_params <- function(X, Y, tree, prune_info){
  tree_with_oob <- rep(list(list(samples = c(), sample_mean = c(), sample_sd = c())),
                       length(tree$nodes))
  oob_indices <- base::setdiff(seq(2*length(tree$drawn_samples)), tree$drawn_samples)
  for (idx in oob_indices) {
    correct_leaf <- find_leaf(X[idx,], tree, prune_info)
    tree_with_oob[[correct_leaf]]$samples <- c(tree_with_oob[[correct_leaf]]$samples, idx)
  }
  for (idx in 1:length(tree$nodes)) {
    if (prune_info[[idx]]$is_pruned_leaf){
      tree_with_oob[[idx]]$sample_mean <- mean(Y[tree_with_oob[[idx]]$samples])
      tree_with_oob[[idx]]$sample_sd <- sd(Y[tree_with_oob[[idx]]$samples])
    }
  }
  return(tree_with_oob)
}