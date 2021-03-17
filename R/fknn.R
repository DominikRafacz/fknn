#' Calculate Fuzzy k-Nearest Neighbors
#'
#' @details Keller, J. et al. “A fuzzy K-nearest neighbor algorithm.” IEEE Transactions on Systems, Man, and Cybernetics SMC-15 (1985): 580-585.
#'
#' @export
fknn <- function(train_features, train_labels, test_features, k, m) {

  n_train <- nrow(train_features)
  n_test <- nrow(test_features)
  d <- ncol(train_features) # == ncol(test_features)
  c <- ncol(train_labels)

  do.call(rbind, lapply(1:n_test, function(obs_ind) {
    distances_squared <- apply((train_features - matrix(rep(test_features[obs_ind, ], each = n_train), nrow = n_train))^2, 1, sum)
    nearest_inds <- order(distances_squared, decreasing = TRUE)[1:k]
    distances_squared <- distances_squared[nearest_inds] ^ (- 1 / (m - 1))
    matrix(distances_squared, ncol = k) %*% as.matrix(train_labels[nearest_inds, ]) / sum(distances_squared)
  }))
}
