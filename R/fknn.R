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

  # value at [x, y] is a distance between x-th input point and y-th test point
  distances <- matrix(apply(
    (matrix(rep(test_features, each = n_train), ncol = d) -
     matrix(rep(t(train_features), times = n_test), ncol= d, byrow = TRUE))^2,
    1,
    sum), ncol = n_test)

  do.call(
    rbind,
    lapply(1:n_test, function(p_ind) {
      nearest_inds <- order(distances[, p_ind], decreasing = TRUE)[1:k]
      apply(
      train_labels[nearest_inds,] *
        matrix(rep(distances[nearest_inds, p_ind] ^ (- 1 / (m - 1)), times = c), ncol = c),
      2,
      sum) / sum (distances[nearest_inds, p_ind] ^ (- 1 / (m - 1)))
    }))
}
