#' Return the identity projection.
#'
#' @return A named list of two function: `project(x)` and `embed(x)`, both
#'   realize the identity function.
#' @export
getIdentityProjection <- function() {
  list(
    project = \(x) x,
    embed = \(x) x)
}

#' Calculate linear projection from data
#'
#' Calculates a PCA based linear projection to a `dim` space from a
#' `d = ncol(y)` dimensional space from `n = nrow(y)` data point stored in `y`.
#'
#' @param y A numeric matrix.
#' @param dim A single positive integer. The target dimension.
#' @return A named list of two function: `project(x)` maps `d` dimensional data
#'   stored in a matrix `x` with `d` columns to the lower dimensional space and
#'   outputs the respective values as matrix with the same number of rows as the
#'   input and `dim` columns. `embed(x)` maps `dim` dimensional data back to the
#'   `d` dimensional space, again via matrices with the respective column count.
#' @export
calculateProjection <- function(y, dim) {
  d <- ncol(y)
  n <- nrow(y)
  center <- colMeans(y)
  yCentered <- sweep(y, 2, center)
  if (n >= 2) {
    covmat <- cov(yCentered)
  } else {
    covmat <- matrix(0, nrow=d, ncol=3)
  }
  eig <- eigen(covmat, symmetric=TRUE)
  projectionMatrix <- eig$vectors[, seq_len(dim), drop=FALSE]
  list(
    project = \(x) sweep(x, 2, center) %*% projectionMatrix,
    embed = \(x) cbind(x, matrix(0, ncol = d-ncol(x), nrow = nrow(x))) %*% t(eig$vectors) |> sweep(2, -center))
}

