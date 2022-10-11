#' @export
getIdentityProjection <- function() {
  list(
    project = \(x) x,
    embed = \(x) x)
}

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

