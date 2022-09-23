#' @export
calculateNormalization <- function(traj) {
  if (length(getTrajIds(traj)) > 1) stop("Cannot deal with multiple trajIds")
  y <- traj$state
  yMean <- colMeans(y)
  yCov <- stats::cov(y)
  eig <- eigen(yCov, symmetric = TRUE)
  yCovSqrt <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
  list(
    normalize = \(traj) {
      traj$state <- t(solve(yCovSqrt, t(traj$state) - yMean))
      return(traj)
    },
    denormalize = \(traj) {
      traj$state <- traj$state %*% yCovSqrt + rep(yMean, each=nrow(traj$state))
      return(traj)
    }
  )
}
