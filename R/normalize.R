#' Calculate Mean-Covariance-Normalization Functions
#'
#' From the state of a reference DEEBtrajs object, two functions are create:
#' `normalize(tr)` and `denormalize(tr)`. Both take a DEEBtrajs object `tr` and
#' return it with a linearly transformed state. The linear transformation of
#' `normalize(tr)` is such that applied to the reference DEEBtrajs object the
#' state data has mean 0 and identity covariance. `denormalize(tr)` is the
#' inverse operation.
#'
#' The function assumes that the transformations are well-defined by the state
#' of the reference DEEBtrajs object.
#'
#' @param trajs An object that can be converted to a DEEBtrajs object. The
#'   reference. It can have one or several trajsIds. All calculations are
#'   performed without referencing the trajsId.
#' @return A named list of two functions. \describe{ \item{`normalize`}{The
#'   function `normalize(tr)`.} \item{`denormalize`}{The function
#'   `denormalize(tr)`.} }
#'
#' @export
calculateNormalization <- function(trajs) {
  trajs <- asTrajs(trajs)
  y <- trajs$state
  yMean <- colMeans(y)
  yCov <- stats::cov(y)
  eig <- eigen(yCov, symmetric = TRUE)
  yCovSqrt <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
  list(
    normalize = \(tr) {
      tr$state <- t(solve(yCovSqrt, t(tr$state) - yMean))
      return(tr)
    },
    denormalize = \(tr) {
      tr$state <- tr$state %*% yCovSqrt + rep(yMean, each=nrow(tr$state))
      return(tr)
    }
  )
}
