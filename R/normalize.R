#' Calculate Mean-Covariance-Normalization Functions
#'
#' From the state of a reference Trajs object, two functions are create:
#' `normalize(tr)` and `denormalize(tr)`. Both take a Trajs or DerivTrajs object
#' `tr` and return it with a linearly transformed `state` and `deriv`. The
#' linear transformation of `normalize(tr)` is such that applied to the
#' reference object the state data has mean 0 and identity covariance.
#' `denormalize(tr)` is the inverse operation.
#'
#' The function assumes that the transformations are well-defined by the state
#' of the reference Trajs or DerivTrajs object.
#'
#' @param trajs A Trajs or DerivTrajs object. The reference. It can have one or
#'   several trajsIds. All calculations are performed without referencing the
#'   trajsId.
#' @param method A single character string. Choose `"None"` for making the two
#'   return functions identities.
#' @return A named list of two functions. \describe{ \item{`normalize`}{The
#'   function `normalize(tr)`.} \item{`denormalize`}{The function
#'   `denormalize(tr)`.} }
#'
#' @export
calculateNormalization <- function(trajs, method = c("MeanAndCov", "None")) {
  trajs <- asDerivTrajs(trajs)
  method <- match.arg(method)

  if (method == "None") {
    return(list(normalize = \(tr) tr, denormalize = \(tr) tr))
  }

  d <- ncol(trajs$state)
  y <- trajs$state
  yMean <- colMeans(y)
  if (nrow(y) < 2) {
    yCov <- diag(1, nrow = d, ncol = d)
  } else {
    yCov <- stats::cov(y)
  }
  eig <- eigen(yCov, symmetric = TRUE)
  if (any(abs(eig$values) < sqrt(.Machine$double.eps))) { # deal with singular matrices
    dg <- apply(y, 2, stats::sd)
    dg[abs(dg) < sqrt(.Machine$double.eps)] <- 1
    yCovSqrt <- diag(dg, nrow = d, ncol = d)
  } else {
    yCovSqrt <- eig$vectors %*% diag(sqrt(eig$values), nrow = d, ncol = d) %*% t(eig$vectors)
  }
  list(
    normalize = \(tr) {
      tr$state <- t(solve(yCovSqrt, t(tr$state) - yMean))
      if ("deriv" %in% names(tr)) {
        tr$deriv <- t(solve(yCovSqrt, t(tr$deriv)))
      }
      return(tr)
    },
    denormalize = \(tr) {
      tr$state <- tr$state %*% yCovSqrt + rep(yMean, each=nrow(tr$state))
      if ("deriv" %in% names(tr)) {
        tr$deriv <- tr$deriv %*% yCovSqrt
      }
      return(tr)
    }
  )
}
