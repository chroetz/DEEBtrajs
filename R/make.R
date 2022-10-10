#' Create a Trajs Object
#'
#' @param time `NULL` or a numeric vector of length 1 or n.
#' @param state A numeric matrix with 1 or n rows.
#' @param trajId `NULL` or an integer vector with 1 or n rows.
#' @param deriv `NULL` or a numeric matrix with 1 or n rows.
#' @return An object of S3 class Trajs.
#'
#' @export
makeTrajs <- function(time = NULL, state, trajId = NULL, deriv = NULL) {
  if (is.null(ncol(state))) state <- matrix(state, ncol=1)
  if (!is.null(deriv) && is.null(ncol(deriv))) deriv <- matrix(deriv, ncol=1)
  n <- max(length(time), length(trajId), nrow(state), nrow(deriv))
  stopifnot(is.null(time) || length(time) == 1 || length(time) == n)
  stopifnot(is.null(trajId) || length(trajId) == 1 || length(trajId) == n)
  stopifnot(nrow(state) == 1 || nrow(state) == n)
  stopifnot(is.null(deriv) || nrow(deriv) == 1 || nrow(deriv) == n)
  if (is.null(time)) time <- 0
  if (is.null(trajId)) trajId <- 1
  time <- as.double(time)
  trajId <- as.integer(trajId)
  if (NROW(state) == 1) state <- state[rep(1,n),]
  state <- matrix(as.double(state), nrow = n)
  if (!is.null(deriv)) {
    if (NROW(deriv) == 1) deriv <- deriv[rep(1,n),]
    deriv <- matrix(as.double(deriv), nrow = n)
  }
  trajs <- .makeTrajs(time, state, trajId, deriv)
  validateTrajs(trajs)
}

.makeTrajs <- function(time = NULL, state = NULL, trajId = NULL, deriv = NULL) {
  trajs <- tibble(
    trajId = trajId,
    time = time,
    state = state,
    deriv = deriv)
  class(trajs) <- c("Trajs", class(trajs))
  return(trajs)
}

#' Create a DerivTrajs Object
#'
#' @param state A numeric matrix with 1 or n rows.
#' @param deriv A numeric matrix with 1 or n rows.
#' @return An object of S3 class DerivTrajs.
#'
#' @export
makeDerivTrajs <- function(state, deriv) {
  if (is.null(nrow(state))) state <- matrix(state, nrow=1)
  if (is.null(nrow(deriv))) deriv <- matrix(deriv, nrow=1)
  n <- max(nrow(state), nrow(deriv))
  if (nrow(state) == 1) state <- state[rep(1,n),]
  state <- matrix(as.double(state), nrow = n)
  if (nrow(deriv) == 1) deriv <- deriv[rep(1,n),]
  deriv <- matrix(as.double(deriv), nrow = n)
  derivTrajs <- .makeDerivTrajs(state, deriv)
  validateDerivTrajs(derivTrajs)
  return(derivTrajs)
}

.makeDerivTrajs <- function(state, deriv) {
  derivTrajs <- tibble(
    state = state,
    deriv = deriv)
  class(derivTrajs) <- c("DerivTrajs", class(derivTrajs))
  return(derivTrajs)
}

