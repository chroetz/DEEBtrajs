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
  n <- max(length(time), length(trajId), NROW(state), NROW(deriv))
  stopifnot(is.null(time) || length(time) == 1 || length(time) == n)
  stopifnot(is.null(trajId) || length(trajId) == 1 || length(trajId) == n)
  stopifnot(NROW(state) == 1 || NROW(state) == n)
  stopifnot(is.null(deriv) || NROW(deriv) == 1 || NROW(deriv) == n)
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
  trajs <- tibble::tibble(
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
#' @return An object of S3 class Trajs.
#'
#' @export
makeDerivTrajs <- function(state, deriv) {
  n <- max(NROW(state), NROW(deriv))
  if (NROW(state) == 1) state <- state[rep(1,n),]
  state <- matrix(as.double(state), nrow = n)
  if (NROW(deriv) == 1) deriv <- deriv[rep(1,n),]
  deriv <- matrix(as.double(deriv), nrow = n)
  trajs <- tibble::tibble(
    state = state,
    deriv = deriv)
  class(trajs) <- c("DerivTrajs", class(trajs))
  return(trajs)
}
