#' @export
makeTrajs <- function(time = NULL, state, trajId = NULL, deriv = NULL) {
  if (is.null(time)) {
    n <- NROW(state)
  } else {
    time <- as.double(time)
    n <- length(time)
  }
  state <- matrix(as.double(state), nrow = n)
  if (!is.null(trajId)) trajId <- as.integer(trajId)
  if (!is.null(deriv)) deriv <- matrix(as.double(deriv), nrow = n)
  trajs <- tibble::tibble(
    trajId = trajId,
    time = time,
    state = state,
    deriv = deriv)
  class(trajs) <- c("Trajs", class(trajs))
  return(trajs)
}

#' @export
makeDerivTrajs <- function(state, deriv) {
  n <- nrow(state)
  state <- matrix(as.double(state), nrow = n)
  deriv <- matrix(as.double(deriv), nrow = n)
  trajs <- tibble::tibble(
    state = state,
    deriv = deriv)
  class(trajs) <- c("DerivTrajs", class(trajs))
  return(trajs)
}
