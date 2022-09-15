Trajs <- function(time, state, trajId = NULL, deriv = NULL) {
  time <- as.double(time)
  n <- length(time)
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
