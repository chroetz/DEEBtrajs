#' @export
setTrajId <- function(trajs, id) {
  trajs <- asTrajs(trajs)
  id <- as.numeric(id)
  trajs$trajId <- id
  validateTrajs(trajs)
}

#' @export
setDeriv <- function(trajs, deriv = NULL) {
  trajs <- asTrajs(trajs)
  if (is.null(deriv)) {
    n <- nrow(trajs$state)
    if (n < 2) stop("Must have at least 2 entries to calculate derivative.")
    deriv <- (trajs$state[2:n,,drop=FALSE] - trajs$state[1:(n-1),,drop=FALSE])/(trajs$time[2:n] - trajs$time[1:(n-1)])
    deriv <- (rbind(deriv[1, ], deriv) + rbind(deriv, deriv[n-1, ])) / 2
  } else {
    deriv <- matrix(as.numeric(as.matrix(deriv)), nrow = nrow(trajs$state))
  }
  trajs$deriv <- deriv
  validateTrajs(trajs)
}
