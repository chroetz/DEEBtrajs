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
    trajs <- mapTrajs2Trajs(trajs, function(trj) {
      n <- nrow(trj$state)
      if (n < 2) stop("Must have at least 2 entries to calculate derivative.")
      deriv <-
        (trj$state[2:n,,drop=FALSE] - trj$state[1:(n-1),,drop=FALSE]) /
        (trj$time[2:n] - trj$time[1:(n-1)])
      deriv <- (rbind(deriv[1, ], deriv) + rbind(deriv, deriv[n-1, ])) / 2
      trj$deriv <- deriv
      return(trj)
    })
  } else {
    deriv <- matrix(as.numeric(as.matrix(deriv)), nrow = nrow(trajs$state))
    trajs$deriv <- deriv
  }
  validateTrajs(trajs)
}
