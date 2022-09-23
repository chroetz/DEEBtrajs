#' @export
setTrajId <- function(trajs, id) {
  trajs <- asTrajs(trajs)
  id <- as.numeric(id)
  trajs$trajId <- id
  validateTrajs(trajs)
}

#' @export
setDeriv <- function(trajs, method = "center", deriv = NULL) {
  trajs <- asTrajs(trajs)
  if (is.null(deriv)) {
    if (hasTrajId(trajs)) {
      trajs <- mapTrajs2Trajs(trajs, function(traj) {
        traj$deriv <- calcDeriv(traj, method)
        return(traj)
      })
    } else {
      trajs$deriv <- calcDeriv(trajs, method)
    }
  } else {
    deriv <- matrix(as.numeric(as.matrix(deriv)), nrow = nrow(trajs$state))
    trajs$deriv <- deriv
  }
  validateTrajs(trajs)
}

calcDeriv <- function(traj, method) {
  n <- nrow(traj$state)
  if (n < 2) stop("Must have at least 2 entries to calculate derivative.")
  deriv <-
    (traj$state[2:n,,drop=FALSE] - traj$state[1:(n-1),,drop=FALSE]) /
    (traj$time[2:n] - traj$time[1:(n-1)])
  if (method == "center") {
    deriv <- (rbind(deriv[1, ], deriv) + rbind(deriv, deriv[n-1, ])) / 2
  } else if (method == "left") {
    deriv <- rbind(deriv, deriv[n-1, ])
  } else if (method == "right") {
    deriv <- rbind(deriv[1, ], deriv)
  } else {
    stop("Unknown method for calculating the derivative: ", method)
  }
  return(deriv)
}
