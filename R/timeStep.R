#' Get the time step length of a DEEBtrajs object.
#'
#' Assuming there is a single time step length for all trajectories in the a
#' trajs object, this time step is returned. Otherwise an error is thrown.
#'
#' @param trajs A DEEBtrajs object with one or many trajectories.
#'
#' @export
getTimeStepTrajs <- function(trajs) {
  trajs <- asTrajs(trajs)
  diffList <- applyTrajId(trajs, \(traj) getTimeStep(traj$time))
  diffs <- unlist(diffList)
  if (diff(range(diffs)) > sqrt(.Machine$double.eps)) {
    stop("Time steps differ!")
  }
  mean(diffs)
}

#' @export
getTimeStep <- function(time) {
  time <- as.double(time)
  diffs <- diff(time)
  if (diff(range(diffs)) > sqrt(.Machine$double.eps)) {
    stop("Time steps differ!")
  }
  mean(diffs)
}

#' @export
isTimeIdentical <- function(trajs1, trajs2) {
  length(trajs1$time) == length(trajs2$time) &&
    all(trajs1$time == trajs2$time)
}

#' @export
isTimeEqual <- function(trajs1, trajs2) {
  length(trajs1$time) == length(trajs2$time) &&
    max(abs(trajs1$time - trajs2$time)) <= .Machine$double.eps*2^8
}

