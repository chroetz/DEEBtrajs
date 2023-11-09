#' Get the (average) time step length of a DEEBtrajs object.
#'
#' Returns the mean time step over all trajectories and steps.
#'
#' @param trajs A DEEBtrajs object with one or many trajectories.
#' @param requireConst If `TRUE`, an error is raised if time steps differ between
#'   steps or trajectories.
#'
#' @export
getTimeStepTrajs <- function(trajs, requireConst = TRUE) {
  trajs <- asTrajs(trajs)
  diffList <- applyTrajId(trajs, \(traj) getTimeStep(traj$time, requireConst))
  diffs <- unlist(diffList)
  if (requireConst && diff(range(diffs)) > sqrt(.Machine$double.eps)) {
    stop("Time steps differ!")
  }
  return(mean(diffs))
}

#' @export
isTimeStepConstTrajs <- function(trajs) {
  trajs <- asTrajs(trajs)
  isConst <- applyTrajId(trajs, \(traj) isTimeStepConst(traj$time))
  return(all(unlist(isConst)))
}

#' @export
getTimeStep <- function(time, requireConst = TRUE) {
  time <- as.double(time)
  diffs <- diff(time)
  if (requireConst && diff(range(diffs)) > sqrt(.Machine$double.eps)) {
    stop("Time steps differ!")
  }
  mean(diffs)
}

#' @export
isTimeStepConst <- function(time) {
  time <- as.double(time)
  diffs <- diff(time)
  return(diff(range(diffs)) < sqrt(.Machine$double.eps))
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

