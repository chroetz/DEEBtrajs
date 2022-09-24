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
