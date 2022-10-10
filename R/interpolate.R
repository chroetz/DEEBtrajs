#' @export
interpolateTrajs <- function(trajs, targetTimes) {
  trajs <- asTrajs(trajs)
  if (ConfigOpts::inheritsOptsClass(targetTimes, "Sequence")) {
    targetTimes <- ConfigOpts::makeSequence(targetTimes)
  }
  targetTimes <- as.double(targetTimes)
  stopifnot(all(is.finite(targetTimes)))
  mapTrajs2Trajs(trajs, .interpolateTrajs, targetTimes=targetTimes)
}


.interpolateTrajs <- function(traj, targetTimes) {
  if (length(traj$time) == length(targetTimes) && all(traj$time == targetTimes)) return(traj)
  beforeSel <- targetTimes < min(traj$time)
  beforeTimes <- targetTimes[beforeSel]
  beforeTraj <- traj[rep(1, sum(beforeSel)),,drop=FALSE]
  beforeTraj$time <- beforeTimes
  afterSel <- targetTimes > max(traj$time)
  afterTimes <- targetTimes[afterSel]
  afterTraj <- traj[rep(nrow(traj), sum(afterSel)),,drop=FALSE]
  afterTraj$time <- afterTimes
  duringSel <- !(beforeSel | afterSel)
  duringTimes <- targetTimes[duringSel]
  if (length(duringTimes) > 0) {
    state <- interpolateMatrix(traj$time, traj$state, duringTimes)
    deriv <- NULL
    if ("deriv" %in% names(traj)) {
      deriv <- interpolateMatrix(traj$time, traj$deriv, duringTimes)
    }
    duringTraj <- .makeTrajs(
      time = duringTimes,
      state = state,
      deriv = deriv
    )
    resTraj <- dplyr::bind_rows(beforeTraj, duringTraj, afterTraj)
  } else {
    resTraj <- dplyr::bind_rows(beforeTraj, afterTraj)
  }
  return(resTraj)
}

interpolateMatrix <- function(time, mat, targetTimes) {
  if (nrow(mat) == 1) {
    return(mat[rep(1, length(time)),,drop=FALSE])
  }
  do.call(
    cbind,
    lapply(seq_len(ncol(mat)), \(j) stats::approx(time, mat[,j], targetTimes)$y))
}

