#' @export
interpolateTrajs <- function(trajs, targetTimes) {
  trajs <- asTrajs(trajs)
  targetTimes <- as.double(targetTimes)
  stopifnot(all(is.finite(targetTimes)))
  if (!hasTrajId(trajs)) return(.interpolateTrajs(trajs, targetTimes))
  lst <- lapply(unique(trajs$trajId), \(trajId) {
    traj <- getTrajsWithId(trajs, trajId)
    traj$trajId <- NULL
    res <- .interpolateTrajs(traj, targetTimes)
    res <- setTrajId(res, trajId)
  })
  return(bindTrajs(lst))
}


.interpolateTrajs <- function(traj, targetTimes) {
  d <- getDim(traj)
  beforeSel <- targetTimes < min(traj$time)
  beforeTimes <- targetTimes[beforeSel]
  beforeTraj <- traj[rep(1, sum(beforeSel)),]
  beforeTraj$time <- beforeTimes
  afterSel <- targetTimes > max(traj$time)
  afterTimes <- targetTimes[afterSel]
  afterTraj <- traj[rep(nrow(traj), sum(afterSel)),]
  afterTraj$time <- afterTimes
  duringSel <- !(beforeSel | afterSel)
  duringTimes <- targetTimes[duringSel]
  if (length(duringTimes) > 0) {
    state <- do.call(
      cbind,
      lapply(seq_len(d), \(j) stats::approx(traj$time, traj$state[,j], duringTimes)$y))
    deriv <- NULL
    if (hasDeriv(traj)) {
      deriv <- do.call(
        cbind,
        lapply(seq_len(d), \(j) stats::approx(traj$time, traj$state[,j], duringTimes)$y))
    }
    duringTraj <- makeTrajs(
      time = duringTimes,
      state = state,
      deriv = deriv
    )
    resTraj <- bindTrajs(beforeTraj, duringTraj, afterTraj)
  } else {
    resTraj <- bindTrajs(beforeTraj, afterTraj)
  }
  return(resTraj)
}
