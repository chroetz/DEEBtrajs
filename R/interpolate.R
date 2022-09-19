#' @export
interpolateTrajs <- function(trajs, targetTimes) {
  trajs <- asTrajs(trajs)
  targetTimes <- as.double(targetTimes)
  stopifnot(all(is.finite(targetTimes)))
  d <- getDim(trajs)
  lst <- lapply(unique(trajs$trajId), \(trajId) {
    traj <- getTrajsWithId(trajs, trajId)
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
      trajId = trajId,
      state = state,
      deriv = deriv
    )
    resTraj <- bindTrajs(beforeTraj, duringTraj, afterTraj)
  })
  return(bindTrajs(lst))
}
