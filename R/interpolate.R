#' @export
interpolateTrajs <- function(trajs, targetTimes = NULL, interSteps = NULL) {

  # TODO allow for pattern target trajs to get interpolation of different times for different trajIds

  trajs <- asTrajs(trajs)
  if (!is.null(targetTimes)) {
    stopifnot(is.null(interSteps))
    if (ConfigOpts::inheritsOptsClass(targetTimes, "Sequence")) {
      targetTimes <- ConfigOpts::makeSequence(targetTimes) # TODO: use as.double() for Opts?
    }
    targetTimes <- as.double(targetTimes)
    stopifnot(all(is.finite(targetTimes)))
    return(mapTrajs2Trajs(trajs, .interpolateTrajsTargetTimes, targetTimes=targetTimes))
  }

  stopifnot(!is.null(interSteps))
  interSteps <- as.integer(interSteps)
  stopifnot(length(interSteps) == 1)
  stopifnot(is.finite(interSteps))
  stopifnot(interSteps > 0)
  return(mapTrajs2Trajs(trajs, .interpolateTrajsInterSteps, interSteps=interSteps))
}


.interpolateTrajsTargetTimes <- function(traj, targetTimes) {
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

#' @export
interpolateTime <- function(time, interSteps) {
  c(
    vapply(
      seq_len(length(time)-1),
      \(i) {
        seq(time[i], time[i+1], length.out = interSteps+1)[-(interSteps+1)]
      },
      double(interSteps)
    ),
    time[length(time)]
  )
}

#' @export
interpolateTimeAvg <- function(trajs, interSteps) {
  avgTimeStep <- getTimeStepTrajs(trajs, requireConst=FALSE)
  nSteps <- round(diff(range(trajs$time)) / avgTimeStep * interSteps + 1)
  outTime <- seq(min(trajs$time), max(trajs$time), length.out = nSteps)
  outTime[length(outTime)] <- max(trajs$time)
  return(outTime)
}

#' @export
interpolateTimeAdaptive <- function(trajs, interSteps) {
  if (isTimeStepConstTrajs(trajs))
    interpolateTime(trajs$time, interSteps)
  else
    interpolateTimeAvg(trajs, interSteps)
}

.interpolateTrajsInterSteps <- function(traj, interSteps) {
  if (interSteps == 1) return(traj)
  tms <- interpolateTime(traj$time, interSteps)
  state <- interpolateMatrix(traj$time, traj$state, tms)
  deriv <- NULL
  if ("deriv" %in% names(traj)) {
    deriv <- interpolateMatrix(traj$time, traj$deriv, tms)
  }
  .makeTrajs(
    time = tms,
    state = state,
    deriv = deriv,
    trajId = traj$trajId[1]
  )
}

interpolateMatrix <- function(time, mat, targetTimes) {
  naRows <- apply(mat, 1, \(x) any(is.na(x)))
  time <- time[!naRows]
  mat <- mat[!naRows,,drop=FALSE]

  if (nrow(mat) == 0) {
    return(matrix(nrow = length(time), ncol = ncol(mat)))
  }
  if (nrow(mat) == 1) {
    return(mat[rep(1, length(time)),,drop=FALSE])
  }
  do.call(
    cbind,
    lapply(seq_len(ncol(mat)), \(j) stats::approx(time, mat[,j], targetTimes)$y))
}

