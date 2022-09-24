#' @export
mapTrajs2Trajs <- function(trajs, fun, ...) {
  trajList <- applyTrajId(trajs, fun, ..., simplify = FALSE)
  trajList <- lapply(seq_along(trajList), \(i) {
    traj <- asTrajs(trajList[[i]])
    setTrajId(traj, as.integer(names(trajList)[i]))
    traj
  })
  return(bindTrajs(trajList))
}

applyTrajId <- function(trajs, fun, ..., simplify = FALSE) {
  trajs <- asTrajs(trajs)
  fun <- match.fun(fun)
  if (!"trajId" %in% names(trajs)) return(fun(trajs, ...))
  trajIds <- unique(trajs$trajId)
  lst <- lapply(trajIds, function(trajId) {
    trj <- trajs[trajs$trajId == trajId, ]
    res <- fun(trj, ...)
    return(res)
  })
  names(lst) <- trajIds
  if (simplify) {
    return(simplify2array(lst))
  } else {
    return(lst)
  }
}

compareTrajStates <- function(traj1, traj2, stateCompFun, timeRange, nBasePoints = 1000) {
  if (length(timeRange) == 2 && nBasePoints > 0) {
    times <- seq(timeRange[1], timeRange[2], length.out = nBasePoints)
    x <- interpolateTrajs(traj1, times)
    y <- interpolateTrajs(traj2, times)
  } else {
    x <- traj1
    y <- traj2
  }
  stopifnot(all.equal(x$time, y$time))
  stopifnot(all(x$trajId == y$trajId))
  diffTraj <- makeTrajs(
    time = x$time,
    trajId = x$trajId,
    state = stateCompFun(x$state, y$state)) # TODO: apply by ID
  return(diffTraj)
}

transformTrajState <- function(trajs, func) {
  trajs <- asTrajs(trajs)
  res <- by(
    trajs$state,
    factor(trajs$trajId),
    func,
    simplify=FALSE)
  makeTrajs(
    time = trajs$time,
    trajId = trajs$trajId,
    state = dplyr::bind_rows(unclass(res)))
}

applyToTrajStateCols <- function(trajs, func) {
  trajs <- asTrajs(trajs)
  res <- by(
    trajs$state,
    factor(trajs$trajId),
    \(x) apply(x, 2, func, simplify = TRUE) |> matrix(ncol = getDim(trajs)),
    simplify = FALSE)
  tibble::tibble(trajId = as.integer(names(res)), value = unclass(res)) |>
    tidyr::unnest(.data$value)
}

makeTrajsStateConst <- function(traj, fun) {
  res <- applyToTrajStateCols(traj, fun)
  constEsti <- makeTrajs(
    time = traj$time,
    trajId = traj$trajId,
    state = 0)
  constEsti <-
    constEsti |>
    dplyr::left_join(res, by = "trajId") |>
    dplyr::mutate(state = .data$value, value = NULL)
}
