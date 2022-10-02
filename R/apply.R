#' @export
mapTrajs2Trajs <- function(trajs, fun, ...) {
  trajList <- applyTrajId(trajs, fun, ..., simplify = FALSE)
  trajListWithId <- lapply(seq_along(trajList), \(i) {
    traj <- asTrajs(trajList[[i]])
    traj <- setTrajId(traj, as.integer(names(trajList)[i]))
    traj
  })
  return(bindTrajs(trajListWithId))
}

#' @export
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

#' Calls a function and the two state matrices of the trajs and returns the resulting trajs of the same times but potentially of different dimension depending on the function.
#' If timeRange and nBasePoints are give an interpoation is done before the function is applied.
#' @export
compareTrajStates <- function(traj1, traj2, stateCompFun, timeRange = NULL, nBasePoints = 1000) {
  if (length(timeRange) == 2 && nBasePoints > 0) {
    times <- seq(timeRange[1], timeRange[2], length.out = nBasePoints)
    x <- interpolateTrajs(traj1, times)
    y <- interpolateTrajs(traj2, times)
  } else {
    x <- traj1
    y <- traj2
  }
  if (length(x$time) != length(y$time)) stop("Trying to compare trajs with different lengths.")
  stopifnot(all.equal(x$time, y$time))
  stopifnot(all(x$trajId == y$trajId))
  diffTraj <- makeTrajs(
    time = x$time,
    trajId = x$trajId,
    state = stateCompFun(x$state, y$state)) # TODO: apply by ID
  return(diffTraj)
}


#' Fills all state rows with at least one NA entry with a given (single) state.
#' @export
fillNaState <- function(trajs, state) {
  sel <- apply(trajs$state, 1, \(x) any(is.na(x)))
  if (all(!sel)) return(trajs)
  if (tibble::is_tibble(state)) {
    filler <-
      trajs |>
      dplyr::filter(sel) |>
      dplyr::left_join(state, by = "trajId")
    trajs$state[sel, ] <- filler$value
  } else {
    trajs$state[sel, ] <- rep(state, each = sum(sel))
  }
  return(trajs)
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

#' trajs to tibble with columns trajId and value
#' @export
applyToTrajStateCols <- function(trajs, func, ...) {
  trajs <- asTrajs(trajs)
  res <- by(
    trajs$state,
    factor(trajs$trajId),
    \(x) apply(x, 2, func, simplify = TRUE, ...) |> matrix(ncol = getDim(trajs)),
    simplify = FALSE)
  tibble::tibble(trajId = as.integer(names(res)), value = unclass(res)) |>
    tidyr::unnest(.data$value)
}

#' Applies function to traj state cols and returns a trajs with the same time and dimension but a constant state.
#' @export
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
