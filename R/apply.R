#' Apply function to each trajectory of a Trajs object to obtain a new Trajs.
#'
#' Applies the function `fun` to each trajectory identified by the column trajId
#' of the DEEBtrajs object `trajs`. The result of `fun` is converted to a Trajs
#' and the respective trajId is set. The collected results are returned as a
#' single Trajs.
#'
#' @param trajs A Trajs object.
#' @param fun A function that maps a Trajs object with a single trajsId to an
#'   arbitrary value.
#' @param ... Further arguments for `fun`.
#' @return A Trajs object with the same trajIds as `trajs` but potentially
#'   different times.
#'
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

#' Apply function to each trajectory of a DEEBtrajs object.
#'
#' Applies the function `fun` to each trajectory identified by the column trajId
#' of the DEEBtrajs object `trajs`.
#'
#' @param trajs A DEEBtrajs object.
#' @param fun A function that maps a DEEBtrajs object with a single trajsId to
#'   an arbitrary value.
#' @param ... Further arguments for `fun`.
#' @param simplify Should the resulting list of values be simplified to an array
#'   if possible?
#' @return A list or an array depending on `simplify`. The trajIds are the names
#'   of the list entries or the names of the last dimension of the array.
#'
#' @export
applyTrajId <- function(trajs, fun, ..., simplify = FALSE) {
  trajs <- asTrajs(trajs)
  fun <- match.fun(fun)
  trajIds <- unique(trajs$trajId)
  lst <- lapply(trajIds, function(trajId) {
    fun(trajs[trajs$trajId == trajId, ], ...)
  })
  names(lst) <- trajIds
  if (simplify) {
    return(simplify2array(lst))
  } else {
    return(lst)
  }
}

#' Apply function to each pair of trajectories from two DEEBtrajs objects.
#'
#' Applies the function `fun` to each pair of trajectories with the same
#' `trajId` of two DEEBtrajs objects. Only trajIds that occur in both objects
#' are used.
#'
#' @param trajs1,trajs2 DEEBtrajs objects.
#' @param fun A function that maps two DEEBtrajs objects with a single identical
#'   trajsId to an arbitrary value.
#' @param ... Further arguments for `fun`.
#' @param simplify Should the resulting list of values be simplified to an array
#'   if possible?
#' @return A list or an array depending on `simplify`. The trajIds are the names
#'   of the list entries or the names of the last dimension of the array.
#'
#' @export
apply2TrajId <- function(trajs1, trajs2, fun, ..., simplify = FALSE) {
  trajs1 <- asTrajs(trajs1)
  trajs2 <- asTrajs(trajs2)
  fun <- match.fun(fun)
  if (!hasTrajId(trajs1) && !hasTrajId(trajs2)) return(fun(trajs1, trajs2, ...))
  trajIds <- intersect(unique(trajs1$trajId), unique(trajs2$trajId))
  lst <- lapply(trajIds, function(trajId) {
    fun(trajs1[trajs1$trajId == trajId, ], trajs2[trajs2$trajId == trajId, ], ...)
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

#' Set all states to a value that is constant in time
#'
#' Applies a function to each state dimension of each trajectory of a DEEBtrajs
#' object and returns a DEEBtrajs object with the same times and trajectory IDs
#' but each trajectory is constantly the result of the function.
#'
#' @param trajs A DEEBtrajs object.
#' @param fun A function mapping a numeric vector (one column, i.e. dimension,
#'   of a state matrix) to a single numeric value, e.g., `mean()`.
#' @return A DEEBtrajs object with the same time and trajId as `trajs` but
#'   constant states.
#'
#' @export
makeTrajsStateConst <- function(trajs, fun) {
  res <- applyToTrajStateCols(trajs, fun)
  constEsti <- makeTrajs(
    time = trajs$time,
    trajId = trajs$trajId,
    state = 0)
  constEsti <-
    constEsti |>
    dplyr::left_join(res, by = "trajId") |>
    dplyr::mutate(state = .data$value, value = NULL)
  return(constEsti)
}
