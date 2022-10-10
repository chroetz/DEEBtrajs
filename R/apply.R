#' Apply function to each trajectory of a Trajs object to obtain a new Trajs.
#'
#' Applies the function `fun` to each trajectory identified by the column trajId
#' of the Trajs object `trajs`. The result of `fun` is converted to a Trajs
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
    traj <- trajList[[i]]
    traj$trajId <- as.integer(names(trajList)[i])
    traj
  })
  return(bindTrajs(trajListWithId))
}

#' Apply function to each trajectory of a Trajs object.
#'
#' Applies the function `fun` to each trajectory identified by the column trajId
#' of the Trajs object `trajs`.
#'
#' @param trajs A Trajs object.
#' @param fun A function that maps a Trajs object with a single trajsId to
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

#' Apply function to each pair of trajectories from two Trajs objects.
#'
#' Applies the function `fun` to each pair of trajectories with the same
#' `trajId` of two Trajs objects. Only trajIds that occur in both objects
#' are used.
#'
#' @param trajs1,trajs2 Trajs objects.
#' @param fun A function that maps two Trajs objects with a single identical
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


#' Apply function columns of trajs state
#'
#' For each column and trajId, apply a function and return a tibble of the
#' results.
#'
#' @param trajs A Trajs object.
#' @param func A function that takes a column vector of the state of a single
#'   trajectory and returns a vector or a tibble.
#' @param ... further arguments to `func`
#' @return A tibble with a column trajId and further columns containing the
#'   results of `func`
#'
#' @export
applyToTrajStateCols <- function(trajs, func, ...) {
  trajs <- asTrajs(trajs)
  d <- getDim(trajs)
  res <- by(
    trajs$state,
    factor(trajs$trajId),
    \(x) apply(x, 2, func, simplify = TRUE, ...) |> matrix(ncol = d),
    simplify = FALSE)
  tibble(trajId = as.integer(names(res)), value = unclass(res)) |>
    tidyr::unnest(.data$value)
}

#' Set all states to a value that is constant in time
#'
#' Applies a function to each state dimension of each trajectory of a Trajs
#' object and returns a Trajs object with the same times and trajectory IDs
#' but each trajectory is constantly the result of the function.
#'
#' @param trajs A Trajs object.
#' @param fun A function mapping a numeric vector (one column, i.e. dimension,
#'   of a state matrix) to a single numeric value, e.g., `mean()`.
#' @return A Trajs object with the same time and trajId as `trajs` but
#'   constant states.
#'
#' @export
makeTrajsStateConst <- function(trajs, fun) {
  res <- applyToTrajStateCols(trajs, fun)
  constEsti <- .makeTrajs(
    time = trajs$time,
    trajId = trajs$trajId)
  constEsti <-
    constEsti |>
    dplyr::left_join(res, by = "trajId") |>
    dplyr::mutate(state = .data$value, value = NULL)
  return(constEsti)
}
