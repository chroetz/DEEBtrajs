#' @export
getDim <- function(trajs) {
  trajs <- asTrajs(trajs)
  return(ncol(trajs$state))
}

#' @export
getCount <- function(trajs) {
  trajs <- asTrajs(trajs)
  if (hasTrajId(trajs)) {
    trajsIds <- unique(trajs$trajId)
    n <- sapply(trajsIds, \(id) sum(trajs$trajId == id))
    names(n) <- trajsIds
    return(n)
  }
  return(nrow(trajs$state))
}

#' @export
getTrajsWithId <- function(trajs, trajId) {
  trajs <- asTrajs(trajs)
  stopifnot(hasTrajId(trajs))
  return(trajs[trajs$trajId == trajId,])
}


#' @export
getTrajIds <- function(trajs) {
  trajs <- asTrajs(trajs)
  if (!hasTrajId(trajs)) return(NULL)
  return(unique(trajs$trajId))
}

#' Get Trajs with (Potentially Interpolated) States at Initial Time
#'
#' @param trajs A Trajs object.
#' @param initialTime A single finite number.
#' @return A Trajs object with one row for each trajId of the input.
#' @export
getInitialState <- function(trajs, initialTime = 0) {
  initialTime <- as.double(initialTime)
  stopifnot(length(initialTime) == 1)
  stopifnot(is.finite(initialTime))
  interpolateTrajs(trajs, initialTime)
}

#' Get Trajs with States that are Closest To Given Time Point
#'
#' @param trajs A Trajs object.
#' @param time A single finite number.
#' @return A Trajs object with one row for each trajId of the input.
#' @export
getClosestInTime <- function(trajs, time) {
  time <- as.double(time)
  stopifnot(length(time) == 1)
  stopifnot(is.finite(time))
  trajs <- asTrajs(trajs)
  lst <- applyTrajId(trajs, getClosestInTimeOne, time=time)
  return(bindTrajs(lst))
}

getClosestInTimeOne <- function(traj, time) {
  i <- which.min(abs(traj$time - time))
  traj[i, ]
}


