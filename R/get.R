#' @export
getDim <- function(trajs) {
  stopifnot(isTrajs(trajs))
  return(ncol(trajs$state))
}

#' @export
getTrajsWithId <- function(trajs, trajId) {
  stopifnot(isTrajs(trajs))
  stopifnot(hasTrajId(trajs))
  return(trajs[trajs$trajId == trajId,])
}

#' @export
getInitialState <- function(trajs, initialTime = 0) {
  initial <- interpolateTrajs(trajs, initialTime)
  initialState <- initial$state
  if (hasTrajId(initialState)) {
    rownames(initialState) <- initial$trajId
  }
  return(initialState)
}

