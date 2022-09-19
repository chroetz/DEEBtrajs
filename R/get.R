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
getInitialState <- function(trajs, initialTime = 0) {
  trajs <- asTrajs(trajs)
  initial <- interpolateTrajs(trajs, initialTime)
  initialState <- initial$state
  if (hasTrajId(trajs)) {
    rownames(initialState) <- initial$trajId
  }
  return(initialState)
}

