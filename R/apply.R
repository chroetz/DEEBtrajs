#' @export
mapTrajs2Trajs <- function(trajs, fun, ...) {
  trajs <- asTrajs(trajs)
  fun <- as.function(fun)
  if (!"trajId" %in% names(trajs)) return(fun(trajs, ...))
  trajIds <- unique(trajs$trajId)
  trajList <- lapply(trajIds, function(trajId) {
    trj <- trajs[trajs$trajId == trajId, ]
    res <- asTrajs(fun(trj, ...))
    res$trajId <- rep(trajId, nrow(res))
    return(res)
  })
  return(bindTrajs(trajList))
}
