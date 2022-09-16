#' @export
setTrajId <- function(trajs, id) {
  stopifnot(isTrajs(trajs))
  id <- as.numeric(id)
  trajs$trajId <- id
  validateTrajs(trajs)
}
