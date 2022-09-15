#' @export
validateTrajs <- function(x) {
  stopifnot(inherits(x, "Trajs"))
  stopifnot(tibble::is_tibble(x))
  stopifnot(all(c("time", "state") %in% colnames(x)))
  stopifnot(all(colnames(x) %in% c("time", "state", "trajId", "deriv")))
  stopifnot(all(sapply(x, is.numeric)))
  stopifnot(is.matrix(x$state))
  stopifnot(!"deriv" %in% colnames(x) || is.matrix(x$deriv))
  return(invisible(x))
}
