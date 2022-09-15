#' @export
hasDeriv <- function(trajs) {
  if (!isTrajs(Trajs)) {
    warning("Object is not a valid Trajs object.")
    return(FALSE)
  }
  return("deriv" %in% colnames(trajs))
}

#' @export
isTrajs <- function(x) {
  inherits(x, "Trajs") &&
    tibble::is_tibble(x) &&
    all(c("time", "state") %in% colnames(x)) &&
    all(colnames(x) %in% c("time", "state", "trajId", "deriv")) &&
    all(sapply(x, is.numeric)) &&
    is.matrix(x$state) &&
    (!"deriv" %in% colnames(x) || is.matrix(x$deriv))
}
