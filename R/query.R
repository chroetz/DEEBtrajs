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
  res <- tryCatch(validateTrajs(x), error = function(cond) FALSE)
  return(!isFALSE(res))
}
