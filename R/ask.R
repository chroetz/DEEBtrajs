#' @export
hasDeriv <- function(trajs) {
  if (!isTrajs(trajs)) {
    warning("Object is not a valid Trajs object.", call. = TRUE, immediate. = TRUE)
    return(FALSE)
  }
  return("deriv" %in% colnames(trajs))
}

#' @export
hasTrajId <- function(trajs) {
  if (!isTrajs(trajs)) {
    warning("Object is not a valid Trajs object.", call. = TRUE, immediate. = TRUE)
    return(FALSE)
  }
  return("trajId" %in% colnames(trajs))
}

#' @export
isTrajs <- function(x) {
  res <- tryCatch(validateTrajs(x), error = function(cond) FALSE)
  return(!isFALSE(res))
}
