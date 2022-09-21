#' @export
validateTrajs <- function(x, allowAdditionalColumns = TRUE) {

  if (isFALSE(getOption("DEETrajs.validate"))) return(invisible(x))

  stopifnot(inherits(x, "Trajs"))
  stopifnot(tibble::is_tibble(x))

  stopifnot(all(c("time", "state") %in% colnames(x)))
  if (!allowAdditionalColumns) {
    stopifnot(all(colnames(x) %in% c("time", "state", "trajId", "deriv")))
  }

  stopifnot(all(sapply(x, is.numeric))) # double or integer

  # For time and trajId, NA and Inf are not allowed. For state and deriv it is.
  stopifnot(all(is.finite(x$time)))
  stopifnot(!"trajId" %in% colnames(x) || all(is.finite(x$trajId)))

  stopifnot(is.matrix(x$state))
  stopifnot(ncol(x$state) >= 1)
  stopifnot(!"deriv" %in% colnames(x) || is.matrix(x$deriv))
  stopifnot(!"deriv" %in% colnames(x) || ncol(x$deriv) == ncol(x$state))

  return(invisible(x))
}
