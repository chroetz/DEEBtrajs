#' @export
validateTrajs <- function(x, force = FALSE) {

  if (isFALSE(getOption("DEEBtrajs.validate")) && !force) return(invisible(x))

  stopifnot(inherits(x, "Trajs"))
  stopifnot(is_tibble(x))

  stopifnot(all(c("trajId", "time", "state") %in% colnames(x)))
  stopifnot(all(colnames(x) %in% c("time", "state", "trajId", "deriv")))

  stopifnot(all(sapply(x, is.numeric)))

  stopifnot(all(is.finite(x$trajId)))
  stopifnot(all(is.finite(x$time)))
  stopifnot(all(is.finite(x$state)))
  stopifnot(!"deriv" %in% colnames(x) || all(is.finite(x$deriv)))

  stopifnot(is.matrix(x$state))
  stopifnot(!"deriv" %in% colnames(x) || is.matrix(x$deriv))
  stopifnot(!"deriv" %in% colnames(x) || ncol(x$deriv) == ncol(x$state))

  return(x)
}

#' @export
validateDerivTrajs <- function(x, force = FALSE) {

  if (isFALSE(getOption("DEEBtrajs.validate")) && !force) return(invisible(x))

  stopifnot(inherits(x, "DerivTrajs"))
  stopifnot(is_tibble(x))

  stopifnot("state" %in% colnames(x))
  stopifnot(all(colnames(x) %in% c("state", "deriv")))

  stopifnot(all(sapply(x, is.numeric)))

  stopifnot(all(is.finite(x$state)))
  stopifnot(!"deriv" %in% colnames(x) || all(is.finite(x$deriv)))

  stopifnot(is.matrix(x$state))
  stopifnot(!"deriv" %in% colnames(x) || is.matrix(x$deriv))
  stopifnot(!"deriv" %in% colnames(x) || ncol(x$deriv) == ncol(x$state))

  return(x)
}
