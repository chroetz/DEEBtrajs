#' @export
writeTrajs <- function(trajs, file) {
  trajs <- asTrajs(trajs)
  trajsFormated <- tibble(
    trajId = if ("trajId" %in% names(trajs)) format(trajs$trajId) else NULL,
    time = format(trajs$time))
  colnames(trajs$state) <- paste0("state", seq_len(ncol(trajs$state)))
  trajsFormated <- cbind(
    trajsFormated,
    as_tibble(format(trajs$state, digits = 1, scientific=FALSE, nsmall=8)))
  if (hasDeriv(trajs)) {
    colnames(trajs$deriv) <- paste0("deriv", seq_len(ncol(trajs$deriv)))
    trajsFormated <- cbind(
      trajsFormated,
      as_tibble(format(trajs$deriv, digits = 1, scientific=FALSE, nsmall=8)))
  }
  utils::write.csv(
    trajsFormated,
    file = file,
    quote = FALSE,
    row.names = FALSE)
}

#' @export
writeDerivTrajs <- function(trajs, file) {
  trajs <- asDerivTrajs(trajs)
  colnames(trajs$state) <- paste0("state", seq_len(ncol(trajs$state)))
  colnames(trajs$deriv) <- paste0("deriv", seq_len(ncol(trajs$deriv)))
  trajsFormated <- cbind(
    as_tibble(format(trajs$state, digits = 1, scientific=FALSE, nsmall=8)),
    as_tibble(format(trajs$deriv, digits = 1, scientific=FALSE, nsmall=8)))
  utils::write.csv(
    trajsFormated,
    file = file,
    quote = FALSE,
    row.names = FALSE)
}

#' @export
readTrajs <- function(file) {
  df <- utils::read.csv(file)
  trajs <- asTrajs(df)
  return(trajs)
}

#' @export
readDerivTrajs <- function(file) {
  df <- utils::read.csv(file)
  derivTrajs <- asDerivTrajs(df)
  return(derivTrajs)
}

#' @export
readTrajsOrDerivTrajs <- function(file) {
  df <- utils::read.csv(file)
  if ("time" %in% names(df)) {
    return(asTrajs(df))
  } else {
    return(asDerivTrajs(df))
  }
}
