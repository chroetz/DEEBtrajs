writeTrajs <- function(trajs, file) {
  trajs <- asTrajs(trajs)
  trajsFormated <- data.frame(
    trajId = if ("trajId" %in% names(trajs)) format(trajs$trajId) else NULL,
    time = format(trajs$time))
  colnames(trajs$state) <- paste0("state", seq_len(ncol(trajs$state)))
  trajsFormated <- cbind(
    trajsFormated,
    as.data.frame(format(trajs$state, digits = 1, scientific=FALSE, nsmall=8)))
  if (hasDeriv(trajs)) {
    colnames(trajs$deriv) <- paste0("deriv", seq_len(ncol(trajs$deriv)))
    trajsFormated <- cbind(
      trajsFormated,
      as.data.frame(format(trajs$deriv, digits = 1, scientific=FALSE, nsmall=8)))
  }
  utils::write.csv(
    trajsFormated,
    file = file,
    quote = FALSE,
    row.names = FALSE)
}


readDeData <- function(file) {
  df <- utils::read.csv(file)
  trajs <- asTrajs(df)
  return(trajs)
}
