#' @export
asTrajs <- function(x) {
  UseMethod("asTrajs")
}

#' @export
asTrajs.Trajs <- function(x) {
  validateTrajs(x)
}

#' @export
asTrajs.data.frame <- function(x) {
  asTrajs.list(x)
}

#' @export
asTrajs.list <- function(x) {
  names <- names(x)
  stateIdxs <- getNameNumIdxs(names, "state")
  derivIdxs <- getNameNumIdxs(names, "deriv")
  trajs <- makeTrajs(
    time = x$time,
    trajId = if ("trajId" %in% names) x$trajId else NULL,
    state = as.matrix(x[stateIdxs]),
    deriv = if (length(derivIdxs) > 0) as.matrix(x[derivIdxs]) else NULL)
  return(trajs)
}

#' @export
asTrajs.matrix <- function(x) {
  names <- colnames(x)
  stateIdxs <- getNameNumIdxs(names, "state")
  derivIdxs <- getNameNumIdxs(names, "deriv")
  trajs <- makeTrajs(
    time = x[,"time"],
    trajId = if ("trajId" %in% names) x[,"trajId"] else NULL,
    state = x[,stateIdxs,drop=FALSE],
    deriv = if (length(derivIdxs) > 0) x[,derivIdxs,drop=FALSE] else NULL)
  return(trajs)
}

getNameNumIdxs <- function(names, prefix) {
  stopifnot(length(prefix) == 1)
  names <- as.character(names)
  prefix <- as.character(prefix)
  stateIdx <- grep(paste0("^", prefix, "\\d*$"), names)
  nums <-
    substr(names[stateIdx], start=nchar(prefix), stop=nchar(names[stateIdx])) |>
    as.integer() |>
    suppressWarnings()
  nums[is.na(nums)] <- 0
  stateIdx <- stateIdx[order(nums)]
  return(stateIdx)
}
