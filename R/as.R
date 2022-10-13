#' @export
asTrajs <- function(x) {
  UseMethod("asTrajs")
}
#' @export
asDerivTrajs <- function(x) {
  UseMethod("asDerivTrajs")
}

#' @export
asTrajs.Trajs <- function(x) {
  validateTrajs(x)
}
#' @export
asDerivTrajs.DerivTrajs <- function(x) {
  validateDerivTrajs(x)
}

#' @export
asDerivTrajs.Trajs <- function(x) {
  makeDerivTrajs(
    state = x$state,
    deriv = if ("deriv" %in% names(x)) x$deriv else NULL)
}


#' @export
asTrajs.data.frame <- function(x) {
  asTrajs.list(x)
}
#' @export
asDerivTrajs.data.frame <- function(x) {
  asDerivTrajs.list(x)
}

#' @export
asTrajs.list <- function(x) {
  names <- names(x)
  stateIdxs <- getNameNumIdxs(names, "state")
  derivIdxs <- getNameNumIdxs(names, "deriv")
  if (!"time" %in% names) stop("Need `time` entry to convert to Trajs.")
  trajs <- makeTrajs(
    time = x$time,
    trajId = if ("trajId" %in% names) x$trajId else 1,
    state = as.matrix(x[stateIdxs]),
    deriv = if (length(derivIdxs) > 0) as.matrix(x[derivIdxs]) else NULL)
  return(trajs)
}
#' @export
asDerivTrajs.list <- function(x) {
  names <- names(x)
  stateIdxs <- getNameNumIdxs(names, "state")
  derivIdxs <- getNameNumIdxs(names, "deriv")
  derivTrajs <- makeDerivTrajs(
    state = as.matrix(x[stateIdxs]),
    deriv = if (length(derivIdxs) > 0) as.matrix(x[derivIdxs]) else NULL)
  return(derivTrajs)
}

#' @export
asTrajs.matrix <- function(x) {
  names <- colnames(x)
  stateIdxs <- getNameNumIdxs(names, "state")
  derivIdxs <- getNameNumIdxs(names, "deriv")
  if (!"time" %in% names) stop("Need `time` column to convert to Trajs.")
  trajs <- makeTrajs(
    time = x[,"time"],
    trajId = if ("trajId" %in% names) x[,"trajId"] else NULL,
    state = x[,stateIdxs,drop=FALSE],
    deriv = if (length(derivIdxs) > 0) x[,derivIdxs,drop=FALSE] else NULL)
  return(trajs)
}
#' @export
asDerivTrajs.matrix <- function(x) {
  names <- colnames(x)
  stateIdxs <- getNameNumIdxs(names, "state")
  derivIdxs <- getNameNumIdxs(names, "deriv")
  derivTrajs <- makeDerivTrajs(
    state = x[,stateIdxs,drop=FALSE],
    deriv = if (length(derivIdxs) > 0) x[,derivIdxs,drop=FALSE] else NULL)
  return(derivTrajs)
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
