asTrajs <- function(x) {
  UseMethod("asTrajs")
}

asTrajs.Trajs <- function(x) {
  x # TODO: validate Trajs
}

asTrajs.data.frame <- function(x) {
  asTrajs.list(x)
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

asTrajs.list <- function(x) {
  names <- names(x)
  stateIdxs <- getNameNumIdxs(names, "state")
  derivIdxs <- getNameNumIdxs(names, "deriv")
  tb <- tibble::tibble(
    time = x$time,
    trajId = if ("trajId" %in% names) x$trajId else NULL,
    state = as.matrix(x[stateIdxs]),
    deriv = if (length(derivIdxs) > 0) as.matrix(x[derivIdxs]) else NULL)
  class(tb) <- c("Trajs", class(tb))
  return(tb)
}

asTrajs.matrix <- function(x) {
  names <- colnames(x)
  stateIdxs <- getNameNumIdxs(names, "state")
  derivIdxs <- getNameNumIdxs(names, "deriv")
  tb <- tibble::tibble(
    time = x[,"time"],
    trajId = if ("trajId" %in% names) x[,"trajId"] else NULL,
    state = x[,stateIdxs,drop=FALSE],
    deriv = if (length(derivIdxs) > 0) x[,derivIdxs,drop=FALSE] else NULL)
  class(tb) <- c("Trajs", class(tb))
  return(tb)
}
