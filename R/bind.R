#' @export
bindTrajs <- function(...) {
  if (...length() == 0) return(NULL)
  if (...length() == 1 && length(..1) == 0) return(NULL)
  res <- dplyr::bind_rows(...)
  return(asTrajs(res))
}
