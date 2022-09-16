#' @export
bindTrajs <- function(...) {
  res <- dplyr::bind_rows(...)
  return(asTrajs(res))
}
