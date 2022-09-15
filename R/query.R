hasDeriv <- function(trajs) {
  if (!isTrajs(Trajs)) {
    warning("Object is not a Trajs object.")
    return(FALSE)
  }
  return("deriv" %in% colnames(trajs))
}

isTrajs <- function(x) {
  inherits(x, "Trajs") && tibble::is_tibble(x)
}
