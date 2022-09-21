.onLoad <- function(libname, pkgname) {
  v <- getOption("DEETrajs.validate")
  if (is.null(v)) {
    options(DEETrajs.validate = TRUE)
  }
  invisible(NULL)
}
