.onLoad <- function(libname, pkgname) {
  v <- getOption("DEEBtrajs.validate")
  if (is.null(v)) {
    options(DEEBtrajs.validate = TRUE)
  }
  invisible(NULL)
}
