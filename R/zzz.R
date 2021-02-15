# Diverse utility functions

#' Memoized version of fd_chull loaded if package is installed
.onLoad <- function(pkgname, libname) {
  if (requireNamespace("memoise", quietly = TRUE)) {
    fd_chull <<- memoise::memoise(fd_chull)
  }
}
