# Diverse utility functions

# Memoized version of fd_chull loaded if package is installed
.onLoad <- function(libname, pkgname) {
  if (requireNamespace("memoise", quietly = TRUE)) {
    fd_chull <<- memoise::memoise(fd_chull)
  }
}
