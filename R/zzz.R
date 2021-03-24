# Diverse utility functions

# Memoized version of fd_chull loaded if package is installed
.onLoad <- function(libname, pkgname) {
  if (requireNamespace("memoise", quietly = TRUE) &
      getOption("fundiversity.memoise", TRUE)) {
    fd_chull_memoised <<- memoise::memoise(fd_chull)
    fd_chull_intersect_memoised <<-  memoise::memoise(fd_chull_intersect)
  } else {
    fd_chull_memoised <<- fd_chull
    fd_chull_intersect_memoised <<- fd_chull_intersect
  }
}
