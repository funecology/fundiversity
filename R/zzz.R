.onLoad <- function(libname, pkgname) {
  if (requireNamespace("memoise", quietly = TRUE)) {
    fd_chull_memoised <<- memoise::memoise(fd_chull)
  }
}
