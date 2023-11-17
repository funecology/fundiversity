use_memoise <- function() {
  requireNamespace("memoise", quietly = TRUE) &&
    isTRUE(getOption("fundiversity.memoise", TRUE))
}
