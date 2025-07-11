# This can be deleted in favour of base R version if we ever start depending
# on R (>= 4.4.0)
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}