library(fundiversity)

if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  options(fundiversity.memoise = TRUE)
  test_check("fundiversity")
}
