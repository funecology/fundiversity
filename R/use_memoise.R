#' Options for \pkg{fundiversity}
#'
#' The memoisation is the convex hull computation in \pkg{fundiversity} is
#' controlled via the `fundiversity.memoise` option:
#' - if unset, the default is to use memoisation if \pkg{memoise} was installed
#'  when \pkg{fundiversity} was loaded, and not to use memoisation otherwise.
#' - if `options(fundiversity.memoise = TRUE)`, memoisation is used and an error
#'  is thrown if \pkg{memoise} is not installed.
#' - if `options(fundiversity.memoise = FALSE)`, memoisation is not used.
#'
#' @name fundiversity-options
NULL

#' @keywords internal
use_memoise <- function() {

  # Cannot use memoise in parallel settings
  if (!inherits(future::plan(), "sequential")) {
    return(FALSE)
  }

  # explicitly set to TRUE by user
  if (isTRUE(getOption("fundiversity.memoise"))) {
    if (exists("fd_chull_memoised")) {
      return(TRUE)
    }
    stop(
      "memoise is not installed ",
      "or was installed after fundiversity was loaded. ",
      "Please install memoise and restart R.",
      call. = FALSE
    )
  }
  # explicitly set to FALSE by user
  if (isFALSE(getOption("fundiversity.memoise"))) {
    return(FALSE)
  }
  # unspecified / default
  # TRUE or FALSE depending on whether memoise was installed when fundiversity
  # was loaded
  return(exists("fd_chull_memoised"))
}

# Added this to make 'testthat::local_mocked_bindings()' work
# in 'test-use_memoise.R'
exists <- NULL
