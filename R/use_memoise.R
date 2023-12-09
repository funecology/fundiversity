# We respect users' explicit choice and error if we cannot honour it. However,
# by default, without any explicit option set by the user, we use memoise only
# if it was installed when fundiversity was loaded, without any messages.
use_memoise <- function() {
  # explicitly set to TRUE by user
  if (isTRUE(getOption("fundiversity.memoise"))) {
    if (exists("fd_chull_memoised")) {
      return(TRUE)
    }
    stop(
      "memoise is not installed",
      "or was installed after fundiversity was loaded.",
      "Please install memoise and restart R.",
      call. = FALSE
    )
  }
  # explicitly set to FALSE by user
  if (isFALSE(getOption("fundiversity.memoise"))) {
    return(FALSE)
  }
  # unspecified / default
  if (is.null(getOption("fundiversity.memoise"))) {
    # TRUE or FALSE depending on whether memoise was installed when fundiversity
    # was loaded
    return(exists("fd_chull_memoised"))
  }
}
