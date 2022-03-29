#' @export
# Copied from base::warnings
qhull_warnings <- function(...) {

  if (!is.null(.pkgenv[["qhull.warning"]])) {
    structure(
      .pkgenv[["qhull.warning"]],
      dots = list(...),
      class = c("qhull.warnings", "warnings")
    )
  }

}
