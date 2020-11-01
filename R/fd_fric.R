#' @export
fd_fric <- function(data) {

  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  geometry::convhulln(data, "FA")$vol

}
