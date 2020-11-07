#' Compute Functional Richness FRic
#'
#' @param data The matrix dataset for which you want to compute the index
#'
#' @examples
#' data(traits_birds)
#' fd_fric(traits_birds)
#'
#' @export
fd_fric <- function(data) {

  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  if (is.vector(data) || ncol(data) == 1) {
    return(diff(range(data)))
  } else {
    return(geometry::convhulln(data, "FA")$vol)
  }

}
