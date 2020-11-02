#' Compute Functional Richness FRikc
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

  geometry::convhulln(data, "FA")$vol

}
