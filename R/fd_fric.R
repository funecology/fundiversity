#' Compute Functional Richness FRic
#'
#' @param data The matrix dataset for which you want to compute the index
#'
#' @examples
#' data(traits_birds)
#' fd_fric(traits_birds)
#'
#' @return The value of FRic (numeric of length 1)
#'
#' @references
#' Cornwell W. K., Schwilk D. W., Ackerly D. D. (2006), A trait-based test for
#' habitat filtering; convex hull volume, Ecology 84(6),
#' \doi{0012-9658(2006)87[1465:ATTFHF]2.0.CO;2}
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
