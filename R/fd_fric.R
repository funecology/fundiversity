#' Compute Functional Richness (FRic)
#'
#' Functional Richness is computed as the volume of the convex hull from all
#' included traits.
#'
#' @param traits The matrix dataset for which you want to compute the index
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
#' \doi{10.1890/0012-9658(2006)87[1465:ATTFHF]2.0.CO;2}
#'
#' @export
fd_fric <- function(traits) {

  if (is.data.frame(traits)) {
    data <- as.matrix(traits)
  }

  if (is.vector(traits) || ncol(traits) == 1) {
    return(diff(range(traits)))
  } else {
    return(geometry::convhulln(traits, "FA")$vol)
  }

}
