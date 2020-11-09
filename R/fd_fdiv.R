#' Compute Functional Divergence (FDiv)
#'
#' @param traits The matrix dataset for which you want to compute the index
#'
#' @examples
#' data(traits_birds)
#' fd_fdiv(traits_birds)
#'
#' @return The value of FDiv (numeric of length 1)
#'
#' @references
#' Villéger S., Mason N. W. H., Mouillot D. (2008), New multidimensional
#' functional diversity indices for a multifaceted framework in functional
#' ecology, Ecology 89(8), \doi{10.1890/07-1206.1}
#'
#' @export
fd_fdiv <- function(traits, sp_com) {

  if (is.data.frame(traits)) {
    traits <- as.matrix(traits)
  }

  if (is.vector(data)) {
    traits <- as.matrix(traits)
  }

  G <- colMeans(traits, na.rm = TRUE)

  dG <- sqrt(colSums((t(traits) - G)^2, na.rm = TRUE))

  mean_dG <- mean(dG)

  deltaD <- sum(dG - mean_dG)

  deltaD_abs <- sum(abs(dG - mean_dG))

  FDiv <- (deltaD + mean_dG) / (deltaD_abs + mean_dG)

  return(FDiv)

}
