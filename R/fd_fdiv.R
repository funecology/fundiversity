#' Compute Functional Divergence (FDiv)
#'
#' @param traits The matrix dataset for which you want to compute the index
#' @param sp_com Abundance matrix
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

  if (!missing(sp_com)) {

    abund <- setNames(
      colSums(sp_com),
      colnames(sp_com)
    )

    if (!all(names(traits) %in% names(abund))) {
      stop(
        "Please provide a sp_com matrix that contains all species ",
        "from your traits dataset.", call. = FALSE
      )
    }

    abund[match(abund, rownames(traits))] <- abund

  } else {

    abund <- setNames(
      rep_len(1, nrow(traits)),
      rownames(traits))

  }

  G <- colMeans(traits, na.rm = TRUE)

  dG <- sqrt(colSums((t(traits) - G)^2, na.rm = TRUE))

  mean_dG <- mean(dG)

  deltaD <- sum(abund*(dG - mean_dG))

  deltaD_abs <- sum(abund*abs(dG - mean_dG))

  FDiv <- (deltaD + mean_dG) / (deltaD_abs + mean_dG)

  return(FDiv)

}
