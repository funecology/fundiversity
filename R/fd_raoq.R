#' Compute Rao's entropy index (Rao's Q)
#'
#' @param traits The matrix dataset for which you want to compute the index
#' @param sp_com Abundance matrix
#'
#' @examples
#' data(traits_birds)
#' fd_raoq(traits_birds)
#'
#' @return The value of Rao's Q (numeric of length 1)
#'
#' @references
#' Pavoine S., Dolédec S. (2005). The apportionment of quadratic entropy: a
#' useful alternative for partitioning diversity in ecological data.
#' Environmental and Ecological Statistics, 12(2), 125–138.
#' \doi{10.1007/s10651-005-1037-2}
#'
#' @export
fd_raoq<- function(traits, sp_com) {

  if (is.data.frame(traits)) {
    traits <- as.matrix(traits)
  }

  if (is.vector(traits)) {
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

  abund <- abund / sum(abund)

  d <- as.matrix(dist(traits))

  Q <- sum(combn(length(abund), 2, function(ij) {
    i <- ij[1]
    j <- ij[2]
    d[i,j] * abund[i] * abund[j]
  }))

  return(Q)
}
