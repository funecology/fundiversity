#' Compute Rao's entropy index (Rao's Q)
#'
#' @param traits The matrix dataset for which you want to compute the index
#' @param sp_com Site-species matrix with sites as rows and species as columns
#'               if not provided, `fd_raoq()` considers all species with equal
#'               abundance in a single site
#' @param dist_matrix A dissimilarity matrix that can be provided instead of a
#'                    trait data.frame
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
fd_raoq <- function(traits, sp_com, dist_matrix = NULL) {

  if ((!is.null(traits) & !is.null(dist_matrix)) |
      (is.null(traits) & is.null(dist_matrix))) {
    stop(
      "Please provide either a trait dataset or a dissimilarity matrix",
      call. = FALSE
    )
  }

  if (is.data.frame(traits)) {
    traits <- as.matrix(traits)
  }

  if (is.vector(traits)) {
    traits <- as.matrix(traits)
  }

  d <- dist_matrix

  if (is.null(dist_matrix)) {
    d <- as.matrix(dist(traits))
  }

  if (!missing(sp_com)) {

    if (!all(labels(d)[[1]] %in% colnames(sp_com))) {
      stop(
        "Please provide a sp_com matrix that contains all species ",
        "from your traits dataset/dissimilarity matrix.", call. = FALSE
      )
    }

    sp_com <- sp_com[,labels(d)[[1]], drop = FALSE]

  } else {

    sp_com <- matrix(1, ncol = length(labels(d)[[1]]),
                     dimnames = list("s1", labels(d)[[1]]))

  }

  # Standardize abundance per site
  sp_com <- sweep(sp_com, 1, rowSums(sp_com), "/")

  # Compute Rao's Quadratic entropy over each row
  q_site = apply(sp_com, 1, function(site_row) {
    Q <- sum(combn(length(site_row), 2, function(ij) {
      i <- ij[1]
      j <- ij[2]
      d[i,j] * site_row[i] * site_row[j]
    }))
  })

 data.frame(site = row.names(sp_com),
            Q = q_site,
            row.names = NULL)
}
