#' Compute Rao's entropy index (Rao's Q)
#'
#' @inheritParams fd_fric
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
#' @importFrom stats dist
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

  if (is.data.frame(traits) | is.vector(traits)) {
    traits <- as.matrix(traits)
  }

  if (is.null(dist_matrix)) {
    dist_matrix <- dist(traits)
  }

  dist_matrix <- as.matrix(dist_matrix)

  if (!missing(sp_com)) {

    if (!all(rownames(dist_matrix) %in% colnames(sp_com))) {
      stop(
        "Please provide a site-species matrix that contains all species ",
        "from your traits dataset/dissimilarity matrix", call. = FALSE
      )
    }

    if (!all(colnames(sp_com) %in% rownames(dist_matrix))) {
      warning(
        "Some species included in your traits dataset/dissimilairy matrix are ",
        "not included in your sp_com matrix and have been dropped from the ",
        "computation.", call. = FALSE
      )
    }

    sp_com <- sp_com[, rownames(dist_matrix), drop = FALSE]

  } else {

    sp_com <- matrix(1, ncol = nrow(dist_matrix),
                     dimnames = list("s1", rownames(dist_matrix)))

  }

  # Standardize abundance per site
  sp_com <- sp_com / rowSums(sp_com)

  # Compute Rao's Quadratic entropy over each row
  q_site <- apply(sp_com, 1, function(sp_site) {
    Q <- sum(dist_matrix * outer(sp_site, sp_site)) / 2
  })

 data.frame(site = row.names(sp_com),
            Q = q_site,
            row.names = NULL)
}
