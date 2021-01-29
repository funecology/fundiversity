#' Compute Rao's entropy index (Rao's Q)
#'
#' @inheritParams fd_fric
#' @param dist_matrix A dissimilarity matrix that can be provided instead of a
#'                    trait data.frame (default: `NULL`).
#'
#' @examples
#' data(traits_birds)
#' fd_raoq(traits_birds)
#'
#' @return a data.frame with two columns:
#' * `site` the names of the sites as the row names of the input `sp_com`,
#' * `Q` the values of Rao's quadratic entropy at each site.
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
fd_raoq <- function(traits = NULL, sp_com, dist_matrix = NULL) {

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
    traits <- remove_species_without_trait(traits)

    dist_matrix <- dist(traits)
  }

  dist_matrix <- as.matrix(dist_matrix)

  if (!missing(sp_com)) {

    common_species <- species_in_common(dist_matrix, sp_com)

    dist_matrix <- dist_matrix[common_species, common_species, drop = FALSE]
    sp_com      <- sp_com[, common_species, drop = FALSE]

  } else {

    sp_com <- matrix(1, ncol = nrow(dist_matrix),
                     dimnames = list("s1", rownames(dist_matrix)))

  }

  # Standardize abundance per site
  sp_com <- sp_com / rowSums(sp_com)

  # Compute Rao's Quadratic entropy for each site
  q_site <- diag(sp_com %*% tcrossprod(dist_matrix, sp_com))

 data.frame(site = rownames(sp_com),
            Q = q_site,
            row.names = NULL)
}
