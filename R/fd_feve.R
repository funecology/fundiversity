#' Compute Functional Evenness (FEve)
#'
#' @inheritParams fd_raoq
#'
#' @examples
#' data(traits_birds)
#' fd_feve(traits_birds)
#' @return a data.frame with two columns:
#' * `site` character column that contains site names based on input `sp_com`
#' row names,
#' * `FEve` numeric column that contains FEve values corresponding to each site.
#'
#' @references
#' Villéger, S., Mason, N.W.H., Mouillot, D., 2008. New Multidimensional
#' Functional Diversity Indices for a Multifaceted Framework in Functional
#' Ecology. Ecology 89, 2290–2301. \doi{10.1890/07-1206.1}
#'
#' @importFrom stats dist
#'
#' @export
fd_feve <- function(traits = NULL, sp_com, dist_matrix = NULL) {
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
    sp_com <- sp_com[, common_species, drop = FALSE]
  } else {
    sp_com <- matrix(1,
      ncol = nrow(dist_matrix),
      dimnames = list("s1", rownames(dist_matrix))
    )
  }

  # Standardize abundance per site
  sp_com <- sp_com / rowSums(sp_com)

  feve_site <- apply(sp_com, 1, function(site_row) {
    fd_feve_single(site_row, dist_matrix)
  })

  data.frame(
    site = rownames(sp_com),
    FEve = feve_site,
    row.names = NULL
  )
}

# Hide gory details of computing single FEve values
fd_feve_single <- function(site_row, dist_matrix) {
  if (sum(site_row > 0) < 3) {
    FEve <- NA_real_
  } else {
    species <- names(site_row)[site_row > 0]

    mst <- vegan::spantree(dist_matrix[species, species])

    one_over_s_minus_one <- 1 / (mst$n - 1)

    parent_nodes <- mst$labels[seq_along(mst$kid) + 1]
    child_nodes  <- mst$labels[mst$kid]

    ew <- mst$dist / (site_row[parent_nodes] + site_row[child_nodes])

    pew <- ew / sum(ew)

    FEve <- (sum(pmin(pew, one_over_s_minus_one)) - one_over_s_minus_one) /
      (1 - one_over_s_minus_one)
  }

  return(FEve)
}
