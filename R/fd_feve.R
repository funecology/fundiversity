#' Compute Functional Evenness (FEve)
#'
#' This function computes Functional Evenness (FEve) following Villéger et al.
#' (2008). NB: By definition FEve is equal to `NA` when the number of species
#' per site is strictly lower than 3.
#'
#' @inheritParams fd_raoq
#'
#' @inheritSection fd_fric Parallelization
#'
#' @examples
#' data(traits_birds)
#' fd_feve(traits_birds)
#'
#' @return a data.frame with two columns:
#' * `site` character column that contains site names based on input `sp_com`
#' row names,
#' * `FEve` numeric column that contains FEve values corresponding to each site.
#'
#' ```{r child = "man/rmdchunks/no_row_names.Rmd"}
#' ```
#'
#' NB: By definition FEve is equal to `NA` when the number of species per site
#' is strictly lower than 3.
#'
#' @references
#' Villéger, S., Mason, N.W.H., Mouillot, D., 2008. New Multidimensional
#' Functional Diversity Indices for a Multifaceted Framework in Functional
#' Ecology. Ecology 89, 2290–2301. \doi{10.1890/07-1206.1}
#'
#' @importFrom stats dist
#' @importFrom future.apply future_apply
#'
#' @export
fd_feve <- function(traits = NULL, sp_com, dist_matrix = NULL) {

  if (!xor(is.null(traits), is.null(dist_matrix))) {
    stop(
      "Please provide either a trait dataset or a dissimilarity matrix",
      call. = FALSE
    )
  }

  if (is.data.frame(traits) || is.vector(traits)) {
    traits <- as.matrix(traits)
  }

  if (!is.null(traits) && !is.numeric(traits)) {
    stop("Non-continuous trait data found in input traits. ",
         "Please provide only continuous trait data", call. = FALSE)
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

  rownames(sp_com) <- rownames(sp_com) %||% paste0("s", seq_len(nrow(sp_com)))

  # Standardize abundance per site
  site_abundances <- rowSums(sp_com, na.rm = TRUE)
  site_abundances[site_abundances == 0] <- 1  # Account for site with no species
  sp_com <- sp_com / site_abundances

  feve_site <- future_apply(sp_com, 1, function(site_row) {
    fd_feve_single(site_row, dist_matrix)
  }, future.globals = FALSE)

  data.frame(
    site = rownames(sp_com),
    FEve = feve_site,
    row.names = NULL
  )
}

# Hide gory details of computing single FEve values
fd_feve_single <- function(site_row, dist_matrix) {

  species <- site_row > 0

  if (sum(species) < 3) {
    return(NA_real_)
  }

  mst <- vegan::spantree(dist_matrix[species, species])

  one_over_s_minus_one <- 1 / (mst$n - 1)

  parent_nodes <- mst$labels[seq_along(mst$kid) + 1]
  child_nodes  <- mst$labels[mst$kid]

  ew <- mst$dist / (site_row[parent_nodes] + site_row[child_nodes])

  pew <- ew / sum(ew)

  FEve <- (sum(pmin(pew, one_over_s_minus_one)) - one_over_s_minus_one) /
    (1 - one_over_s_minus_one)

  return(FEve)
}
