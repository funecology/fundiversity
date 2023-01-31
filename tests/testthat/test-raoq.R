# Preamble code ----------------------------------------------------------------
data("traits_birds")
simple_site_sp <- matrix(1, nrow = 1, ncol = nrow(traits_birds),
                        dimnames = list("s1", row.names(traits_birds)))


# Tests for valid inputs -------------------------------------------------------

test_that("Rao's entropy output format", {

  rq <- expect_silent(fd_raoq(traits_birds, sp_com = simple_site_sp))

  expect_s3_class(rq, "data.frame")
  expect_identical(dim(rq), c(1L, 2L))
  expect_named(rq, c("site", "Q"))


  rq <- expect_silent(fd_raoq(traits_birds))

  expect_s3_class(rq, "data.frame")
  expect_identical(dim(rq), c(1L, 2L))
  expect_named(rq, c("site", "Q"))

})

test_that("Rao's entropy computation are in line with other packages", {
  expect_equal(fd_raoq(traits_birds, simple_site_sp)$Q, 170.0519,
               tolerance = 1e-6)
})

test_that("Rao's entropy works in 1D", {

  expect_identical(
    fd_raoq(traits_birds[, 1], sp_com = simple_site_sp),
    fd_raoq(traits_birds[, 1, drop = FALSE], sp_com = simple_site_sp)
  )

})

test_that("Rao's Quadratic Entropy works on subset of site/species", {
  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  expect_message(fd_raoq(traits_birds, site_sp[, 2:ncol(site_sp),
                                               drop = FALSE]),
                 paste0("Differing number of species between trait dataset ",
                        "and site-species matrix\nTaking subset of species"))

  expect_message(fd_raoq(traits_birds[2:nrow(traits_birds),], site_sp),
                 paste0("Differing number of species between trait dataset ",
                        "and site-species matrix\nTaking subset of species"))
})

test_that("Rao's quadratric entropy works for sites with no species", {
  data("traits_plants")
  data("site_sp_plants")

  raoq <- expect_silent(
    fd_raoq(traits_plants, site_sp_plants[10,, drop = FALSE])
  )

  expect_identical(raoq$Q[[1]], 0)
})

test_that("Rao's Quadratic Entropy works on sparse matrices", {

  skip_if_not_installed("Matrix")

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  sparse_site_sp <- Matrix(site_sp, sparse = TRUE)

  sparse_dist_mat <- Matrix(as.matrix(dist(traits_birds)), sparse = TRUE)

  # Only site-species matrix is sparse
  expect_silent(fd_raoq(traits_birds, sparse_site_sp))

  expect_equal(
    fd_raoq(traits_birds, sparse_site_sp),
    fd_raoq(traits_birds, site_sp)
  )

  # Only distance matrix is sparse
  expect_silent(fd_raoq(sp_com = site_sp, dist_matrix = sparse_dist_mat))

  expect_equal(
    fd_raoq(sp_com = site_sp, dist_matrix = sparse_dist_mat),
    fd_raoq(sp_com = site_sp, dist_matrix = dist(traits_birds))
  )

  # Both site-species and distance matrix are sparse
  expect_silent(fd_raoq(sp_com = sparse_site_sp, dist_matrix = sparse_dist_mat))

  expect_equal(
    fd_raoq(sp_com = sparse_site_sp, dist_matrix = sparse_dist_mat),
    fd_raoq(sp_com = site_sp, dist_matrix = dist(traits_birds))
  )

})

test_that("Rao's Quadratic Entropy works on data.frame as well as matrix", {

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  site_sp_df <- as.data.frame(site_sp)

  raoq    <- expect_silent(fd_raoq(traits_birds, site_sp))
  raoq_df <- expect_silent(fd_raoq(traits_birds, site_sp_df))

  expect_s3_class(raoq, "data.frame")
  expect_identical(dim(raoq), c(1L, 2L))
  expect_named(raoq, c("site", "Q"))

  expect_equal(
    raoq,
    raoq_df
  )
})


# Tests for invalid inputs -----------------------------------------------------

test_that("Rao's entropy fails gracefully", {
  # No traits and no dissimilarity
  expect_error(
    fd_raoq(NULL, matrix(1), NULL),
    "Please provide either a trait dataset or a dissimilarity matrix",
    fixed = TRUE
  )

  # Both traits AND dissimilarity
  expect_error(
    fd_raoq(data.frame(a = 1), matrix(1), matrix(1)),
    "Please provide either a trait dataset or a dissimilarity matrix",
    fixed = TRUE
  )

  # Species matrix doesn't contain species from trait data
  expect_error(
    fd_raoq(data.frame(a = 1, row.names = "sp1"), matrix(1)),
    paste0("No species in common found between trait dataset ",
           "and site-species matrix"),
    fixed = TRUE
  )

  ## Categorical trait data
  # Add non-continuous traits
  traits_birds_cat <- as.data.frame(traits_birds)
  traits_birds_cat$cat_trait <- "a"

  expect_error(
    fd_raoq(traits_birds_cat, site_sp_birds),
    paste0("Non-continuous trait data found in input traits. ",
           "Please provide only continuous trait data"),
    fixed = TRUE
  )
})
