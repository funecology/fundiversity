# Preamble code ----------------------------------------------------------------
data("traits_birds")
traits_birds_sc <- scale(traits_birds)


# Tests for valid inputs -------------------------------------------------------

test_that("Functional Dispersion output format", {

  fdis <- expect_silent(fd_fdis(traits_birds))

  expect_s3_class(fdis, "data.frame")
  expect_identical(dim(fdis), c(1L, 2L))
  expect_named(fdis, c("site", "FDis"))

  expect_equal(fdis$FDis, 133.3902, tolerance = 1e-7)
})

test_that("Functional Dispersion works on subset of site/species", {
  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  expect_message(fd_fdis(traits_birds, site_sp[, 2:ncol(site_sp),
                                               drop = FALSE]),
                 paste0("Differing number of species between trait dataset ",
                        "and site-species matrix\nTaking subset of species"))

  expect_message(fd_fdis(traits_birds[2:nrow(traits_birds),], site_sp),
                 paste0("Differing number of species between trait dataset ",
                        "and site-species matrix\nTaking subset of species"))
})

test_that("Functional Dispersion works for site with no species", {
  data("traits_plants")
  data("site_sp_plants")

  fdis <- expect_silent(
    fd_fdis(traits_plants, site_sp_plants[10,, drop = FALSE])
  )

  expect_identical(fdis$FDis[[1]], 0)
})

test_that("Functional Dispersion works in 1D", {
  expect_identical(
    fd_fdis(traits_birds[, 1]),
    fd_fdis(traits_birds[, 1, drop = FALSE])
  )

})

test_that("Functional Dispersion works with sparse matrices", {

  skip_if_not_installed("Matrix")

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  sparse_site_sp <- Matrix(site_sp, sparse = TRUE)

  fdis <- expect_silent(fd_fdis(traits_birds, sparse_site_sp))

  expect_s3_class(fdis, "data.frame")
  expect_identical(dim(fdis), c(1L, 2L))
  expect_named(fdis, c("site", "FDis"))

  expect_equal(
    fd_fdis(traits_birds, sparse_site_sp),
    fd_fdis(traits_birds, site_sp)
  )

})

test_that("Functional Dispersion works on data.frame as well as matrix", {

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  site_sp_df <- as.data.frame(site_sp)

  fdis    <- expect_silent(fd_fdis(traits_birds, site_sp))
  fdis_df <- expect_silent(fd_fdis(traits_birds, site_sp_df))

  expect_s3_class(fdis, "data.frame")
  expect_identical(dim(fdis), c(1L, 2L))
  expect_named(fdis, c("site", "FDis"))

  expect_equal(
    fdis,
    fdis_df
  )
})

test_that("Functional Dispersion works when sites have no names", {

  site_sp_no_names <- site_sp_birds
  rownames(site_sp_no_names) <- NULL

  fdis <- expect_silent(fd_fdis(traits_birds, site_sp_no_names))

  expect_s3_class(fdis, "data.frame")
  expect_identical(dim(fdis), c(1L, 2L))
  expect_named(fdis, c("site", "FDis"))

  expect_equal(fdis$FDis, 133.3902, tolerance = 1e-7)
  expect_equal(fdis[1, "site"], "1")

})

# Tests for invalid inputs -----------------------------------------------------

test_that("Functional Dispersion fails gracefully", {

  # No traits
  expect_error(
    fd_fdis(NULL, matrix(1)),
    "Please provide a trait dataset", fixed = TRUE
  )

  # Species matrix doesn't contain species from trait data
  expect_error(
    fd_fdis(data.frame(a = 1, row.names = "sp1"), matrix(1)),
    paste0("No species in common found between trait dataset ",
           "and site-species matrix"),
    fixed = TRUE
  )

  ## Categorical trait data
  # Add non-continuous traits
  traits_birds_cat <- as.data.frame(traits_birds_sc)
  traits_birds_cat$cat_trait <- "a"

  expect_error(
    fd_fdis(traits_birds_cat, site_sp_birds),
    paste0("Non-continuous trait data found in input traits. ",
           "Please provide only continuous trait data"),
    fixed = TRUE
  )
})

