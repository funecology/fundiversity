# Preamble code ----------------------------------------------------------------
data("traits_birds")
traits_birds_sc <- scale(traits_birds)


# Tests for valid inputs -------------------------------------------------------

test_that("Functional Divergence output format", {

  fdiv <- expect_silent(fd_fdiv(traits_birds))

  expect_s3_class(fdiv, "data.frame")
  expect_identical(dim(fdiv), c(1L, 2L))
  expect_named(fdiv, c("site", "FDiv"))

  expect_equal(fdiv$FDiv, 0.7282172, tolerance = 1e-7)
})

test_that("Functional Divergence works on subset of site/species", {
  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  expect_message(fd_fdiv(traits_birds, site_sp[, 2:ncol(site_sp),
                                               drop = FALSE]),
                 paste0("Differing number of species between trait dataset ",
                        "and site-species matrix\nTaking subset of species"))

  expect_message(fd_fdiv(traits_birds[2:nrow(traits_birds),], site_sp),
                 paste0("Differing number of species between trait dataset ",
                        "and site-species matrix\nTaking subset of species"))
})

test_that("Functional Divergence works for site with no species", {
  data("traits_plants")
  data("site_sp_plants")

  fdiv <- expect_silent(
    fd_fdiv(traits_plants, site_sp_plants[10,, drop = FALSE])
  )

  expect_identical(fdiv$FDiv[[1]], 0)
})

test_that("Functional Divergence works in 1D", {
  expect_identical(
    fd_fdiv(traits_birds[, 1]),
    fd_fdiv(traits_birds[, 1, drop = FALSE])
  )

})

test_that("Functional Divergence works with sparse matrices", {

  skip_if_not_installed("Matrix")

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  sparse_site_sp <- Matrix(site_sp, sparse = TRUE)

  fdiv <- expect_silent(fd_fdiv(traits_birds, sparse_site_sp))

  expect_s3_class(fdiv, "data.frame")
  expect_identical(dim(fdiv), c(1L, 2L))
  expect_named(fdiv, c("site", "FDiv"))

  expect_equal(
    fd_fdiv(traits_birds, sparse_site_sp),
    fd_fdiv(traits_birds, site_sp)
  )

})

test_that("Functional Divergence works with matrices without rownames", {

  traits_birds_un <- traits_birds
  rownames(traits_birds_un) <- NULL

  expect_identical(
    fd_fdiv(traits_birds_un),
    fd_fdiv(traits_birds)
  )

})

test_that("Functional Divergence works on data.frame as well as matrix", {

  site_sp <- matrix(1, ncol = nrow(traits_birds))
  colnames(site_sp) <-  rownames(traits_birds)
  rownames(site_sp) <- "s1"

  site_sp_df <- as.data.frame(site_sp)

  fdiv    <- expect_silent(fd_fdiv(traits_birds, site_sp))
  fdiv_df <- expect_silent(fd_fdiv(traits_birds, site_sp_df))

  expect_s3_class(fdiv, "data.frame")
  expect_identical(dim(fdiv), c(1L, 2L))
  expect_named(fdiv, c("site", "FDiv"))

  expect_equal(
    fdiv,
    fdiv_df
  )
})

test_that("Functional Divergence works with unmemoised version", {

  withr::local_options(fundiversity.memoise = FALSE)

  fdiv <- expect_silent(fd_fdiv(traits_birds))

  expect_s3_class(fdiv, "data.frame")
  expect_identical(dim(fdiv), c(1L, 2L))
  expect_named(fdiv, c("site", "FDiv"))

  expect_equal(fdiv$FDiv, 0.7282172, tolerance = 1e-7)
})

# Tests for invalid inputs -----------------------------------------------------

test_that("Functional Divergence fails gracefully", {

  # No traits
  expect_error(
    fd_fdiv(NULL, matrix(1)),
    "Please provide a trait dataset", fixed = TRUE
  )

  # Species matrix doesn't contain species from trait data
  expect_error(
    fd_fdiv(data.frame(a = 1, row.names = "sp1"), matrix(1)),
    paste0("No species in common found between trait dataset ",
           "and site-species matrix"),
    fixed = TRUE
  )

  ## Categorical trait data
  # Add non-continuous traits
  traits_birds_cat <- as.data.frame(traits_birds_sc)
  traits_birds_cat$cat_trait <- "a"

  expect_error(
    fd_fdiv(traits_birds_cat, site_sp_birds),
    paste0("Non-continuous trait data found in input traits. ",
           "Please provide only continuous trait data"),
    fixed = TRUE
  )
})
