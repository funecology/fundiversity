# Preamble code
data("traits_birds")
traits_birds_sc <- scale(traits_birds)

# Actual tests
test_that("Functional Dispersion output format", {

  fdis <- expect_silent(fd_fdis(traits_birds))

  expect_s3_class(fdis, "data.frame")
  expect_length(fdis, 2)
  expect_equal(nrow(fdis), 1)
  expect_equal(colnames(fdis), c("site", "FDis"))

  expect_equal(fdis$FDis, 146.2072, tolerance = 1e-7)
})

test_that("Function Dispersion works on subset of site/species", {
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
  expect_length(fdis, 2)
  expect_equal(nrow(fdis), 1)
  expect_equal(colnames(fdis), c("site", "FDis"))

  expect_equal(
    fd_fdis(traits_birds, sparse_site_sp),
    fd_fdis(traits_birds, site_sp)
  )

})

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
})
