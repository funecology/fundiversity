# Preamble code
data("traits_birds")
traits_birds_sc <- scale(traits_birds)

# Actual tests
test_that("Functional Divergence output format", {

  fdiv <- expect_silent(fd_fdiv(traits_birds))

  expect_s3_class(fdiv, "data.frame")
  expect_length(fdiv, 2)
  expect_equal(nrow(fdiv), 1)
  expect_equal(colnames(fdiv), c("site", "FDiv"))

  expect_equal(fdiv$FDiv, 0.7282172, tolerance = 1e-7)
})

test_that("Function Divergence works on subset of site/species", {
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

  expect_equal(fdiv$FDiv[[1]], 0)
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
  expect_length(fdiv, 2)
  expect_equal(nrow(fdiv), 1)
  expect_equal(colnames(fdiv), c("site", "FDiv"))

  expect_equal(
    fd_fdiv(traits_birds, sparse_site_sp),
    fd_fdiv(traits_birds, site_sp)
  )

})

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
})
