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

  expect_equal(fd_fdiv(traits_birds)$FDiv, 0.6011971, tolerance = 1e-7)
})

test_that("Functional Divergence works in 1D", {
  expect_identical(
    fd_fdiv(traits_birds[, 1]),
    fd_fdiv(traits_birds[, 1, drop = FALSE])
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
