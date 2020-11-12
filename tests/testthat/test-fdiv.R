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
    fd_fric(NULL, matrix(1)),
    "Please provide a trait dataset", fixed = TRUE
  )

  # Species matrix doesn't contain species from trait data
  expect_error(
    fd_raoq(data.frame(a = 1, row.names = "sp1"), matrix(1)),
    paste0("Please provide a site-species matrix that contains all species ",
           "from your traits dataset"),
    fixed = TRUE
  )
})
