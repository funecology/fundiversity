# Preamble code
data("traits_birds")
traits_birds_sc <- scale(traits_birds)

# Actual tests
test_that("Functional Divergence output format", {

  fdiv <- expect_silent(fd_fdiv(traits_birds))

  expect_type(fdiv, "double")
  expect_length(fdiv, 1)

})

test_that("Functional Divergence works in 1D", {

  expect_identical(
    fd_fdiv(traits_birds[, 1]),
    fd_fdiv(traits_birds[, 1, drop = FALSE])
  )

})
