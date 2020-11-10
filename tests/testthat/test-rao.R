# Preamble code
data("traits_birds")
traits_birds_sc <- scale(traits_birds)

# Actual tests
test_that("Rao's entropy output format", {

  rq <- expect_silent(fd_fdiv(traits_birds))

  expect_type(rq, "double")
  expect_length(rq, 1)

})

test_that("Functional Divergence works in 1D", {

  expect_identical(
    fd_raoq(traits_birds[, 1]),
    fd_raoq(traits_birds[, 1, drop = FALSE])
  )

})
