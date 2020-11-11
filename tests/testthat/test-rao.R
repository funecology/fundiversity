# Preamble code
data("traits_birds")
simple_site_sp = matrix(1, nrow = 1, ncol = nrow(traits_birds),
                        dimnames = list("s1", row.names(traits_birds)))

# Actual tests
test_that("Rao's entropy output format", {

  rq <- expect_silent(fd_raoq(traits_birds, sp_com = simple_site_sp))

  expect_s3_class(rq, "data.frame")
  expect_length(rq, 2)
  expect_equal(nrow(rq), 1)
  expect_equal(colnames(rq), c("site", "Q"))


  rq <- expect_silent(fd_raoq(traits_birds))

  expect_s3_class(rq, "data.frame")
  expect_length(rq, 2)
  expect_equal(nrow(rq), 1)
  expect_equal(colnames(rq), c("site", "Q"))

})

test_that("Rao's entropy works in 1D", {

  expect_identical(
    fd_raoq(traits_birds[, 1], sp_com = simple_site_sp),
    fd_raoq(traits_birds[, 1, drop = FALSE], sp_com = simple_site_sp)
  )

})

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
})
