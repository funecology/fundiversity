test_that("fd_chull_intersect() gives good value", {

  box_trait <- matrix(
    c(-0.5, 0.5, -0.5, 0.5, -0.5, -0.5, 0.5, 0.5), ncol = 2,
    dimnames = list(
      species = paste0("sp", 1:4), traits = paste0("trait", 1:2)
    )
  )

  expect_equal(fd_chull_intersect(box_trait, box_trait)$vol,  1)
  expect_equal(fd_chull_intersect(box_trait, box_trait)$area, 4)

})
