test_that("memoised fd_chull_intersect works", {
  skip_if_not_installed("memoise")

  expect_true(memoise::is.memoised(fd_chull_intersect))

  data("traits_birds")
  ch <- fd_chull_intersect(traits_birds, traits_birds)

  expect_true(memoise::has_cache(fd_chull_intersect)(traits_birds, traits_birds))
})
