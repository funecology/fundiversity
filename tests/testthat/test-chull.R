test_that("memoised fd_chull works", {
  data("traits_birds")

  ch <- fd_chull_memoised(traits_birds)
  expect_true(memoise::has_cache(fd_chull_memoised)(traits_birds))
})
