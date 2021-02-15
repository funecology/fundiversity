test_that("memoised fd_chull works", {
  data("traits_birds")

  ch <- fd_chull(traits_birds)
  if (requireNamespace("memoise", quietly = TRUE)) {
    expect_true(memoise::has_cache(fd_chull)(traits_birds))
  }
})
