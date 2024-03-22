test_that("use_memoise() works when memoise is installed", {

  # Pretend fd_chull_memoised() exists = memoise installed before fundiversity
  local_mocked_bindings(exists = function(...) TRUE)

  # If memoise installed & options TRUE
  withr::local_options(fundiversity.memoise = TRUE)
  expect_true(use_memoise())

  # If memoise installed & options FALSE
  withr::local_options(fundiversity.memoise = FALSE)
  expect_false(use_memoise())

  # If memoise installed & options NULL
  withr::local_options(fundiversity.memoise = NULL)
  expect_true(use_memoise())

})


test_that("use_memoise() sends back FALSE when memoise isn't installed", {

  local_mocked_bindings(exists = function(...) FALSE)

  # If memoise not installed & options TRUE
  withr::local_options(fundiversity.memoise = TRUE)
  expect_error(
    use_memoise(),
    regexp = paste0(
      "memoise is not installed or was installed after fundiversity was ",
      "loaded. Please install memoise and restart R."
    ),
    fixed = TRUE
  )

  # If memoise not installed & options FALSE
  withr::local_options(fundiversity.memoise = FALSE)
  expect_false(use_memoise())

  # If memoise not installed & options NULL
  withr::local_options(fundiversity.memoise = NULL)
  expect_false(use_memoise())

})


test_that("use_memoise() sends back FALSE when plan is parallel", {

  future::plan("multisession")

  withr::local_options(fundiversity.memoise = TRUE)
  expect_false(use_memoise())

  # If memoise not installed & options FALSE
  withr::local_options(fundiversity.memoise = FALSE)
  expect_false(use_memoise())

  # If memoise not installed & options NULL
  withr::local_options(fundiversity.memoise = NULL)
  expect_false(use_memoise())

  future::plan("sequential")
})

test_that("use_memoise() really triggers memoization when TRUE", {

  skip_if_not_installed("memoise")

  devnull <- fd_fric(c(1, 2, 5, 9))
  expect_true(
    memoise::has_cache(fd_chull_memoised)(matrix(c(1, 2, 5, 9), nrow = 4))
  )

})
