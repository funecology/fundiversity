test_that("use_memoise() works when memoise is installed", {

  # Pretend fd_chull_memoised() exists = memoise installed before fundiversity
  fd_chull_memoised <- NULL

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
  expect_error(use_memoise(), regexp = "memoise is not installedor was installed after fundiversity was loaded.Please install memoise and restart R.", fixed = TRUE)

  # If memoise not installed & options FALSE
  withr::local_options(fundiversity.memoise = FALSE)
  expect_false(use_memoise())

  # If memoise not installed & options NULL
  withr::local_options(fundiversity.memoise = NULL)
  expect_false(use_memoise())

})


test_that("use_memoise() sends back FALSE when plan is parallel", {

  # Check both fd_chull() and fd_chull_intersect()

  ## If memoise not installed
  # If memoise not installed & options TRUE

  # If memoise not installed & options FALSE

  # If memoise not installed & options NULL

})
