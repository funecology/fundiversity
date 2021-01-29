traits <- data.frame(tr = 1:3)
rownames(traits) <- letters[1:3]

sitesp           <- matrix(1, ncol = 3)
rownames(sitesp) <- "s1"
colnames(sitesp) <- letters[1:3]

test_that("species_in_common() works", {
  expect_silent(species_in_common(traits, sitesp))
  expect_equal(species_in_common(traits, sitesp), letters[1:3])
  expect_equal(
    suppressMessages(species_in_common(traits, sitesp[, 1:2, drop = FALSE])),
    c("a", "b"))
  expect_equal(
    suppressMessages(species_in_common(traits[1:2,, drop = FALSE], sitesp)),
    c("a", "b"))

  expect_message(
    species_in_common(traits[1:2,, drop = FALSE], sitesp),
    paste0("Differing number of species between trait dataset ",
           "and site-species matrix\nTaking subset of species"))
  expect_message(
    species_in_common(traits, sitesp[, 1:2, drop = FALSE]),
    paste0("Differing number of species between trait dataset ",
           "and site-species matrix\nTaking subset of species"))
})

test_that("species_in_common() works with sparse matrices", {
  sparse_sitesp <- as(sitesp, "sparseMatrix")

  expect_silent(species_in_common(traits, sparse_sitesp))
  expect_equal(species_in_common(traits, sparse_sitesp), letters[1:3])
  expect_equal(
    suppressMessages(species_in_common(traits, sparse_sitesp[, 1:2,
                                                             drop = FALSE])),
    c("a", "b"))
  expect_equal(
    suppressMessages(species_in_common(traits[1:2,, drop = FALSE],
                                       sparse_sitesp)),
    c("a", "b"))

  expect_message(
    species_in_common(traits[1:2,, drop = FALSE], sparse_sitesp),
    paste0("Differing number of species between trait dataset ",
           "and site-species matrix\nTaking subset of species"))
  expect_message(
    species_in_common(traits, sparse_sitesp[, 1:2, drop = FALSE]),
    paste0("Differing number of species between trait dataset ",
           "and site-species matrix\nTaking subset of species"))
})

test_that("species_in_common() fails gracefully", {
  expect_error(
    species_in_common(traits[1,, drop = FALSE], sitesp[, 2:3, drop = FALSE]),
    "No species in common found between trait dataset and site-species matrix")

  expect_error(species_in_common(c(0,1,0), sitesp),
               "Trait dataset not of good type, check trait dataset")
  expect_error(species_in_common("a", sitesp),
               "Trait dataset not of good type, check trait dataset")

  expect_error(
    species_in_common(traits, c(0, 1, 0)),
    "Site-species matrix not of good type, check site-species matrix")

  expect_error(
    species_in_common(traits, "a"),
    "Site-species matrix not of good type, check site-species matrix")
})
