test_that("remove_species_without_trait() works", {

  data("traits_birds")

  expect_silent(remove_species_without_trait(traits_birds))
  expect_identical(remove_species_without_trait(traits_birds), traits_birds)

  traits_birds_missing <- traits_birds
  traits_birds_missing[1, 1] <- NA

  expect_equal(
    suppressMessages(remove_species_without_trait(traits_birds_missing)),
    traits_birds_missing[2:nrow(traits_birds), ])
  expect_message(remove_species_without_trait(traits_birds_missing),
                 "Removed 1 species with missing trait(s)", fixed = TRUE)

  traits_birds_missing[2, 2] <- NA

  expect_identical(
    suppressMessages(remove_species_without_trait(traits_birds_missing)),
    traits_birds_missing[3:nrow(traits_birds), ])
  expect_message(remove_species_without_trait(traits_birds_missing),
                 "Removed 2 species with missing trait(s)", fixed = TRUE)
})
