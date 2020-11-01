## code to prepare `DATASET` dataset goes here

library(magrittr)

csv_files <- rdryad::dryad_download("10.5061/dryad.c0n737b")[[1]] %>%
  { grep(".\\csv$", ., value = TRUE) }

library(dplyr)
library(tibble)

traits_plants <- read.csv(grep("Traits_plants", csv_files, value = TRUE)) %>%
  select(Species, where(is.numeric)) %>%
  column_to_rownames("Species") %>%
  as.matrix()

traits_birds <- read.csv(grep("Traits_birds", csv_files, value = TRUE)) %>%
  select(Species, where(is.numeric)) %>%
  column_to_rownames("Species") %>%
  as.matrix()

usethis::use_data(
  traits_plants,
  traits_birds,
  overwrite = TRUE
)
