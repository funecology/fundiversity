## code to prepare `DATASET` dataset goes here

library(magrittr)
library(dplyr)
library(tibble)

csv_files <- rdryad::dryad_download("10.5061/dryad.c0n737b")[[1]] %>%
  { grep(".\\csv$", ., value = TRUE) }

# Trait matrices ---------------------------------------------------------------

traits_plants <- read.csv(grep("Traits_plants", csv_files, value = TRUE)) %>%
  select(Species, where(is.numeric)) %>%
  column_to_rownames("Species") %>%
  as.matrix()

traits_birds <- read.csv(grep("Traits_birds", csv_files, value = TRUE)) %>%
  select(Species, where(is.numeric)) %>%
  column_to_rownames("Species") %>%
  as.matrix()


# Site-species matrices --------------------------------------------------------

# Birds site-species matrix
elev_birds <- read.csv(grep("Elevation_birds", csv_files, value = TRUE))

elevations <- union(elev_birds$Min..elevation..m.a.s.l..,
                    elev_birds$Max..elevation..m.a.s.l..) %>%
  sort()

elev_names <- paste0("elev_", elevations)

site_sp_birds <- elev_birds %>%
  select(Species, Min..elevation..m.a.s.l.., Max..elevation..m.a.s.l..) %>%
  group_by(Species) %>%
  summarise(all_elev = lst({elevations >= Min..elevation..m.a.s.l.. &
                           elevations <= Max..elevation..m.a.s.l..} %>%
                             setNames(nm = elev_names) %>%
                             t() %>%
                             as.data.frame())) %>%
  tidyr::unnest(all_elev) %>%
  mutate(across(where(is.logical), as.numeric)) %>%
  as.data.frame() %>%
  tibble::column_to_rownames("Species") %>%
  as.matrix() %>%
  t()


# Plants site-species matrix
elev_plants <- read.csv(grep("Elevation_plants", csv_files, value = TRUE))

plant_elev <- union(elev_plants$Min..elevation..m.a.s.l..,
                    elev_plants$Max..elevation..m.a.s.l..) %>%
  sort()

plant_elev_names <- paste0("elev_", plant_elev)

site_sp_plants <- elev_plants %>%
  select(Species, Min..elevation..m.a.s.l.., Max..elevation..m.a.s.l..) %>%
  group_by(Species) %>%
  summarise(all_elev = lst({plant_elev >= Min..elevation..m.a.s.l.. &
      plant_elev <= Max..elevation..m.a.s.l..} %>%
        setNames(nm = plant_elev_names) %>%
        t() %>%
        as.data.frame())) %>%
  tidyr::unnest(all_elev) %>%
  mutate(across(where(is.logical), as.numeric)) %>%
  as.data.frame() %>%
  tibble::column_to_rownames("Species") %>%
  as.matrix() %>%
  t()

# Save data in the package -----------------------------------------------------

usethis::use_data(
  traits_plants,
  traits_birds,
  site_sp_plants,
  site_sp_birds,
  overwrite = TRUE
)
