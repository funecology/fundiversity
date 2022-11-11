# Comparison of packages

# Packages ---------------------------------------------------------------------
library("dplyr")
library("fundiversity")


# Functions --------------------------------------------------------------------

join_list_by_site = function(df_list) {
  Reduce(function(x, y) inner_join(x, y, by = "site"), df_list)
}

# Precomputation ---------------------------------------------------------------

traits_birds_sc = scale(traits_birds)

trait_dist_birds = dist(traits_birds, method = "euclidean")

trait_dist_birds_sc = dist(traits_birds_sc, method = "euclidean")



# FDis -------------------------------------------------------------------------


# fundiversity
fundiversity_fdis = fd_fdis(traits_birds, site_sp_birds)

fundiversity_fdis_sc = fd_fdis(traits_birds_sc, site_sp_birds)

fundiversity_dispersions =
  list(
    fundiversity_fdis = fundiversity_fdis,
    fundiversity_fdis_sc = fundiversity_fdis_sc
  ) %>%
  purrr::imap(
    ~.x %>%
      rename(!!.y := FDis)
  ) %>%
  join_list_by_site()

# BAT
# Noticeable difference when using raw traits: creates a functional tree...
BAT_dispersion_traits = BAT::dispersion(
  site_sp_birds, tree = traits_birds
)

BAT_dispersion_traits_sc = BAT::dispersion(
  site_sp_birds, tree = traits_birds_sc
)

# Not sure how dissimilarity are taken?
BAT_dispersion_dist = BAT::dispersion(
  site_sp_birds, distance = trait_dist_birds
)

BAT_dispersion_dist_sc = BAT::dispersion(
  site_sp_birds, distance = trait_dist_birds_sc
)

BAT_dispersions = list(
  BAT_traits = BAT_dispersion_traits,
  BAT_traits_sc = BAT_dispersion_traits_sc,
  BAT_dist = BAT_dispersion_dist,
  BAT_dist_sc = BAT_dispersion_dist_sc
) %>%
  purrr::imap(function(x, y) {
    df = x %>%
      as.data.frame() %>%
      tibble::rownames_to_column("site")

    colnames(df)[[2]] = y

    df
  }) %>%
  join_list_by_site()

## FD
# Refer Laliberté & Legendre (2010) for functional dispersion
FD_dist = FD::fdisp(trait_dist_birds, site_sp_birds)

FD_dist_sc = FD::fdisp(trait_dist_birds_sc, site_sp_birds)

FD_dispersions =
  list(
    FD_dist = FD_dist,
    FD_dist_sc = FD_dist_sc
  ) %>%
  purrr::imap(
    ~.x[["FDis"]] %>%
      tibble::enframe(name = "site", value = .y)
  ) %>%
  join_list_by_site()

# mFD::alpha.fd.multidim(..., ind_vect = "fdis")
mFD_dispersion_traits = mFD::alpha.fd.multidim(
  traits_birds, site_sp_birds, ind_vect = "fdis", scaling = FALSE,
  verbose = FALSE
)

mFD_dispersion_traits_sc = mFD::alpha.fd.multidim(
  traits_birds_sc, site_sp_birds, ind_vect = "fdis", scaling = FALSE,
  verbose = FALSE
)

mFD_dispersions = list(
  mFD_traits = mFD_dispersion_traits,
  mFD_traits_sc = mFD_dispersion_traits_sc
) %>%
  purrr::imap(
    ~.x %>%
      .[["functional_diversity_indices"]] %>%
      as.data.frame() %>%
      tibble::rownames_to_column("site") %>%
      select(site, fdis) %>%
      rename(!!.y := fdis)
  ) %>%
  join_list_by_site()


# hillR
hillR_dispersion_traits = hillR::hill_func(site_sp_birds, traits_birds)
hillR_dispersion_traits_sc = hillR::hill_func(site_sp_birds, traits_birds_sc)

hillR_dispersions = list(
  hillR_traits = hillR_dispersion_traits,
  hillR_traits_sc = hillR_dispersion_traits_sc
) %>%
  purrr::imap(
    ~.x %>%
      .["FDis", ] %>%
      tibble::enframe("site", "fdis") %>%
      rename(!!.y := fdis)
  ) %>%
  join_list_by_site()

## Diagnostic plots

list(
  fundiversity_dispersions,
  BAT_dispersions,
  FD_dispersions,
  mFD_dispersions,
  hillR_dispersions
) %>%
  {Reduce(function(x, y) inner_join(x, y, by = "site"), .)} %>%
  select(-site) %>%
  GGally::ggpairs()

# Conclusion:
# - With unscaled traits:
# fundiversity::fd_fdis() is equivalent BAT::dispersion() with distance and
# equivalent to FD::fdisp() and mFD::fd.alpha.multidim(indvect = "fdis")
# - With scaled traits:
# fundiversity::fd_fdis() is equivalent to the same function but the
# discrepancies are greater (computational error due to dissimilarity
# computations or scaling?)
# General obs:
# BAT::dispersion() with traits has nothing to do with the rest given
# it computes a functional tree

# Rao's Q ----------------------------------------------------------------------

# fundiversity
fundiversity_rao_traits    = fd_raoq(traits_birds, site_sp_birds)
fundiversity_rao_traits_sc = fd_raoq(traits_birds_sc, site_sp_birds)
fundiversity_rao_dist      = fd_raoq(dist_matrix = trait_dist_birds,
                                     sp_com = site_sp_birds)
fundiversity_rao_dist_sc   = fd_raoq(dist_matrix = trait_dist_birds_sc,
                                     sp_com = site_sp_birds)

fundiversity_raos = list(
  fundiversity_rao_traits = fundiversity_rao_traits,
  fundiversity_rao_traits_sc = fundiversity_rao_traits_sc,
  fundiversity_rao_dist = fundiversity_rao_dist,
  fundiversity_rao_dist_sc = fundiversity_rao_dist_sc
) %>%
  purrr::imap(
    ~.x %>%
      rename(!!.y := Q)
  ) %>%
  join_list_by_site()

# adiv
adiv_rao_dist    = adiv::QE(site_sp_birds, trait_dist_birds)
adiv_rao_dist_sc = adiv::QE(site_sp_birds, trait_dist_birds_sc)

adiv_raos = list(
  adiv_rao_dist = adiv_rao_dist,
  adiv_rao_dist_sc = adiv_rao_dist_sc
) %>%
  purrr::imap(
    ~.x %>%
      tibble::rownames_to_column("site") %>%
      rename(!!.y := diversity)
  ) %>%
  join_list_by_site()


# BAT
BAT_rao_traits    = BAT::rao(site_sp_birds, distance = traits_birds)
BAT_rao_traits_sc = BAT::rao(site_sp_birds, distance = traits_birds_sc)
BAT_rao_dist      = BAT::rao(site_sp_birds, distance = trait_dist_birds)
BAT_rao_dist_sc   = BAT::rao(site_sp_birds, distance = trait_dist_birds_sc)

BAT_raos = list(
  BAT_rao_traits    = BAT_rao_traits,
  BAT_rao_traits_sc = BAT_rao_traits_sc,
  BAT_rao_dist      = BAT_rao_dist,
  BAT_rao_dist_sc   = BAT_rao_dist_sc
) %>%
  purrr::imap(
    ~ .x %>%
      as.data.frame() %>%
      tibble::rownames_to_column("site") %>%
      rename(!!.y := Rao)
  ) %>%
  join_list_by_site()


# hillR
hillR_rao_traits = hillR::hill_func(site_sp_birds, traits_birds)
hillR_rao_traits_sc = hillR::hill_func(site_sp_birds, traits_birds_sc)
hillR_rao_dist = hillR::hill_func(
  site_sp_birds, trait_dist_birds, traits_as_is = TRUE
)
hillR_rao_dist_sc = hillR::hill_func(
  site_sp_birds, trait_dist_birds_sc, traits_as_is = TRUE
)

hillR_raos = list(
  hillR_rao_traits    = hillR_rao_traits,
  hillR_rao_traits_sc = hillR_rao_traits_sc,
  hillR_rao_dist      = hillR_rao_dist,
  hillR_rao_dist_sc   = hillR_rao_dist_sc
) %>%
  purrr::imap(
    ~ .x[1,] %>%
      tibble::enframe("site", .y)
  ) %>%
  join_list_by_site()


# mFD
mFD_rao_dist    = mFD::alpha.fd.hill(site_sp_birds, trait_dist_birds, q = 2, tau = "max")
mFD_rao_dist_sc = mFD::alpha.fd.hill(site_sp_birds, trait_dist_birds_sc, q = 2, tau = "max")

mFD_raos = list(
  mFD_rao_dist = mFD_rao_dist,
  mFD_rao_dist_sc = mFD_rao_dist_sc
) %>%
  purrr::imap(
    ~ .x %>%
      .[["asb_FD_Hill"]] %>%
      as.data.frame() %>%
      tibble::rownames_to_column("site") %>%
      rename(!!.y := FD_q2)
  ) %>%
  join_list_by_site()

# FD?
## Comparisons and plots


all_raos = list(
  fundiversity_raos,
  adiv_raos,
  BAT_raos,
  hillR_raos,
  mFD_raos
) %>%
  join_list_by_site()

all_raos %>%
  select(-site) %>%
  GGally::ggpairs()

