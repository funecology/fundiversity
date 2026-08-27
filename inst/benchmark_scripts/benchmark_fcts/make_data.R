make_data = function(n_species = 10, n_traits = 5, n_sites = 20) {
  traits = matrix(runif(n_species * n_traits),
                  ncol = n_traits, nrow = n_species,
                  dimnames = list(species = paste0("sp", seq(n_species)),
                                  traits  = paste0("t", seq(n_traits)))
  )

  site_sp = matrix(
    as.numeric(runif(n_species * n_sites) >= 0.5),
    nrow = n_sites,
    ncol = n_species,
    dimnames = list(
      sites   = paste0("s", seq(n_sites)),
      species = paste0("sp", seq(n_species))
    )
  )

  list(traits = traits, site_sp = site_sp)
}
