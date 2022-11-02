# Test script to test for cache size evolution in function of computation

# Packages ---------------------------------------------------------------------

# Make sure to load memoised functions
options(fundiversity.memoise = TRUE)
pkgload::load_all()

# Function ---------------------------------------------------------------------

# Magic function that returns size of cache of chull
get_cache_size = function(my_fun) {
  cach_env = environment(my_fun)

  lobstr::obj_size(
    lapply(cach_env$`_cache`$keys(), cach_env$`_cache`$get)
  )
}


# Tests ------------------------------------------------------------------------
# What is memoized is not fd_fric() per se but fd_chull()
memoise::is.memoised(fd_fric)
memoise::is.memoised(fd_chull)

get_cache_size(fd_chull)

ko = fd_fric(traits_birds)

get_cache_size(fd_chull)

ko = fd_fric(traits_birds, site_sp_birds)

get_cache_size(fd_chull)

ko = fd_fric(traits_plants)

get_cache_size(fd_chull)

ko = fd_fric(traits_plants, site_sp_plants)

get_cache_size(fd_chull)


# Simulation -------------------------------------------------------------------

# Reloading the package erases the cache
pkgload::load_all()
get_cache_size(fd_chull)

# Simulate a community
set.seed(20221102)
n_species = 500
n_sites   = 1000
n_traits  = 5

site_sp = matrix(rbinom(n_species*n_sites, 1, prob = 0.3), ncol = n_species, nrow = n_sites)
rownames(site_sp) = paste0("site", seq(n_sites))
colnames(site_sp) = paste0("species", seq(n_species))


traits = matrix(
  rnorm(n_species*n_traits), ncol = n_traits, nrow = n_species
)
rownames(traits) = paste0("species", seq(n_species))
colnames(traits) = paste0("trait", seq(n_traits))

ko = fd_fric(traits, site_sp)

get_cache_size(fd_chull)
