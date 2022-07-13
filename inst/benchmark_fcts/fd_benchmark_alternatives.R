fd_benchmark_alternatives <- function(list_alternatives, seed = 20210906) {

  bench::press(
    n_sites   = bench_sites,
    n_traits  = bench_traits,
    n_species = bench_species,
    {
      set.seed(seed)
      given_data = make_data(n_species, n_traits, n_sites)
      bench::mark(
        exprs = list_alternatives,
        iterations = n_iterations,
        check = FALSE,
        memory = FALSE
      )
    }
  )

}
