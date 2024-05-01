fd_benchmark_multicore <- function(fd_index) {

  fd_fct <- rlang::as_function(fd_index)

  bench::press(
    n_sites   = bench_sites,
    n_traits  = bench_traits,
    n_species = bench_species,
    {
      set.seed(20210906)
      given_data = make_data(n_species, n_traits, n_sites)
      bench::mark(
        iterations = n_iterations,
        fundiversity_multicore =
          {
            fd_fct(given_data[["traits"]], given_data[["site_sp"]])
          },
        check = FALSE,
        memory = FALSE
      )
    }
  )

}
