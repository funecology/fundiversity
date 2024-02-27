# Script to make benchmark across different functions of similar packages

# Options ----------------------------------------------------------------------
options(fundiversity.memoise = FALSE)  # Deactivate memoisation

# Package & Functions ----------------------------------------------------------
pkgload::load_all()  # Need last version of fundiversity
library("magrittr")
source(
  here::here("inst", "benchmark_scripts", "benchmark_fcts",
             "fd_benchmark_multicore.R")
)
source(here::here("inst", "benchmark_scripts", "benchmark_fcts", "make_data.R"))

# Benchmarks Parameters --------------------------------------------------------

# Data Parameters
bench_sites   = c(5e1 , 1e2, 5e2)
bench_traits  = c(2,    4)
bench_species = c(2e2,  5e2, 1e3)

# Number of Repeats per function
n_iterations = 20

# Number of Cores over which to parallelize
n_cores = c(1, seq(2, 6, by = 2))


# Running the benchmark --------------------------------------------------------
all_funs = alist(
  fdis = fd_fdis, fdiv = fd_fdiv, feve = fd_feve, fric = fd_fric
) %>%
  purrr::cross2(n_cores) %>%
  purrr::map(setNames, c("fundiv_function", "n_core")) %>%
  purrr::map(
    function(x) {

      fundiv_function = x$fundiv_function
      n_core          = x$n_core

      future::plan(
        list(
          future::tweak(
            future::multisession, workers = n_core
          )
        )
      )

      fd_benchmark_multicore(get(fundiv_function)) %>%
        tidyr::unnest(c(time, gc)) %>%
        select(-expression) %>%
        mutate(fd_fct = as.character(fundiv_function), n_core = n_core)

    }
  )

saveRDS(all_funs, "all_multicore_bench.Rds")
