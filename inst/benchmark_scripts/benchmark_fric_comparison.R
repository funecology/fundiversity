# Script to make benchmark across different functions of similar packages

# Options ----------------------------------------------------------------------
options(fundiversity.memoise = FALSE)  # Deactivate memoisation

# Package & Functions ----------------------------------------------------------
pkgload::load_all()  # Need last version of fundiversity
source(
  here::here("inst", "benchmark_scripts", "benchmark_fcts",
             "fd_benchmark_alternatives.R")
)
source(here::here("inst", "benchmark_scripts", "benchmark_fcts", "make_data.R"))


# FD indices expressions -------------------------------------------------------

fric_exprs <- list(
  "fric" = rlang::exprs(
    fundiversity_fd_fric_unparallel =
      {
        fd_fric(given_data[["traits"]], given_data[["site_sp"]])
      },
    BAT_alpha_tree = BAT::alpha(
      given_data[["site_sp"]], given_data[["traits"]]
    ),
    BAT_alpha_hull = BAT::hull.alpha(
      BAT::hull.build(given_data[["site_sp"]], given_data[["traits"]])
    ),
    mFD_alpha_fd   = mFD::alpha.fd.multidim(
      given_data[["traits"]], given_data[["site_sp"]], ind_vect = "fric",
      scaling = FALSE, verbose = FALSE
    )
  ),
  "fric_intersect" = rlang::exprs(
    fundiversity_fd_fric_intersect_unparallel =
      {
        fd_fric_intersect(given_data[["traits"]], given_data[["site_sp"]])
      },
    betapart_functional_beta = betapart::functional.beta.pair(
      given_data[["site_sp"]], given_data[["traits"]]
    ),
    hillR_funct_pairwise = hillR::hill_func_parti_pairwise(
      given_data[["site_sp"]], given_data[["traits"]]
    )
  )
)

# Functional Richness Benchmark ------------------------------------------------

bench_sites   = c(5e1 , 1e2)
bench_traits  = c(2, 4)
bench_species = c(2e2,  5e2)

n_iterations = 30

## Regular indices benchmark
# Parallelize across indices
future::plan(
  future.batchtools::batchtools_slurm,
  template = here::here("inst", "benchmark_scripts", ".batchtools.slurm.tmpl"),
  label    = "funbench_fric",
  resources = list(
    job.name = "funbench_fric",
    walltime = 7200,
    memory = "150G",
    ncpus = 1,
    output   = "/work/%u/%x-%j.log",
    email  = Sys.getenv("USEREMAIL")
  )
)

fric_exprs["fric_intersect"] %>%
  furrr::future_imap(
    ~ fd_benchmark_alternatives(.x, seed=20210915) %>%
      saveRDS(glue::glue("fric_bench_{index}_alternatives.rds", index = .y))
  )
