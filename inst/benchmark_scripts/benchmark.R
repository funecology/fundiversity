# Script to make benchmark across different functions of similar packages

# Options ----------------------------------------------------------------------
options(fundiversity.memoise = FALSE)  # Deactivate memoisation

# Package & Functions ----------------------------------------------------------
pkgload::load_all()  # Need last version of fundiversity
source(
  here::here("inst", "benchmark_scripts", "benchmark_fcts",
             "fd_benchmark_multicore.R")
)
source(
  here::here("inst", "benchmark_scripts", "benchmark_fcts",
             "fd_benchmark_alternatives.R")
)
source(here::here("inst", "benchmark_scripts", "benchmark_fcts", "make_data.R"))


# FD indices expressions -------------------------------------------------------

dbfd_indices <- rlang::exprs(
  FD_dbFD_fdiv = FD::dbFD(
    given_data[["traits"]], given_data[["site_sp"]], calc.FDiv = TRUE,
    calc.FGR = FALSE, calc.FRic = FALSE, calc.CWM = FALSE
  ),
  FD_dbFD_feve = FD::dbFD(
    given_data[["traits"]], given_data[["site_sp"]], calc.FDiv = FALSE,
    calc.FGR = FALSE, calc.FRic = FALSE, calc.CWM = FALSE
  ),
  FD_dbFD_fric = FD::dbFD(
    given_data[["traits"]], given_data[["site_sp"]], calc.FRic = TRUE,
    calc.CWM = FALSE, calc.FGR = FALSE, calc.FDiv = FALSE
  ),
  FD_dbFD_raoq =  FD::dbFD(
    given_data[["traits"]], given_data[["site_sp"]], calc.FRic = FALSE,
    calc.CWM = FALSE, calc.FGR = FALSE, calc.FDiv = FALSE
  )
)

fd_indices <- list(
  "fdis" = rlang::exprs(
    fundiversity_fd_fdis_unparallel =
      {
        fd_fdis(given_data[["traits"]], given_data[["site_sp"]])
      },
    BAT_dispersion = BAT::dispersion(
      given_data[["site_sp"]], given_data[["traits"]]
    ),
    FD_fdisp        = FD::fdisp(
      dist(given_data[["traits"]]), given_data[["site_sp"]]
    ),
    mFD_alpha_fd   = mFD::alpha.fd.multidim(
      given_data[["traits"]], given_data[["site_sp"]], ind_vect = "fdis",
      scaling = FALSE, verbose = FALSE
    )
  ),
  "fdiv" = rlang::exprs(
    fundiversity_fd_fdiv_unparallel =
      {
        fd_fdiv(given_data[["traits"]], given_data[["site_sp"]])
      },
    mFD_alpha_fd   = mFD::alpha.fd.multidim(
      given_data[["traits"]], given_data[["site_sp"]], ind_vect = "fdiv",
      scaling = FALSE, verbose = FALSE
    )
  ),
  "feve" = rlang::exprs(
    fundiversity_fd_feve_unparallel =
      {
        fd_feve(given_data[["traits"]], given_data[["site_sp"]])
      },
    mFD_alpha_fd   = mFD::alpha.fd.multidim(
      given_data[["traits"]], given_data[["site_sp"]], ind_vect = "feve",
      scaling = FALSE, verbose = FALSE
    )
  ),
  "raoq" = rlang::exprs(
    fundiversity_fd_raoq_unparallel =
      {
        fd_raoq(given_data[["traits"]], given_data[["site_sp"]])
      },
    adiv_qe           = adiv::QE(
      given_data[["site_sp"]], dist(given_data[["traits"]])
    ),
    BAT_rao           = BAT::rao(
      given_data[["site_sp"]], distance = given_data[["traits"]]
    ),
    hillR_hill_func   = hillR::hill_func(
      given_data[["site_sp"]], given_data[["traits"]], fdis = FALSE
    ),
    mFD_alpha_fd_hill = mFD::alpha.fd.hill(
      given_data[["site_sp"]], dist(given_data[["traits"]]), q = 2,
      tau = "max"
    )
  )
)

# Benchmarks -------------------------------------------------------------------
bench_sites   = c(5e1 , 1e2, 5e2)
bench_traits  = c(2,    4,  10)
bench_species = c(2e2,  5e2, 1e3)

n_iterations = 10

## Regular indices benchmark
# Parallelize across indices
future::plan(
  future.batchtools::batchtools_slurm,
  template = here::here("inst", "benchmark_scripts", ".batchtools.slurm.tmpl"),
  label     = "funbench",
  resources = list(
    job.name = "funbench",
    walltime = 5760,
    memory = "150G",
    ncpus  = 1,
    output   = "/work/%u/%j-%x.log",
    email  = Sys.getenv("USEREMAIL")
  ),
  finalize = FALSE
)

fd_indices %>%
  furrr::future_imap(
    ~ fd_benchmark_alternatives(.x) %>%
      saveRDS(glue::glue("bench_{index}_alternatives.rds", index = .y))
  )
