# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# TODO OPTIONAL Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("some/dir")

# installs branches to benchmark
touchstone::branch_install()

# benchmark a function call from your package (10 calls per branch)
touchstone::benchmark_run(
  expr_before_benchmark = library(fundiversity),
  fric = fd_fric(traits_birds, site_sp_birds),
  n = 10
)

touchstone::benchmark_run(
  expr_before_benchmark = library(fundiversity),
  fdiv =  fd_fdiv(traits_birds, site_sp_birds),
  n = 10
)

touchstone::benchmark_run(
  expr_before_benchmark = library(fundiversity),
  feve = fd_feve(traits_birds, site_sp_birds),
  n = 10
)

touchstone::benchmark_run(
  expr_before_benchmark = library(fundiversity),
  fdis = fd_fdis(traits_birds, site_sp_birds),
  n = 10
)

touchstone::benchmark_run(
  expr_before_benchmark = library(fundiversity),
  raoq = fd_raoq(traits_birds, site_sp_birds),
  n = 10
)

touchstone::benchmark_run(
  expr_before_benchmark = library(fundiversity),
  fric_intersect = fd_fric_intersect(traits_birds, site_sp_birds),
  n = 10
)

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
