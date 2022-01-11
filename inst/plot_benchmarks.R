# Packages ---------------------------------------------------------------------
library("ggplot2")
library("dplyr")


# Functions --------------------------------------------------------------------
plot_benchmark_plot = function(df, title_string) {
  df %>%
    rename(fd_fct = expression) %>%
    mutate(
      fct_name = sub("_", "::", attr(fd_fct, "description"), fixed = TRUE) %>%
        paste0("()")
    ) %>%
    ggplot(aes(n_sites, median, color = as.factor(n_species),
               shape = as.factor(n_traits), linetype = as.factor(n_traits))) +
    geom_point() +
    geom_line() +
    facet_wrap(vars(fct_name)) +
    labs(x = "Number of sites", y = "Median Time (in seconds)",
         color = "Number of species", shape = "Number of traits",
         linetype = "Number of traits", caption = "30 iterations",
         title = title_string) +
    scale_color_viridis_d() +
    scale_x_log10() +
    bench::scale_y_bench_time() +
    theme_bw() +
    theme(aspect.ratio = 1,
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(family = "mono", face = "bold"))
}

# Load data --------------------------------------------------------------------
bench_fdis = here::here(
  "inst", "saved_benchmarks", "bench_fdis_alternatives.rds"
  ) %>%
  readRDS()
bench_fdiv = here::here(
  "inst", "saved_benchmarks", "bench_fdiv_alternatives.rds"
  ) %>%
  readRDS()
bench_feve = here::here(
  "inst", "saved_benchmarks", "bench_feve_alternatives.rds"
  ) %>%
  readRDS()
bench_fric = here::here(
  "inst", "saved_benchmarks", "bench_fric_alternatives.rds"
) %>%
  readRDS()
bench_raoq = here::here(
  "inst", "saved_benchmarks", "bench_raoq_alternatives.rds"
) %>%
  readRDS()

# Figures ----------------------------------------------------------------------

bench_figs = list(fdis = bench_fdis,
                  fdiv = bench_fdiv,
                  feve = bench_feve,
                  fric = bench_fric,
                  raoq = bench_raoq) %>%
  purrr::map2(
    c("Functional Dispersion", "Functional Divergence", "Functional Evenness",
      "Functional Richness", "Rao's Quadratic Entropy"),
    ~plot_benchmark_plot(.x, .y)
  )
