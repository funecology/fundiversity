# Packages ---------------------------------------------------------------------
library("ggplot2")
library("dplyr")


# Functions --------------------------------------------------------------------
plot_benchmark_plot = function(df, title_string) {
  df %>%
    rename(fd_fct = expression) %>%
    mutate(
      fct_name = sub("_", "::", attr(fd_fct, "description"), fixed = TRUE) %>%
        paste0("()") %>%
        {sub("_unparallel", "", ., fixed = TRUE)}
    ) %>%
    select(fct_name, n_sites, n_traits, n_species, time, gc) %>%
    tidyr::unnest(c(time, gc)) %>%
    ggplot(aes(n_sites, time, color = as.factor(n_species),
               shape = as.factor(n_traits), linetype = as.factor(n_traits))) +
    ggdist::stat_lineribbon() +
    facet_wrap(vars(fct_name)) +
    labs(x = "Number of sites", y = "Execution Time",
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

init = attr(bench_fric$expression, "description")
init[seq(1, length(init), by = 4)] = "fundiversity_fd_fric"
attr(bench_fric$expression, "description") = init


bench_raoq = here::here(
  "inst", "saved_benchmarks", "bench_raoq_alternatives.rds"
) %>%
  readRDS()

# Figures ----------------------------------------------------------------------

# Full figures for Supplementary materials
bench_figs = list(
  fdis = bench_fdis,
  fdiv = bench_fdiv,
  feve = bench_feve,
  fric = bench_fric,
  raoq = bench_raoq
) %>%
  purrr::map2(
    c("Functional Dispersion", "Functional Divergence", "Functional Evenness",
      "Functional Richness", "Rao's Quadratic Entropy"),
    ~plot_benchmark_plot(.x, .y)
  )


# Simpler figure for main text
list(
  fdis = bench_fdis,
  fdiv = bench_fdiv,
  feve = bench_feve,
  fric = bench_fric,
  raoq = bench_raoq
) %>%
  bind_rows(.id = "fd_index") %>%
  filter(n_species == 200, n_traits %in% 3:4, n_sites == 100) %>%
  mutate(
    pkg_name = attr(expression, "description") %>%
      stringr::str_extract("^[:alpha:]+")
  ) %>%
  ggplot(aes(pkg_name, median, color = pkg_name)) +
  geom_point() +
  facet_wrap(
    vars(fd_index),
    labeller = labeller(
      fd_index = c(
        fdis = "Functional Dispersion",
        fdiv = "Functional Divergence",
        feve = "Functional Evenness",
        fric = "Functional Richness",
        raoq = "Rao's Quadratic Entropy"
      )
    )
  ) +
  labs(x = "Package", y = "Median Execution Time") +
  bench::scale_y_bench_time() +
  theme_bw() +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(family = "mono", face = "bold"),
        legend.text = element_text(family = "mono", face = "bold"))

