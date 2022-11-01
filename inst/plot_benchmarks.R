# Packages ---------------------------------------------------------------------
library("ggplot2")
library("dplyr")

# Load data --------------------------------------------------------------------
# Correspondence data.frame between internal vs. actual names
# These are collated from the benchmark*.R files
fd_fct_names = tibble::tribble(
  ~index_name,      ~internal_name,                   ~actual_name,
  "fric",           "fundiversity_fd_fric",           "fundiversity::fd_fric()",
  "fric",           "BAT_alpha_tree",                 "BAT::alpha()",
  "fric",           "BAT_alpha_hull",                 "BAT::hull.alpha()",
  "fric",           "mFD_alpha_fd",                   "mFD::alpha.fd.multidim(ind_vect = 'fric')",
  "fdis",           "fundiversity_fd_fdis",           "fundiversity::fd_fdis()",
  "fdis",           "BAT_dispersion",                 "BAT::dispersion()",
  "fdis",           "FD_fdisp",                       "FD::fdisp()",
  "fdis",           "mFD_alpha_fd",                   "mFD::alpha.fd.multidim(ind_vect = 'fdis')",
  "fdiv",           "fundiversity_fd_fdiv",           "fundiversity::fd_fdiv()",
  "fdiv",           "mFD_alpha_fd",                   "mFD::alpha.fd.multidim(ind_vect = 'fdiv')",
  "feve",           "fundiversity_fd_feve",           "fundiversity::fd_feve()",
  "feve",           "mFD_alpha_fd",                   "mFD::alpha.fd.multidim(ind_vect = 'feve')",
  "raoq",           "fundiversity_fd_raoq",           "fundiversity::fd_raoq()",
  "raoq",           "adiv_qe",                        "adiv::QE()",
  "raoq",           "BAT_rao",                        "BAT::rao()",
  "raoq",           "hillR_hill_func",                "hillR::hill_func(fdis = FALSE)",
  "raoq",           "mFD_alpha_fd_hill",              "mFD::alpha.fd.hill(q = 2, tau = 'max')",
  "fric_intersect", "fundiversity_fd_fric_intersect", "fundiversity::fd_fric_intersect()",
  "fric_intersect", "betapart_functional_beta",       "betapart::functional.beta.pair()",
  "fric_intersect", "hillR_funct_pairwise",           "hillR::hill_func_parti_pairwise()"
)

# Benchmark using single core
single_bench = list.files(
  "inst/saved_benchmarks", "*alternatives.rds", full.names = TRUE
) %>%
  setNames(
    c("fdis", "fdiv", "feve", "raoq", "fric", "fric_intersect")
  ) %>%
  purrr::map(readRDS) %>%
  bind_rows(.id = "fundiversity_index")

all_bench = bind_rows(list(single_bench))


# Benchmark using multiple cores
para_bench = readRDS("inst/saved_benchmarks/all_multicore_bench.Rds") %>%
  bind_rows() %>%
  select(fd_fct, n_core, everything())

# Figure 1: Benchmark across packages ------------------------------------------

# Preprocessing before plotting
bench_df = single_bench %>%
  filter(
    (fundiversity_index != "fric_intersect" & n_traits == 4) |
      (fundiversity_index == "fric_intersect" & n_traits == 3),
    n_species == 500, n_sites == 100) %>%
  rename(fd_fct = expression) %>%
  mutate(
    fd_fct = sub("_", "::", attr(fd_fct, "description"), fixed = TRUE) %>%
      paste0("()") %>%
      {sub("_unparallel", "", ., fixed = TRUE)}
  ) %>%
  mutate(package = stringr::str_extract(fd_fct, "^\\w+")) %>%
  tidyr::unnest(c(time, gc)) %>%
  select(
    fundiversity_index, package, fd_fct, n_sites, n_traits, n_species, time
  ) %>%
  mutate(
    package = case_when(
      package == "BAT" & grepl("hull", fd_fct) ~ "BAT_hull",
      package == "BAT" & grepl("tree", fd_fct) ~ "BAT_tree",
      TRUE ~ package) %>%
      factor(
        level = c("fundiversity", "adiv", "BAT", "BAT_hull", "BAT_tree",
                  "betapart", "FD", "hillR", "mFD") %>%
          rev()
      )
  )

# Actual figure
single_index_comparison = bench_df %>%
  tidyr::nest(bench_df = !fundiversity_index) %>%
  mutate(bench_df = lapply(bench_df, function(x) {
    x %>%
      mutate(package = forcats::fct_drop(package))
  }),
  index_title = case_when(
    fundiversity_index == "fdis"           ~ "Functional Dispersion",
    fundiversity_index == "fdiv"           ~ "Functional Divergence",
    fundiversity_index == "feve"           ~ "Functional Evenness",
    fundiversity_index == "fric"           ~ "Functional Richness",
    fundiversity_index == "fric_intersect" ~ "Functional beta-diversity",
    fundiversity_index == "raoq"           ~ "Rao's Quadratic Entropy"
  ),
  bench_plot = purrr::map2(
    bench_df, index_title,
    ~.x %>%
      ggplot(aes(x = time, y = package)) +
      ggbeeswarm::geom_beeswarm(alpha = 1/3, groupOnX = FALSE) +
      bench::scale_x_bench_time(name = "Running Time") +
      scale_y_discrete(
        labels = function(x) {
          x = case_when(
            x == "fundiversity"          ~ "fundiversity",
            x == "BAT_hull"              ~ "BAT (hull)",
            x == "BAT_tree"              ~ "BAT (tree)",
            TRUE ~ x
          )
          bold   = ifelse(
            grepl("fundiversity", levels(.x$package)), "**", ""
          )
          colours = ifelse(
            grepl("fundiversity", levels(.x$package)), "black", "grey30"
          )
          glue::glue("<span style='color:{colours}; font-family:mono'>",
                     "{bold}{x}{bold}</span>")
        }
      ) +
      labs(y = NULL,
           title = .y) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        axis.text.y = ggtext::element_markdown(),
        plot.title = element_text(size = rel(1)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  )
  )

fig_all_indices = patchwork::wrap_plots(single_index_comparison$bench_plot,
                                        ncol = 3)

ggsave(
  plot = fig_all_indices,
  here::here("inst", "manuscript", "figures", "fig1_sequential_benchmark.png"),
  width = 800, height = 400, units = "px", dpi = 300, scale = 4
)


# Figure 2: Benchmark parallel vs. non parallel --------------------------------

fig_parallel_benchmarks = para_bench %>%
  filter(
    n_core %in% c(1, 6), n_traits == 4, n_sites == 100, n_species == 500
  ) %>%
  mutate(
    parallel = ifelse(n_core == 1, "sequential", "parallel") %>%
      factor(levels = c("sequential", "parallel"))
  ) %>%
  ggplot(aes(time, parallel)) +
  ggbeeswarm::geom_beeswarm(alpha = 1/3, groupOnX = FALSE) +
  facet_wrap(
    vars(fd_fct),
    labeller = labeller(
      fd_fct = c(
        fd_fdis = "Functional Dispersion", fd_fdiv = "Functional Divergence",
        fd_feve = "Functional Evenness", fd_fric = "Functional Richness"
      )
    )
  ) +
  bench::scale_x_bench_time(name = "Running Time") +
  scale_y_discrete(
    NULL,
    labels = function(x) {
      case_when(
        x == "sequential" ~ glue::glue("<span style='color:grey30'>{x}</span>"),
        x == "parallel"   ~ glue::glue(
          "<span style='color:black'>**{x}**</span>"
        )
      )
    }
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.text.y = ggtext::element_markdown(),
    plot.title = element_text(size = rel(1)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(
  plot = fig_parallel_benchmarks,
  here::here("inst", "manuscript", "figures", "fig2_parallel_benchmark.png"),
  width = 400, height = 400, units = "px", dpi = 300, scale = 4
)


# Figure S1: External Comparison Sequential All Parameters ---------------------

fig_s1_full_comparison = single_bench %>%
  rename(fd_fct = expression) %>%
  mutate(
    fd_fct = sub("_unparallel", "", attr(fd_fct, "description"), fixed = TRUE)
  ) %>%
  full_join(
    fd_fct_names,
    by = c(fundiversity_index = "index_name", fd_fct = "internal_name")
  ) %>%
  filter(n_traits != 7) %>%
  tidyr::unnest(c(time, gc)) %>%
  select(fundiversity_index, actual_name, n_sites, n_traits, n_species, time) %>%
  ggplot(
    aes(n_sites, time, color = factor(n_species), shape = factor(n_traits),
        linetype = factor(n_traits))
  ) +
  stat_smooth(formula = y ~ x, method = "lm", alpha = 1/5, size = 1/2) +
  geom_point() +
  facet_wrap(
    vars(fundiversity_index, actual_name), ncol = 5,
    labeller = labeller(
      fundiversity_index = c(
        fdis           = "Functional\nDispersion",
        fdiv           = "Functional\nDivergence",
        feve           = "Functional\nEvenness",
        fric           = "Functional\nRichness",
        fric_intersect = "Functional\nRichness\nintersect",
        raoq           = "Rao's\nQuadratic\nEntropy"
      ),
      actual_name = label_wrap_gen(15))
  ) +
  labs(x = "Number of sites", y = "Execution Time",
       color = "Number of species", shape = "Number of traits",
       linetype = "Number of traits", caption = "30 iterations") +
  scale_color_viridis_d() +
  scale_x_log10() +
  bench::scale_y_bench_time() +
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 7)
  )

ggsave(
  "inst/manuscript/figures/s1_external_comparison_full.png",
  fig_s1_full_comparison,
  width = 210, height = 148, units = "mm", scale = 1.5
)

# Figure S2: Internal Comparison all Parameters --------------------------------

fig_s2_all_para_comparison = para_bench %>%
  ggplot(
    aes(n_sites, time, color = factor(n_species), shape = factor(n_traits),
        linetype = factor(n_traits))
  ) +
  stat_smooth(formula = y ~ x, method = "lm", alpha = 1/5, size = 1/2) +
  geom_point() +
  facet_wrap(
    vars(fd_fct, n_core),
    labeller = labeller(
      fd_fct = c(
        fd_fdis           = "Functional\nDispersion",
        fd_fdiv           = "Functional\nDivergence",
        fd_feve           = "Functional\nEvenness",
        fd_fric           = "Functional\nRichness"
      ),
      n_core = label_both)
  ) +
  labs(x = "Number of sites", y = "Execution Time",
       color = "Number of species", shape = "Number of traits",
       linetype = "Number of traits", caption = "20 iterations") +
  scale_color_viridis_d() +
  scale_x_log10() +
  bench::scale_y_bench_time() +
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 7)
  )

ggsave(
  "inst/manuscript/figures/s2_internal_comparison_full.png",
  fig_s2_all_para_comparison,
  width = 210, height = 148, units = "mm", scale = 1.5
)
