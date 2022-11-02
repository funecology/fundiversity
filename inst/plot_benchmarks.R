# Packages ---------------------------------------------------------------------
library("ggplot2")
library("dplyr")

# Load data --------------------------------------------------------------------
# Correspondence data.frame between internal vs. actual names
# These are collated from the benchmark*.R files
fd_fct_names = tibble::tribble(
  ~index_name,      ~fct_internal_name,               ~fct_actual_name,
  "fric",           "fundiversity_fd_fric",           "fundiversity\nfd_fric()",
  "fric",           "BAT_alpha_tree",                 "BAT\nalpha()",
  "fric",           "BAT_alpha_hull",                 "BAT\nhull.alpha()",
  "fric",           "mFD_alpha_fd",                   "mFD\nalpha.fd.multidim(\nind_vect = 'fric')",
  "fdis",           "fundiversity_fd_fdis",           "fundiversity\nfd_fdis()",
  "fdis",           "BAT_dispersion",                 "BAT\ndispersion()",
  "fdis",           "FD_fdisp",                       "FD\nfdisp()",
  "fdis",           "mFD_alpha_fd",                   "mFD\nalpha.fd.multidim(\nind_vect = 'fdis')",
  "fdiv",           "fundiversity_fd_fdiv",           "fundiversity\nfd_fdiv()",
  "fdiv",           "mFD_alpha_fd",                   "mFD\nalpha.fd.multidim(\nind_vect = 'fdiv')",
  "feve",           "fundiversity_fd_feve",           "fundiversity\nfd_feve()",
  "feve",           "mFD_alpha_fd",                   "mFD\nalpha.fd.multidim(\nind_vect = 'feve')",
  "raoq",           "fundiversity_fd_raoq",           "fundiversity\nfd_raoq()",
  "raoq",           "adiv_qe",                        "adiv\nQE()",
  "raoq",           "BAT_rao",                        "BAT\nrao()",
  "raoq",           "hillR_hill_func",                "hillR\nhill_func(\nfdis = FALSE)",
  "raoq",           "mFD_alpha_fd_hill",              "mFD\nalpha.fd.hill(\nq = 2, tau = 'max')",
  "fric_intersect", "fundiversity_fd_fric_intersect", "fundiversity\nfd_fric_intersect()",
  "fric_intersect", "betapart_functional_beta",       "betapart\nfunctional.beta.pair()",
  "fric_intersect", "hillR_funct_pairwise",           "hillR\nhill_func_parti_\npairwise()"
)

full_index_df = tibble::tribble(
  ~index_name,      ~index_full_name,
  "fdis"          , "Functional Dispersion",
  "fdiv"          , "Functional Divergence",
  "feve"          , "Functional Evenness",
  "fric"          , "Functional Richness",
  "fric_intersect", "Functional Richness intersect",
  "raoq"          , "Rao's Quadratic Entropy"
)

fd_fct_names = fd_fct_names %>%
  inner_join(full_index_df, by = "index_name")

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
    by = c(fundiversity_index = "index_name", fd_fct = "fct_internal_name")
  ) %>%
  filter(n_traits != 7) %>%
  tidyr::unnest(c(time, gc)) %>%
  select(
    fundiversity_index, index_full_name, fct_actual_name, n_sites, n_traits,
    n_species, time
  ) %>%
  mutate(
    n_species = factor(n_species),
    n_traits = factor(n_traits)
  ) %>%
  tidyr::nest(df = -c(fundiversity_index, index_full_name)) %>%
  mutate(
    sub_plot = purrr::map2(
      index_full_name, df,
      ~.y %>%
        ggplot(
          aes(
            n_sites, time, color = n_species, shape = n_traits,
            linetype = n_traits
          )
        ) +
        stat_smooth(
          formula = y ~ x, method = "lm", alpha = 1/5, linewidth = 1/2
        ) +
        geom_point() +
        facet_grid(
          cols = vars(fct_actual_name)
        ) +
        labs(
          x        = "Number of sites",   y       = "Execution Time",
          color    = "Number of species", shape   = "Number of traits",
          linetype = "Number of traits",  title   = .x
        ) +
        scale_color_viridis_d(drop = FALSE) +
        scale_x_log10(
          breaks = c(50, 100, 300, 500), limits = c(50, 500)
        ) +
        bench::scale_y_bench_time(
          breaks = c(
            bench::as_bench_time("100ms"), bench::as_bench_time("1.67m"),
            bench::as_bench_time("1.16d")
          ),
          limits = c(
            bench::as_bench_time("2ms"), bench::as_bench_time("3.8d")
          )
        ) +
        scale_shape(drop = FALSE) +
        scale_linetype(drop = FALSE) +
        guides(linetype = guide_legend(override.aes = list(color = "black"))) +
        coord_cartesian(clip = "off") +
        theme_bw() +
        theme(
          aspect.ratio      = 1,
          panel.grid        = element_blank(),
          plot.tag.position = "topleft",
          strip.background  = element_blank(),
          strip.text        = element_text(size = 7)
        )
    )
  )

fig_s1_final = patchwork::wrap_plots(
  fig_s1_full_comparison$sub_plot, ncol = 1, guides = "collect"
)

ggsave(
  "inst/manuscript/figures/s1_external_comparison_full.pdf",
  fig_s1_final,
  width = 6, height = 8, units = "in", scale = 1.7
)

ggsave(
  "inst/manuscript/figures/s1_external_comparison_full.png",
  fig_s1_final,
  width = 6, height = 8, units = "in", scale = 1.7, dpi = 600
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


# Table S1: Functional Diversity Dependencies ----------------------------------

fd_pkg_list = c(
  "fundiversity", "adiv", "BAT", "betapart", "entropart", "FD", "hilldiv",
  "hillR", "hypervolume", "mFD", "TPD", "vegan"
) %>%
  purrr::set_names(., nm = .)

fd_full_deps = fd_pkg_list %>%
  lapply(pak::pkg_deps, dependencies = "all") %>%
  dplyr::bind_rows(.id = "source_pkg")

fd_hard_deps = fd_pkg_list %>%
  lapply(pak::pkg_deps, dependencies = "hard") %>%
  dplyr::bind_rows(.id = "source_pkg")

fd_deps_df = fd_full_deps %>%
  group_by(source_pkg) %>%
  summarise(
    n_all_deps = n() - 1  # Remove source package from list
  ) %>%
  inner_join(
    fd_hard_deps %>%
      group_by(source_pkg) %>%
      summarise(
        n_hard_deps = n() - 1  # Remove source package from list
      ),
    by = "source_pkg"
  ) %>%
  relocate(n_hard_deps, .before = n_all_deps)


saveRDS(fd_deps_df, "inst/manuscript/figures/tab_s1_pkg_deps.Rds")
