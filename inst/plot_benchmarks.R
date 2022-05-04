# Packages ---------------------------------------------------------------------
library("ggplot2")
library("dplyr")

# Load data --------------------------------------------------------------------
# Benchmark using single core
single_bench = list.files(
  "inst/saved_benchmarks", "*alternatives.rds", full.names = TRUE
) %>%
  setNames(
    c("fdis", "fdiv", "feve", "raoq", "fric", "fric_intersect")
  ) %>%
  purrr::map(readRDS) %>%
  bind_rows(.id = "fundiversity_index")

# Benchmark using multiple cores (4)
multi_bench = list.files(
  "inst/saved_benchmarks", "*multicore.rds", full.names = TRUE
) %>%
  setNames(c("fdis", "feve", "raoq")) %>%
  purrr::map(readRDS) %>%
  bind_rows(.id = "fundiversity_index")

all_bench = bind_rows(list(single_bench, multi_bench))


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
      bench::scale_x_bench_time(name = "Time") +
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
            grepl("fundiversity", levels(.x$package)), "black", "grey35"
          )
          glue::glue("<span style='color:{colours}; font-family:mono'>{bold}{x}{bold}</span>")
        }
      ) +
      labs(y = NULL,
           title = .y) +
      theme_bw() +
      theme(
        aspect.ratio = 1,
        strip.background = element_blank(),
        axis.text.y = ggtext::element_markdown(),
        plot.title = element_text(size = rel(1)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  )
  )

fig_all_indices = patchwork::wrap_plots(single_index_comparison$bench_plot,
                                        ncol = 2)

saveRDS(fig_all_indices, here::here("inst", "fig1_simplified_benchmark.Rds"))


# Figure S1: Full comparison between all parameters and packages ---------------

fig_s1_full_comparison = all_bench %>%
  rename(fd_fct = expression) %>%
  mutate(
    fd_fct = sub("_", "::", attr(fd_fct, "description"), fixed = TRUE) %>%
      paste0("()") %>%
      {sub("_unparallel", "", ., fixed = TRUE)}
  ) %>%
  mutate(
    fd_fct = purrr::map2_chr(
      fundiversity_index, fd_fct,
      ~ifelse(
        grepl("multicore", .y),
        gsub("multicore", paste0("fd_", .x, "()\n(multicore)"), .y, fixed = TRUE),
        .y
      )
    )
  ) %>%
  filter(n_traits != 7) %>%
  tidyr::unnest(c(time, gc)) %>%
  select(fundiversity_index, fd_fct, n_sites, n_traits, n_species, time) %>%
  ggplot(
    aes(n_sites, time, color = factor(n_species), shape = factor(n_traits),
        linetype = factor(n_traits))
  ) +
  stat_smooth(formula = y ~ x, method = "lm", alpha = 1/5, size = 1/2) +
  geom_point() +
  facet_wrap(
    vars(fundiversity_index, fd_fct), ncol = 7,
    labeller = labeller(
      fundiversity_index = c(
        fdis           = "Functional\nDispersion",
        fdiv           = "Functional\nDivergence",
        feve           = "Functional\nEvenness",
        fric           = "Functional\nRichness",
        fric_intersect = "Functional\nRichness\nintersect",
        raoq           = "Rao's\nQuadratic\nEntropy"
      ),
      fd_fct = label_value)
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

