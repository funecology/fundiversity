# Packages ---------------------------------------------------------------------
library("ggplot2")
library("dplyr")

# Load data --------------------------------------------------------------------
# Benchmark using single core
single_bench = list.files(
  "inst/saved_benchmarks", "*alternatives.rds", full.names = TRUE
) %>%
  setNames(
    c("fdis", "fdiv", "feve", "fric_old", "raoq", "fric", "fric_intersect")
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

# Figures ----------------------------------------------------------------------

# Full figure for Supplementary materials (?)
all_bench %>%
  filter(fundiversity_index != "fric_old") %>%
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
        gsub("multicore", paste0("fd_", .x, "_multicore"), .y, fixed = TRUE),
        .y
      )
    )
  ) %>%
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
