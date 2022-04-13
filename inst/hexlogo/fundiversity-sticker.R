#'
#' Create an Hexagonal Sticker for the Package
#'

library("ggplot2")
library("dplyr")

# Generate traits for species

set.seed(424242)
traits <- matrix(rnorm(20), ncol = 2)
rownames(traits) <- paste0("sp", seq(nrow(traits)))
colnames(traits) <- paste0("t", seq(ncol(traits)))
traits <- as.data.frame(traits)
traits <- tibble::rownames_to_column(traits, "species")


# Generate sites
sites <- data.frame(
  site    = c(rep("A", 6), rep("B", 7)),
  species = paste0("sp", c(1, 3, 4, 7, 9, 10,
                           1, 2, 3, 4, 6, 8, 9))
)

full <- merge(sites, traits, by = "species")

site_avg <- full %>%
  group_by(site) %>%
  summarise(across(t1:t2, mean))

# Actual plot ------------------------------------------------------------------



p <- ggplot2::ggplot(full, aes(t1, t2)) +
  ggforce::geom_mark_hull(
    aes(fill = site, color = site),
    concavity = 100, alpha = 2/3, size = 1/2, expand = unit(1, "mm"),
    radius = unit(1, "mm")
  ) +
  geom_segment(
    data = full %>%
      inner_join(site_avg %>%
                   rename(t1_avg = t1, t2_avg = t2),
                 by = "site"),
               aes(x = t1_avg, y = t2_avg, xend = t1, yend = t2),
    size = 1/3, linetype = 2, color = "#121212"
  ) +
  geom_point(size = 1/3) +
  geom_point(data = site_avg, aes(color = site), size = 1.1) +
  scale_fill_manual(values = c(A = "#F1A340", B = "#998EC3")) +
  scale_color_manual(values = c(A = scales::muted("#F1A340", l = 50),
                                B = scales::muted("#998EC3", l = 50))) +
  coord_cartesian(clip = "off") +
  ggplot2::theme_void() +
  hexSticker::theme_transparent() +
  theme(legend.position = "none")

p

# Sticker -----------------------------------------------------

hexSticker::sticker(

  subplot   = p,
  package   = "fundiversity",
  filename  = here::here("inst", "hexlogo", "fundiversity-hexlogo.png"),
  dpi       = 300,

  p_size    = 13,         # Title
  u_size    =  4,         # URL
  # p_family  = "Aller_Rg",

  p_color   = "#121212",   # Title
  h_fill    = "#DBDBDB",   # Background
  h_color   = "#5C5C5C",   # Border
  u_color   = "#121212",   # URL

  # p_x       = 1.00,        # Title
  p_y       = 1.63,        # Title
  s_x       = 1.05,        # Subplot
  s_y       = 0.9,        # Subplot

  s_width   = 1.3,         # Subplot
  s_height  = 1.3,         # Subplot

  url       = "https://github.com/Bisaloo/fundiversity",

  spotlight = TRUE,
  l_alpha   = 0.7,
  l_width   = 5,
  l_height  = 1,
  l_y = 1.6
)

