# sysfonts::font_add_google("Roboto Slab")
# showtext::showtext_auto()

library(ggplot2)
library(dplyr)
library(ggfx)
library(ggforce)
library(shadowtext)


# Helper for creating a hex dataframe
rotate <- function(df, angle) {
  
  angle <- pi * angle
  x <- df$x
  y <- df$y
  
  df$x <- x * cos(angle) - y * sin(angle)
  df$y <- x * sin(angle) + y * cos(angle)
  
  df
  
} 


# Create hexagon data
hex <- seq(0, 2, by = 1/3) %>%
  purrr::map_df(rotate, df = tibble(x = 0, y = 1))


# Read in mr fussy jpg
mr_fussy <- jpeg::readJPEG("man/figures/mrfussy.jpg") %>% 
  grid::rasterGrob(interpolate = TRUE, width = 2, y = unit(0.53, "npc"))


# Create the hex
p <- ggplot(hex, aes(x, y)) +
  as_reference(
    geom_shape(fill = "black", colour = "black", size = 4), 
    id = "hex"
  ) +
  with_blend(
    annotation_custom(mr_fussy),
    bg_layer = "hex", blend_type = "in"
  ) +
  geom_shape(fill = NA, colour = "black", size = 6) +
  annotate(
    "text", x = 0, y = -0.4, label = "pedant", family = "Roboto Slab", 
    fontface = "bold", colour = "yellow", size = 25
  ) +
  theme_void() +
  coord_cartesian(c(-.86, .86), c(-.95, .95))


# Save the hex
ragg::agg_png(
  "man/figures/logo.png", width = 630, height = 670
)
p
dev.off()
