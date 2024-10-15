library(tidyverse)
library(tidyterra)
library(terra)
library(viridis)
library(patchwork)


clims <- list.files(
  "E:/Sync/Masters/04_vi_pristine/data/rasters/inputs",
  pattern = "clim",
  full.names = T
) %>%
  str_subset(pattern = ".dat$") %>%
  map(rast)

dem <- "E:/Sync/Masters/04_vi_pristine/data/rasters/inputs/dem.dat" %>%
  rast()

slope <- "E:/Sync/Masters/04_vi_pristine/data/rasters/inputs/slope.dat" %>%
  rast()

bcb <- bcmaps::bc_bound_hres()

f <- c(clims, dem, slope)

names(f) <- map(f, sources) %>%
  unlist() %>%
  basename()

names(f) <- c("clim_MAP", "clim_MAT", "clim_MCMT", "clim_MWMT", "DEM", "slope")

f <- rev(f)

p <- map2(f, LETTERS[1:length(f)], \(x, pal) {
  ggplot() +
    geom_spatraster(data = x) +
    scale_fill_viridis_c(option = pal, na.value = "transparent") +
    theme_void() +
    theme(legend.position = "none")
})

layout_um <- map(1:length(f), \(x) {
  area(t = x,
       l = 1,
       b = 9 + x,
       r = 2)
})

layout <- c(layout_um[[6]],
            layout_um[[5]],
            layout_um[[4]],
            layout_um[[3]],
            layout_um[[2]],
            layout_um[[1]])

stack <- p[[1]] + p[[2]] + p[[3]] + p[[4]] + p[[5]] + p[[6]] + plot_layout(design = layout)

ggsave(here::here("figures", "test_stack.png"), plot = stack, height = 6, width = 4)

quant <- read_csv("E:/Sync/Masters/analysis_03_decay/data/csv/quantiles.csv")

quantiled <- map2(f, names(f), \(layer, name) {
  quants <- global(layer, fun = quantile, probs = seq(0, 1, .2), na.rm = T)
  
  classify(layer, as.numeric(quants))
})


pq <- map2(quantiled, LETTERS[1:length(f)], \(x, pal) {
  ggplot() +
    geom_spatraster(data = x) +
    scale_fill_viridis_d(option = pal, na.value = "transparent") +
    theme_void() +
    theme(legend.position = "none")
})

stackq <- pq[[1]] + pq[[2]] + pq[[3]] + pq[[4]] + pq[[5]] + pq[[6]] + plot_layout(design = layout) +
  plot_annotation(theme = theme(plot.background = element_rect(color  = '#00000000')))

ggsave(here::here("figures", "test_stackq.png"), plot = stackq, height = 6, width = 4, bg = "transparent")
