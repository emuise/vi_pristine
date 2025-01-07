library(bcmaps)
library(tidyverse)
library(tidyterra)
library(terra)
library(patchwork)

# change this to where you want things you don't need to save to live
scratch <- here::here("data", "scratch")
dir.create(scratch, showWarnings = F)

figure_loc <- here::here("figures")

bcb_hres <- bcmaps::bc_bound_hres() %>%
  vect()

vi_mainland <- bcb_hres %>%
  disagg() %>%
  mutate(Shape_Area = expanse(.)) %>%
  arrange(desc(Shape_Area)) %>%
  filter(row_number() == 2)

# great lakes data for the inset map

url <- "https://www.sciencebase.gov/catalog/file/get/530f8a0ee4b0e7e46bd300dd"

dest <- here::here(scratch, "great_lakes.zip")

if (!file.exists(dest)) {
  download.file(url, destfile = dest, mode = "wb")
}

unzip_dir <- here::here(scratch, "great_lakes")

unzip(dest, exdir = unzip_dir)

great_lakes <- list.files(unzip_dir, pattern = "hydro.*\\.shp$", recursive = T, full.names = T) %>%
  map(vect) %>%
  map(aggregate) %>%
  vect() %>%
  project("epsg:3347")

# making map

vi_ext <- ext(vi_mainland)

main <- ggplot() +
  geom_spatvector(data = bcb_hres, fill = "#e5e5e5", col = "#00000000") +
  geom_spatvector(data = vi_mainland) +
  geom_spatvector(data = vi_mainland, fill = "#00000000") +
  theme_bw() +
  scale_fill_manual(name = NULL, values = "#0bc818", labels = "Protected Areas") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.9)) +
  coord_sf(xlim = c(vi_ext$xmin, vi_ext$xmax),
           ylim = c(vi_ext$ymin, vi_ext$ymax)) +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.background = element_blank())

# download vectors of NA coutnries for the inset map
cad <- geodata::gadm("Canada", level = 1, path = "scratch")

world <- geodata::world(path = "scratch") %>%
  project("epsg:3347")

cad_ext <- cad %>%
  project("epsg:3347") %>% 
  ext()

vi_box <- vi_ext %>% 
  vect(crs = "epsg:3005") %>%
  project("epsg:4437")

# inset map

inset <- ggplot() +
  geom_spatvector(data = world, fill = "#7f7f7f") +
  geom_spatvector(data = cad %>% project("epsg:3347"), fill = "#b3b3b3") +
  geom_spatvector(data = great_lakes, fill = "lightblue") +
  geom_spatvector(data = bcb_hres, fill = "#e5e5e5") +
  geom_spatvector(data = vi_box, col = "red", fill = "#00000000", linewidth = 1) +
  #geom_spatvector(data = vi_mainland, col = "red") +
  coord_sf(xlim = c(cad_ext$xmin, cad_ext$xmax),
           ylim = c(cad_ext$ymin, cad_ext$ymax)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.background = element_blank())

study_area <- main +
  inset_element(inset, 0, 0, 0.4, 0.4)

ggsave(here::here(figure_loc, "study_area_no_pa.png"), plot = study_area, height = 6, width = 6)
