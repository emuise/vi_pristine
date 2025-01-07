input_loc <- here::here("data", "inputs")
dir.create(input_loc, showWarnings = F)
tab_loc <- here::here(base_data_loc, "tabs")
dir.create(tab_loc, showWarnings = F, recursive = T)

scratch <- here::here("scratch")
dir.create(scratch, showWarnings = F)

split_loc <- here::here("data", "pca_splits")
dir.create(split_loc, showWarnings = F)

treat_loc <- here::here(tab_loc, "treatment_strata")
dir.create(treat_loc, showWarnings = F)

mahal_tab_loc <- here::here(tab_loc, "mahal")
dir.create(mahal_tab_loc, showWarnings = F)

raster_loc <- here::here(base_data_loc, "rasters")
dir.create(raster_loc, showWarnings = F, recursive = T)

mahal_ras_loc <- here::here(raster_loc, "mahal")
dir.create(mahal_ras_loc, showWarnings = F, recursive = T)

nn_save_loc <- here::here(raster_loc, "nn_dist")
dir.create(nn_save_loc, showWarnings = F)

shapefile_loc <- here::here(input_loc, "shapefiles")
dir.create(shapefile_loc, showWarnings = F)

figure_loc <- here::here(base_loc, "figures")
dir.create(figure_loc, showWarnings = F)

