# generate VI input data folder information

# covariate information

library(terra)
library(tidyverse)

(function() {
  print("Generating input data")
  print("clipping/masking covariates")
  confound_loc <- "E:/Sync/Masters/analysis_03_decay/data/rasters"
  
  clims <- list.files(confound_loc, pattern = "clim.*\\.dat$", full.names = T)
  
  topo_loc <- "F:/mosaiced/topo/"
  
  topos <- list.files(topo_loc, pattern = ".dat$", full.names = T)
  
  topo <- c("F:/mosaiced/topo/DEM.dat", "F:/mosaiced/topo/slope.dat")
  
  confounds <- c(clims, topo)
  
  confound_snames <- basename(confounds) %>%
    str_to_lower() %>%
    here::here(input_loc, "covariates", .)
  
  dir.create(dirname(confound_snames[[1]]), showWarnings = F)
  
  if (!all(file.exists(confound_snames))) {
    dir.create(dirname(confound_snames)[[1]], showWarnings = F)
    
    confound_rasts <- confounds %>%
      map(rast) %>%
      map(crop, vi_rast, mask = T, .progress = T)
    
    map2(
      confound_rasts,
      confound_snames,
      writeRaster,
      filetype = "envi",
      overwrite = T
    )
  }
  
  confound_all <- rast(confound_snames)
  
  names(confound_all) <- confounds %>%
    basename() %>%
    tools::file_path_sans_ext()
  
  # variable information (DHI and structure)
  
  print("clipping/masking structure and DHIs")
  
  var_oi <- c(
    "CumDHI",
    "MinDHI",
    "VarDHI",
    "elev_cv",
    "percentage_first_returns_above_2m",
    "elev_p95",
    "total_biomass"
  )
  
  var_in <- here::here(
    "E:/Sync/Masters/analysis_03_decay/data/rasters/bc_mosaic_masked",
    glue("{var_oi}.dat")
  )
  
  var_snames <- basename(var_in) %>%
    here::here(input_loc, "variables", .)
  
  dir.create(dirname(var_snames[[1]]), showWarnings = F)
  
  if (!all(file.exists(var_snames))) {
    dir.create(dirname(var_snames)[[1]], showWarnings = F)
    
    var_rasts <- var_in %>%
      map(rast) %>%
      map(crop, vi_rast, mask = T, .progress = T)
    
    map2(var_rasts,
         var_snames,
         writeRaster,
         filetype = "envi",
         overwrite = T)
    
  }
  
  var_all <- rast(var_snames)
  
  names(var_all) <- var_snames %>%
    basename() %>%
    tools::file_path_sans_ext()
  
  # human footprint and disturbance
  print("clipping/masking human footprint and disturbance")
  hf <- "E:/Sync/Masters/analysis_03_decay/data/rasters/bc_albers_footprint.dat" %>%
    rast()
  dist <- "E:/Sync/Masters/analysis_03_decay/data/rasters/disturbance.dat" %>%
    rast()
  valid <- list(hf, dist)
  
  valid_names <- c("human_footprint", "disturbance")
  
  valid_snames <- here::here(input_loc, "pressures", glue("{valid_names}.dat"))
  
  dir.create(dirname(valid_snames[[1]]), showWarnings = F)
  
  if (!all(file.exists(valid_snames))) {
    valid_vi <- valid %>%
      map(crop, vi_rast, mask = T)
    
    
    map2(valid_vi,
         valid_snames,
         writeRaster,
         filetype = "envi",
         overwrite = T)
    
  }
  
  valid_vi <- rast(valid_snames)
  
  names(valid_vi) <- c("human_footprint", "disturbance")
}
)()
