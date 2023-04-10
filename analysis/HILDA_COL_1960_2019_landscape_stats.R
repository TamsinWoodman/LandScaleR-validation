
library("terra")
library("rnaturalearth")
library("landscapemetrics")
library("raster")

getHILDACOLLandscapeMetrics <- function() {
  
  start_time <- Sys.time()
  
  # Get HILDA+ maps
  hilda_dir <- "~/Projects/03_Disaggregate_land_use/data/interim/20211006_hildap_vGLOB-1.0-f_netcdf/hildap_vGLOB-1.0-f_netcdf/"
  hilda <- loadHILDA(hilda_dir = hilda_dir)
  hilda_colombia <- getHILDAColombia(hilda = hilda)
  
  # Set HILDA+ levels
  hilda_colombia_LC_classes <- c("11", 
                                 "22", 
                                 "33",
                                 "40",
                                 "42",
                                 "43",
                                 "44",
                                 "55",
                                 "66",
                                 "77")
  hilda_colombia_LC_display_classes <- c("Urban",
                                         "Cropland",
                                         "Pasture",
                                         "Unknown/Other forest",
                                         "Evergreen, broad leaf forest",
                                         "Deciduous, needle leaf forest",
                                         "Deciduous, broad leaf forest",
                                         "Grass/shrubland",
                                         "Other land",
                                         "Water")
  hilda_levels <- getLevelsList(hilda_LC_classes = hilda_colombia_LC_classes,
                                hilda_LC_classes_display = hilda_colombia_LC_display_classes,
                                hilda_nlayers = nlyr(hilda_colombia))
  levels(hilda_colombia) <- hilda_levels
  
  # Set layer names
  all_timesteps <- paste0("X", 
                          1960:2019)
  names(hilda_colombia) <- all_timesteps
  
  # Crop to the extent of a downscaled map so that landscape pattern metrics 
  # cover exactly the same area
  downscaled_map <- rast("~/Projects/03_Disaggregate_land_use/model_outputs/paper_simulations/20221010_HILDA_COL_DS_1960_2019_Maxwell/Sim9_HILDA_DS_COL_null_model_Discrete_Time1.tif")
  hilda_colombia <- crop(hilda_colombia,
                         downscaled_map)
  
  # Set up for landscape pattern metrics
  hilda_colombia_raster <- raster::stack(hilda_colombia)
  
  ## Set the CRS to EPSG:4326, which is the same as "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ## according to https://spatialreference.org/ref/epsg/4326/
  crs(hilda_colombia_raster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  ## Project to World Eckert IV equal area projection, which is EPSG:54012 or ESRI:54012
  ## The PROJ4 text below is from https://spatialreference.org/ref/esri/54012/
  hilda_colombia_raster <- raster::projectRaster(hilda_colombia_raster,
                                                 crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                                                 method = "ngb")
  
  check_landscape(hilda_colombia_raster)
  
  # Set up analysis directory and prefix
  analysis_dir <- "~/Projects/03_Disaggregate_land_use/analysis/DS_methods_paper/20221010_HILDA_COL_DS_1960_2019/"
  analysis_prefix <- "HILDA_COL_1960_2019"
  
  # Landscape-level stats
  getLandscapeStats(input_maps = hilda_colombia_raster,
                    analysis_dir = analysis_dir,
                    analysis_prefix = analysis_prefix)
  
  # Class-level stats
  getClassStats(input_maps = hilda_colombia_raster,
                analysis_dir = analysis_dir,
                analysis_prefix = analysis_prefix)
  
  # Save the unprojected HILDA+ map to file
  terra::writeRaster(hilda_colombia,
                     filename = "~/Projects/03_Disaggregate_land_use/data/interim/20221115_HILDA_COL_LULC_1960_2019.tif",
                     overwrite = TRUE)
  
  # Save the projected HILDA+ map to file
  raster::writeRaster(hilda_colombia_raster,
                      filename = "~/Projects/03_Disaggregate_land_use/data/interim/20221115_HILDA_COL_LULC_1960_2019_EckIV.tif",
                      overwrite = TRUE)
  
  end_time <- Sys.time()
  run_time <- difftime(end_time,
                       start_time,
                       units = "auto")
  print("Finished in: ")
  print(format(run_time))
  
}

loadHILDA <- function(hilda_dir) {
  
  hilda <- rast(paste0(hilda_dir,
                       "hildaplus_vGLOB-1.0-f_states.nc"))
  crs(hilda) <- "EPSG:4326"
  NAflag(hilda) <- 0
  names(hilda) <- paste0("X",
                         1899:2019)
  
  return(hilda)
}

getHILDAColombia <- function(hilda) {
  
  ## Get outline of Colombia
  colombia_outline <- ne_countries(country = "colombia",
                                   scale = "large",
                                   returnclass = "sf")
  colombia_polygon <- vect(colombia_outline)
  
  ## Crop HILDA+ to Colombia outline and 1960-2019
  hilda_colombia <- crop(hilda, 
                         colombia_polygon,
                         mask = TRUE)
  
  all_timesteps <- paste0("X", 
                          1960:2019)
  hilda_colombia <- hilda_colombia[[all_timesteps]]
  
  return(hilda_colombia)
}

getLevelsList <- function(hilda_LC_classes,
                          hilda_LC_classes_display,
                          hilda_nlayers) {
  
  hilda_levels <- data.frame(id = as.numeric(hilda_LC_classes),
                             Land_cover = hilda_LC_classes_display)
  hilda_levels_list <- vector("list",
                              hilda_nlayers)
  hilda_levels_list <- lapply(hilda_levels_list,
                              FUN = function(x) x <- hilda_levels)
  
  return(hilda_levels_list)
}

getLandscapeStats <- function(input_maps,
                              analysis_dir,
                              analysis_prefix) {
  
  landscape_metrics <- calculate_lsm(input_maps,
                                     what = c("lsm_l_area_mn",
                                              "lsm_l_area_cv",
                                              "lsm_l_ed",
                                              "lsm_l_enn_mn",
                                              "lsm_l_enn_sd",
                                              "lsm_l_ai",
                                              "lsm_l_np",
                                              "lsm_l_pd",
                                              "lsm_l_cai_mn",
                                              "lsm_l_cai_cv"))
  
  write.csv(landscape_metrics,
            file = paste0(analysis_dir,
                          analysis_prefix,
                          "_landscape_metrics.csv"),
            row.names = FALSE)
  
  return(NULL)
}

getClassStats <- function(input_maps,
                          analysis_dir,
                          analysis_prefix) {
  
  class_metrics <- calculate_lsm(input_maps,
                                 what = c("lsm_c_ca",
                                          "lsm_c_pland",
                                          "lsm_c_area_mn",
                                          "lsm_c_area_cv",
                                          "lsm_c_ed",
                                          "lsm_c_enn_mn",
                                          "lsm_c_enn_sd",
                                          "lsm_c_ai",
                                          "lsm_c_np",
                                          "lsm_c_pd",
                                          "lsm_c_cai_mn",
                                          "lsm_c_cai_cv"))
  
  write.csv(class_metrics,
            file = paste0(analysis_dir,
                          analysis_prefix,
                          "_class_metrics.csv"),
            row.names = FALSE)
  
  return(NULL)
}

# Call the main function
getHILDACOLLandscapeMetrics()
