
library("terra")
library("rnaturalearth")
library("landscapemetrics")
library("raster")

## Get user args
downscaling_args <- commandArgs(trailingOnly = TRUE)

simulation_type <- downscaling_args[1]
fuzzy_multiplier <- as.numeric(downscaling_args[2])

getDSCOLLandscapeMetrics <- function(sim,
                                     simulation_type,
                                     fuzzy_multiplier) {
  
  # Set up variables
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
  all_timesteps <- paste0("X", 
                          1960:2019)
  
  if (simulation_type == "fuzzy") {
    map_prefix <- paste0("Sim",
                         sim,
                         "_HILDA_DS_COL_",
                         simulation_type,
                         "_",
                         fuzzy_multiplier)
  } else {
    map_prefix <- paste0("Sim",
                         sim,
                         "_HILDA_DS_COL_",
                         simulation_type)
  }
  
  analysis_dir <- "~/03_Downscaling/analysis/DS_methods_paper/20221010_HILDA_COL_DS_1960_2019/"
  
  # Get downscaled maps
  downscaled_colombia <- loadDownscaledMaps(sim = sim,
                                            simulation_type = simulation_type,
                                            fuzzy_multiplier = fuzzy_multiplier,
                                            map_prefix = map_prefix)
  
  # Classify downscaled maps
  downscaled_colombia <- classifyDownscaledMaps(downscaled_maps = downscaled_colombia,
                                                hilda_LC_classes = hilda_colombia_LC_classes)
  
  # Get HILDA+ maps
  hilda_dir <- "~/03_Downscaling/data/"
  hilda_colombia <- loadHILDAColombia(hilda_dir = hilda_dir)
  
  # Add ref map to downscaled maps
  downscaled_colombia <- addRefMap(downscaled_maps = downscaled_colombia,
                                   ref_map = hilda_colombia[["X1960"]])
  
  # Create levels
  hilda_levels <- getLevelsList(hilda_LC_classes = hilda_colombia_LC_classes,
                                hilda_LC_classes_display = hilda_colombia_LC_display_classes,
                                hilda_nlayers = nlyr(hilda_colombia))
  
  # Make the downscaled and HILDA+ maps match
  levels(hilda_colombia) <- hilda_levels
  levels(downscaled_colombia) <- hilda_levels
  
  names(hilda_colombia) <- all_timesteps
  names(downscaled_colombia) <- all_timesteps
  
  hilda_colombia <- crop(hilda_colombia,
                         ext(downscaled_colombia))
  
  ### Analyses
  # Similarity
  getSimilarityTable(hilda_maps = hilda_colombia,
                     downscaled_maps = downscaled_colombia,
                     analysis_dir = analysis_dir,
                     analysis_prefix = map_prefix)
  
  # Set up for landscape pattern metrics
  ## Load HILDA+ Colombia World Eckert VI projection file
  ## World Eckert IV projection is EPSG:54012 or ESRI:54012 according to https://spatialreference.org/ref/esri/54012/
  hilda_colombia_raster <- loadHILDAColombiaEckIV(hilda_dir = hilda_dir)
  
  ## Create raster of downscaled maps
  downscaled_colombia_raster <- raster::stack(downscaled_colombia)
  
  ## Set the CRS to EPSG:4326, which is the same as "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ## according to https://spatialreference.org/ref/epsg/4326/
  crs(downscaled_colombia_raster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  ## Project to World Eckert IV equal area projection, which is EPSG:54012 or ESRI:54012
  ## The PROJ4 text below is from https://spatialreference.org/ref/esri/54012/
  downscaled_colombia_raster <- projectRaster(downscaled_colombia_raster,
                                              crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                                              method = "ngb")
  
  # Check the landscapes
  check_landscape(hilda_colombia_raster)
  check_landscape(downscaled_colombia_raster)
  
  # Landscape-level stats
  getLandscapeStats(downscaled_maps = downscaled_colombia_raster,
                    analysis_dir = analysis_dir,
                    analysis_prefix = map_prefix)
  
  # Class-level stats
  getClassStats(downscaled_maps = downscaled_colombia_raster,
                analysis_dir = analysis_dir,
                analysis_prefix = map_prefix)
  
}

loadDownscaledMaps <- function(sim,
                               simulation_type,
                               fuzzy_multiplier,
                               map_prefix) {
  
  downscaled <- rast(paste0("~/03_Downscaling/model_outputs/DS_methods_paper/20221010_HILDA_COL_DS_1960_2019/",
                            map_prefix,
                            "_Discrete_Time",
                            1:59,
                            ".tif"))
  names(downscaled) <- paste0("X",
                              1961:2019)
  
  return(downscaled)
}

classifyDownscaledMaps <- function(downscaled_maps,
                                   hilda_LC_classes) {
  
  classification_mat <- cbind(1:length(hilda_LC_classes),
                              as.numeric(hilda_LC_classes))
  
  downscaled_maps <- classify(downscaled_maps,
                              classification_mat,
                              others = NA)
  
  return(downscaled_maps)
}

loadHILDAColombia <- function(hilda_dir) {
  
  hilda <- rast(paste0(hilda_dir,
                       "20221115_HILDA_COL_LULC_1960_2019.tif"))
  crs(hilda) <- "EPSG:4326"
  NAflag(hilda) <- 0
  names(hilda) <- paste0("X",
                         1960:2019)
  
  return(hilda)
}

addRefMap <- function(downscaled_maps,
                      ref_map) {
  
  downscaled_maps <- c(crop(ref_map,
                            ext(downscaled_maps)),
                       downscaled_maps)
  
  return(downscaled_maps)
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

getSimilarityTable <- function(hilda_maps,
                               downscaled_maps,
                               analysis_dir,
                               analysis_prefix) {
  
  are_cells_equal <- hilda_maps == downscaled_maps
  
  similarity_to_hilda <- global(are_cells_equal, fun = "sum", na.rm = TRUE) / global(are_cells_equal, fun = "notNA")
  
  write.csv(similarity_to_hilda,
            file = paste0(analysis_dir,
                          analysis_prefix,
                          "_similarity.csv"))
  
  return(NULL)
}

loadHILDAColombiaEckIV <- function(hilda_dir) {
  
  hilda <- raster::stack(paste0(hilda_dir,
                                "20221115_HILDA_COL_LULC_1960_2019_EckIV.tif"))
  raster::crs(hilda) <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  return(hilda)
}

getLandscapeStats <- function(downscaled_maps,
                              analysis_dir,
                              analysis_prefix) {
  
  landscape_metrics <- calculate_lsm(downscaled_maps,
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

getClassStats <- function(downscaled_maps,
                          analysis_dir,
                          analysis_prefix) {
  
  class_metrics <- calculate_lsm(downscaled_maps,
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

# Run the main function for each simulation
for (i in 1:10) {
  
  getDSCOLLandscapeMetrics(sim = i,
                           simulation_type = simulation_type,
                           fuzzy_multiplier = fuzzy_multiplier)
  
}
