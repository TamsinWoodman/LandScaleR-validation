
library("terra")
library("rnaturalearth")

## Get user args
downscaling_args <- commandArgs(trailingOnly = TRUE)

simulation_type <- downscaling_args[1]
fuzzy_multiplier <- as.numeric(downscaling_args[2])

getDSCOLFoM <- function(sim,
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
  first_timesteps <- all_timesteps[1:length(all_timesteps) - 1]
  second_timesteps <- all_timesteps[2:length(all_timesteps)]
  
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
  
  #analysis_dir <- "~/03_Downscaling/analysis/DS_methods_paper/20221010_HILDA_COL_DS_1960_2019/"
  analysis_dir <- "analysis/DS_methods_paper/20221010_HILDA_COL_DS_1960_2019/"
  
  # Get downscaled maps
  downscaled_colombia <- loadDownscaledMaps(sim = sim,
                                            simulation_type = simulation_type,
                                            fuzzy_multiplier = fuzzy_multiplier,
                                            map_prefix = map_prefix)
  
  # Classify downscaled maps
  downscaled_colombia <- classifyDownscaledMaps(downscaled_maps = downscaled_colombia,
                                                hilda_LC_classes = hilda_colombia_LC_classes)
  
  # Get HILDA+ maps
  #hilda_dir <- "~/03_Downscaling/data/"
  hilda_dir <- "data/interim/"
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
  
  # Get yearly FoM data frame
  figure_of_merit_yearly_df <- getFoMDF(hilda_colombia = hilda_colombia,
                                        downscaled_colombia = downscaled_colombia,
                                        first_timesteps = first_timesteps,
                                        second_timesteps = second_timesteps)
  
  # Save yearly Figure of Merit to file
  write.csv(figure_of_merit_yearly_df,
            file = paste0(analysis_dir,
                          map_prefix,
                          "_FoM_yearly.csv"))
  
  # Calculate FoM over the entire simulation
  first_year <- "X1960"
  last_year <- "X2019"
  
  figure_of_merit_df <- getFoMDF(hilda_colombia = hilda_colombia[[c(first_year,
                                                                    last_year)]],
                                 downscaled_colombia = downscaled_colombia[[c(first_year,
                                                                              last_year)]],
                                 first_timesteps = first_year,
                                 second_timesteps = last_year)
  
  # Save Figure of Merit to file
  write.csv(figure_of_merit_df,
            file = paste0(analysis_dir,
                          map_prefix,
                          "_FoM.csv"))
  
}

loadDownscaledMaps <- function(sim,
                               simulation_type,
                               fuzzy_multiplier,
                               map_prefix) {
  
  downscaled <- rast(paste0("model_outputs/paper_simulations/20221010_HILDA_COL_DS_1960_2019_Maxwell/", #"~/03_Downscaling/model_outputs/DS_methods_paper/20221010_HILDA_COL_DS_1960_2019/",
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

generateTransitions <- function(maps,
                                first_timesteps,
                                second_timesteps) {
  
  map_transitions <- maps[[1:length(first_timesteps)]]
  
  for (i in 1:length(second_timesteps)) {
    
    first_timestep <- first_timesteps[i]
    second_timestep <- second_timesteps[i]
    
    map_transitions[[i]] <- concats(maps[[first_timestep]],
                                    maps[[second_timestep]])
  }
  
  return(map_transitions)
}

getFoMMaps <- function(downscaled_transitions,
                       hilda_transitions,
                       second_timesteps) {
  
  no_change_cats <- c(0, 11, 22, 33, 44, 55, 66, 77, 88, 99)
  no_change_cats_names <- c("Urban_Urban",
                            "Cropland_Cropland",
                            "Pasture_Pasture",
                            "Unknown/Other forest_Unknown/Other forest",
                            "Evergreen, broad leaf forest_Evergreen, broad leaf forest",
                            "Deciduous, needle leaf forest_Deciduous, needle leaf forest",
                            "Deciduous, broad leaf forest_Deciduous, broad leaf forest",
                            "Grass/shrubland_Grass/shrubland",
                            "Other land_Other land",
                            "Water_Water")
  
  no_change_mat <- cbind(no_change_cats, 
                         rep(101, length(no_change_cats)))
  no_change_levels <- levels(downscaled_transitions)[[1]]
  no_change_levels <- no_change_levels[!no_change_levels$Land_cover_Land_cover %in% no_change_cats_names, ]
  no_change_levels <- rbind(no_change_levels,
                            c(101, "No change"))
  no_change_levels$ID <- as.numeric(no_change_levels$ID)
  no_change_levels_list <- vector("list",
                                  nlyr(downscaled_transitions))
  no_change_levels_list <- lapply(no_change_levels_list,
                                  FUN = function(x) x <- no_change_levels)
  
  downscaled_transitions <- classify(downscaled_transitions,
                                     no_change_mat)
  levels(downscaled_transitions) <- no_change_levels_list
  names(downscaled_transitions) <- second_timesteps
  
  hilda_transitions <- classify(hilda_transitions,
                                no_change_mat)
  levels(hilda_transitions) <- no_change_levels_list
  names(hilda_transitions) <- second_timesteps
  
  ## Calculate figure of merit index
  ### Get raster with figure of merit cells
  figure_of_merit <- ifel(hilda_transitions != 101 & downscaled_transitions == 101, # Cells that are changed in HILDA+ but don't change in downscaling
                          1,
                          ifel(hilda_transitions == downscaled_transitions & hilda_transitions != 101, # Cells that are changed in both and correctly predicted by downscaling
                               2,
                               ifel(hilda_transitions != 101 & hilda_transitions != downscaled_transitions, # Cells that are changed in both and incorrectly predicted by downscaling
                                    3,
                                    ifel(hilda_transitions == 101 & downscaled_transitions != 101, # Cells that are unchanged in HILDA+ but changed in downscaling
                                         4,
                                         5))))
  
  return(figure_of_merit)
}

getFoMDF <- function(hilda_colombia,
                     downscaled_colombia,
                     first_timesteps,
                     second_timesteps) {
  
  # Generate transitions
  hilda_transitions <- generateTransitions(maps = hilda_colombia,
                                           first_timesteps = first_timesteps,
                                           second_timesteps = second_timesteps)
  
  downscaled_transitions <- generateTransitions(maps = downscaled_colombia,
                                                first_timesteps = first_timesteps,
                                                second_timesteps = second_timesteps)
  
  # Get FoM maps
  figure_of_merit_maps <- getFoMMaps(downscaled_transitions = downscaled_transitions,
                                     hilda_transitions = hilda_transitions,
                                     second_timesteps = second_timesteps)
  
  # Calculate figure of merit
  figure_of_merit_freq <- freq(figure_of_merit_maps)
  
  figure_of_merit_df <- data.frame(layer = unique(figure_of_merit_freq$layer),
                                   year = second_timesteps,
                                   FoM = NA)
  
  for (i in figure_of_merit_df$layer) {
    
    tmp_vals <- figure_of_merit_freq[figure_of_merit_freq$layer == i, ]
    
    figure_of_merit_df$FoM[i] <- (tmp_vals$count[tmp_vals$value == 2] / sum(tmp_vals$count[tmp_vals$value >= 1 & tmp_vals$value <= 4])) * 100
  }
  
  return(figure_of_merit_df)
}

# Run the main function for each simulation
for (i in 1:10) {
  
  getDSCOLFoM(sim = i,
              simulation_type = simulation_type,
              fuzzy_multiplier = fuzzy_multiplier)
  
}
