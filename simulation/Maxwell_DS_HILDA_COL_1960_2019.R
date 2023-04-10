
library("landdownscaleR")

## Get user args
downscaling_args <- commandArgs(trailingOnly = TRUE)

simulation_type <- downscaling_args[1]
fuzzy_multiplier <- as.numeric(downscaling_args[2])

## Set up parameters
data_dir <- "~/03_Downscaling/data/20221006_HILDA_LUC_Colombia_1960_2019/"

hilda_colombia_LC_types <- c("11", "22", "33", "40", "42", "43", "44", "55", "66", "77")
hilda_colombia_LC_class_names <- paste0("LC",
                                        hilda_colombia_LC_types)

hilda_1960_colombia_file_name <- paste0(data_dir,
                                        "20221006_HILDA_Colombia_LULC_1960.tif")

first_timestep <- 1960:2018
second_timestep <- 1961:2019
hilda_LC_deltas_1960_2019_file_list <- as.list(paste0(data_dir,
                                                      "20221006_HILDA_Colombia_LUC_",
                                                      first_timestep,
                                                      "_",
                                                      second_timestep,
                                                      ".tif"))

final_LC_types <- matrix(data = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                         nrow = 10,
                         ncol = 10,
                         byrow = TRUE,
                         dimnames = list(hilda_colombia_LC_class_names,
                                         hilda_colombia_LC_class_names))

kernel_radius <- 1

# Print simulation parameters
print(paste0("Simulation type: ",
             simulation_type))
if (simulation_type == "fuzzy") {
  print(paste0("Multiplier: ",
               fuzzy_multiplier))
}

for (i in 1:10) {

  random_seed <- round(as.numeric(Sys.time()))
  print(paste0("Random seed: ",
               random_seed))
  
  if (simulation_type == "fuzzy") {
    output_file_prefix <- paste0("Sim",
                                 i,
                                 "_HILDA_DS_COL_",
                                 simulation_type,
                                 "_",
                                 fuzzy_multiplier)
  } else {
    output_file_prefix <- paste0("Sim",
                                 i,
                                 "_HILDA_DS_COL_",
                                 simulation_type)
  }

  ## Run downscaling
  downscaleLC(ref_map_file_name = hilda_1960_colombia_file_name,
              LC_deltas_file_list = hilda_LC_deltas_1960_2019_file_list,
              LC_deltas_classes = hilda_colombia_LC_class_names,
              ref_map_type = "discrete",
              ref_map_LC_classes = hilda_colombia_LC_class_names,
              cell_size_unit = "km",
              match_LC_classes = final_LC_types,
              kernel_radius = kernel_radius,
              simulation_type = simulation_type,
              fuzzy_multiplier = fuzzy_multiplier,
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = "~/03_Downscaling/model_outputs/DS_methods_paper/20221010_HILDA_COL_DS_1960_2019/")

}
