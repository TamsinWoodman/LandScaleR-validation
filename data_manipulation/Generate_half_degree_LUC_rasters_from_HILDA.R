
library("rnaturalearth")
library("terra")
library("ggplot2")
library("landdownscaleR")

source("~/Projects/03_Disaggregate_land_use/LandScaleR-validation/data_manipulation/Generate_half_degree_LUC_rasters_from_HILDA_functions.R")

data_dir <- "~/Projects/03_Disaggregate_land_use/data/interim/20211006_hildap_vGLOB-1.0-f_netcdf/hildap_vGLOB-1.0-f_netcdf/"

##### Load hilda dataset
hilda <- rast(paste0(data_dir,
                     "hildaplus_vGLOB-1.0-f_states.nc"))
crs(hilda) <- "EPSG:4326"
NAflag(hilda) <- 0
names(hilda) <- paste0("X",
                       1899:2019)

### Note that the LUC datasets for Colombia were generated using terra version 1.6-17

##### Colombia
## Get outline of Colombia
colombia_outline <- ne_countries(country = "colombia",
                                 scale = "large",
                                 returnclass = "sf")
colombia_polygon <- vect(colombia_outline)
plot(colombia_polygon)

## Crop HILDA+ to Colombia outline
hilda_colombia <- crop(hilda, 
                       colombia_polygon,
                       snap = "out",
                       mask = TRUE,
                       touches = FALSE)
plot(hilda_colombia[["X2019"]])
plot(colombia_polygon,
     add = TRUE)

## Set up 0.5 degree grid of Colombia
ext(colombia_outline)

coarse_colombia_cells <- rast(xmin = -82, 
                              xmax = -66.5,
                              ymin = -4.5,
                              ymax = 14,
                              resolution = 0.5,
                              crs = "EPSG:4326")
values(coarse_colombia_cells) <- 1
coarse_colombia_cells <- crop(coarse_colombia_cells,
                              colombia_polygon,
                              snap = "in",
                              mask = TRUE,
                              touches = FALSE)

plot(coarse_colombia_cells,
     col = "lightpink")
plot(colombia_polygon,
     add = TRUE)

## Get polygons of HILDA cells assigned to each coarse cell
coarse_colombia_coords <- crds(coarse_colombia_cells)
coarse_colombia_cell_numbers <- cellFromXY(coarse_colombia_cells,
                                           coarse_colombia_coords)

hilda_colombia_polygons <- assignRefMapCells(hilda_colombia,
                                             LC_deltas_coords = coarse_colombia_coords, 
                                             LC_deltas_cell_numbers = coarse_colombia_cell_numbers)

plot(coarse_colombia_cells,
     col = "lightpink")
plot(hilda_colombia_polygons,
     add = TRUE)

## Loop through timesteps
second_timesteps <- 1961:2019
colombia_output_dir <- "~/Projects/03_Disaggregate_land_use/data/interim/20221006_HILDA_LUC_Colombia_1960_2019/"

hilda_colombia_LC_types <- c("11", "22", "33", "40", "42", "43", "44", "55", "66", "77")
hilda_colombia_LC_types_raster <- paste0("LC",
                                         hilda_colombia_LC_types)
hilda_colombia_LC_display_types <- c("Urban",
                                     "Cropland",
                                     "Pasture",
                                     "Unknown/Other forest",
                                     "Evergreen, broad leaf forest",
                                     "Deciduous, needle leaf forest",
                                     "Deciduous, broad leaf forest",
                                     "Grass/shrubland",
                                     "Other land",
                                     "Water")

for (i in second_timesteps) {
  
  print(i)
  
  ## First timestep
  agg_raster_1 <- aggregateLULC(LULC_raster = hilda_colombia,
                                timestep = paste0("X", i - 1),
                                LULC_polygons = hilda_colombia_polygons,
                                coarse_raster_coords = coarse_colombia_coords,
                                coarse_raster_cell_numbers = coarse_colombia_cell_numbers,
                                coarse_crs = crs(coarse_colombia_cells))
  
  # Handle the case where most timesteps don't contain the 43 class
  if (!"43" %in% names(agg_raster_1)) {
    empty_layer <- rast(agg_raster_1[["11"]],
                        vals = 0)
    empty_layer <- mask(empty_layer,
                        agg_raster_1[["11"]])
    agg_raster_1 <- c(agg_raster_1[[c("11", "22", "33", "40", "42")]],
                      empty_layer,
                      agg_raster_1[[c("44", "55", "66", "77", "cell_area")]])
  }
  
  names(agg_raster_1) <- c(hilda_colombia_LC_types_raster,
                           "cell_area")
  
  ## Second timestep
  agg_raster_2 <- aggregateLULC(LULC_raster = hilda_colombia,
                                timestep = paste0("X", i),
                                LULC_polygons = hilda_colombia_polygons,
                                coarse_raster_coords = coarse_colombia_coords,
                                coarse_raster_cell_numbers = coarse_colombia_cell_numbers,
                                coarse_crs = crs(coarse_colombia_cells))
  
  if (!"43" %in% names(agg_raster_2)) {
    empty_layer <- rast(agg_raster_2[["11"]],
                        vals = 0)
    empty_layer <- mask(empty_layer,
                        agg_raster_2[["11"]])
    agg_raster_2 <- c(agg_raster_2[[c("11", "22", "33", "40", "42")]],
                      empty_layer,
                      agg_raster_2[[c("44", "55", "66", "77", "cell_area")]])
  }
  
  names(agg_raster_2) <- c(hilda_colombia_LC_types_raster,
                           "cell_area")
  
  ## Calculate LUC
  agg_LUC <- agg_raster_2 - agg_raster_1
  agg_LUC <- c(agg_LUC[[1:length(hilda_colombia_LC_display_types)]],
               agg_raster_2[["cell_area"]])
  
  ## Write raster to file
  writeRaster(agg_LUC,
              filename = paste0(colombia_output_dir,
                                "20221006_HILDA_Colombia_LUC_",
                                i - 1, 
                                "_",
                                i,
                                ".tif"),
              overwrite = TRUE)
  
}

## Write reference maps for 1960 and 2009
writeRaster(hilda_colombia[["X1960"]],
            paste0(colombia_output_dir,
                   "20221006_HILDA_Colombia_LULC_1960.tif"))
writeRaster(hilda_colombia[["X2009"]],
            paste0(colombia_output_dir,
                   "20221006_HILDA_Colombia_LULC_2009.tif"))
