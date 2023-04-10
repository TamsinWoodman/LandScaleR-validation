
aggregateLULC <- function(LULC_raster,
                          timestep,
                          LULC_polygons,
                          coarse_raster_coords,
                          coarse_raster_cell_numbers,
                          coarse_crs) {
  
  LULC_raster_timestep <- LULC_raster[[timestep]]
  LULC_cat_raster_timestep <- terra::segregate(LULC_raster_timestep) * terra::cellSize(LULC_raster_timestep,
                                                                                       unit = "km")
  
  agg_raster_df <- data.frame(x = coarse_raster_coords[ , 1],
                              y = coarse_raster_coords[ , 2])
  
  for (i in 1:length(coarse_raster_cell_numbers)) {
    
    cell_number <- coarse_raster_cell_numbers[i]
    assigned_LULC_cells <- terra::crop(LULC_cat_raster_timestep,
                                       LULC_polygons[LULC_polygons$coarse_ID == cell_number],
                                       mask = TRUE,
                                       touches = TRUE)
    agg_LULC_areas <- unlist(global(assigned_LULC_cells,
                                    fun = "sum",
                                    na.rm = TRUE))
    agg_raster_df[i, names(LULC_cat_raster_timestep)] <- agg_LULC_areas
    agg_raster_df[i, "cell_area"] <- sum(agg_LULC_areas)
    
  }
  
  agg_LULC_raster <- rast(agg_raster_df,
                          type = "xyz",
                          crs = coarse_crs)
  
  return(agg_LULC_raster)
}
