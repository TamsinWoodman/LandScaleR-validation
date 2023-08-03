
plotLandscapeMetric <- function(landscape_metric,
                                landscape_metrics_summary,
                                y_label) {
  
  lm_plot <- ggplot(data = landscape_metrics_summary[landscape_metrics_summary$metric == landscape_metric, ],
                    aes(x = layer,
                        y = mean_lm,
                        ymin = mean_lm - sd_lm,
                        ymax = mean_lm + sd_lm,
                        color = Allocation,
                        fill = Allocation)) +
    geom_line() +
    geom_ribbon(color = NA,
                alpha = 0.2) +
    scale_color_discrete(type = carto_pal(8, "Safe"), 
                         name = "",
                         labels = c("HILDA+",
                                    alloc_methods_display)) +
    scale_fill_discrete(type = carto_pal(8, "Safe"), 
                        name = "",
                        labels = c("HILDA+",
                                   alloc_methods_display)) +
    scale_x_continuous(name = "Year",
                       breaks = c(1, 21, 41, 61),
                       labels = c("1960", "1980", "2000", "2020")) +
    ylab(y_label) +
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.title.y = element_text(vjust = 3))
  
  return(lm_plot)
}

plotClassMetric <- function(class_metric,
                            class_metric_summary,
                            hilda_colombia_LC_display_classes,
                            LC_classes = NA,
                            y_label) {
  
  if (!is.na(LC_classes)) {
    class_metric_summary <- class_metric_summary[class_metric_summary$class %in% LC_classes, ]
    hilda_colombia_LC_display_classes <- hilda_colombia_LC_display_classes[names(hilda_colombia_LC_display_classes) %in% LC_classes]
  }
  
  cm_plot <- ggplot(data = class_metric_summary[class_metric_summary$metric == class_metric, ],
                    aes(x = layer,
                        y = mean_cm,
                        ymin = mean_cm - sd_cm,
                        ymax = mean_cm + sd_cm,
                        color = Allocation,
                        fill = Allocation)) +
    geom_line() +
    geom_ribbon(color = NA,
                alpha = 0.2) +
    facet_wrap(vars(class),
               labeller = as_labeller(hilda_colombia_LC_display_classes,
                                      default = label_wrap_gen(20)),
               nrow = 3, 
               ncol = 4) + 
    scale_color_discrete(type = carto_pal(8, "Safe"), 
                         name = "",
                         labels = c("HILDA+",
                                    alloc_methods_display)) +
    scale_fill_discrete(type = carto_pal(8, "Safe"), 
                        name = "",
                        labels = c("HILDA+",
                                   alloc_methods_display)) +
    scale_x_continuous(name = "Year",
                       breaks = c(1, 21, 41),
                       labels = c("1960", "1980", "2000")) +
    ylab(y_label) +
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.title.y = element_text(vjust = 3))
  
  return(cm_plot)
}

loadMapsFromOneSimulation <- function(sim_number, 
                                      allocation_method,
                                      hilda_colombia) {
  
  sim_maps <- rast(paste0(model_outputs_dir,
                          "Sim",
                          sim_number,
                          "_HILDA_DS_COL_",
                          allocation_method,
                          "_Discrete_Time",
                          1:59,
                          ".tif"))
  
  # Classify the maps
  hilda_colombia_LC_classes <- c("11", "22", "33", "40", "42", "43", "44", "55", "66", "77")
  classification_mat <- cbind(1:10, 
                              as.numeric(hilda_colombia_LC_classes))
  
  sim_maps <- classify(sim_maps,
                       classification_mat)
  
  # Add the reference map
  sim_maps <- c(crop(hilda_colombia[["X1960"]],
                     ext(sim_maps)),
                sim_maps)
  
  # Add levels
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
  
  hilda_levels <- data.frame(id = as.numeric(hilda_colombia_LC_classes),
                             Land_cover = hilda_colombia_LC_display_classes)
  hilda_levels_list <- vector("list",
                              nlyr(sim_maps))
  hilda_levels_list <- lapply(hilda_levels_list,
                              FUN = function(x) x <- hilda_levels)
  
  levels(sim_maps) <- hilda_levels_list
  names(sim_maps) <- paste0("X",
                            1960:2019)
  
  return(sim_maps)
}

plotHILDALUC <- function(hilda_LUC_colombia,
                         LC_classes = paste0("LC", c("11", "22", "33", "40", "42", "43", "44", "55", "66", "77")),
                         change_limits) {
  
  # Prepare data
  hilda_LUC_colombia_df <- as.data.frame(hilda_LUC_colombia,
                                         xy = TRUE)
  
  hilda_colombia_LC_classes <- c("11", "22", "33", "40", "42", "43", "44", "55", "66", "77")
  hilda_colombia_LUC_classes <- paste0("LC",
                                       hilda_colombia_LC_classes)
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
  hilda_LUC_colombia_df_long <- tidyr::pivot_longer(hilda_LUC_colombia_df,
                                                    cols = all_of(paste0("LC",
                                                                         hilda_colombia_LC_classes)),
                                                    names_to = "land_cover",
                                                    values_to = "land_cover_area")
  hilda_LUC_colombia_df_long$land_cover <- factor(hilda_LUC_colombia_df_long$land_cover,
                                                  levels = paste0("LC",
                                                                  hilda_colombia_LC_classes))
  
  hilda_colombia_LC_labeller <- hilda_colombia_LC_display_classes
  names(hilda_colombia_LC_labeller) <- hilda_colombia_LUC_classes
  
  # Get Colombia outline
  colombia_outline <- ne_countries(country = "colombia",
                                   scale = "large",
                                   returnclass = "sf")
  
  # Generate plot
  hilda_LUC_colombia_pl <- ggplot() +
    geom_sf(data = colombia_outline,
            colour = NA,
            fill = NA) +
    geom_tile(data = hilda_LUC_colombia_df_long[hilda_LUC_colombia_df_long$land_cover %in% LC_classes, ],
              aes(x = x,
                  y = y, 
                  fill = land_cover_area)) +
    facet_wrap(vars(land_cover),
               labeller = as_labeller(hilda_colombia_LC_labeller)) +
    scale_fill_distiller(palette = "RdBu", 
                         name = expression("Change in area (km" ^2* ")"),
                         limits = change_limits) +
    scale_x_continuous(breaks = c(-82, -74, -68)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.key.width = unit(0.3, "in"),
          legend.title = element_text(size = 10,
                                      hjust = 2,
                                      vjust = 1),
          strip.text = element_text(size = 8),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "darkgrey"))
  
  return(hilda_LUC_colombia_pl)
}

plotHILDALULC <- function(hilda_colombia_LULC,
                          LULC_palette) {
  
  hilda_colombia_df <- as.data.frame(hilda_colombia_LULC, 
                                     xy = TRUE)
  colnames(hilda_colombia_df) <- c("x",
                                   "y",
                                   "Land_cover")
  
  # Get Colombia outline
  colombia_outline <- ne_countries(country = "colombia",
                                   scale = "large",
                                   returnclass = "sf")
  
  # Generate plot
  hilda_colombia_pl <- ggplot() +
    geom_sf(data = colombia_outline,
            colour = NA,
            fill = NA) +
    geom_tile(data = hilda_colombia_df,
              aes(x = x,
                  y = y, 
                  fill = Land_cover)) +
    scale_fill_manual(values = LULC_palette) +
    guides(fill = guide_legend(nrow = 2)) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          plot.margin = margin(0, 0, 0, 0, unit = "pt"),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  return(hilda_colombia_pl)
}

makePolygon <- function(min_x,
                        max_x,
                        min_y,
                        max_y) {
  
  # Make landscape polygon
  landscape_x_coords <- c(min_x, min_x, max_x, max_x)
  landscape_y_coords <- c(min_y, max_y, max_y, min_y)
  
  landscape_coords <- cbind(id = 1, 
                            part = 1, 
                            x = landscape_x_coords, 
                            y = landscape_y_coords)
  
  landscape_polygon <- vect(landscape_coords, 
                            type = "polygons", 
                            crs = "EPSG:4326")
  
  return(landscape_polygon)
}

getCroppedMapDF <- function(min_x,
                            max_x,
                            min_y,
                            max_y, 
                            maps_list,
                            map_names) {
  
  # Make landscape polygon
  landscape_polygon <- makePolygon(min_x,
                                   max_x,
                                   min_y,
                                   max_y)
  
  # Crop the maps
  cropped_maps_list <- vector("list",
                              length(maps_list))
  
  for (i in 1:length(cropped_maps_list)) {
    
    cropped_maps_list[[i]] <- crop(maps_list[[i]],
                                   landscape_polygon)
  }
  
  # Make data frame of maps
  cropped_maps_df_list <- vector("list",
                                 length(cropped_maps_list))
  
  for (i in 1:length(cropped_maps_df_list)) {
    
    cropped_maps_df_list[[i]] <- terra::as.data.frame(cropped_maps_list[[i]],
                                                      xy = TRUE)
    cropped_maps_df_list[[i]]$Allocation <- map_names[i]
  }
  
  cropped_maps_df <- bind_rows(cropped_maps_df_list)
  
  return(cropped_maps_df)
}

plotCroppedMapDF <- function(cropped_map_df,
                             landscape_outline,
                             add_landscape_outline = TRUE,
                             LULC_palette,
                             x_breaks,
                             y_breaks) {
  
  if (add_landscape_outline == TRUE) {
    
    cropped_map_plot <- ggplot() + 
      geom_sf(data = landscape_outline,
              colour = NA,
              fill = NA) +
      geom_raster(data = cropped_map_df, 
                  aes(x = x,
                      y = y, 
                      fill = X2019)) + 
      facet_wrap(vars(Allocation)) + 
      scale_fill_manual(values = LULC_palette) + 
      scale_x_longitude(breaks = x_breaks) +
      scale_y_latitude(breaks = y_breaks) + 
      theme_bw() + 
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            legend.title = element_blank())
    
  } else {
    
    cropped_map_plot <- ggplot() + 
      geom_raster(data = cropped_map_df, 
                  aes(x = x,
                      y = y, 
                      fill = X2019)) + 
      facet_wrap(vars(Allocation)) + 
      scale_fill_manual(values = LULC_palette) + 
      scale_x_longitude(breaks = x_breaks) +
      scale_y_latitude(breaks = y_breaks) + 
      theme_bw() + 
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            legend.title = element_blank())
    
  }
  
  return(cropped_map_plot)
}
