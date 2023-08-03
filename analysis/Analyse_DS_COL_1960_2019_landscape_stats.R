
##### Set up #####

library("dplyr")
library("ggplot2")
library("viridis")
library("viridisLite")
library("terra")
library("cowplot")
library("egg")
library("metR")
library("rnaturalearth")
library("sf")
library("RColorBrewer")
library("landdownscaleR")
library("rcartocolor")

source("~/Projects/03_Disaggregate_land_use/LandScaleR-validation/analysis/Analyse_DS_COL_1960_2019_landscape_stats_functions.R")

analysis_dir <- "~/Projects/03_Disaggregate_land_use/analysis/DS_methods_paper/20221010_HILDA_COL_DS_1960_2019/"
input_dir <- "~/Projects/03_Disaggregate_land_use/data/interim/20221006_HILDA_LUC_Colombia_1960_2019/"
model_outputs_dir <- "~/Projects/03_Disaggregate_land_use/model_outputs/paper_simulations/20221010_HILDA_COL_DS_1960_2019_Maxwell/"
alloc_methods <- c("deterministic", 
                   "fuzzy_1",
                   "fuzzy_1.25",
                   "fuzzy_1.5",
                   "fuzzy_1.75",
                   "fuzzy_2",
                   "null_model")
alloc_methods_display <- c("Quasi-deterministic",
                           "Fuzzy 1.0",
                           "Fuzzy 1.25",
                           "Fuzzy 1.5",
                           "Fuzzy 1.75",
                           "Fuzzy 2.0",
                           "Random")
alloc_methods_and_hilda <- c("hilda",
                             alloc_methods)
alloc_methods_and_hilda_display <- c("HILDA+",
                                     alloc_methods_display)
simulations <- 1:10

hilda_colombia_LC_classes <- c("11", "22", "33", "40", "42", "43", "44", "55", "66", "77")
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
names(hilda_colombia_LC_display_classes) <- hilda_colombia_LC_classes

hilda_colombia <- rast("~/Projects/03_Disaggregate_land_use/data/interim/20221115_HILDA_COL_LULC_1960_2019.tif")

discrete_LC_palette <- c("#FEC5FF",
                         "#FFF18B",
                         "#FEC358",
                         "#EDF8E9",
                         "#BAE4B3",
                         "#74C476",
                         "#1D7139",
                         "#7A980A",
                         "#696969",
                         "#150575")

colombia_outline <- ne_countries(country = "colombia",
                                 scale = "large",
                                 returnclass = "sf")

##### Assigned reference map cells (Figure 2) #####

# Load maps of LULC in Colombia in 1960 and LULC change from 1960 to 1961
hilda_1960_colombia <- rast(paste0(input_dir,
                                   "20221006_HILDA_Colombia_LULC_1960.tif"))
hilda_LUC_colombia_1961 <- rast(paste0(input_dir,
                                       "20221006_HILDA_Colombia_LUC_1960_1961.tif"))

# Assign reference map cells
assigned_hilda_1960_colombia <- assignRefMapCells(hilda_1960_colombia, 
                                                  crds(hilda_LUC_colombia_1961),
                                                  cellFromXY(hilda_LUC_colombia_1961, 
                                                             crds(hilda_LUC_colombia_1961)))

# Generate a set of example grid cells in northern Colombia
example_coarse_numbers <- c(15, 16, 38, 39, 60, 61, 62)
example_coarse_assigned <- assigned_hilda_1960_colombia[assigned_hilda_1960_colombia$coarse_ID %in% example_coarse_numbers]
hilda_LUC_colombia_1961_example_assigned <- crop(hilda_LUC_colombia_1961,
                                                 example_coarse_assigned)

example_assigned_ref_cells_list <- vector("list",
                                          length(example_coarse_numbers))

for (i in 1:length(example_coarse_numbers)) {
  
  cell_number <- example_coarse_numbers[i]
  
  tmp_assigned <- crop(hilda_1960_colombia, 
                       assigned_hilda_1960_colombia[assigned_hilda_1960_colombia$coarse_ID == cell_number],
                       mask = TRUE,
                       snap = "near",
                       touches = FALSE)
  
  new_tmp_assigned <-  mask(rast(tmp_assigned,
                                 vals = cell_number),
                            tmp_assigned)
  
  example_assigned_ref_cells_list[[i]] <- new_tmp_assigned
}

example_assigned_ref_cells <- mosaic(sprc(example_assigned_ref_cells_list))

# Make figure showing the assigned, non-overlapping fine and coarse resolution 
# grid cells in the north of Colombia (Figure 2)
example_assigned_ref_cells_df <- as.data.frame(example_assigned_ref_cells, 
                                               xy = TRUE)
colnames(example_assigned_ref_cells_df) <- c("x", "y", "Cell_number")
example_assigned_ref_cells_df$Cell_number <- as.factor(example_assigned_ref_cells_df$Cell_number)

hilda_LUC_colombia_1961_example_assigned_outline <- st_as_sf(as.polygons(hilda_LUC_colombia_1961_example_assigned, dissolve = FALSE))

example_assigned_ref_cells_plot <- ggplot() + 
  geom_tile(data = example_assigned_ref_cells_df,
            aes(x = x, 
                y = y, 
                fill = Cell_number)) + 
  geom_sf(data = hilda_LUC_colombia_1961_example_assigned_outline,
          color = "black",
          fill = NA) +
  scale_fill_viridis(discrete = TRUE) + 
  scale_y_continuous(breaks = c(11, 11.5, 12, 12.5)) +
  scale_x_continuous(breaks = c(-73.5, -72.5, -71.5)) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_blank())
example_assigned_ref_cells_plot

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230125_Assigned_ref_map_cells.tiff",
#      height = 2.25,
#      width = 3.5,
#      units = "in",
#      res = 300)
# example_assigned_ref_cells_plot
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230125_Assigned_ref_map_cells_eps.eps",
#            height = 2.25,
#            width = 3.5)
# example_assigned_ref_cells_plot
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230125_Assigned_ref_map_cells_pdf.pdf",
#     height = 2.25,
#     width = 3.5)
# example_assigned_ref_cells_plot
# dev.off()

##### Reference and LULC change maps (Figure 3) #####

# Plot LULC in Colombia in 1960
levels(hilda_1960_colombia) <- data.frame(id = as.numeric(hilda_colombia_LC_classes),
                                          LC = hilda_colombia_LC_display_classes)
hilda_1960_colombia_df <- as.data.frame(hilda_1960_colombia, xy = TRUE)

hilda_1960_colombia_pl <- ggplot() +
  geom_sf(data = colombia_outline,
          colour = NA,
          fill = NA) +
  geom_tile(data = hilda_1960_colombia_df,
            aes(x = x,
                y = y, 
                fill = LC)) +
  scale_fill_manual(values = discrete_LC_palette) +
  scale_x_continuous(breaks = c(-82, -74, -68)) +
  guides(fill = guide_legend(nrow = 5)) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        plot.margin = margin(0, 0, 0, 0, unit = "pt"),
        axis.title = element_blank(),
        panel.background = element_blank())

hilda_1960_colombia_pl

# Plot LULC change in Colombia from 1960 to 1961
hilda_LUC_colombia_1961_pl <- plotHILDALUC(hilda_LUC_colombia_1961,
                                           LC_classes = c("LC22", "LC33", "LC42", "LC55"),
                                           change_limits = c(-50, 50))
hilda_LUC_colombia_1961_pl

# Make figure of LULC in Colombia in 1960 and LULC change from 1960 to 1961
# (Figure 3)
input_maps_plot <- ggarrange(hilda_1960_colombia_pl + theme(legend.text = element_text(size = 9)),
                             hilda_LUC_colombia_1961_pl,
                             ncol = 2,
                             widths = c(1, 0.9),
                             labels = c("a)", "b)"),
                             label.args = list(gp = grid::gpar(fontface = 2,
                                                               fontsize = 14)))

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230802_HILDA_Colombia_1960_1961_maps_tiff.tiff",
#      height = 5.5,
#      width = 7.4,
#      units = "in",
#      res = 300)
# input_maps_plot
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230802_HILDA_Colombia_1960_1961_maps_eps.eps",
#            height = 5.5,
#            width = 7.4)
# input_maps_plot
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230802_HILDA_Colombia_1960_1961_maps_pdf.pdf",
#     height = 5.5,
#     width = 7.4)
# input_maps_plot
# dev.off()

##### Similarity (Figures 4 and S2) #####
# Similarity is known as overall accuracy. It gives the number of matching grid
# cells between two LULC maps.

# Load similarity datasets
similarity_df <- data.frame()

for (i in 1:length(alloc_methods)) {
  
  similarity_file_names <- paste0(analysis_dir,
                                  "Sim",
                                  simulations,
                                  "_HILDA_DS_COL_",
                                  alloc_methods[i],
                                  "_similarity.csv")
  
  similarity_list <- lapply(similarity_file_names, 
                            read.csv)
  
  similarity_tmp_df <- bind_rows(similarity_list,
                                 .id = "simulation")
  
  similarity_tmp_df$allocation <- alloc_methods[i]
  
  similarity_df <- rbind(similarity_df,
                         similarity_tmp_df)
}

# Process similarity data frame to remove "X" in front of each year and set 
# column names
colnames(similarity_df) <- c("Simulation",
                             "Year",
                             "Similarity",
                             "Allocation")

similarity_df$Year <- sapply(strsplit(similarity_df$Year,
                                      "X"),
                             "[",
                             2)
similarity_df$Year <- as.numeric(similarity_df$Year)

# Summarise similarity data by year and allocation method
similarity_summary <- similarity_df %>%
  group_by(Allocation, Year) %>%
  summarise(mean_Similarity = mean(Similarity), 
            sd_Similarity = sd(Similarity))

# Generate plot of similarity over time and by allocation method
similarity_plot <- ggplot(data = similarity_summary,
                          aes(x = Year,
                              y = mean_Similarity,
                              ymin = mean_Similarity - sd_Similarity,
                              ymax = mean_Similarity + sd_Similarity,
                              color = Allocation,
                              fill = Allocation)) +
  geom_line(lwd = 0.3) +
  geom_ribbon(color = NA,
              alpha = 0.2) +
  scale_color_discrete(type = carto_pal(8, "Safe")[2:8],
                       name = "",
                       labels = alloc_methods_display) +
  scale_fill_discrete(type = carto_pal(8, "Safe")[2:8],
                      name = "",
                      labels = alloc_methods_display) +
  scale_x_continuous(name = "Year",
                     breaks = c(1960, 1980, 2000, 2020),
                     labels = c("1960", "1980", "2000", "2020")) +
  ylab(expression("Overall accuracy")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 15, unit = "pt"))
similarity_plot

# Check mean and standard deviation of similarity in 2019
similarity_summary[similarity_summary$Year == "2019", ]

# Find minimum and maximum standard deviation of similarity
# Year 1960 is excluded because downscaled maps are the same as the HILDA+ map
# in this year
min(similarity_summary[similarity_summary$Year != "1960", ]$sd_Similarity)
max(similarity_summary[similarity_summary$Year != "1960", ]$sd_Similarity)

# Calculate percentage increase in LULC classes in each year
hilda_LUC_colombia_file_names <- paste0(input_dir,
                                        "20221006_HILDA_Colombia_LUC_",
                                        1960:2018, 
                                        "_",
                                        1961:2019,
                                        ".tif")

hilda_colombia_LUC_classes <- paste0("LC",
                                     hilda_colombia_LC_classes)

hilda_LUC_area <- unlist(lapply(hilda_LUC_colombia_file_names,
                                FUN = function(x) {
                                  
                                  hilda_LUC_rast <- rast(x)
                                  increase_LC <- ifel(hilda_LUC_rast[[hilda_colombia_LUC_classes]] > 0, 
                                                      hilda_LUC_rast[[hilda_colombia_LUC_classes]],
                                                      0)
                                  total_increase_LC <- sum(global(increase_LC, 
                                                                  fun = "sum",
                                                                  na.rm = TRUE))
                                  total_area <- sum(global(hilda_LUC_rast[["cell_area"]],
                                                           fun = "sum",
                                                           na.rm =TRUE))
                                  
                                  percent_increase_LC <- (total_increase_LC / total_area) * 100
                                  
                                  return(percent_increase_LC)
                                }))

hilda_LUC_area_df <- data.frame(Year = 1961:2019,
                                Increase = hilda_LUC_area)
ggplot(hilda_LUC_area_df, 
       aes(x = Year, y = Increase)) + 
  geom_point() + 
  theme_bw() + 
  theme(panel.grid = element_blank())

# Combine similarity and increase in LULC data frames
similarity_and_LUC <- similarity_summary %>%
  left_join(hilda_LUC_area_df)

similarity_and_LUC$diff_Similarity <- c(0, 
                                        similarity_and_LUC$mean_Similarity[2:nrow(similarity_and_LUC)] - similarity_and_LUC$mean_Similarity[1:(nrow(similarity_and_LUC) - 1)])

similarity_and_LUC$diff_Similarity[similarity_and_LUC$Year == 1960] <- 0

# Plot similarity predicted by LULC change
similarity_and_LUC_plot <- ggplot(similarity_and_LUC,
                                  aes(x = Increase,
                                      y = diff_Similarity, 
                                      col = Allocation)) + 
  geom_point(alpha = 0.5,
             shape = 16,
             size = 1.1) +
  scale_color_discrete(type = carto_pal(8, "Safe")[2:8], 
                       name = "",
                       labels = alloc_methods_display) +
  geom_hline(yintercept = 0,
             colour = "black",
             linetype = "dashed") +
  xlab("Percentage landscape change") +
  ylab(" Change in overall accuracy") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 15, unit = "pt"))
similarity_and_LUC_plot

# Create figure showing similarity change (Figure 4)
similarity_combined_plot <- ggarrange(similarity_plot + theme(legend.text = element_text(size = 11),
                                                              axis.title = element_text(size = 12)),
                                      similarity_and_LUC_plot + theme(legend.text = element_text(size = 11),
                                                                      axis.title = element_text(size = 12)),
                                      nrow = 2,
                                      labels = c("a)", "b)"),
                                      label.args = list(gp = grid::gpar(fontface = 2,
                                                                        fontsize = 14)))

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_similarity_tiff.tiff",
#      height = 6,
#      width = 5.5,
#      units = "in",
#      res = 300)
# similarity_combined_plot
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_similarity_eps.eps",
#      height = 6,
#      width = 5.5)
# similarity_combined_plot
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_similarity_pdf.pdf",
#      height = 6,
#      width = 5.5)
# similarity_combined_plot
# dev.off()

# Show difference in similarity for the two years with the largest total LULC
# change allocated
similarity_and_LUC[similarity_and_LUC$Year %in% c(2001, 2013), ]

# Find the years when similarity increased
print(similarity_and_LUC[similarity_and_LUC$diff_Similarity > 0, ], n = 21)

# Plot LULC in 2000, 2001, 2012, and 2013 to show adding/removing 
# grass/shrubland causes changes in similarity
hilda_2000_colombia_pl <- plotHILDALULC(hilda_colombia[["X2000"]],
                                        LULC_palette = discrete_LC_palette)
hilda_2001_colombia_pl <- plotHILDALULC(hilda_colombia[["X2001"]],
                                        LULC_palette = discrete_LC_palette)
hilda_2012_colombia_pl <- plotHILDALULC(hilda_colombia[["X2012"]],
                                        LULC_palette = discrete_LC_palette[-6])
hilda_2013_colombia_pl <- plotHILDALULC(hilda_colombia[["X2013"]],
                                        LULC_palette = discrete_LC_palette[-6])
hilda_2000_colombia_pl
hilda_2001_colombia_pl
hilda_2012_colombia_pl
hilda_2013_colombia_pl

# Make figure of grass/shrubland changes in 2000, 2001, 2012, and 2013 
# (Figure S2)
grass_shrubland_change_pl_tmp <- ggarrange(hilda_2000_colombia_pl + theme(legend.position = "none"),
                                           hilda_2001_colombia_pl + theme(legend.position = "none"),
                                           hilda_2012_colombia_pl + theme(legend.position = "none"),
                                           hilda_2013_colombia_pl + theme(legend.position = "none"),
                                           nrow = 2,
                                           ncol = 2,
                                           labels = c("a)", "b)", "c)", "d)"),
                                           label.args = list(gp = grid::gpar(fontface = 2,
                                                                             fontsize = 14)))
grass_shrubland_change_pl <- plot_grid(grass_shrubland_change_pl_tmp,
                                       get_legend(hilda_2000_colombia_pl + guides(fill = guide_legend(nrow = 3))),
                                       nrow = 2,
                                       rel_heights = c(1, 0.1))

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230130_HILDA_Grass_and_shrubland_change_tiff.tiff",
#      height = 7,
#      width = 5.5,
#      units = "in",
#      res = 300)
# grass_shrubland_change_pl
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230130_HILDA_Grass_and_shrubland_change_eps.eps",
#            height = 7,
#            width = 5.5)
# grass_shrubland_change_pl
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230130_HILDA_Grass_and_shrubland_change_pdf.pdf",
#     height = 7,
#     width = 5.5)
# grass_shrubland_change_pl
# dev.off()

# Check how much grass/shrubland increased in HILDA+ from 2000 to 2001
global(hilda_LUC_colombia_2001[["LC55"]],
       fun = "sum", 
       na.rm = TRUE)

##### Landscape-level pattern metrics (Figure 5) #####

# Load landscape-level metrics
hilda_landscape_metrics <- read.csv(paste0(analysis_dir,
                                           "HILDA_COL_1960_2019_landscape_metrics.csv"))

landscape_metrics_df <- data.frame()

for (i in 1:length(alloc_methods)) {
  
  landscape_metrics_file_names <- paste0(analysis_dir,
                                         "Sim",
                                         simulations,
                                         "_HILDA_DS_COL_",
                                         alloc_methods[i],
                                         "_landscape_metrics.csv")
  
  landscape_metrics_list <- lapply(landscape_metrics_file_names, 
                                   read.csv)
  
  landscape_metrics_tmp_df <- bind_rows(landscape_metrics_list,
                                        .id = "Simulation")
  
  landscape_metrics_tmp_df$Allocation <- alloc_methods[i]
  
  landscape_metrics_df <- rbind(landscape_metrics_df,
                                landscape_metrics_tmp_df)
}

# Combine HILDA+ and downscaled landscape pattern metrics
hilda_landscape_metrics$Simulation <- 1
hilda_landscape_metrics$Allocation <- "hilda"

landscape_metrics_df <- rbind(hilda_landscape_metrics, 
                              landscape_metrics_df)
landscape_metrics_df$Allocation <- factor(landscape_metrics_df$Allocation,
                                          levels = alloc_methods_and_hilda)

# Summarise landscape pattern metrics by allocation method and year
landscape_metrics_summary <- landscape_metrics_df %>%
  group_by(Allocation, metric, layer) %>%
  summarise(mean_lm = mean(value), 
            sd_lm = sd(value))

# Print all landscape pattern metrics in 2019
print(landscape_metrics_summary[landscape_metrics_summary$layer == 59, ], n = 80)

# Make plot of mean patch area, number of patches, Aggregation Index, and edge
# density (Figure 5)
area_mn_l_plot <- plotLandscapeMetric("area_mn",
                                      landscape_metrics_summary,
                                      "Mean patch area (ha)")
area_mn_l_plot
np_l_plot <- plotLandscapeMetric("np",
                                 landscape_metrics_summary,
                                 "Number of patches")
np_l_plot
ai_l_plot <- plotLandscapeMetric("ai",
                                 landscape_metrics_summary,
                                 "Aggregation Index")
ai_l_plot
ed_l_plot <- plotLandscapeMetric("ed",
                                 landscape_metrics_summary,
                                 expression(paste("Edge density (m ha" ^-1* ")")))
ed_l_plot

np_l_plot_legend_bottom <- np_l_plot + theme(legend.position = "bottom",
                                             legend.text = element_text(size = 11))
landscape_plot_tmp <- ggarrange(area_mn_l_plot + theme(legend.position = "none",
                                                       axis.text.x = element_blank(),
                                                       axis.title.x = element_blank(),
                                                       axis.title.y = element_text(size = 12)),
                                  np_l_plot + theme(legend.position = "none",
                                                    axis.text.x = element_blank(),
                                                    axis.title.x = element_blank(),
                                                    axis.title.y = element_text(size = 12)),
                                  ai_l_plot + theme(legend.position = "none",
                                                    axis.title.y = element_text(size = 12,
                                                                                vjust = 9.5),
                                                    axis.title.x = element_text(size = 12)),
                                  ed_l_plot + theme(legend.position = "none",
                                                    axis.title.y = element_text(size = 12,
                                                                                vjust = 11),
                                                    axis.title.x = element_text(size = 12)),
                                  nrow = 2,
                                  ncol = 2,
                                  labels = c("a)", "b)", "c)", "d)"),
                                  label.args = list(gp = grid::gpar(fontface = 2,
                                                                    fontsize = 14)))
landscape_plot <- plot_grid(landscape_plot_tmp,
                            get_legend(np_l_plot_legend_bottom),
                            nrow = 2,
                            rel_heights = c(1, 0.2))

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_Landscape_level_stats_tiff.tiff",
#      height = 6,
#      width = 7.4,
#      units = "in",
#      res = 300)
# landscape_plot
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_Landscape_level_stats_eps.eps",
#            height = 6,
#            width = 7.4)
# landscape_plot
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_Landscape_level_stats_pdf.pdf",
#     height = 6,
#     width = 7.4)
# landscape_plot
# dev.off()

# Check minimum and maximum standard deviation for landscape pattern metrics in 
# Figure 5
print("Mean patch area:")
min(landscape_metrics_summary$sd_lm[landscape_metrics_summary$metric == "area_mn"], na.rm = TRUE)
max(landscape_metrics_summary$sd_lm[landscape_metrics_summary$metric == "area_mn"], na.rm = TRUE)
print("Number of patches:")
min(landscape_metrics_summary$sd_lm[landscape_metrics_summary$metric == "np"], na.rm = TRUE)
max(landscape_metrics_summary$sd_lm[landscape_metrics_summary$metric == "np"], na.rm = TRUE)
print("Aggregation Index:")
min(landscape_metrics_summary$sd_lm[landscape_metrics_summary$metric == "ai"], na.rm = TRUE)
max(landscape_metrics_summary$sd_lm[landscape_metrics_summary$metric == "ai"], na.rm = TRUE)
print("Edge density:")
min(landscape_metrics_summary$sd_lm[landscape_metrics_summary$metric == "ed"], na.rm = TRUE)
max(landscape_metrics_summary$sd_lm[landscape_metrics_summary$metric == "ed"], na.rm = TRUE)

##### Class-level landscape pattern metrics (Figures 6 and S1) #####

# Load class-level pattern metrics
hilda_class_metrics <- read.csv(paste0(analysis_dir,
                                       "HILDA_COL_1960_2019_class_metrics.csv"))

class_metrics_df <- data.frame()

for (i in 1:length(alloc_methods)) {
  
  class_metrics_file_names <- paste0(analysis_dir,
                                     "Sim",
                                     simulations,
                                     "_HILDA_DS_COL_",
                                     alloc_methods[i],
                                     "_class_metrics.csv")
  
  class_metrics_list <- lapply(class_metrics_file_names, 
                               read.delim,
                               sep = ",")
  
  class_metrics_tmp_df <- bind_rows(class_metrics_list,
                                    .id = "Simulation")
  
  class_metrics_tmp_df$Allocation <- alloc_methods[i]
  
  class_metrics_df <- rbind(class_metrics_df,
                            class_metrics_tmp_df)
}

colnames(class_metrics_df) <- c("Simulation",
                                "layer",
                                "level",
                                "class",
                                "id",
                                "metric",
                                "value",
                                "Allocation")

# Combine HILDA+ and downscaled class-level pattern metrics
hilda_class_metrics$Simulation <- 1
hilda_class_metrics$Allocation <- "hilda"

class_metrics_df <- rbind(hilda_class_metrics, 
                          class_metrics_df)
class_metrics_df$Allocation <- factor(class_metrics_df$Allocation,
                                      levels = alloc_methods_and_hilda)

# Summarise class-level pattern metrics by allocation method and year
class_metrics_summary <- class_metrics_df %>%
  group_by(class, Allocation, metric, layer) %>%
  summarise(mean_cm = mean(value), 
            sd_cm = sd(value))

# Create figure showing the proportion of Colombia covered by each LULC class 
# (Figure S1)
pland_c_plot <- plotClassMetric("pland",
                                class_metrics_summary,
                                hilda_colombia_LC_display_classes,
                                y_label = "Percentage of landscape")
pland_c_free_plot <- pland_c_plot + facet_wrap(vars(class),
                                               labeller = as_labeller(hilda_colombia_LC_display_classes,
                                                                      default = label_wrap_gen(20)),
                                               scales = "free_y",
                                               nrow = 3, 
                                               ncol = 4)
pland_c_free_plot

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_Downscaled_land_percentage_tiff.tiff",
#      height = 5.5,
#      width = 7.4,
#      units = "in",
#      res = 300)
# pland_c_free_plot + theme(legend.position = "bottom",
#                      strip.text = element_text(size = 9))
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_Downscaled_land_percentage_eps.eps",
#            height = 5.5,
#            width = 7.4)
# pland_c_free_plot + theme(legend.position = "bottom",
#                      strip.text = element_text(size = 9))
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_Downscaled_land_percentage_pdf.pdf",
#     height = 5.5,
#     width = 7.4)
# pland_c_free_plot + theme(legend.position = "bottom",
#                      strip.text = element_text(size = 9))
# dev.off()

# Check minimum and maximum standard deviations for the percentage of Colombia 
# covered by each LULC class
min(class_metrics_summary$sd_cm[class_metrics_summary$metric == "pland"], na.rm = TRUE)
max(class_metrics_summary$sd_cm[class_metrics_summary$metric == "pland"], na.rm = TRUE)

# Check how much each LULC class decreased from 1960 to 2019 in the HILDA+ 
# dataset
class_metrics_summary[class_metrics_summary$metric == "pland" & 
                        class_metrics_summary$Allocation == "hilda" & 
                        class_metrics_summary$layer %in% c(1, 60), ]

# Make a figure with four class-level landscape pattern metrics for the three 
# LULC classes that were largest in the HILDA+ Colombia dataset in 2019
# Aggregation Index (Figure 6)
ai_c_pasture_plot <- plotClassMetric("ai",
                                     class_metrics_summary,
                                     LC_classes = "33",
                                     hilda_colombia_LC_display_classes,
                                     "Aggregation Index")
ai_c_forest_plot <- plotClassMetric("ai",
                                    class_metrics_summary,
                                    LC_classes = "42",
                                    hilda_colombia_LC_display_classes,
                                    "Aggregation Index")
ai_c_grassland_plot <- plotClassMetric("ai",
                                       class_metrics_summary,
                                       LC_classes = "55",
                                       hilda_colombia_LC_display_classes,
                                       "Aggregation Index")

# Mean area
area_mn_c_pasture_plot <- plotClassMetric("area_mn",
                                          class_metrics_summary,
                                          LC_classes = "33",
                                          hilda_colombia_LC_display_classes,
                                          "    Mean patch area (ha)")
area_mn_c_forest_plot <- plotClassMetric("area_mn",
                                         class_metrics_summary,
                                         LC_classes = "42",
                                         hilda_colombia_LC_display_classes,
                                         "Mean patch area (ha)")
area_mn_c_grassland_plot <- plotClassMetric("area_mn",
                                            class_metrics_summary,
                                            LC_classes = "55",
                                            hilda_colombia_LC_display_classes,
                                            "Mean patch area (ha)")

# Number of patches
np_c_pasture_plot <- plotClassMetric("np",
                                     class_metrics_summary,
                                     LC_classes = "33",
                                     hilda_colombia_LC_display_classes,
                                     "Number of patches")
np_c_forest_plot <- plotClassMetric("np",
                                    class_metrics_summary,
                                    LC_classes = "42",
                                    hilda_colombia_LC_display_classes,
                                    "Number of patches")
np_c_grassland_plot <- plotClassMetric("np",
                                       class_metrics_summary,
                                       LC_classes = "55",
                                       hilda_colombia_LC_display_classes,
                                       "Number of patches")

# Edge density
ed_c_pasture_plot <- plotClassMetric("ed",
                                     class_metrics_summary,
                                     LC_classes = "33",
                                     hilda_colombia_LC_display_classes,
                                     expression(paste("Edge density (m ha" ^-1* ")")))
ed_c_forest_plot <- plotClassMetric("ed",
                                    class_metrics_summary,
                                    LC_classes = "42",
                                    hilda_colombia_LC_display_classes,
                                    expression(paste("Edge density (m ha" ^-1* ")")))
ed_c_grassland_plot <- plotClassMetric("ed",
                                       class_metrics_summary,
                                       LC_classes = "55",
                                       hilda_colombia_LC_display_classes,
                                       expression(paste("Edge density (m ha" ^-1* ")")))

# Put plots together
class_metrics_plot <- ggarrange(area_mn_c_pasture_plot + theme(legend.position = "none",
                                                               axis.title.x = element_blank(),
                                                               axis.text.x = element_blank(),
                                                               axis.title.y = element_text(size = 10,
                                                                                           vjust = 0.7),
                                                               strip.text = element_text(size = 9,
                                                                                         margin = margin(t = 9.5, b = 9))),
                                area_mn_c_forest_plot + theme(legend.position = "none",
                                                              axis.title.x = element_blank(),
                                                              axis.text.x = element_blank(),
                                                              axis.title.y = element_blank()),
                                area_mn_c_grassland_plot + theme(legend.position = "none",
                                                                 axis.title.x = element_blank(),
                                                                 axis.text.x = element_blank(),
                                                                 axis.title.y = element_blank(),
                                                                 strip.text = element_text(size = 9,
                                                                                           margin = margin(t = 9.5, b = 9))),
                                np_c_pasture_plot + theme(legend.position = "none",
                                                          axis.title.x = element_blank(),
                                                          axis.text.x = element_blank(),
                                                          axis.title.y = element_text(size = 10),
                                                          strip.text = element_text(size = 9,
                                                                                    margin = margin(t = 9.5, b = 9))),
                                np_c_forest_plot + theme(legend.position = "none",
                                                         axis.title.x = element_blank(),
                                                         axis.text.x = element_blank(),
                                                         axis.title.y = element_blank()),
                                np_c_grassland_plot + theme(legend.position = "none",
                                                            axis.title.x = element_blank(),
                                                            axis.text.x = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            strip.text = element_text(size = 9,
                                                                                      margin = margin(t = 9.5, b = 9))),
                                ai_c_pasture_plot + theme(legend.position = "none",
                                                          axis.title.x = element_blank(),
                                                          axis.text.x = element_blank(),
                                                          axis.title.y = element_text(size = 10,
                                                                                      vjust = 7.7),
                                                          strip.text = element_text(size = 9,
                                                                                    margin = margin(t = 9.5, b = 9))),
                                ai_c_forest_plot + theme(legend.position = "none",
                                                         axis.title.x = element_blank(),
                                                         axis.text.x = element_blank(),
                                                         axis.title.y = element_blank()),
                                ai_c_grassland_plot + theme(legend.position = "none",
                                                            axis.title.x = element_blank(),
                                                            axis.text.x = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            strip.text = element_text(size = 9,
                                                                                      margin = margin(t = 9.5, b = 9))),
                                ed_c_pasture_plot + theme(legend.position = "none",
                                                          axis.title.y = element_text(size = 10,
                                                                                      vjust = 5.9),
                                                          axis.title.x = element_text(size = 10),
                                                          strip.text = element_text(size = 9,
                                                                                    margin = margin(t = 9.5, b = 9))),
                                ed_c_forest_plot + theme(legend.position = "none",
                                                         axis.title.y = element_blank(),
                                                         axis.title.x = element_text(size = 10)),
                                ed_c_grassland_plot + theme(legend.position = "none",
                                                            axis.title.y = element_blank(),
                                                            axis.title.x = element_text(size = 10),
                                                            strip.text = element_text(size = 9,
                                                                                      margin = margin(t = 9.5, b = 9))),
                                ncol = 3,
                                labels = c("a)", "", "",
                                           "b)", "", "",
                                           "c)", "", "",
                                           "d)", "", ""),
                                label.args = list(gp = grid::gpar(fontface = 2,
                                                                  fontsize = 14)))
class_metrics_plot_with_legend <- plot_grid(class_metrics_plot,
                                            get_legend(area_mn_c_pasture_plot),
                                            ncol = 2,
                                            rel_widths = c(1, 0.3))

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_Colombia_class_metrics_tiff.tiff",
#      height = 7,
#      width = 7.4,
#      units = "in",
#      res = 300)
# class_metrics_plot_with_legend
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_Colombia_class_metrics_eps.eps",
#            height = 7,
#            width = 7.4)
# class_metrics_plot_with_legend
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230803_HILDA_Colombia_class_metrics__pdf.pdf",
#     height = 7,
#     width = 7.4)
# class_metrics_plot_with_legend
# dev.off()

# Check minimum and maximum standard deviation for class-level landscape pattern
# metrics in Figure 6
print("Mean patch area:")
min(class_metrics_summary$sd_cm[class_metrics_summary$metric == "area_mn"], na.rm = TRUE)
max(class_metrics_summary$sd_cm[class_metrics_summary$metric == "area_mn"], na.rm = TRUE)
print("Number of patches:")
min(class_metrics_summary$sd_cm[class_metrics_summary$metric == "np"], na.rm = TRUE)
max(class_metrics_summary$sd_cm[class_metrics_summary$metric == "np"], na.rm = TRUE)
print("Aggregation Index:")
min(class_metrics_summary$sd_cm[class_metrics_summary$metric == "ai"], na.rm = TRUE)
max(class_metrics_summary$sd_cm[class_metrics_summary$metric == "ai"], na.rm = TRUE)
print("Edge density:")
min(class_metrics_summary$sd_cm[class_metrics_summary$metric == "ed"], na.rm = TRUE)
max(class_metrics_summary$sd_cm[class_metrics_summary$metric == "ed"], na.rm = TRUE)

# View results for class-level landscape pattern metrics in Figure 6 in 2019
print(class_metrics_summary[class_metrics_summary$layer == 59 & class_metrics_summary$metric %in% c("ai", "np", "area_mn", "ed", "pland"), ], n = 360)

##### Cropped maps of landscape patterns (Figure 7) #####

# Load 2019 maps for HILDA+ and simulation 1 of quasi-deterministic, fuzzy 2.0, 
# and random simulations
ds_det_2019 <- rast(paste0(model_outputs_dir,
                           "Sim1_HILDA_DS_COL_deterministic_Discrete_Time59.tif"))
ds_fuzzy2_2019 <- rast(paste0(model_outputs_dir,
                              "Sim1_HILDA_DS_COL_fuzzy_2_Discrete_Time59.tif"))
ds_random_2019 <- rast(paste0(model_outputs_dir,
                              "Sim1_HILDA_DS_COL_null_model_Discrete_Time59.tif"))
hilda_2019 <- hilda_colombia[["X2019"]]

# Classify downscaled maps
classification_mat <- cbind(1:10, 
                            as.numeric(hilda_colombia_LC_classes))
ds_det_2019 <- classify(ds_det_2019,
                        classification_mat,
                        others = NA)
ds_fuzzy2_2019 <- classify(ds_fuzzy2_2019,
                           classification_mat,
                           others = NA)
ds_random_2019 <- classify(ds_random_2019,
                           classification_mat,
                           others = NA)

# Set levels of downscaled maps
hilda_2019_levels <- data.frame(id = as.numeric(hilda_colombia_LC_classes),
                                X2019 = hilda_colombia_LC_display_classes)
levels(ds_det_2019) <- hilda_2019_levels
levels(ds_fuzzy2_2019) <- hilda_2019_levels
levels(ds_random_2019) <- hilda_2019_levels

# Crop maps and convert to data frame for plotting
# The cropped region is focused around Bogota, the capital city of Colombia
ds_LULC_method_comparison_df <- getCroppedMapDF(-75, -72.5, 4, 6.5,
                                                list(hilda_2019,
                                                     ds_det_2019,
                                                     ds_fuzzy2_2019,
                                                     ds_random_2019),
                                                c("HILDA+",
                                                  "Quasi-deterministic",
                                                  "Fuzzy 2.0",
                                                  "Random"))
ds_LULC_method_comparison_df$Allocation <- factor(ds_LULC_method_comparison_df$Allocation,
                                                  levels = c("HILDA+",
                                                             "Quasi-deterministic",
                                                             "Fuzzy 2.0",
                                                             "Random"))

# Plot the cropped maps
bogota_landscape_outline <- st_as_sf(makePolygon(-75, -72.5, 4, 6.5))

cropped_map_plot <- plotCroppedMapDF(ds_LULC_method_comparison_df,
                                     bogota_landscape_outline,
                                     LULC_palette = discrete_LC_palette[-6],
                                     x_breaks = c(-75, -74, -73),
                                     y_breaks = c(4, 5, 6))
cropped_map_plot

# Plot HILDA+ Colombia data in 2019 with a rectangle to show the cropped area
hilda_2019_colombia_df <- as.data.frame(hilda_2019, 
                                        xy = TRUE)

hilda_2019_colombia_pl <- ggplot() +
  geom_sf(data = colombia_outline,
          colour = NA,
          fill = NA) +
  geom_tile(data = hilda_2019_colombia_df,
            aes(x = x,
                y = y, 
                fill = X2019)) +
  scale_fill_manual(values = discrete_LC_palette[-6]) +
  scale_x_continuous(breaks = c(-82, -74, -68)) +
  geom_sf(data = bogota_landscape_outline,
          colour = "black",
          fill = NA) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        plot.margin = margin(0, 0, 0, 0, unit = "pt"),
        axis.title = element_blank(),
        panel.background = element_blank())

hilda_2019_colombia_pl

# Create figure showing HILDA+ LULC in Colombia in 2019 with the cropped maps
# (Figure 7)
cropped_maps_2019 <- ggarrange(hilda_2019_colombia_pl + theme(legend.position = "none"), 
                               cropped_map_plot + theme(legend.position = "none"), 
                               ncol = 2,
                               labels = c("a)", "b)"),
                               label.args = list(gp = grid::gpar(fontface = 2,
                                                                 fontsize = 14)))
cropped_maps_2019_with_legend <- plot_grid(cropped_maps_2019,
                                           get_legend(hilda_2019_colombia_pl),
                                           nrow = 2,
                                           rel_heights = c(1, 0.1))

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230802_HILDA_Colombia_2019_cropped_maps_tiff.tiff",
#      height = 5,
#      width = 7.4,
#      units = "in",
#      res = 300)
# cropped_maps_2019_with_legend
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230802_HILDA_Colombia_2019_cropped_maps_eps.eps",
#            height = 5,
#            width = 7.4)
# cropped_maps_2019_with_legend
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230802_HILDA_Colombia_2019_cropped_maps_pdf.pdf",
#     height = 5,
#     width = 7.4)
# cropped_maps_2019_with_legend
# dev.off()

##### Variation in one method of LULC allocation (Figure S3) #####

# Load maps for the fuzzy 2.0 LULC allocation method
ds_fuzzy2_sim1 <- loadMapsFromOneSimulation(1,
                                            "fuzzy_2",
                                            hilda_colombia)
ds_fuzzy2_sim2 <- loadMapsFromOneSimulation(2,
                                            "fuzzy_2",
                                            hilda_colombia)
ds_fuzzy2_sim3 <- loadMapsFromOneSimulation(3,
                                            "fuzzy_2",
                                            hilda_colombia)
ds_fuzzy2_sim4 <- loadMapsFromOneSimulation(4,
                                            "fuzzy_2",
                                            hilda_colombia)

# Generate cropped maps for two locations
bogota_cropped_ds_fuzzy2_maps_df <- getCroppedMapDF(-75, -72.5, 4, 6.5,
                                                    list(ds_fuzzy2_sim1[["X2019"]],
                                                         ds_fuzzy2_sim2[["X2019"]],
                                                         ds_fuzzy2_sim3[["X2019"]],
                                                         ds_fuzzy2_sim4[["X2019"]]),
                                                    c("1", "2", "3", "4"))
amazon_cropped_ds_fuzzy2_maps_df <- getCroppedMapDF(-71.5, -69, 2.0, 4.5,
                                                    list(ds_fuzzy2_sim1[["X2019"]],
                                                         ds_fuzzy2_sim2[["X2019"]],
                                                         ds_fuzzy2_sim3[["X2019"]],
                                                         ds_fuzzy2_sim4[["X2019"]]),
                                                    c("1", "2", "3", "4"))

# Plot the cropped maps
bogota_ds_fuzzy2_cropped_map_plot <- plotCroppedMapDF(bogota_cropped_ds_fuzzy2_maps_df,
                                                      bogota_landscape_outline,
                                                      add_landscape_outline = FALSE,
                                                      LULC_palette = discrete_LC_palette[-6],
                                                      x_breaks = c(-75, -74, -73),
                                                      y_breaks = c(4, 5, 6)) +
  theme(legend.position = "bottom")
amazon_ds_fuzzy2_cropped_map_plot <- plotCroppedMapDF(amazon_cropped_ds_fuzzy2_maps_df,
                                                      amazon_landscape_outline,
                                                      add_landscape_outline = FALSE,
                                                      LULC_palette = discrete_LC_palette[-6],
                                                      x_breaks = c(-70.5, -69.5),
                                                      y_breaks = c(2, 3, 4)) +
  theme(legend.position = "bottom")

bogota_ds_fuzzy2_cropped_map_plot
amazon_ds_fuzzy2_cropped_map_plot

# Save cropped maps as a figure to show variation within four repeated 
# simulations of the fuzzy 2.0 allocation method (Figure S3)
ds_fuzzy2_cropped_maps_2019 <- ggarrange(bogota_ds_fuzzy2_cropped_map_plot + theme(legend.position = "none"), 
                                         amazon_ds_fuzzy2_cropped_map_plot + theme(legend.position = "none"), 
                                         ncol = 2,
                                         labels = c("a)", "b)"),
                                         label.args = list(gp = grid::gpar(fontface = 2,
                                                                           fontsize = 14)))
ds_fuzzy2_cropped_maps_2019_with_legend <- plot_grid(ds_fuzzy2_cropped_maps_2019,
                                                     get_legend(bogota_ds_fuzzy2_cropped_map_plot),
                                                     nrow = 2,
                                                     rel_heights = c(1, 0.1))

# tiff("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230130_HILDA_Colombia_2019_ds_fuzzy2_maps_tiff.tiff",
#      height = 5,
#      width = 7.4,
#      units = "in",
#      res = 300)
# ds_fuzzy2_cropped_maps_2019_with_legend
# dev.off()
# 
# setEPS()
# postscript("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230130_HILDA_Colombia_2019_ds_fuzzy2_maps_eps.eps",
#            height = 5,
#            width = 7.4)
# ds_fuzzy2_cropped_maps_2019_with_legend
# dev.off()
# 
# pdf("~/Projects/03_Disaggregate_land_use/figures/03_Downscaling_methods_paper/20230130_HILDA_Colombia_2019_ds_fuzzy2_maps_pdf.pdf",
#     height = 5,
#     width = 7.4)
# ds_fuzzy2_cropped_maps_2019_with_legend
# dev.off()
