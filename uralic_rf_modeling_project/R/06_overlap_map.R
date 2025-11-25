# Initialize with geometry
prob_combined <- st_sf(geometry = geometry_data)
colnames(prob_combined)
# Loop over results_list to extract probabilities
for (lang in names(results_list)) {
  model_info <- results_list[[lang]]
  if (is.null(model_info)) next
  
  preds <- predict(model_info$model, data = df)$predictions[, "1"]
  
  prob_combined[[lang]] <- preds
}

# Define threshold for moderate suitability
threshold <- 0.01

# Drop geometry to isolate just the probability columns
prob_numeric <- st_drop_geometry(prob_combined)[, names(results_list)]

# Apply threshold to get binary overlap matrix
binary_overlap <- prob_numeric > threshold

# Count how many languages exceed the threshold in each cell
prob_combined$Overlap_Count <- rowSums(binary_overlap, na.rm = TRUE)
str(prob_combined)

#Count how many languages exceed the threshold per cell
###prob_combined$Overlap_Count <- rowSums(prob_combined[, names(results_list)] > threshold, na.rm = TRUE)

library(ggplot2)
# library for scale bar
library(ggspatial)

combined_rf_overlap <- ggplot(prob_combined) +
  geom_sf(aes(fill = Overlap_Count), color = NA) +
  scale_fill_viridis_c(
    option = "C",
    name = "Nr of overlapping environmentally similar areas",
    breaks = 1:max(prob_combined$Overlap_Count, na.rm = TRUE),
    limits = c(1, max(prob_combined$Overlap_Count, na.rm = TRUE))
  ) +
  labs(
    title = paste("Geographic overlap of ecological similarity (> ", threshold, ")"),
    subtitle = "Language overlap"
  ) +
  theme_minimal()

combined_rf_overlap <- combined_rf_overlap +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    line_width = 1.5,
    text_cex = 0.8,
    style = "ticks",
    unit_category = "metric",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    bar_cols = c("black", "black"),
    text_col = "black"
  ) 
# Display it
print(combined_rf_overlap)

# ADDING LANGUAGE OUTLINES 
# Add language outlines to the existing combined_rf_overlap map
combined_rf_overlap <- combined_rf_overlap +
  geom_sf(data = language_outlines_sf, fill = NA, color = "green", size = 2) +
  labs(
    caption = "Language outlines in black"
  )

# Display the map
print(combined_rf_overlap)

# Save to file
ggsave(filename = file.path(output_dir, "combined_rf_overlap.png"), plot = combined_rf_overlap, width = 18, height = 12, dpi = 300)
ggsave(
  filename = file.path(output_dir, "combined_rf_overlap.pdf"),
  plot = combined_rf_overlap,
  width = 10,
  height = 8,
  dpi = 300
)

# OVERLAP MAP WITH PHYSICAL GEOGRAPHY 
library(ggplot2)
library(sf)
library(rnaturalearth)  # for physical geography data
library(rnaturalearthdata)
library(viridis)
library(patchwork)      # for combining maps
# Download physical geography data
rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
lakes <- ne_download(scale = 50, type = "lakes", category = "physical", returnclass = "sf")
land <- ne_countries(scale = 50, returnclass = "sf")

# Clip to your study area for clarity (optional)
# e.g., bounding box for Eurasia
uralic_rf_outline <- st_union(uralic_rf)  # merge all geometry features to one outline
uralic_rf_outline <- st_transform(uralic_rf_outline, st_crs(uralic_rf))

target_crs <- st_crs(uralic_rf)
land <- st_transform(land, crs = target_crs)
rivers <- st_transform(rivers, crs = target_crs)
lakes <- st_transform(lakes, crs = target_crs)

# Crop (mask) the physical layers to the outline
land <- st_intersection(land, uralic_rf_outline)
rivers <- st_intersection(rivers, uralic_rf_outline)
lakes <- st_intersection(lakes, uralic_rf_outline)



# Create the physical geography map
phys_geo_map <- ggplot() +
  geom_sf(data = land, fill = "grey95", color = "grey70", size = 0.1) +
  geom_sf(data = lakes, fill = "lightblue", color = "lightblue", size = 0.1) +
  geom_sf(data = rivers, color = "#4682B4", size = 0.3) +
  labs(
    title = "Physical Geography of Northern Eurasia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

strategic_rivers <- rivers %>% filter(name %in% c("Oka,Volga", "Ob", "Yenisei", "Lena", "Pechora", "Kama", "Dvina", "Neva"))
# Path to your shapefile
gmba_path <- "E:/URKO_PhDposition/Article 3_ecological niche/GMBA_Inventory_v2.0_standard_300/GMBA_Inventory_v2.0_standard_300.shp"

# Read it into R
gmba_mountains <- st_read(gmba_path)
# Reproject to match your main map projection
gmba_mountains <- st_transform(gmba_mountains, st_crs(uralic_rf))
gmba_mountains <- st_intersection(gmba_mountains, uralic_rf_outline)
# Check the structure
colnames(gmba_mountains)
mountain_names <- unique(gmba_mountains$Name_EN)

# Define group names to merge
altai_names <- c("Mongolian Altai", "Gobi Altai", "Northern Altai")
sayan_names <- c("Eastern Sayan", "Western Sayan")
scandes_names <- c("Southern Scandes", NA)

# Filter and merge mountain groups
selected_mountains <- gmba_mountains %>%
  filter(
    Name_EN %in% c(
      "Ural Mountains",
      "Central Siberian Plateau",
      "Carpathian Mountains",
      "Fell Lapland",
      altai_names,
      sayan_names,
      scandes_names
    ) | is.na(Name_EN)
  ) %>%
  mutate(Name_EN = case_when(
    Name_EN %in% altai_names ~ "Altai",
    Name_EN %in% sayan_names ~ "Sayan",
    Name_EN %in% scandes_names | is.na(Name_EN) ~ "Scandes",
    TRUE ~ Name_EN
  ))

# Check final names
print(unique(selected_mountains$Name_EN))
mountain_labels_df <- data.frame(
  name = c("Ural Mountains", "Central Siberian Plateau", "Carpathian Mountains", "Altai", "Sayan", "Scandes", "Fell Lapland"),
  lon = c(60, 105, 25, 90, 95, 20, 25),
  lat = c(61, 65, 47, 49, 53, 68, 69)
)
mountain_labels_sf <- st_as_sf(mountain_labels_df, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(uralic_rf))

# Path to your swamps shapefile
swamp_path <- "E:/URKO_PhDposition/Article 3_ecological niche/glwd_swamps.shp"

# Read the shapefile
swamps <- st_read(swamp_path)
# Reproject to match your map projection
swamps <- st_transform(swamps, st_crs(uralic_rf))

# Check it visually (optional)
plot(st_geometry(swamps), col = "green")

phys_geo_map <- phys_geo_map +
  geom_sf_text(data = strategic_rivers, aes(label = name),
               size = 2.5, fontface = "italic", color = "blue")+
  geom_sf(data = swamps, fill = "#006400", color = NA, alpha = 0.4)+
  # Add mountain polygons
  geom_sf(data = selected_mountains, fill = "#C0C0C0", color = "#A9A9A9", alpha = 0.4, size = 0.3) +
  labs(
    title = "Physical Geography with Strategic Mountain Ranges"
  )+
  geom_sf_text(data = mountain_labels_sf, aes(label = name),
               size = 3, fontface = "bold", color = "black")


print(phys_geo_map)
ggsave(
  filename = file.path(output_dir, "phys_geo_map.pdf"),
  plot = phys_geo_map,
  device = cairo_pdf,   # Use cairo_pdf for better vector rendering
  width = 10,
  height = 8,
  dpi = 300
)

## ENVIRONMENT AND OVERLAP
# Merge by geometry using spatial join
colnames(uralic_rf)
prob_combined_with_env <- st_join(prob_combined, uralic_rf %>% select(dem_mean, swamp))

# Double-check the merge
print(names(prob_combined_with_env))
# Filter for high overlap (???3)
high_overlap <- prob_combined_with_env %>%
  filter(Overlap_Count >= 3)

# Elevation summary for high overlap areas
high_overlap_elevation_summary <- high_overlap %>%
  summarize(
    Min_Elevation = min(dem_mean, na.rm = TRUE),
    Mean_Elevation = mean(dem_mean, na.rm = TRUE),
    Max_Elevation = max(dem_mean, na.rm = TRUE)
  )

# Elevation summary for the entire uralic_rf dataset
full_elevation_summary <- uralic_rf %>%
  summarize(
    Min_Elevation = min(dem_mean, na.rm = TRUE),
    Mean_Elevation = mean(dem_mean, na.rm = TRUE),
    Max_Elevation = max(dem_mean, na.rm = TRUE)
  )
# Swamp presence
swamp_cells <- sum(high_overlap$swamp == 1, na.rm = TRUE)
swamp_percent <- round((swamp_cells / nrow(high_overlap)) * 100, 2)

# Summary of river length for high overlap areas
river_summary_high_overlap <- high_overlap %>%
  summarize(
    Min_River = min(river_length, na.rm = TRUE),
    Mean_River = mean(river_length, na.rm = TRUE),
    Max_River = max(river_length, na.rm = TRUE)
  )

# Summary of river length for the entire uralic_rf dataset
river_summary_full <- uralic_rf %>%
  summarize(
    Min_River = min(river_length, na.rm = TRUE),
    Mean_River = mean(river_length, na.rm = TRUE),
    Max_River = max(river_length, na.rm = TRUE)
  )
# Output
cat("High overlap areas (???3 languages):\n")
cat("Total number of cells:", nrow(high_overlap), "\n")
cat("Mean elevation:", round(mean_elevation, 2), "m\n")
cat("Swamp presence in high overlap cells:", swamp_cells, "cells (", swamp_percent, "%)\n")






# HIGH OVERLAP MAP
# interested in areas where ???3 language models predict moderate suitability
high_overlap <- prob_combined %>%
  filter(Overlap_Count >= 2)

st_crs(uralic_rf)
st_crs(high_overlap) <- st_crs(uralic_rf)
st_crs(high_overlap)

# Extract centroids and convert to coordinates
high_coords <- st_coordinates(st_centroid(high_overlap))
# Bind to data
high_overlap_coords <- cbind(high_overlap, high_coords)
high_overlap_coords <- high_overlap_coords %>%
  mutate(region = case_when(
    X >= 20 & X <= 40 & Y >= 55 ~ "Fennoscandia",
    X > 40 & X <= 60 & Y >= 55 ~ "Western Siberia",
    X > 60 & X <= 80 & Y >= 55 ~ "Central Siberia",
    X > 80                   ~ "Eastern Siberia",
    TRUE ~ "Other"
  ))

# full background geometry
full_grid_geometry <- st_geometry(uralic_rf)

high_overlap$Overlap_Factor <- factor(high_overlap$Overlap_Count)

ggplot() +
  # Background grid (just outlines)
  geom_sf(data = full_grid_geometry, fill = NA, color = "grey80", size = 0.1) +
  
  # High-overlap zones filled by Overlap_Count
  geom_sf(data = high_overlap, aes(fill = Overlap_Count), color = NA) +
  
  scale_fill_viridis_c(
    option = "plasma",
    name = "Language overlap",
    breaks = 1:max(high_overlap$Overlap_Count, na.rm = TRUE),
    limits = c(1, max(high_overlap$Overlap_Count, na.rm = TRUE))
  ) +
  
  labs(
    title = "High language model overlap zones",
    caption = paste("Cells with overlap ???", min(high_overlap$Overlap_Count))
  ) +
  theme_minimal()



class(pca_data_subset)
str(pca_data_subset)
library(sf)
# Reattach geometry to make a spatial version
pca_data_sf <- st_sf(pca_data_subset, geometry = geometry_data[1:nrow(pca_data_subset)])
class(pca_data_sf)       # Should return: "sf" "data.frame"
plot(pca_data_sf["Ecozone"])  # Quick plot by ecozone

high_overlap_labeled <- st_join(high_overlap, pca_data_sf)


