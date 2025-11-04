# Smoothed Outline Overlay 12.5.2025
install.packages(c("sf", "ggplot2", "viridis", "dplyr", "patchwork"))
library(sf)
library(ggplot2)
library(viridis)
library(dplyr)
install.packages("patchwork")
library(patchwork)
unique(language_outlines_sf$Language)
names(rf_sf_list)

# Store map objects
rf_map_list <- list()

for (lang in names(rf_sf_list)) {
  
  rf_sf <- rf_sf_list[[lang]]
  
  # Get corresponding outline for this language
  outline_sf <- language_outlines_sf %>% filter(Language == lang)
  
  # Map: probability + outline overlay
  p <- ggplot() +
    # Layer 1: predicted probabilities
    geom_sf(data = rf_sf, aes(fill = Predicted_Probability), color = NA) +
    scale_fill_viridis_c(
      option = "plasma",
      name = "Similarity",
      breaks = c(0.1, 0.25, 0.5, 0.75, 1.0),
      limits = c(0.002, 1)
    ) +
# Layer 2: outline polygon with mapped aesthetic for legend
  geom_sf(data = outline_sf, aes(color = "Presence"), fill = NA, size = 15) +
  scale_color_manual(
    name = "Actual locations",  # Legend title for outline
    values = c("Presence" = "green")
  ) +
    # Guides: Similarity first, Outline second
    guides(
      fill = guide_colorbar(order = 1),    # Similarity first
      color = guide_legend(order = 2)      # Outline second
    ) +
    labs(
      title = paste(lang)
    ) +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.2, "cm"),
      legend.spacing = unit(0.5, "cm"),
      legend.box.spacing = unit(0.5, "cm")
    )
  
  # Save each map
  ggsave(file.path(output_dir, paste0("rf_map_", lang, "_outline.png")), plot = p, width = 8, height = 6, dpi = 300)
  
  # Store map in list
  rf_map_list[[lang]] <- p
}

# Combine maps into 3x3 grid, collect legend
map_grid <- wrap_plots(rf_map_list, ncol = 3, guides = "collect") & 
  theme(legend.position = "right")

# Display
print(map_grid)

# Save combined map
ggsave(filename = file.path(output_dir, "combined_rf_maps_3x3_outline.png"),
       plot = map_grid, width = 18, height = 16, dpi = 600)
