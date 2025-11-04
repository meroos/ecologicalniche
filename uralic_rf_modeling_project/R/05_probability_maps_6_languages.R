library(sf)
library(ggplot2)
library(viridis)
library(dplyr)


geometry_data <- st_geometry(uralic_rf)
# Create empty sf layer (geometry only, no data)
rf_outline <- st_sf(geometry = st_geometry(rf_sf))

for (lang in names(results_list)) {
  
  model_info <- results_list[[lang]]
  if (is.null(model_info)) next
  
  rf_model <- model_info$model
  
  # Predict across all data
  preds <- predict(rf_model, data = df)$predictions[, "1"]
  
  # Build sf object with predictions
  rf_sf <- st_sf(
    Predicted_Probability = preds,
    Actual = df[[lang]],
    geometry = geometry_data
  )
  
  # Map
  p <- ggplot() +
    geom_sf(data = rf_outline, fill = NA, color = "grey90", size = 0.1) +
    # Layer 1: predicted probabilities
    # Predicted probabilities
    geom_sf(data = rf_sf, aes(fill = Predicted_Probability), color = NA) +
    
    scale_fill_viridis_c(
      option = "plasma",
      name = "Probability",
      breaks = c(0.1, 0.25, 0.5, 0.75, 1.0),
      limits = c(0.002, 1)
    ) +
    
    # Actual presence points
    geom_sf(
      data = rf_sf[rf_sf$Actual == 1, ],
      aes(color = "Presence"),
      fill = NA,
      size = 0.5, alpha = 0.9
    ) +
    
    scale_color_manual(
      name = "Actual locations",
      values = c("Presence" = "#00ffff")
    ) +
    
    labs(
      title = paste(lang),
      fill = "Probability"
    ) +
    
    theme_minimal()
  
  # Save
  ggsave(file.path(output_dir,filename = paste0("rf_map_", lang, ".jpeg")), plot = p, width = 8, height = 6, dpi = 300)
  
  # Store map object if needed
  assign(paste0(lang, "_rf_map"), p)
}


install.packages("patchwork")
library(patchwork)
# Combine with shared legend only
map_grid <- (
  Finnic_rf_map + Khanty_rf_map +
    Mansi_rf_map + Permic_rf_map + Saami_rf_map + Samoyedic_rf_map 
) +
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "right")

# Display it
print(map_grid)

# Save to file
ggsave(filename = file.path(output_dir, "combined_rf_maps_6.png"), plot = map_grid, width = 18, height = 12, dpi = 300)



# how expansive or focused each model's predicted habitat is?
# Create empty list to store summary
prob_stats_list <- list()

for (lang in names(results_list)) {
  
  model_info <- results_list[[lang]]
  if (is.null(model_info)) next
  
  rf_model <- model_info$model
  
  # Predict probabilities across entire dataset
  preds <- predict(rf_model, data = df)$predictions[, "1"]
  
  # Calculate summary stats
  prob_stats <- data.frame(
    Language = lang,
    Mean_Prob = mean(preds),
    Median_Prob = median(preds),
    Max_Prob = max(preds),
    SD_Prob = sd(preds),
    Pct_above_0.5 = mean(preds > 0.5) * 100,
    Pct_above_0.75 = mean(preds > 0.75) * 100
  )
  
  prob_stats_list[[lang]] <- prob_stats
}

# Combine all into a single table
prob_summary_df <- do.call(rbind, prob_stats_list)

# Round for clarity
prob_summary_df <- prob_summary_df %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Save to CSV
write.csv(prob_summary_df,
          file = file.path(output_dir, "rf_predicted_probability_summary.csv"),
          row.names = FALSE)

# Preview
print(prob_summary_df)
