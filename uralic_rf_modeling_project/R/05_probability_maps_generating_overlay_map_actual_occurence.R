# STEP 1. Generate and store rf_sf for each language 12.5.2025
library(sf)

colnames(uralic_rf)
# Export as shapefile (a folder with multiple files will be created)
st_write(uralic_rf, "uralic_rf_8_5_2025.shp")


rf_sf_list <- list()  # Store rf_sf objects per language

for (lang in names(results_list)) {
  
  model_info <- results_list[[lang]]
  if (is.null(model_info)) next
  
  rf_model <- model_info$model
  
  # Predict across all data
  preds <- predict(rf_model, data = df)$predictions[, "1"]
  
  # Build sf object with predictions and actuals
  rf_sf <- st_sf(
    Predicted_Probability = preds,
    Actual = df[[lang]],
    geometry = geometry_data
  )
  
  rf_sf_list[[lang]] <- rf_sf  # Store in list
}

# STEP 2. Create smoothed outlines from Actual == 1
language_outline_list <- list()

for (lang in names(rf_sf_list)) {
  
  rf_sf <- rf_sf_list[[lang]]
  
  # Get points where Actual == 1
  presence_sf <- rf_sf %>% filter(Actual == 1)
  if (nrow(presence_sf) == 0) next
  
  # Dissolve and smooth
  presence_union <- st_union(presence_sf)
  
  outline <- presence_union %>%
    st_buffer(dist = 20000) %>%
    st_simplify(dTolerance = 10000)
  
  language_outline_list[[lang]] <- st_sf(Language = lang, geometry = outline)
}

# Combine into single sf object
language_outlines_sf <- do.call(rbind, language_outline_list)

# Optional: Save to file
st_write(language_outlines_sf, "rf_language_presence_outlines.shp")

library(ggplot2)
# cehck the outlines before plotting
ggplot(language_outlines_sf) +
  geom_sf(aes(fill = Language), color = "black", alpha = 0.3) +
  theme_minimal() +
  ggtitle("Sanity Check: Smoothed Language Outlines")

