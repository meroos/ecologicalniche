library(dplyr)
library(sf)
library(ggplot2)

# Load the shapefile
uralic_rf <- st_read("data/uralic_rf_3_9_2024.shp")
# Save geometry for later
uralicrf_geometry_data <- st_geometry(uralic_rf)
colnames(uralic_rf)
str(uralic_rf)
# Define predictors and languages
predictors <- c("T_mean", "T_seasonal", "P_annual", "P_seasonal", 
                "snow_mean", "permafrost", "dem_mean", "roughness", 
                "river_lenght", "lake_dist", "sea_dist", "woodland", 
                "swamp", "soil_quality", "biodiversity")
response_var <- c("Finnic", "Hungarian", "Khanty", "Mansi", "Mordvin", 
               "Mari", "Permic", "Samoyedic", "Saami")

# Identifying missing values
colSums(is.na(uralic_rf))
# Impute missing values only environmental variables using column mean
uralic_rf <- uralic_rf %>%
  mutate(across(
    all_of(predictors),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  ))
colSums(is.na(uralic_rf))

language_columns <- c("Finnic", "Hungarian", "Khanty", "Mansi", "Mordvin", "Mari", "Permic", "Saami", "Samoyedic")
uralic_rf$Language <- language_columns[uralic_rf$Uralic_bra]
colnames(uralic_rf)
table(uralic_rf$Language)

uralic_rf <- uralic_rf %>% select(-Uralic_bra) #don't need this column anymore

uralic_rf_d <- st_drop_geometry(uralic_rf, TRUE)

class(uralic_rf)

df <-uralic_rf
sapply(df, class)
output_dir <- "x:/xxx/xxx/rf_output_directory" dir.create(output_dir, showWarnings = FALSE)
saveRDS(uralic_rf_d, "data/uralic_rf_cleaned.RDS")
