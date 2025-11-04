library(dplyr)
library(sf)
library(ggplot2)
setwd("D:/URKO_PhDposition/Article 3_ecological niche")
getwd()
mydata_uralic_sp <- read_sf("grid50_RF_3_5_2024.shp")
uralic_rf <- mydata_uralic_sp[,c(7,6,15,18,30,67,23,28,59,45,47,48,55,53,54,40:44,49:52)]
uralic_rf <- st_transform(uralic_rf, "ESRI:102025")
plot(st_geometry(uralic_rf))
colnames(uralic_rf)

# Rename and reorder columns
uralic_rf <- rename(
  uralic_rf,
  T_mean = bio1,
  T_seasonal = bio4,
  P_annual = bio12,
  P_seasonal = bio15,
  dem_mean = demmean,
  roughness = roughnessm,
  snow_mean = snowmean,
  permafrost = pfrost_fra,
  woodland = woodland,
  soil_quality = soil_quali,
  river_lenght = river_LENG,
  biodiversity = biodiversi,
  swamp = peat_frac,
  lake_dist = lakes_dist,
  sea_dist = sea_distan,
  Finnic = Finnic_bin,
  Hungarian = Hungar_bin,
  Khanty = Khanty_bin,
  Mansi = Mansi_bin,
  Mordvin = Mordvi_bin,
  Mari = Mari_bin,
  Permic = Permic_bin,
  Samoyedic = Samyed_bin,
  Saami = Saami_bin
)


# Load the shapefile
uralic_rf <- st_read("data/uralic_rf.shp")
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
output_dir <- "E:/URKO_PhDposition/Article 3_ecological niche/rf_output_directory" dir.create(output_dir, showWarnings = FALSE)
saveRDS(uralic_rf_d, "data/uralic_rf_cleaned.RDS")
