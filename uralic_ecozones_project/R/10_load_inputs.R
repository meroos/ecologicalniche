
suppressPackageStartupMessages({
  library(sf); library(dplyr); library(readr); library(here)
})

crs_target <- "ESRI:102025"
grid_path  <- here("data","grid50_RF_3_5_2024.shp")
stopifnot(file.exists(grid_path))

# Read, reproject, select, impute, add row_id
env_sf <- read_sf(grid_path) |>
  st_transform(crs_target) |>
  dplyr::select(
    T_mean=bio1, T_seasonal=bio4, P_annual=bio12, P_seasonal=bio15,
    dem_mean=demmean, roughness=roughnessm, snow_mean=snowmean, permafrost=pfrost_fra,
    woodland=woodland, soil_quality=soil_quali, river_length=river_LENG, biodiversity=biodiversi,
    swamp=peat_frac, lake_dist=lakes_dist, sea_dist=sea_distan,
    Finnic=Finnic_bin, Hungarian=Hungar_bin, Khanty=Khanty_bin, Mansi=Mansi_bin,
    Mordvin=Mordvi_bin, Mari=Mari_bin, Permic=Permic_bin, Samoyedic=Samyed_bin, Saami=Saami_bin,
    Uralic_bra
  )

env_sf$row_id <- seq_len(nrow(env_sf))

env_sf <- env_sf |>
  mutate(across(where(is.numeric) & !any_of("Uralic_bra"),
                ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Geometry-free CSV for audit
env_df <- env_sf |> st_drop_geometry()
write_csv(env_df, here("output","env_grid_clean.csv"))


message("Wrote: output/env_grid_clean.csv and data/env_grid_clean.gpkg")

