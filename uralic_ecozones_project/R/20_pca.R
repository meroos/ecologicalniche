
suppressPackageStartupMessages({ library(readr); library(dplyr); library(here) })

env_df <- read_csv(here("output","env_grid_clean.csv"), show_col_types = FALSE)

env_vars <- env_df |>
  dplyr::select(T_mean, T_seasonal, P_annual, P_seasonal, snow_mean, permafrost,
                dem_mean, roughness, river_length, lake_dist, sea_dist,
                woodland, swamp, soil_quality, biodiversity)

pca <- prcomp(env_vars, center = TRUE, scale. = TRUE)

scores <- as.data.frame(pca$x[,1:8])
scores$row_id <- env_df$row_id
write_csv(scores, here("output","pca_scores_1to8.csv"))

loadings <- as.data.frame(pca$rotation[,1:8])
loadings$variable <- rownames(pca$rotation)
loadings <- loadings |> dplyr::relocate(variable)
write_csv(loadings, here("output","pca_loadings_PC1_PC8.csv"))

message("Wrote: output/pca_scores_1to8.csv and output/pca_loadings_PC1_PC8.csv")

