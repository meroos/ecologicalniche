
suppressPackageStartupMessages({ library(readr); library(dplyr); library(here); library(sf) })

set.seed(123)

scores <- read_csv(here("output","pca_scores_1to8.csv"), show_col_types = FALSE)
stopifnot("row_id" %in% names(scores))

km <- kmeans(scores[,1:8], centers = 9, nstart = 25)

scores$Cluster_Original <- as.integer(km$cluster)
cluster_mapping <- c("1"="5","2"="1","3"="8","4"="7","5"="6","6"="4","7"="2","8"="9","9"="3")
scores$Ecozone_renamed <- factor(cluster_mapping[as.character(scores$Cluster_Original)],
                                 levels = as.character(1:9))

eco_names <- c(
  "1"="North-Atlantic fjord landscape",
  "2"="Cold continental mountains",
  "3"="Subarctic tundra",
  "4"="Boreal taiga zone",
  "5"="Continental highlands",
  "6"="Temperate grasslands and xeric shrublands",
  "7"="Temperate broadleaf forests",
  "8"="Arctic tundra",
  "9"="Subarctic peatlands"
)
scores$Ecozone_Name <- eco_names[as.character(scores$Ecozone_renamed)]

# Saving ecozone label key
eco_key <- tibble::tibble(Ecozone_renamed = as.integer(names(eco_names)),
                          Ecozone_Name = unname(eco_names))
readr::write_csv(eco_key, here("output","ecozone_code_name_mapping.csv"))

# Persist full scores with ecozones
readr::write_csv(scores, here("output","pca_scores_with_ecozones.csv"))

# Language table
env_df <- read_csv(here("output","env_grid_clean.csv"), show_col_types = FALSE)
language_columns <- c("Finnic","Hungarian","Khanty","Mansi","Mari","Mordvin","Permic","Saami","Samoyedic")
env_df$Language <- language_columns[env_df$Uralic_bra]
env_df$Language <- factor(env_df$Language, levels = language_columns)

lang_table <- env_df |>
  left_join(scores |> dplyr::select(row_id, Ecozone_renamed, Ecozone_Name), by = "row_id") |>
  dplyr::select(row_id, Language, Ecozone_renamed, Ecozone_Name)
readr::write_csv(lang_table, here("output","env_language_ecozone_table.csv"))

# Centroids of ecozones in PC space (means of scores by Ecozone_renamed)
centroids <- scores |>
  dplyr::group_by(Ecozone_renamed) |>
  dplyr::summarise(across(dplyr::starts_with("PC"), mean, .names = "{col}"),
                   Ecozone_Name = dplyr::first(Ecozone_Name), .groups = "drop")
readr::write_csv(centroids, here("output","ecozone_centroids_PC1_PC8.csv"))

# Writing geometry with ecozones to GeoPackage
#env_sf <- st_read(here("data","env_grid_clean.gpkg"), layer = "env_grid_clean", quiet = TRUE) |>
#  left_join(scores |> dplyr::select(row_id, Ecozone_renamed, Ecozone_Name), by = "row_id")

gpkg_out <- here("data","env_with_ecozones.gpkg")
if (file.exists(gpkg_out)) file.remove(gpkg_out)
st_write(env_sf, gpkg_out, layer = "env_with_ecozones", quiet = TRUE)

message("Wrote: output/* CSVs and data/env_with_ecozones.gpkg")

