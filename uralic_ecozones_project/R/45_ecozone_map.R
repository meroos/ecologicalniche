
suppressPackageStartupMessages({ library(sf); library(ggplot2); library(here) })

ecozone_colors <- c(
  "North-Atlantic fjord landscape"="lightgrey",
  "Cold continental mountains"="purple",
  "Subarctic tundra"="lightblue",
  "Boreal taiga zone"="forestgreen",
  "Continental highlands"="#b99aff",
  "Temperate grasslands and xeric shrublands"="yellow",
  "Temperate broadleaf forests"="lightgreen",
  "Arctic tundra"="blue",
  "Subarctic peatlands"="darkred"
)

env_sf <- st_read(here("data","env_with_ecozones.gpkg"), layer = "env_with_ecozones", quiet = TRUE)

g_map <- ggplot(env_sf) +
  geom_sf(aes(fill = Ecozone_Name), linewidth = 0) +
  scale_fill_manual(values = ecozone_colors, drop = FALSE, name = "Ecozone") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Ecozones, PCA k-means (k = 9)")

ggsave(here("output","ecozones_map.pdf"),  g_map, width = 10, height = 7, device = cairo_pdf)
ggsave(here("output","ecozones_map.jpeg"), g_map, width = 10, height = 7, dpi = 600)
message("Wrote ecozones map to output/")

