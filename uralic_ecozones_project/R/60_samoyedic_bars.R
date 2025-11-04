
suppressPackageStartupMessages({ library(sf); library(readr); library(dplyr); library(ggplot2); library(here); library(scales) })

env_sf <- st_read(here("data","env_with_ecozones.gpkg"), layer = "env_with_ecozones", quiet = TRUE)

langs_path <- here("data","uralic_languages_shp.shp")
if (!file.exists(langs_path)) stop("Missing data/uralic_languages_shp.shp with fields Branch and Language")
langs   <- st_read(langs_path, quiet = TRUE)
if (!st_crs(env_sf) == st_crs(langs)) langs <- st_transform(langs, st_crs(env_sf))

samoy_src <- langs |>
  filter(Branch == "Samoyedic") |>
  select(Subgroup = Language)

env_join <- st_join(env_sf, samoy_src, left = TRUE)

sublang_order <- c("Tundra Nenets","Forest Nenets","Tomsk region Selkup",
                   "Tundra Enets","Northern Selkup","Forest Enets",
                   "Nganasan","Kamas and Mator")

eco_levels <- c("North-Atlantic fjord landscape","Cold continental mountains","Subarctic tundra",
                "Boreal taiga zone","Continental highlands","Temperate grasslands and xeric shrublands",
                "Temperate broadleaf forests","Arctic tundra","Subarctic peatlands")

sam_tab <- env_join |>
  st_drop_geometry() |>
  filter(Language == "Samoyedic", !is.na(Subgroup), !is.na(Ecozone_Name)) |>
  mutate(Subgroup = factor(Subgroup, levels = sublang_order),
         Ecozone_Name = factor(Ecozone_Name, levels = eco_levels)) |>
  count(Subgroup, Ecozone_Name, name = "Count") |>
  group_by(Subgroup) |>
  mutate(Proportion = 100 * Count / sum(Count)) |>
  ungroup()

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

g_sam <- ggplot(sam_tab, aes(Subgroup, Proportion, fill = Ecozone_Name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Proportion > 0.2, paste0(round(Proportion,1), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = ecozone_colors, drop = FALSE, name = "Ecozone") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Samoyedic subgroups across ecozones", x = "Subgroup", y = "Proportion (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("output","samoyedic_subgroups_barchart.pdf"),  g_sam, width = 11, height = 8, device = cairo_pdf)
ggsave(here("output","samoyedic_subgroups_barchart.jpeg"), g_sam, width = 11, height = 8, dpi = 600)

# Also export subgroup proportions as CSV
readr::write_csv(sam_tab, here("output","samoyedic_subgroups_proportions.csv"))

message("Wrote Samoyedic subgroup barplots and CSV to output/")

