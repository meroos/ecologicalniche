
suppressPackageStartupMessages({ library(readr); library(dplyr); library(tidyr); library(ggplot2); library(here); library(scales) })

tab <- read_csv(here("output","env_language_ecozone_table.csv"), show_col_types = FALSE) |>
  filter(!is.na(Language), !is.na(Ecozone_renamed))

eco_labels <- c(
  "1"="North-Atlantic fjord landscape", "2"="Cold continental mountains",
  "3"="Subarctic tundra", "4"="Boreal taiga zone", "5"="Continental highlands",
  "6"="Temperate grasslands and xeric shrublands", "7"="Temperate broadleaf forests",
  "8"="Arctic tundra", "9"="Subarctic peatlands"
)

language_order <- c("Hungarian","Finnic","Saami","Mordvin","Mari","Permic","Mansi","Khanty","Samoyedic")

df <- tab |>
  count(Language, Ecozone_renamed, name = "Count") |>
  group_by(Language) |>
  mutate(Proportion = 100 * Count / sum(Count)) |>
  ungroup() |>
  mutate(
    Language = factor(Language, levels = language_order),
    Ecozone = factor(eco_labels[as.character(Ecozone_renamed)], levels = unname(eco_labels))
  )

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

g <- ggplot(df, aes(Language, Proportion, fill = Ecozone)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Proportion > 0.2, paste0(round(Proportion,1), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = ecozone_colors, drop = FALSE) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Proportional distribution of languages across ecozones",
       x = "Language", y = "Proportion (%)", fill = "Ecozone") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("output","languages_across_ecozones.pdf"),  g, width = 11, height = 8, device = cairo_pdf)
ggsave(here("output","languages_across_ecozones.jpeg"), g, width = 11, height = 8, dpi = 600)

wide <- df |>
  select(Language, Ecozone, Proportion) |>
  pivot_wider(names_from = Language, values_from = Proportion) |>
  arrange(Ecozone)
write_csv(wide, here("output","language_ecozone_proportion_table.csv"))

message("Wrote language barplots and wide CSV to output/")

