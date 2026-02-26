library(ggplot2)
library(dplyr)
library(pdp)
library(forcats)
library(patchwork)

# Variable order (top to bottom on y-axis)
var_order <- c(
  "T_mean", "T_seasonal", "P_annual", "P_seasonal",
  "snow_mean", "permafrost", "dem_mean", "roughness",
  "river_length", "lake_dist", "sea_dist", "woodland",
  "swamp", "soil_quality", "biodiversity"
)
# Data is in `all_importances` with columns: Language, Variable, Importance

# Reorder variable factor and normalize importance by language
all_importances <- all_importances %>%
  mutate(Variable = factor(Variable, levels = rev(var_order))) %>%
  group_by(Language) %>%
  mutate(MDA_scaled = (Importance - min(Importance)) /
           (max(Importance) - min(Importance))) %>%
  ungroup()

write.csv(
  all_importances,
  file = file.path(output_dir, "All_Variable_Importances_By_Language.csv"),
  row.names = FALSE
)

# Plotting with Okabe-Ito friendly gradient, normalized colors
importance_plot <- ggplot(all_importances, aes(x = Importance, y = Variable)) +
  geom_col(aes(fill = MDA_scaled)) +
  facet_wrap(~ Language, scales = "free_x") +
  scale_fill_gradient(
    low = "#E69F00",  # orange
    high = "#56B4E9", # sky blue
    name = "Relative MDA"
  ) +
  labs(
    x = "Mean Decrease Accuracy",
    y = "Variable",
    title = "Variable importance by language",
    subtitle = "Permutation importance (MDA), color scaled per language"
  ) +
  theme_minimal(base_size = 12)

print(importance_plot)
# Save to file
ggsave(filename = file.path(output_dir, "variable_importance.jpeg"),
       plot = importance_plot,
       width = 12, height = 8, dpi = 600)

# direction of importance
library(pdp)

# Function to generate PDP for a language's top variable
generate_pdp <- function(language, results_list, df, predictors) {
  model <- results_list[[language]]$model
  if (is.null(model)) return(NULL)

  # Get top variable
  top_var <- results_list[[language]]$variable_importance %>%
    arrange(desc(Importance)) %>%
    slice(1) %>%
    pull(Variable)

  # Prepare training data (from full df)
  rf_data <- df[, c(predictors, language)]
  rf_data[[language]] <- factor(rf_data[[language]])

  # Partial dependence
  pd <- partial(model,
                pred.var = top_var,
                train = rf_data,
                prob = TRUE)

  # Plot
  p <- autoplot(pd) +
    labs(
      title = paste("Partial Dependence:", top_var, "(", language, ")"),
      x = top_var,
      y = "Predicted Suitability"
    ) +
    theme_minimal()

  return(p)
}

# Output directory
output_dir <- "E:/xxx/xxx/rf_output_directory"

# For each language
for (lang in names(results_list)) {
  cat("Generating PDP for", lang, "...\n")
  plot <- tryCatch({
    generate_pdp(lang, results_list, df = uralic_rf_d, predictors = predictors)
  }, error = function(e) {
    message("Error for ", lang, ": ", e$message)
    NULL
  })

  # Save if plot was generated
  if (!is.null(plot)) {
    ggsave(filename = file.path(output_dir, paste0("PDP_", lang, ".png")),
           plot = plot, width = 6, height = 4, dpi = 300)
  }
}



# Set variable order for consistent Y-axis
var_order <- c(
  "T_mean", "T_seasonal", "P_annual", "P_seasonal",
  "snow_mean", "permafrost", "dem_mean", "roughness",
  "river_length", "lake_dist", "sea_dist", "woodland",
  "swamp", "soil_quality", "biodiversity"
)

# Normalizing MDA per language
all_importances <- all_importances %>%
  mutate(Variable = factor(Variable, levels = rev(var_order))) %>%
  group_by(Language) %>%
  mutate(MDA_scaled = (Importance - min(Importance)) /
           (max(Importance) - min(Importance))) %>%
  ungroup()

# Generating partial dependence plot for top variable
generate_pdp <- function(language, model, top_var, train_data) {
  pd <- partial(model,
                pred.var = top_var,
                train = train_data,
                prob = TRUE)

  autoplot(pd) +
    labs(
      title = paste("Partial Dependence:", top_var),
      x = top_var,
      y = "Predicted Suitability"
    ) +
    theme_minimal(base_size = 12)
}

# Main loop per language
for (lang in names(results_list)) {
  cat("??? Processing", lang, "...\n")

  # Skip if model is missing
  if (is.null(results_list[[lang]]$model)) next

  # Extract components
  model <- results_list[[lang]]$model
  top_var <- results_list[[lang]]$variable_importance %>%
    arrange(desc(Importance)) %>%
    slice(1) %>%
    pull(Variable)

  # Training data
  rf_data <- uralic_rf_d[, c(predictors, lang)]
  rf_data[[lang]] <- factor(uralic_rf_d[[lang]])

  # PDP
  pdp_plot <- generate_pdp(lang, model, top_var, rf_data)

  # Barplot for this language
  lang_barplot <- all_importances %>%
    filter(Language == lang) %>%
    ggplot(aes(x = Importance, y = Variable, fill = MDA_scaled)) +
    geom_col() +
    scale_fill_gradient(low = "#E69F00", high = "#56B4E9", name = "Relative MDA") +
    labs(
      title = paste("Variable Importance -", lang),
      x = "Mean Decrease Accuracy",
      y = "Variable"
    ) +
    theme_minimal(base_size = 12)

  # Combine
  combined <- lang_barplot / pdp_plot + plot_layout(heights = c(2, 1))

  # Save
  ggsave(
    filename = file.path(output_dir, paste0("Importance_and_PDP_", lang, ".png")),
    plot = combined,
    width = 10, height = 8, dpi = 300
  )

  cat("??? Saved plot for", lang, "\n")
}


# Combining All Language Panels into a Single PDF
# Creating a list to hold all combined plots
combined_plots <- list()

# Looping again to collect plots
for (lang in names(results_list)) {
  if (is.null(results_list[[lang]]$model)) next

  model <- results_list[[lang]]$model
  top_var <- results_list[[lang]]$variable_importance %>%
    arrange(desc(Importance)) %>%
    slice(1) %>%
    pull(Variable)

  rf_data <- uralic_rf_d[, c(predictors, lang)]
  rf_data[[lang]] <- factor(uralic_rf_d[[lang]])

  pdp_plot <- generate_pdp(lang, model, top_var, rf_data)

  lang_barplot <- all_importances %>%
    filter(Language == lang) %>%
    ggplot(aes(x = Importance, y = Variable, fill = MDA_scaled)) +
    geom_col() +
    scale_fill_gradient(low = "#E69F00", high = "#56B4E9", name = "Relative MDA") +
    labs(
      title = paste("Variable Importance -", lang),
      x = "Mean Decrease Accuracy",
      y = "Variable"
    ) +
    theme_minimal(base_size = 12)

  # Combine plots
  combined_plot <- lang_barplot / pdp_plot + plot_layout(heights = c(2, 1))
  combined_plots[[lang]] <- combined_plot
}

# Combining all into one big plot and save as PDF
all_combined <- wrap_plots(combined_plots, ncol = 1)

ggsave(
  filename = file.path(output_dir, "All_Importance_PDPs.pdf"),
  plot = all_combined,
  width = 10,
  height = 8 * length(combined_plots),  # adjust based on number of languages
  dpi = 300
)

# let's annotate
library(ggplot2)
library(dplyr)
library(pdp)
library(patchwork)
library(cowplot)

# Output
output_dir <- "E:/xxx/xxx/xxx"

# Function to create PDP with annotation
generate_pdp_with_arrow <- function(language, model, top_var, train_data) {
  pd <- partial(model, pred.var = top_var, train = train_data, prob = TRUE)

  # Determine slope direction
  slope <- tail(pd$yhat, 1) - head(pd$yhat, 1)
  direction <- ifelse(slope > 0, "???", ifelse(slope < 0, "???", "???"))

  autoplot(pd) +
    labs(
      title = paste0("PDP: ", top_var, " ", direction),
      x = top_var,
      y = "Predicted Suitability"
    ) +
    theme_minimal(base_size = 11)
}

# Generate combined panels per language and store
# Create base plots *without* legend
combined_plots_nolegend <- list()

for (lang in names(results_list)) {
  if (is.null(results_list[[lang]]$model)) next

  model <- results_list[[lang]]$model
  top_var <- results_list[[lang]]$variable_importance %>%
    arrange(desc(Importance)) %>%
    slice(1) %>%
    pull(Variable)

  rf_data <- uralic_rf_d[, c(predictors, lang)]
  rf_data[[lang]] <- factor(uralic_rf_d[[lang]])

  pdp_plot <- generate_pdp_with_arrow(lang, model, top_var, rf_data)

  bar_data <- all_importances %>%
    filter(Language == lang)

  # Plot barplot with legend (for 1st only)
  lang_barplot <- ggplot(bar_data, aes(x = Importance, y = Variable, fill = MDA_scaled)) +
    geom_col() +
    scale_fill_gradient(low = "#E69F00", high = "#56B4E9", name = "Relative MDA") +
    labs(
      title = paste("Variable Importance -", lang),
      x = "MDA",
      y = "Variable"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none") # remove legend

  # Combine
  combined <- plot_grid(lang_barplot, pdp_plot, ncol = 2, rel_widths = c(1.4, 1))
  combined_plots_nolegend[[lang]] <- combined
}

# Get shared legend from one plot
sample_barplot <- ggplot(bar_data, aes(x = Importance, y = Variable, fill = MDA_scaled)) +
  geom_col() +
  scale_fill_gradient(low = "#E69F00", high = "#56B4E9", name = "Relative MDA") +
  theme_minimal(base_size = 11)

shared_legend <- get_legend(sample_barplot + theme(legend.position = "bottom"))

# Combine all panels into grid
final_grid <- plot_grid(plotlist = combined_plots_nolegend, ncol = 3)

# Add legend underneath
final_combined <- plot_grid(final_grid, shared_legend, ncol = 1, rel_heights = c(1, 0.05))

# Save
ggsave(
  filename = file.path(output_dir, "Variable_Importance_PDP_Combined_sharedLegend.jpeg"),
  plot = final_combined,
  width = 16, height = 9, dpi = 600
)

# sanity check
ggplot(uralic_rf, aes(x = snow_mean, fill = factor(Hungarian))) +
  geom_density(alpha = 0.5)
cor_matrix <- cor(uralic_rf_d[, predictors])
corrplot::corrplot(cor_matrix, method = "color")
ggplot(uralic_rf, aes(fill = biodiversity)) +
  geom_sf() +
  scale_fill_viridis_c()
uralic_rf %>%
  filter(Saami == 1 | Samoyedic == 1) %>%
  ggplot(aes(x = biodiversity)) +
  geom_histogram() +
  facet_wrap(~ifelse(Saami == 1, "Saami", "Samoyedic"))


