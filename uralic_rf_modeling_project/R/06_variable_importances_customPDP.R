library(ggplot2)
library(dplyr)
library(pdp)
library(patchwork)
library(cowplot)

citation("pdp")
# Output directory
output_dir <- "x:/xxx/xxx/rf_output_directory"

# Custom PDP variables for all languages
custom_pdp_vars <- c(
  Finnic = "T_seasonal",
  Hungarian = "P_annual", # curves show decline in suitability with higher values, captures climatic constraint in steppe transition zone
  Khanty = "T_mean",  
  Mansi = "roughness", # Uralic foothill signal, increasing suitability with terrain complexity 
  Mari = "lake_dist", # cultural-ecological relevance, settled near river and lakes Volga basin
  Mordvin = "lake_dist", # lakes/river vicinity
  Permic = "T_seasonal", # strong alignment with mountainous ecozone signal, Ural foothills
  Saami = "P_annual", # Sharp increase with moderate seasonality, meaningful cold adaptation signal
  Samoyedic = "permafrost" # strong linear decline in suitability
)

pdp_var_labels <- c(
  T_seasonal = "Temp. seasonality",
  roughness = "Roughness",
  P_annual = "Ann. precipitation",
  permafrost = "Permafrost",
  lake_dist = "Lake distance",
  T_mean = "Mean temperature"
)

# Function to create PDP
generate_pdp_with_arrow <- function(language, model, top_var, train_data) {
  pd <- partial(model,
                pred.var = top_var,
                train = train_data,
                prob = TRUE)
  
  direction <- ifelse(last(pd$yhat) > first(pd$yhat), "???", "???")
  
  autoplot(pd) +
    labs(
      title = paste0(pdp_var_labels[[top_var]], " ", direction),
      x = top_var,  # ??? use original variable name here
      y = "Predicted similarity"
    ) +
    theme_minimal(base_size = 12)
}



# Generate combined plots without legend
combined_plots_nolegend <- list()

for (lang in names(results_list)) {
  if (is.null(results_list[[lang]]$model)) next
  
  model <- results_list[[lang]]$model
  
  # ??? Use the manually selected PDP variable
  top_var <- custom_pdp_vars[[lang]]
  
  rf_data <- uralic_rf_d[, c(predictors, lang)]
  rf_data[[lang]] <- factor(uralic_rf_d[[lang]])
  
  pdp_plot <- generate_pdp_with_arrow(lang, model, top_var, rf_data)
  
  bar_data <- all_importances %>%
    filter(Language == lang)
  
  lang_barplot <- ggplot(bar_data, aes(x = Importance, y = Variable, fill = MDA_scaled)) +
    geom_col() +
    scale_fill_gradient(low = "#56B4E9", high = "#E69F00", name = "Relative MDA") +
    labs(
      title = paste(lang),
      x = "MDA",
      y = "Variable"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none")
  
  combined <- plot_grid(lang_barplot, pdp_plot, ncol = 2, rel_widths = c(1.4, 1))
  combined_plots_nolegend[[lang]] <- combined
}

# Extract a shared legend
sample_barplot <- ggplot(bar_data, aes(x = Importance, y = Variable, fill = MDA_scaled)) +
  geom_col() +
  scale_fill_gradient(low = "#56B4E9", high = "#E69F00", name = "Relative MDA") +
  theme_minimal(base_size = 11)

shared_legend <- get_legend(sample_barplot + theme(legend.position = "bottom"))

# Combine all language panels into a grid
final_grid <- plot_grid(plotlist = combined_plots_nolegend, ncol = 3)

# Add legend below the grid
final_combined <- plot_grid(final_grid, shared_legend, ncol = 1, rel_heights = c(1, 0.05))

# Save final plot
ggsave(
  filename = file.path(output_dir, "Variable_Importance_PDP_Combined_sharedLegend_FINA.jpeg"),
  plot = final_combined,
  width = 18, height = 9, dpi = 600
)

#sanity check for hungary
library(pdp)
library(ggplot2)

# Prepare data
lang <- "Hungarian"
top_var <- "P_annual"
model <- results_list[[lang]]$model

rf_data <- uralic_rf_d[, c(predictors, lang)]
rf_data[[lang]] <- factor(rf_data[[lang]])

# Generate PDP
pd <- partial(model,
              pred.var = top_var,
              train = rf_data,
              prob = TRUE)

# Plot
autoplot(pd) +
  labs(
    title = paste("Partial Dependence:", top_var, "(", lang, ")"),
    x = top_var,
    y = "Predicted Suitability"
  ) +
  theme_minimal()

