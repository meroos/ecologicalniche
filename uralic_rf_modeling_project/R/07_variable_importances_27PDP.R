library(ggplot2)
library(dplyr)
library(pdp)
library(patchwork)
library(cowplot)

# Output directory
output_dir_pdp <- "E:/URKO_PhDposition/Article 3_ecological niche/rf_output_directory/PDPs_by_variable_per_language"
dir.create(output_dir_pdp, showWarnings = FALSE)

# Ecozone-based best PDP variable candidates
pdp_candidates <- list(
  Samoyedic = c("biodiversity", "snow_mean", "permafrost"),
  Saami     = c("biodiversity", "T_seasonal", "snow_mean"),
  Khanty    = c("woodland", "snow_mean", "T_mean"),
  Mansi     = c("roughness", "T_mean", "P_seasonal"),
  Mari      = c("lake_dist", "P_annual", "T_seasonal"),
  Mordvin   = c("lake_dist", "T_mean", "P_seasonal"),
  Finnic    = c("T_seasonal", "lake_dist", "woodland"),
  Permic    = c("T_seasonal", "dem_mean", "snow_mean"),
  Hungarian = c("soil_quality", "P_annual", "T_mean")
)

# PDP generator with arrow annotation
generate_pdp_with_arrow <- function(model, var, train_data) {
  pd <- partial(model,
                pred.var = var,
                train = train_data,
                prob = TRUE)
  direction <- ifelse(last(pd$yhat) > first(pd$yhat), "???", "???")
  autoplot(pd) +
    labs(
      title = paste("PDP:", var, direction),
      x = var,
      y = "Predicted Suitability"
    ) +
    theme_minimal(base_size = 11)
}

# Loop through languages
for (lang in names(pdp_candidates)) {
  model <- results_list[[lang]]$model
  if (is.null(model)) next
  
  vars <- pdp_candidates[[lang]]
  rf_data <- uralic_rf_d[, c(predictors, lang)]
  rf_data[[lang]] <- factor(uralic_rf_d[[lang]])
  
  pdp_plots <- lapply(vars, function(var) {
    generate_pdp_with_arrow(model, var, rf_data)
  })
  
  # Add language title
  title <- ggdraw() + draw_label(paste("Language:", lang), fontface = 'bold', size = 14)
  combined <- plot_grid(plotlist = pdp_plots, ncol = 3)
  final <- plot_grid(title, combined, ncol = 1, rel_heights = c(0.12, 1))
  
  # Save plot
  ggsave(
    filename = file.path(output_dir, paste0("PDP_3Vars_", lang, ".jpeg")),
    plot = final,
    width = 10, height = 4, dpi = 600
  )
}

