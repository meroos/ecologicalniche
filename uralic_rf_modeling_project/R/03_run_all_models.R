library(caret)
source("R/02_rf_model_function.R")

df <- readRDS("data/uralic_rf_cleaned.RDS")
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

predictors <- c("T_mean", "T_seasonal", "P_annual", "P_seasonal", 
                "snow_mean", "permafrost", "dem_mean", "roughness", 
                "river_length", "lake_dist", "sea_dist", "woodland", 
                "swamp", "soil_quality", "biodiversity")

response_var <- c("Finnic", "Hungarian", "Khanty", "Mansi", "Mordvin", 
                  "Mari", "Permic", "Saami", "Samoyedic")

results_list <- list()

for (lang in response_var) {
  cat("\nRunning model for:", lang, "\n")
  tryCatch({
    results_list[[lang]] <- run_rf_language_model(df, lang, predictors, output_dir)
  }, error = function(e) {
    message(paste("<U+26A0><U+FE0F>  Skipping", lang, "due to error:", e$message))
  })
}

saveRDS(results_list, "output/results_list.RDS")
