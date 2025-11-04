# 2.6.2025 checking

results_list <- list()

for (lang in response_var) {
  cat("\nRunning model for", lang, "...\n")
  
  # Skip if only one class exists
  if (length(unique(df[[lang]])) < 2) {
    cat("?????? Skipping", lang, "- only one class present\n")
    next
  }
  
  # Run with error-catching
  result <- tryCatch({
    run_rf_language_model(df, lang, predictors, output_dir)
  }, error = function(e) {
    cat("??? Error for", lang, ":", conditionMessage(e), "\n")
    NULL
  })
  
  results_list[[lang]] <- result
}


# Summary of AUCs and thresholds
auc_summary <- data.frame(
  Language = response_var,
  Train_AUC = sapply(results_list, function(x) round(x$train_auc, 3)),
  Test_AUC = sapply(results_list, function(x) round(x$test_auc, 3)),
  Optimal_Threshold = sapply(results_list, function(x) round(x$optimal_threshold, 3))
)

print(auc_summary)
write.csv(auc_summary, file = file.path(output_dir, "AUC_Threshold_Summary.csv"), row.names = FALSE)


# Performance summary table
model_summary <- data.frame(
  Language = character(),
  Grid_Cells = integer(),
  OOB_Error_Percent = numeric(),
  Variance_Explained = character(),
  Test_AUC = numeric(),
  stringsAsFactors = FALSE
)

for (lang in names(results_list)) {
  res <- results_list[[lang]]
  if (!is.null(res)) {
    n <- nrow(res$model$predictions)  # number of training rows
    oob <- res$model$prediction.error * 100
    variance_expl <- round(100 - oob, 2)
    model_summary <- rbind(model_summary, data.frame(
      Language = lang,
      Grid_Cells = n,
      OOB_Error_Percent = round(oob, 1),
      Variance_Explained = variance_expl,
      Test_AUC = round(res$test_auc, 3)
    ))
  }
}

# Order by language
model_summary <- model_summary[order(model_summary$Language), ]

# Preview
print(model_summary)

# Save
write.csv(model_summary, file = file.path(output_dir,"Model_Performance_Summary.csv"), row.names = FALSE)

# Model evaluation summary
library(caret)


Grid_cells = nrow(rf_data)
Test_Cells = nrow(test_data)

# Initializing summary table
eval_summary <- data.frame()

for (lang in response_var) {
  res <- results_list[[lang]]
  if (!is.null(res)) {
    
    # Recreate data
    rf_data <- df[, c(predictors, lang)]
    rf_data[[lang]] <- factor(rf_data[[lang]], levels = c(0, 1))
    
    # Store grid cell count
    grid_cells <- nrow(rf_data)
    
    # Split into test set using the same seed
    set.seed(123)
    train_idx <- createDataPartition(rf_data[[lang]], p = 0.8, list = FALSE)
    test_data <- rf_data[-train_idx, ]
    
    # Predict probabilities and convert to classes using threshold
    probs <- predict(res$model, data = test_data)$predictions[, "1"]
    labels <- test_data[[lang]]
    preds <- ifelse(probs > res$optimal_threshold, 1, 0)
    
    # Confusion matrix
    cm <- confusionMatrix(as.factor(preds), as.factor(labels), positive = "1")
    
    # F1 Score
    precision <- cm$byClass["Pos Pred Value"]
    recall <- cm$byClass["Sensitivity"]
    f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
      2 * (precision * recall) / (precision + recall)
    } else {
      NA
    }
    
    # Add row to summary
    eval_summary <- rbind(eval_summary, data.frame(
      Language = lang,
      Grid_cells = grid_cells,
      Test_Cells = nrow(test_data),
      Accuracy = round(cm$overall["Accuracy"], 3),
      Sensitivity = round(cm$byClass["Sensitivity"], 3),
      Specificity = round(cm$byClass["Specificity"], 3),
      Balanced_Accuracy = round(cm$byClass["Balanced Accuracy"], 3),
      Kappa = round(cm$overall["Kappa"], 3),
      R2 = round(cor(as.numeric(as.character(labels)), probs)^2, 3),
      F1 = round(f1, 3),
      AUC = round(res$test_auc, 3)
    ))
  }
}

# Preview
print(eval_summary)


# View or export
print(results_list)
rownames(eval_summary) <- NULL
print(eval_summary)
write.csv(eval_summary, file = file.path(output_dir,"Model_Evaluation_Summary.csv"), row.names = FALSE)


# combine all metrics to one
# Ensuring all summaries use consistent Language naming
eval_summary$Language <- as.character(eval_summary$Language)
auc_summary$Language <- as.character(auc_summary$Language)
model_summary$Language <- as.character(model_summary$Language)

# Optional: drop duplicate AUC from eval_summary (we keep the one from auc_summary)
eval_summary <- eval_summary %>% select(-AUC)

# Merge tables by Language
combined_summary <- model_summary %>%
  left_join(auc_summary[, c("Language", "Train_AUC", "Optimal_Threshold")], by = "Language") %>%
  left_join(eval_summary, by = "Language")
rm(Grid_Cells)

# Reorder and rename for clarity
combined_summary <- combined_summary %>%
  select(all_of(c(
    "Language", "Grid_cells", "Test_Cells", 
    "OOB_Error_Percent", "Variance_Explained",
    "Train_AUC", "Test_AUC", "Optimal_Threshold",
    "Accuracy", "Balanced_Accuracy", "F1", 
    "Sensitivity", "Specificity", "R2", "Kappa"
  )))

# Preview
print(combined_summary)

write.csv(combined_summary,file = file.path(output_dir, "combined_model_evaluation_summary.csv"), row.names = FALSE)

dev.off()
