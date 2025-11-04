run_rf_language_model <- function(df, response_var, predictors, output_dir = ".") {
  library(ranger)
  library(pROC)
  library(caret)
  
  set.seed(123)
  
  rf_data <- df[, c(predictors, response_var)]
  rf_data[[response_var]] <- factor(rf_data[[response_var]], levels = c(0, 1))
  
  train_index <- createDataPartition(rf_data[[response_var]], p = 0.8, list = FALSE)
  train_data <- rf_data[train_index, ]
  test_data <- rf_data[-train_index, ]
  
  class_proportions <- table(train_data[[response_var]]) / nrow(train_data)
  class_weights <- ifelse(as.numeric(train_data[[response_var]]) == 1,
                          1 / class_proportions["1"],
                          1 / class_proportions["0"])
  
  mtry_values <- seq(2, floor(sqrt(length(predictors))), by = 1)
  ntree_values <- seq(100, 1000, by = 100)
  tuning_grid <- expand.grid(mtry = mtry_values, ntree = ntree_values)
  
  best_oob <- Inf
  best_params <- list()
  
  for (i in 1:nrow(tuning_grid)) {
    rf_tune <- try(ranger(
      dependent.variable.name = response_var,
      data = train_data,
      mtry = tuning_grid$mtry[i],
      num.trees = tuning_grid$ntree[i],
      importance = "impurity",
      probability = TRUE,
      case.weights = class_weights
    ), silent = TRUE)
    
    if (!inherits(rf_tune, "try-error") && rf_tune$prediction.error < best_oob) {
      best_oob <- rf_tune$prediction.error
      best_params <- list(mtry = tuning_grid$mtry[i], ntree = tuning_grid$ntree[i])
    }
  }

  rf_model <- ranger(
    dependent.variable.name = response_var,
    data = train_data,
    mtry = best_params$mtry,
    num.trees = best_params$ntree,
    importance = "impurity",
    probability = TRUE,
    case.weights = class_weights
  )
  
  train_pred_probs <- predict(rf_model, data = train_data)$predictions[, "1"]
  roc_train <- roc(as.numeric(train_data[[response_var]]) - 1, train_pred_probs)
  youden_index <- roc_train$sensitivities + roc_train$specificities - 1
  optimal_threshold <- roc_train$thresholds[which.max(youden_index)]
  
  test_pred_probs <- predict(rf_model, data = test_data)$predictions
  test_pred_class <- ifelse(test_pred_probs[, "1"] > optimal_threshold, 1, 0)
  confusion <- confusionMatrix(as.factor(test_pred_class), as.factor(test_data[[response_var]]))
  roc_test <- roc(as.numeric(test_data[[response_var]]) - 1, test_pred_probs[, "1"])
  
  jpeg(file.path(output_dir, paste0("ROC_", response_var, ".jpeg")), width = 800, height = 600)
  plot(roc_train, col = "blue", lwd = 2, main = paste("ROC -", response_var))
  plot(roc_test, col = "red", lwd = 2, add = TRUE)
  legend("bottomright",
         legend = c(paste("Train AUC =", round(auc(roc_train), 3)),
                    paste("Test AUC =", round(auc(roc_test), 3))),
         col = c("blue", "red"), lwd = 2)
  dev.off()
  
  cm_table <- as.data.frame(confusion$table)
  write.csv(cm_table, file = file.path(output_dir, paste0("ConfusionMatrix_", response_var, ".csv")), row.names = FALSE)
  
  var_imp <- data.frame(Variable = names(rf_model$variable.importance),
                        Importance = rf_model$variable.importance)
  write.csv(var_imp, file = file.path(output_dir, paste0("VarImp_", response_var, ".csv")), row.names = FALSE)
  
  return(list(
    model = rf_model,
    confusion_matrix = confusion,
    train_auc = auc(roc_train),
    test_auc = auc(roc_test),
    optimal_threshold = optimal_threshold,
    variable_importance = var_imp
  ))
}
