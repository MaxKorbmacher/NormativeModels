#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Usage: Rscript calculate_z_scores.R /path/to/predictions.csv /path/to/rmse_training.csv /path/to/output_zscores.csv")
}

predictions_path <- args[1]
rmse_path <- args[2]
output_path <- args[3]

cat("🔵 Predictions file:", predictions_path, "\n")
cat("🔵 RMSE file:", rmse_path, "\n")
cat("🔵 Output Z-scores to:", output_path, "\n\n")

# ---- Load files ----
predictions <- read_csv(predictions_path)
rmse_table <- read_csv(rmse_path)

# ---- Prepare ----
zscore_results <- predictions

# ---- For each model prediction column ----
prediction_cols <- names(predictions) %>% 
  str_subset("_predicted$")  # columns ending with _predicted

# Loop over each prediction column
for (pred_col in prediction_cols) {
  
  # Extract base model name (without _predicted)
  base_col <- str_remove(pred_col, "_predicted$")
  
  # Check if observed column exists
  if (!(base_col %in% names(predictions))) {
    warning(paste("⚠️ Observed column", base_col, "not found. Skipping."))
    next
  }
  
  # Find corresponding RMSE
  rmse_row <- rmse_table %>% filter(str_detect(model_name, fixed(base_col)))
  
  if (nrow(rmse_row) == 0) {
    warning(paste("⚠️ No RMSE found for", base_col, "in RMSE table. Skipping."))
    next
  }
  
  training_rmse <- rmse_row$RMSE[1]
  
  # Calculate Z-score
  z_col_name <- paste0(base_col, "_Zscore")
  zscore_results[[z_col_name]] <- (zscore_results[[base_col]] - zscore_results[[pred_col]]) / training_rmse
  
  cat("✅ Calculated Z-scores for", base_col, "\n")
}

# ---- Save ----
cat("💾 Saving Z-scores to:", output_path, "\n")
write_csv(zscore_results, output_path)

cat("🎉 Z-score calculation completed successfully.\n")
