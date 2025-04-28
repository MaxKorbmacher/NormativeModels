#!/usr/bin/env Rscript

# ---- Libraries ----
suppressPackageStartupMessages({
  library(tidyverse)
})

# ---- Parse arguments ----
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Usage: Rscript calculate_z_scores.R /path/to/predictions.csv /path/to/rmse_results.csv /path/to/output.csv")
}

predictions_path <- args[1]  # Path to the predictions file
rmse_path <- args[2]         # Path to the RMSE results
output_path <- args[3]       # Path to save the output

cat("🔵 Prediction data:", predictions_path, "\n")
cat("🔵 RMSE data:", rmse_path, "\n")
cat("🔵 Output will be saved to:", output_path, "\n\n")

# ---- Load prediction data ----
predictions <- read.csv(predictions_path)

# ---- Load RMSE results ----
rmse_results <- read.csv(rmse_path)

# Check the column names of the predictions and RMSE results
cat("Columns in predictions data:", colnames(predictions), "\n")
cat("Columns in RMSE results:", colnames(rmse_results), "\n")

# ---- Initialize Z-scores result ----
z_scores_results <- predictions  # Start with the predictions data

# Loop through each row in the RMSE results
for (i in 1:nrow(rmse_results)) {
  # Extract model name and RMSE value from the current row
  model_file <- rmse_results$model_file[i]
  rmse_value <- rmse_results$RMSE[i]
  
  # Remove '_female.Rdata' or '_male.Rdata' from model_file to get the observed column
  observed_col <- str_replace(model_file, "_(female|male)\\.Rdata$", "")
  pred_col <- paste0(observed_col, "_predicted")
  
  # Check if both observed and predicted columns exist in the predictions data
  if (observed_col %in% colnames(predictions) && pred_col %in% colnames(predictions)) {
    cat("Processing observed and predicted columns:", observed_col, "and", pred_col, "\n")
    
    # Calculate Z-scores: Z = (observed - predicted) / RMSE
    z_scores_results[[paste0(observed_col, "_z_score")]] <- 
      (predictions[[observed_col]] - predictions[[pred_col]]) / rmse_value
  } else {
    cat("⚠️ Columns for", observed_col, "or", pred_col, "not found in predictions data.\n")
  }
}

# ---- Save the result with Z-scores ----
cat("💾 Saving Z-scores to:", output_path, "\n")
write.csv(z_scores_results, output_path, row.names = FALSE)
cat("🎉 Z-scores calculated and saved successfully.\n")
