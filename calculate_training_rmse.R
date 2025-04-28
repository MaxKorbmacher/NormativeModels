#!/usr/bin/env Rscript

# ---- Libraries ----
suppressPackageStartupMessages({
  library(tidyverse)
})

# ---- Parse arguments ----
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript calculate_rmse_mfp.R /path/to/models /path/to/output_rmse.csv")
}

model_dir <- args[1]
output_path <- args[2]

# ---- Find all model files ----
model_files <- list.files(model_dir, pattern = "\\.Rdata$", full.names = TRUE)

# ---- Prepare RMSE results ----
rmse_results <- tibble()

# ---- Main loop ----
for (model_file in model_files) {
  cat("🔵 Loading model:", basename(model_file), "\n")
  
  # Load model
  env <- new.env()
  load(model_file, envir = env)
  
  model_name <- ls(env)[1]
  model <- env[[model_name]]
  
  # Check that model$y exists
  if (is.null(model$y)) {
    warning(paste0("⚠️ No $y (outcome) stored in model: ", model_file))
    next
  }
  
  observed <- model$y
  
  # Predict on training data (no newdata)
  preds <- tryCatch({
    predict(model)
  }, error = function(e) {
    warning(paste0("⚠️ Prediction failed for ", model_file, ": ", e$message))
    return(rep(NA, length(observed)))
  })
  
  # Remove missing
  valid_idx <- complete.cases(preds, observed)
  
  if (sum(valid_idx) == 0) {
    warning(paste0("⚠️ No valid cases after removing NA for ", model_file))
    next
  }
  
  # Calculate RMSE
  rmse <- sqrt(mean((observed[valid_idx] - preds[valid_idx])^2))
  
  # Save RMSE
  rmse_results <- bind_rows(rmse_results, tibble(
    model_file = basename(model_file),
    model_name = model_name,
    RMSE = rmse
  ))
  
  cat("✅ RMSE for", basename(model_file), "=", round(rmse, 4), "\n\n")
}

# ---- Save RMSE results ----
if (nrow(rmse_results) == 0) {
  cat("⚠️ No RMSE results calculated. Check your models.\n")
} else {
  cat("💾 Saving RMSE table to:", output_path, "\n")
  write_csv(rmse_results, output_path)
  cat("🎉 RMSE calculation completed successfully.\n")
}
