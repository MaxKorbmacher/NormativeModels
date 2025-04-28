#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript calculate_training_rmse.R /path/to/models /path/to/output_rmse.csv")
}

model_dir <- args[1]
output_rmse_path <- args[2]

cat("🔵 Model folder:", model_dir, "\n")
cat("🔵 Output RMSEs to:", output_rmse_path, "\n\n")

model_files <- list.files(model_dir, pattern = "\\.Rdata$", full.names = TRUE)

rmse_results <- tibble(model_name = character(), RMSE = numeric())

for (i in seq_along(model_files)) {
  model_file <- model_files[i]
  
  cat("🔵 Processing model", i, "of", length(model_files), ":", basename(model_file), "...\n")
  
  # Load model
  env <- new.env()
  load(model_file, envir = env)
  model_name_in_file <- ls(env)[1]
  model <- env[[model_name_in_file]]
  
  # Extract
  outcome_var <- all.vars(model$formula)[1]
  observed <- model$model[[outcome_var]]
  fitted <- model$fitted.values
  
  # Calculate RMSE
  rmse_value <- sqrt(mean((observed - fitted)^2))
  
  rmse_results <- rmse_results %>% add_row(model_name = model_name_in_file, RMSE = rmse_value)
  
  cat("✅ RMSE for", model_name_in_file, ":", round(rmse_value, 4), "\n")
}

cat("💾 Saving RMSE table to:", output_rmse_path, "\n")
write.csv(rmse_results, output_rmse_path, row.names = FALSE)

cat("🎉 Training RMSE calculation completed successfully.\n")
