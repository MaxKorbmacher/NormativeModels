#!/usr/bin/env Rscript

# ---- Libraries ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(mfp)
  library(future)
  library(furrr)
})

# ---- Parse arguments ----
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Usage: Rscript predict_all_models_parallel.R /path/to/models /path/to/newdata.csv /path/to/output.csv")
}

model_dir <- args[1]
newdata_path <- args[2]
output_path <- args[3]

cat("🔵 Model folder:", model_dir, "\n")
cat("🔵 Prediction data:", newdata_path, "\n")
cat("🔵 Output will be saved to:", output_path, "\n\n")

# ---- Load prediction data ----
newdata <- read.csv(newdata_path)

# ---- Find all model files ----
model_files <- list.files(model_dir, pattern = "\\.Rdata$", full.names = TRUE)

# ---- Prepare results ----
prediction_results <- newdata  # Keep ALL original columns

# ---- Define safe prediction function ----
safe_predict <- function(model, newdata) {
  model_vars <- all.vars(model$terms)
  
  missing_vars <- setdiff(model_vars, names(newdata))
  if (length(missing_vars) > 0) {
    stop(paste0("❌ Missing variables for prediction: ", paste(missing_vars, collapse = ", ")))
  }
  
  prediction_data <- newdata[, model_vars, drop = FALSE]
  
  if (any(is.na(prediction_data))) {
    warning("⚠️ Prediction data contains NAs.")
  }
  
  preds <- predict(model, newdata = prediction_data)
  
  if (all(is.na(preds))) {
    warning("⚠️ All predictions are NA.")
  }
  
  return(preds)
}

# ---- Set up parallelization ----
plan(multisession, workers = availableCores())

# ---- Main parallel loop ----
predictions_list <- future_map(model_files, function(model_file) {
  
  # Load model
  env <- new.env()
  load(model_file, envir = env)
  model_name <- ls(env)[1]
  model <- env[[model_name]]
  
  # Determine if model is for male or female
  model_sex <- ifelse(str_detect(basename(model_file), "female"), 0,
                      ifelse(str_detect(basename(model_file), "male"), 1, NA))
  
  if (is.na(model_sex)) {
    warning(paste("⚠️ Could not determine sex for", model_file))
    return(NULL)
  }
  
  # Subset newdata according to sex
  subdata <- newdata %>% filter(sex == model_sex)
  
  # Predict
  pred_column_name <- str_replace(basename(model_file), "\\.Rdata$", "")
  pred_column_name <- str_replace_all(pred_column_name, " ", "_")
  
  preds <- tryCatch({
    safe_predict(model, subdata)
  }, error = function(e) {
    warning(paste("⚠️ Prediction failed for", model_file, ":", e$message))
    rep(NA, nrow(subdata))
  })
  
  tibble(eid = subdata$eid, pred_column = pred_column_name, pred_value = preds)
  
}, .progress = TRUE)

# ---- Merge all predictions ----
all_preds <- bind_rows(predictions_list)

if (nrow(all_preds) > 0) {
  all_preds_wide <- all_preds %>%
    pivot_wider(names_from = pred_column, values_from = pred_value)
  
  prediction_results <- left_join(prediction_results, all_preds_wide, by = "eid")
}

# ---- Save results ----
cat("💾 Saving all predictions to:", output_path, "\n")
write.csv(prediction_results, output_path, row.names = FALSE)
cat("🎉 Parallel prediction completed successfully.\n")
