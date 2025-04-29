#!/usr/bin/env Rscript

# ---- Libraries ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(mfp)
})

# ---- Parse arguments ----
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Usage: Rscript predict_all_models.R /path/to/models /path/to/newdata.csv /path/to/output.csv")
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
  
  preds <- rep(NA, nrow(prediction_data))  # default to NAs
  if (any(complete.cases(prediction_data))) {
    preds[complete.cases(prediction_data)] <- predict(model, newdata = prediction_data[complete.cases(prediction_data), ])
  }
  
  if (all(is.na(preds))) {
    warning("⚠️ All predictions are NA.")
  }
  
  return(preds)
}

# ---- Main loop ----
for (i in seq_along(model_files)) {
  model_file <- model_files[i]
  
  cat("🔵 Loading model", i, "of", length(model_files), ":", basename(model_file), "...\n")
  
  # Load model
  env <- new.env()
  load(model_file, envir = env)
  model_name <- ls(env)[1]
  model <- env[[model_name]]
  
  # Determine if model is for male or female
  model_sex <- ifelse(str_detect(basename(model_file), "female"), "female",
                      ifelse(str_detect(basename(model_file), "male"), "male", NA))
  
  if (is.na(model_sex)) {
    warning(paste("⚠️ Could not determine sex for", model_file))
    next
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
  
  # Add predictions to result dataframe
  if (!(pred_column_name %in% names(prediction_results))) {
    prediction_results[[pred_column_name]] <- NA  # Pre-fill with NA if column does not exist yet
  }
  
  # ✅ Fix for NA in sex:
  sex_mask <- !is.na(prediction_results$sex) & prediction_results$sex == model_sex
  prediction_results[sex_mask, pred_column_name] <- preds
  
  cat("✅ Done predicting", pred_column_name, "\n\n")
}

# ---- Merge male and female prediction columns ----

# Identify prediction columns that have _male or _female suffix
pred_cols_male <- grep("_male$", names(prediction_results), value = TRUE)
pred_cols_female <- grep("_female$", names(prediction_results), value = TRUE)

# Extract the base names (without _male/_female)
base_pred_cols <- unique(str_replace(c(pred_cols_male, pred_cols_female), "_(male|female)$", ""))

# For each base column, merge male and female predictions into one
for (col_base in base_pred_cols) {
  male_col <- paste0(col_base, "_male")
  female_col <- paste0(col_base, "_female")
  
  # Create the unified column
  prediction_results[[paste0(col_base, "_predicted")]] <- coalesce(
    prediction_results[[female_col]],
    prediction_results[[male_col]]
  )
}

# Optionally: Remove the old _male and _female columns
prediction_results <- prediction_results %>%
  select(-all_of(c(pred_cols_male, pred_cols_female)))

cat("🔵 Merged male and female prediction columns into unified _predicted columns.\n\n")

# ---- Save results ----
cat("💾 Saving all predictions to:", output_path, "\n")
write.csv(prediction_results, output_path, row.names = FALSE)
cat("🎉 Prediction completed successfully.\n")
