#!/bin/bash

# Usage: ./run_predictions.sh /path/to/models /path/to/newdata.csv /path/to/output.csv

MODEL_DIR="$1"
PREDICTION_DATA="$2"
OUTPUT="$3"

# Run the R script
Rscript predict_all_models.R "$MODEL_DIR" "$PREDICTION_DATA" "$OUTPUT"
