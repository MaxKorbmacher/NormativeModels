# Normative Models for Brain volume

This repository contains code to apply trained normative models for regional volumetrics based on vanilla FreeSurfer recon-all pipeline outputs using the Desikan Killiany atlas.

The models were trained using multivariate fractional polynomial regression (MFPR), which has previously been suggested to provide the most accurate region-level normative models (https://doi.org/10.1016/s2589-7500(23)00250-9). Following the authors' suggestion, we use a fractional polynomial up to the fourth degree of freedom and a linear relationship with the total intracranial volume to prediction the regional brain volume:

RegionalVolume~EstimatedTotalIntraCranialVol+fp(Age,df=4)

One model per cortical and subcortical region was trained. These models are sex-specific -- they were trained for males and females separately.

Here, we provide a wrapper script (run_predictions.sh) that loads the pre-trained models from a specified directory (you can download them to), applies them to a dataset (of your choice) based on the sex column, and saves the predictions in a new CSV file. It expects three arguments: the path to the model directory, the path to the new data CSV, and the path for the output CSV. The script checks for missing variables, handles errors, and generates predictions for each model, saving them in separate columns.

The resulting predictions are then used to estimate Z scores.
