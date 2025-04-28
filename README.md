# Normative Models for Brain volume

This repository contains code to apply trained normative models for regional volumetrics based on vanilla FreeSurfer recon-all pipeline outputs using the Desikan Killiany atlas.

The models were trained using multivariate fractional polynomial regression (MFPR), which has previously been suggested to provide the most accurate region-level normative models (https://doi.org/10.1016/s2589-7500(23)00250-9). Following the authors' suggestion, we use a fractional polynomial up to the fourth degree of freedom and a linear relationship with the total intracranial volume to prediction the regional brain volume:
```
mfp(RegionalVolume~EstimatedTotalIntraCranialVol+fp(Age,df=4),data=YourData)
```
One model per cortical and subcortical region was trained. These models are sex-specific -- they were trained for males and females separately.

Here, we provide different script leading you all the way from data processing to loading pre-trained models from a specified directory (you can download them to), applying them to a dataset (of your choice) based on the sex column, saving predictions and Z-scores into new CSV files. A detailed overview of scripts and how to use them can be found below.

The probably most intersting part are the Z-scores which indicate deviations from the norm, and are estimated as follows:


$\frac{y-\hat{y}}{RMSE}$

## What are the moving parts?
- stats2table_bash.sh and merge.py are to prepare the recon-all outputs
- The wrapper run_predictions.sh calls predict_all_models.R to obtain (raw) predictions
- calculate_z_scores.R to obtain Z-scores
- calculate_training_rmse.R is necessary to get the Z-scores, as they are defined as (y-y_hat)/RMSE
- MFPR.R contains the training procedure


## How does this work?
### Prerequisites
0.1 Prerequisites: recon-all, model training (estimatimate RMSE or leave the provided output_rmse.csv untouched)
```
recon-all -s Subject1 -i Subject1_ses-BL_T1w.nii.gz -all
Rscript MFPR.R
Rscript calculate_training_rmse.R /path/to/models /path/to/output_rmse.csv
```
0.2 You can merge the FreeSurfer produced tables (stats files) using stats2table_bash.sh
```
cd /path/with/FS/ouput/folders
sh /path/to/stats2table_bash.sh
```
0.3 Now, the resulting tables need to be merged (again!) into a single table including all participants. There is a provided merge.py script in the repository that can be used for that purpose. Note, this script is only used for cortical and _not_ subcortical volumes. Run this from your terminal:
```
python3 merge.py "path/where/recon-all/output/tables/are"
```
0.4 Add respective demographics, etc. **Necessary**: The participant's sex labelled as "female" or "male" (_not_ "Female", not 0 or 1 or any other sort of thing).
0.5 Download the models and code and put them all into the same folder
0.6 Now, navigate to the folder containing ALL the code and models using your terminal running
```
cd path/to/the/folder
```
### The actual pipeline: super simple
1. To obtain predictions on newdata.csv, and a nice output.csv putting it all together, run:
```
./run_predictions.sh /path/to/models /path/to/newdata.csv /path/to/output.csv
```
2.  Now, you can use the resulting predictions to estimate Z scores. These can indicate your norm-deviations.
```
Rscript calculate_z_scores.R output.csv output_rmse.csv Zscores.csv
```
