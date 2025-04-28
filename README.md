# Normative Models for Brain volume

This repository contains code to apply trained normative models for regional volumetrics based on vanilla FreeSurfer recon-all pipeline outputs using the Desikan Killiany atlas.

The models were trained using multivariate fractional polynomial regression (MFPR), which has previously been suggested to provide the most accurate region-level normative models (https://doi.org/10.1016/s2589-7500(23)00250-9). Following the authors' suggestion, we use a fractional polynomial up to the fourth degree of freedom and a linear relationship with the total intracranial volume to prediction the regional brain volume:
```
mfp(RegionalVolume~EstimatedTotalIntraCranialVol+fp(Age,df=4),data=YourData)
```
One model per cortical and subcortical region was trained. These models are sex-specific -- they were trained for males and females separately.

Here, we provide a wrapper script (run_predictions.sh or run_parallel.sh) that loads the pre-trained models from a specified directory (you can download them to), applies them to a dataset (of your choice) based on the sex column, and saves the predictions in a new CSV file. It expects three arguments: the path to the model directory, the path to the new data CSV, and the path for the output CSV. The script checks for missing variables, handles errors, and generates predictions for each model, saving them in separate columns.

## What are the moving parts?
- The wrapper run_predictions.sh calls predict_all_models.R >> use this unless you have crazy loads of data
- For parallelisation run run_parallel.sh predict_all_models_parallel.R
- MFPR.R contains the training procedure
- stats2table_bash.sh and merge.py are to prepare the recon-all outputs

## How does this work?
1. Run your recon-all and put all the output data into one table, together with the participant's sex labelled as "female" or "male" (not "Female", not 0 or 1 or any other sort of thing).
2. You can merge the FreeSurfer produced tables (stats files) using stats2table_bash.sh
3. Now, the resulting tables need to be merged (again!) into a single table including all participants. There is a provided merge.py script in the repository that can be used for that purpose: run 'python3 merge.py "path/where/recon-all/output/tables/are"' (without apostrophes) from your terminal. Note, this script is only used for cortical and _not_ subcortical volumes.
6. Now, navigate to the folder containing ALL the code using your terminal running 'cd path/to/the/folder'.
7. run './run_predictions.sh /path/to/models /path/to/newdata.csv /path/to/output.csv'

## What do you get?
A new dataframe containing all your old data and the new predictions together.

## Next steps?
Now, you can use the resulting predictions to estimate Z scores. These can indicate your norm deviations.

