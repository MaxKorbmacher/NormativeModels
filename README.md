# _Brain Reference_: Normative Models for Brain Volume

This repository contains code to apply pre-trained normative models (training n= 62 414) for regional brain volumetrics based on FreeSurfer **recon-all** outputs using the **Desikan-Killiany atlas**.

The models were trained using **multivariate fractional polynomial regression (MFPR)**, following evidence that MFPR provides highly accurate region-level normative models ([Ge et al. 2024](https://doi.org/10.1016/s2589-7500(23)00250-9)).

Model formula:
```
mfp(RegionalVolume ~ EstimatedTotalIntraCranialVol + fp(Age, df=4), data=YourData)
```
Each cortical and subcortical region was modeled separately and sex-specific.

---

## ✨ Quickstart

```bash
# 1. Prepare data with FreeSurfer and merge tables
# 2. Run predictions
./run_predictions.sh /path/to/models /path/to/newdata.csv /path/to/output.csv

# 3. Calculate Z-scores
Rscript calculate_z_scores.R output.csv output_rmse.csv Zscores.csv
```


---

## 📦 Usage

- Prepare cortical volume tables using FreeSurfer and provided scripts (`stats2table_bash.sh`, `merge.py`).
- Add a `sex` column ("female" or "male") to your merged CSV.
- Download pretrained models from [OSF](https://osf.io/6r8dy/).
- Run predictions and calculate Z-scores.

### Example Input Table

| eid | sex    | lh_transversetemporal_volume | rh_transversetemporal_volume |
|-----|--------|------------------------------|------------------------------|
| 001 | female | 4500                         | 4600                         |
| 002 | male   | 4700                         | 4800                         |

---

## 🔎 Components

Scripts included:
- `stats2table_bash.sh` — extract regional stats from FreeSurfer outputs
- `merge.py` — merge extracted stats into a single table
- `run_predictions.sh` — wrapper to call `predict_all_models.R`
- `calculate_z_scores.R` — compute Z-scores
- `calculate_training_rmse.R` — estimate RMSE if retraining models
- `MFPR.R` — model training

---

## 🛤️ Pipeline Overview

### 1. Data Preparation
```bash
# FreeSurfer recon-all
recon-all -s Subject1 -i Subject1_ses-BL_T1w.nii.gz -all

# Extract stats
cd /path/to/FS_output/folders
sh /path/to/stats2table_bash.sh

# Merge stats
python3 merge.py "path/to/tables" "session_id_suffix" "path/to/save"

# Add demographics (sex: "female" or "male")
```

### 2. Prediction
```bash
# Download pretrained models and place them in the working folder
cd /path/to/folder
./run_predictions.sh /path/to/models /path/to/newdata.csv /path/to/output.csv
```

### 3. Z-score Calculation
```bash
# Using predicted values and training RMSE
Rscript calculate_z_scores.R output.csv output_rmse.csv Zscores.csv
```
Z-scores are computed as:
$Z = \frac{y - \hat{y}}{RMSE}$
where $y$ = observed volume, $\hat{y}$ = predicted volume, and RMSE = the root mean squared error of the models in the training data.

---

## 🧩 Requirements

- FreeSurfer (`recon-all`)
- Python 3 (`pandas`, `numpy`)
- R (`mfp`, `tidyverse`)
- CSV or Excel files containing brain regions and demographics
- A `sex` column with values exactly `"female"` or `"male"`

---

## 📚 Notes

- Subcortical volumes are currently not merged automatically in the provided merge.py script; the focus is on cortical volumes. However, the models are provided for both.
- You can retrain models using `MFPR.R` and recompute RMSE with `calculate_training_rmse.R`.
- Provided `output_rmse.csv` can be used if you skip retraining.

---

_This repository was improved in readability and structure with assistance from ChatGPT._  
_Last update: September 2025._

_Please cite_  

"Normative Modelling of Brain Volume for Diagnostic and Prognostic Stratification in Multiple Sclerosis"
Max Korbmacher, Ingrid Anne Lie, Kristin Wesnes, Eric Westman, Thomas Espeseth, Ole Andreas Andreassen, Lars T. Westlye, Stig Wergeland, Hanne Flinstad Harbo, Gro Owren Nygaard, Kjell-Morten Myhr, Einar August Høgestøl, Øivind Torkildsen, Alzheimer’s Disease Neuroimaging Initiative
doi: https://doi.org/10.1101/2025.09.14.25335702 

_when using contents from thos repository._
