# Simple script to merge FS volume output tables
# Author: Max Korbmacher
# Adapted April 2025
#
########################
# Run as:
# python3 merge_volume_only.py "path/to/tables/" "session_id_suffix" "path/to/save/"
# Output will be saved as session_id_suffix.csv
#

import pandas as pd
import numpy as np
from functools import reduce
import os
import sys

# ---- Parse arguments ----
T1path = sys.argv[1]  # e.g., "/path/to/output/"
proc = sys.argv[2]    # e.g., "20220209165345"
savepath = sys.argv[3] # e.g., "/path/to/save/"

# ---- Initialize ----
dict = {}
df_MRI_list = []
hem_list = ["lh", "rh"]
i = 0

# ---- Only volume files ----
MRI_list = ["aparc_stats_volume"]

# ---- Load volume files ----
for MRI in MRI_list:
    for hem in hem_list:
        file_path = os.path.join(T1path, f"{MRI}_{hem}_{proc}.csv")
        df = pd.read_csv(file_path, delimiter=",")
        
        df = df.rename(index=str, columns={f"{hem}.{MRI}": 'eid'})
        df = df.replace({'FS_': ''}, regex=True)
        df['eid'] = df[df.columns[0]]  # assign first column to eid
        df.drop(columns=df.columns[0], axis=1, inplace=True)
        
        # Quick fixes (if BrainSegVolNotVent and eTIV exist but we don't need duplicates)
        if 'BrainSegVolNotVent' in df.columns:
            df.drop('BrainSegVolNotVent', axis=1, inplace=True)
        if 'eTIV' in df.columns:
            df.drop('eTIV', axis=1, inplace=True)
        
        df_MRI_list.append(df)
        print(df.head(5))
        i += 1

# ---- Merge all volume data frames ----
df_MRI_merged = reduce(lambda left, right: pd.merge(left, right, on='eid'), df_MRI_list)

print('✅ Merged MRI columns:', len(df_MRI_merged.columns))
print('✅ Preview merged data:')
print(df_MRI_merged.head(4))
print('✅ Columns:')
print(df_MRI_merged.columns)

df = df_MRI_merged

print('✅ Number of subjects in final file:', len(df))
print(df.head(5))

# ---- Save merged file ----
output_file = os.path.join(savepath, f"{proc}.csv")
print(f"💾 Writing output CSV to: {output_file}")
df.to_csv(output_file, sep=',', index=None)
