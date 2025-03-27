# -*- coding: utf-8 -*-
"""
Created on Mon Mar 24 08:48:10 2025

@author: drewm
"""

#%%
import os
import pandas as pd
import numpy as np

os.chdir("C:/Users/drewm/Documents/Internship portfolio/3.6. Principles of Research/TSH FT4 Project")

#created csv for each excel sheet in the original data

#create list of all the csv file names (which are inside their own "raw_tft_files" folder)
tft_files = [f for f in os.listdir("raw_tft_files") if f.endswith(".csv")]
print(tft_files)

#create dataframe for each csv file in the list and add to main dataframe
tft_data_frame = []

for tft in tft_files:
    tft_data = pd.read_csv(os.path.join("raw_tft_files", tft))
    tft_data_frame.append(tft_data)
    
#optional: concatenate all dfs into one df
final_tft_file = pd.concat(tft_data_frame, ignore_index = True) 

final_tft_file.head()
final_tft_file.info() 

#save the dataframe as a csv file for safe keeping
final_tft_file.to_csv("TFT_DATA_PROCESSED.csv", index = False)

#%%
#data was filtered in R to remove non-numerical items

#extract different measurements from the data
final_tft_file = pd.read_csv("TFT_FINAL_PROCESSED.csv")

TSH_data = final_tft_file.loc[final_tft_file["TestName"] == "TSH", :]
FT4_data = final_tft_file.loc[final_tft_file["TestName"] == "FT4", :]
FT3_data = final_tft_file.loc[final_tft_file["TestName"] == "FT3 - THYROXINE FREE (T3)", :]

#remove test requests that do not have TSH

options = TSH_data["RequestNumber"]

filtered_FT4 = FT4_data[FT4_data["RequestNumber"].isin(options)]
filtered_FT3 = FT3_data[FT3_data["RequestNumber"].isin(options)]

TSH_options = filtered_FT4["RequestNumber"]
TSH_FT3_options = filtered_FT3["RequestNumber"]

filtered_TSH = TSH_data[TSH_data["RequestNumber"].isin(TSH_options)]
filtered_TSH_for_FT3 = TSH_data[TSH_data["RequestNumber"].isin(TSH_FT3_options)]


#%%
#save the final extracted files as csv for safe keeping

filtered_TSH.to_csv("TSH_extracted.csv", index = False)
filtered_FT4.to_csv("FT4_extracted.csv", index = False)
filtered_FT3.to_csv("FT3_extracted.csv", index = False)
filtered_TSH_for_FT3.to_csv("TSH_extracted_for_FT3.csv", index = False)

#%%
#Tuesday 25/03

TSH_extracted = pd.read_csv("TSH_extracted.csv")
FT4_extracted = pd.read_csv("FT4_extracted.csv")
FT3_extracted = pd.read_csv("FT3_extracted.csv")
TSH_FT3_extracted = pd.read_csv("TSH_extracted_for_FT3.csv")

#sort the values by request number in preparation for joining TSH/FT4 and TSH/FT3
TSH_sorted = TSH_extracted.sort_values(by = ["RequestNumber"], ascending = True)
FT4_sorted = FT4_extracted.sort_values(by = ["RequestNumber"], ascending = True)
FT3_sorted = FT3_extracted.sort_values(by = ["RequestNumber"], ascending = True)
TSH_FT3_sorted = TSH_FT3_extracted.sort_values(by = ["RequestNumber"], ascending = True)

#separate into male and female datasets
#TSH data
TSH_male = TSH_sorted.loc[TSH_sorted["Gender"] == "M", :]
TSH_female = TSH_sorted.loc[TSH_sorted["Gender"] == "F", :]

#FT4 data
FT4_male = FT4_sorted.loc[FT4_sorted["Gender"] == "M", :]
FT4_female = FT4_sorted.loc[FT4_sorted["Gender"] == "F", :]

#FT3 data
FT3_male = FT3_sorted.loc[FT3_sorted["Gender"] == "M", :]
FT3_female = FT3_sorted.loc[FT3_sorted["Gender"] == "F", :]

TSH_FT3_male = TSH_FT3_sorted.loc[TSH_FT3_sorted["Gender"] == "M", :]
TSH_FT3_female = TSH_FT3_sorted.loc[TSH_FT3_sorted["Gender"] == "F", :]

#save as files

TSH_male.to_csv("TSH_male.csv", index = False)
TSH_female.to_csv("TSH_female.csv", index = False)
FT4_male.to_csv("FT4_male.csv", index = False)
FT4_female.to_csv("FT4_female.csv", index = False)

FT3_male.to_csv("FT3_male.csv", index = False)
FT3_female.to_csv("FT3_female.csv", index = False)
TSH_FT3_male.to_csv("TSH_FT3_male.csv", index = False)
TSH_FT3_female.to_csv("TSH_FT3_female.csv", index = False)

#manually join TSH & FT4, and FT3 and TSH-FT3 in excel
#data can be analysed in excel or imported into python again

#%%

#make data numeric

numeric_FT3_male = pd.to_numeric(FT3_male["Result"])
numeric_FT3_female = pd.to_numeric(FT3_female["Result"])
numeric_TSH_male = pd.to_numeric(TSH_male["Result"])
numeric_TSH_female = pd.to_numeric(TSH_female["Result"])
numeric_TSH3_male = pd.to_numeric(TSH_FT3_male["Result"])
numeric_TSH3_female = pd.to_numeric(TSH_FT3_female["Result"])
numeric_FT4_male = pd.to_numeric(FT4_male["Result"])
numeric_FT4_female = pd.to_numeric(FT4_female["Result"])

#get basic data characteristics
display(numeric_FT4_female.describe())

#find hypo, hyper and euthyroid numbers in our dataset based on TSH levels
#hypothyroid is TSH > 4.78 mIU/L and hyperthyroid is TSH < 0.55 mIU/L
hypothyroid = TSH_extracted[TSH_extracted["TSH_result"].values > 4.78]
hyperthyroid = TSH_extracted[TSH_extracted["TSH_result"].values < 0.55]
euthyroid = TSH_extracted[0.55 < TSH_extracted["TSH_result"].values < 4.78]

display(hypothyroid.describe())
display(hyperthyroid.describe())
display(euthyroid.describe())

#determine significance of the means between male and female
import scipy.stats as stats

x = FT4_male["Result"]
y = FT4_female["Result"]

t_stat = stats.ttest_ind(x, y, nan_policy = "omit")
print(t_stat) #repeat for TSH and FT3
