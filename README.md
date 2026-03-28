# Property Data Cleaning Analysis in R

This project focuses on cleaning, transforming, merging, and analysing Irish property transaction data using R.

## Project Overview

The project uses two property datasets and demonstrates a full analyst workflow:

- data cleaning and standardisation
- missing value handling and imputation
- outlier detection and removal
- feature engineering
- dataset merging
- exploratory data analysis (EDA)
- answering assignment-style business questions

## Main Work Completed

### 1. Data Cleaning
- converted blanks to missing values
- standardised text values and county names
- parsed date fields
- cleaned bedroom and bathroom information
- normalised values such as parking, VAT, and not full market price

### 2. Feature Engineering
- created sale month, year, and quarter
- derived property age
- extracted property type
- created property size categories

### 3. Imputation
- used geohash-based filling for stable fields in Dataset 1
- used county-level mode imputation for missing bedrooms, bathrooms, and property size

### 4. Data Integration
- aligned the structure of both datasets
- merged them into one cleaned dataset

### 5. Exploratory Data Analysis
- applied log transformation to handle price skewness
- analysed price distributions across counties
- examined price trends over time
- studied the relationship between price and property characteristics

## Files in this Repository

- `scripts/property_data_cleaning_analysis.R` — main cleaned R script
- `data/PropertyDataset1.csv` — input dataset 1
- `data/PropertyDataset2.csv` — input dataset 2

## Tools Used

- R
- dplyr
- tidyr
- stringr
- lubridate
- readr
- ggplot2

## Why This Project Matters

This project shows my ability to work with messy real-world style data, clean and transform it systematically, combine multiple datasets, and generate useful insights through analysis and visualisation.

## Note

This project was originally completed as part of an academic assignment and has been lightly polished for portfolio presentation while retaining my original workflow and logic.
