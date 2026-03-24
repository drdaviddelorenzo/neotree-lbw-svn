# Question 10: Length of Stay for Survivors

## Question

What is the average length of stay (LOS) for babies who **survive to discharge**, distributed by birthweight categories (LBW) and SGA categories (SVN)?

## Script

`Q10_length_of_stay_survivors.R`

## What This Script Does

This script analyzes `lengthofstay` (in Days) for **surviving newborn admissions**:

1. **Filters for Survival**: Excludes deaths (NND, Stillbirths, Died). Only babies with "Alive" status are included.
2. **Filters Outliers**: Excludes stays > 90 days (configurable `MAX_LOS_DAYS`).
    * **Note**: The number of excluded outliers is reported in the output tables.
3. **Stratifies data** by:
    * **Birthweight Categories**: ELBW, VLBW, LBW, NBW, HBW
    * **SGA Categories**: Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA
4. **Calculates statistics**: Mean, Median, SD, IQR (Q1, Q3), Min, Max LOS.

## variable Selection & Investigation

We investigated two potential variables for Length of Stay: `timespent` and `lengthofstay`.

* **`timespent`**: Found to be **unsuitable**.
  * Correlation between `timespent` and the actual duration (Discharge Time - Admission Time) was **~0.04** (near zero).
  * This variable likely represents data entry time or another administrative duration.
* **`lengthofstay`**: Found to be the **correct variable**.
  * Correlation with calculated duration was **~1.0** (perfect match).
  * Units are in **Days**.

## Outlier Threshold Rationale (`MAX_LOS_DAYS = 90`)

Based on the distribution of `lengthofstay` for survivors:

* **Median**: 3 days
* **95th Percentile**: 29 days
* **99th Percentile**: 56 days
* **99.5th Percentile**: 76 days

We selected a conservative threshold of **90 days** because:

1. It covers **>99.5%** of all valid survivor records.
2. It allows for legitimate long stays (e.g., extremely premature babies often stay 2-3 months).
3. Values significantly above this (e.g., >120 days) are increasingly likely to be data entry errors or outliers that would skew the mean.

## Data Sources & Dictionaries

* **Malawi (KCH)**: Newborn Admissions (`lengthofstay` variable)
* **Zimbabwe (SMCH)**: Newborn Admissions (`lengthofstay` variable)

For variable definitions, refer to the data dictionaries:

* Malawi: `LBW_WORKING_SAMPLE/Dictionary_MWI.xlsx`
* Zimbabwe: `LBW_WORKING_SAMPLE/Dictionary_ZIM.xlsx`

## Output Files

Saved to `00-simple_questions_DEF/Q10_outputs/`:

1. **00_sample_size_summary.csv** - Detailed breakdown of filtering steps.
2. **01_los_stats_overall.csv** - Combined statistics by category, including `n_excluded` (outliers) and `n_analyzed`.
3. **02_los_stats_by_facility.csv** - Statistics broken down by facility.

## Output Variables Description

The columns in `01_los_stats_overall.csv` and `02_los_stats_by_facility.csv` are defined as follows:

* **category**: The specific LBW (e.g., ELBW) or SGA category (e.g., Term-SGA).
* **n_total**: Total number of survivors with valid `lengthofstay` data in this category.
* **n_excluded**: Number of babies excluded because their stay was > 90 days.
* **n_analyzed**: Number of babies included in the statistics (`n_total` - `n_excluded`).
* **mean_los**: Average Length of Stay (days) for the analyzed babies.
* **median_los**: Median Length of Stay (days).
* **min_los**: Minimum Length of Stay (days).
* **max_los**: Maximum Length of Stay (days) within the included range (≤ 90).
* **sd_los**: Standard Deviation of Length of Stay.
* **q1_los**: 25th Percentile (First Quartile).
* **q3_los**: 75th Percentile (Third Quartile).
* **facility**: Facility name (KCH, SMCH) or "Overall".
* **classification**: The stratification type ("Birthweight" or "SGA").

## Important Notes

* **Survival Bias**: This analysis specifically looks at *survivors*.
* **Outlier Counts**: The number of excluded records is explicitly counted in the output tables (`n_excluded`).


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
