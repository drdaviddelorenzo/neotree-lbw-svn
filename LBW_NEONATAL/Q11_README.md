# Question 11: Temperature on Admission

## Question

What is the prevalence of **hypothermia** and the average admission temperature, distributed by birthweight categories (LBW) and SGA categories (SVN)?

## Script

`Q11_temperature_on_admission.R`

## What This Script Does

1. **Analyzes** Admission Temperature.
2. **Validates data**: Filters out invalid temperatures (< 30.0°C or > 45.0°C).
3. **Classifies Hypothermia** (WHO definition):
    * **Severe**: < 32.0°C
    * **Moderate**: 32.0 - 35.9°C
    * **Mild**: 36.0 - 36.4°C
    * **Normothermia**: 36.5 - 37.5°C
    * **Hyperthermia**: > 37.5°C
4. **Calculates Measures**:
    * Mean Temperature
    * Prevalence of Severe Hypothermia
    * Prevalence of Moderate Hypothermia
    * Prevalence of Any Hypothermia (< 36.5°C)

## Data Sources & Variables

* **Malawi (KCH)**: Uses `temperatureonarrival`.
* **Zimbabwe (SMCH)**: Uses `temperature`.

## Output Files

Saved to `00-simple_questions_DEF/Q11_outputs/`:

1. **01_temp_stats_overall.csv** - Combined statistics by category.
2. **02_temp_stats_by_facility.csv** - Statistics broken down by facility.

## Output Variables Description

The columns in the output files are defined as follows:

* **category**: The specific LBW (e.g., ELBW) or SGA category (e.g., Term-SGA).
* **n_total**: Total number of babies in this category (with valid birthweight).
* **n_missing**: Number of babies in this category with missing or invalid temperature data.
* **n_analyzed**: Number of babies included in the temperature analysis (`n_total` - `n_missing`).
* **pct_missing**: Percentage of babies with missing temperature data.
* **mean_temp**: Average admission temperature (°C) for analyzed babies.
* **sd_temp**: Standard Deviation of admission temperature.
* **pct_hypo_severe**: Percentage of analyzed babies with temp < 32.0°C.
* **pct_hypo_mod**: Percentage of analyzed babies with temp 32.0-35.9°C.
* **pct_any_hypo**: Percentage of analyzed babies with temp < 36.5°C.
* **facility**: Facility name (KCH, SMCH) or "Overall".
* **classification**: The stratification type ("Birthweight" or "SGA").

## Notes on Data Quality

* **Sample Size**: The number of records with valid temperature data may be lower than the total admissions due to missing data. This is particularly noted for some facilities.
* **Thresholds**: Temperatures < 30°C and > 45°C are treated as outliers/errors and excluded.


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
