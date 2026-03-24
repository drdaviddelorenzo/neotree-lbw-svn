# Question 12: Age at Time of Death

## Question

What is the breakdown of **Age at Time of Death** for babies who died (Neonatal Death), stratified by LBW and SGA categories?

## Script

`Q12_age_at_death.R`

## What This Script Does

1. **Filters for Neonatal Deaths**: Includes outcomes `Neonatal Death`, `NND`, `NND <24`, `NND >24` (and `BID`).
2. **Calculates Age at Death**:
    * **Logic**: `Age at Admission (days) + Duration of Admission (days)`
    * **Methodology Note**: The variable `lengthofstay` is usually missing for deaths. Therefore, **Duration of Admission** is calculated as:
        `datetimedeath` - `datetimeadmission`
    * `Age at Admission` is converted from hours (`age / 24`).
3. **Categorizes Death Timing**:
    * 0 - 1 days
    * >1 - 3 days
    * >3 - 7 days
    * >7 - 14 days
    * >14 - 21 days
    * > 21 days
4. **Stratifies**: By Birthweight (LBW) and SGA categories, and by Facility.

## Data Sources & Variables

* **Variables Used**:
  * `neotreeoutcome`: To identify deaths.
  * `age`: Age at admission (in hours).
  * `datetimeadmission`: To calculate duration.
  * `datetimedeath`: To calculate duration.
  * `facility`: For stratification (KCH, SMCH).

## Output Files

Saved to `00-simple_questions_DEF/Q12_outputs/`:

1. **00_summary_stats.csv**: Total admissions, total deaths, and analyzed sample size.
2. **01_age_at_death_stratified.csv**: Breakdown of deaths by age bin for each category (Overall).
3. **02_age_at_death_by_facility.csv**: Breakdown of deaths by age bin and facility.

## Output Variables Description

* **facility**: Facility name (KCH, SMCH) or "Overall".
* **category**: The LBW or SGA category.
* **n_total_deaths**: Total number of deaths in this category (denominator for the bins).
* **0-1 days**, **1-3 days**, etc.: Number of deaths occurring within this time window.
* **classification**: The stratification type ("Birthweight" or "SGA").


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
