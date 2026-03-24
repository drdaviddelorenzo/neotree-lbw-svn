# Question 13: Inborn vs Outborn Status

## Question

What is the breakdown of **Inborn vs Outborn** admissions, stratified by LBW and SGA categories?

## Script

`Q13_inborn_outborn.R`

## What This Script Does

1. **Analyses** Inborn vs Outborn status using the `inorout` variable.
2. **Classifies Status**:
    * **Inborn**: `inorout` is TRUE
    * **Outborn**: `inorout` is FALSE
    * **Unknown**: `inorout` is missing
3. **Stratifies**: By Birthweight (LBW) and SGA categories, and by Facility.

## Data Sources & Variables

* **Variables Used**:
  * `inorout`: TRUE (Inborn) / FALSE (Outborn).
  * `birthweight`, `gestation`: For stratification categories.
  * `facility`: For stratification.

## Output Files

Saved to `00-simple_questions_DEF/Q13_outputs/`:

1. **01_inborn_outborn_overall.csv**: Breakdown of Inborn/Outborn status (Overall).
2. **02_inborn_outborn_by_facility.csv**: Breakdown of Inborn/Outborn by facility.

## Output Variables Description

* **category**: The LBW or SGA category.
* **n_total**: Total babies in this category.
* **n_inborn**: Number of Inborn babies.
* **n_outborn**: Number of Outborn babies.
* **n_unknown**: Number of babies with unknown status.
* **pct_inborn**: Percentage of Inborn babies.
* **pct_outborn**: Percentage of Outborn babies.
* **pct_unknown**: Percentage of Unknown status babies.
* **facility**: Facility name (KCH, SMCH) or "Overall".
* **classification**: The stratification type ("Birthweight" or "SGA").


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
