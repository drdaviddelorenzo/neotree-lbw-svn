# Question 20: Outcomes by Maternal HIV Status

## Question

What is the breakdown of **Neonatal Outcomes** (NND vs Alive) for each LBW/SGA category, stratified by **Maternal HIV Status**?

## Script

`Q20_outcomes_by_maternal_hiv.R`

## HIV Groups (Overlapping)

The analysis produces rows for the following groups. Note that **"HIV True (Tested)" is the sum of Reactive, Non-Reactive, and Unknown results**, so the groups are not mutually exclusive.

1. **HIV False (No Test)**: Mother did not receive an HIV test.
2. **HIV True (Tested)**: Mother received an HIV test (Total Tested).
3. **HIV True & Reactive**: Tested Positive.
4. **HIV True & Non-Reactive**: Tested Negative.
5. **HIV True & Unknown**: Tested, but result is Unknown or Missing.
6. **HIV test NA**: `mathivtest` is `NA` (Missing data).

## What This Script Does

1. **Cleans Data**: Standardizes `mathivtest` to Boolean and `hivtestresult` to R/NR.
2. **Calculates Stats**: Computes NND and DC counts for each group independently.
3. **Outputs Wide Format**:
    * Rows: Facility + HIV Group.
    * Columns: Category_NND (Deaths) and Category_DC (Discharged/Alive).

## Output Files

Saved to `00-simple_questions_DEF/Q20_outputs/`:

1. **01_hiv_outcomes_LBW_wide.csv**: Wide format table for Birthweight categories.
2. **02_hiv_outcomes_SGA_wide.csv**: Wide format table for SGA categories.

## Output Variables Description

* **facility**: Facility name (KCH, SMCH).
* **hiv_group**: The maternal HIV status group.
* **[Category]_NND**: Number of Neonatal Deaths in that category and group.
* **[Category]_DC**: Number of Survivors (Discharged/Alive) in that category and group.


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
