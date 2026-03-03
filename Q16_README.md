# Question 16: Outcomes by Maternal Age Intervals

## Question

What is the breakdown of **Neonatal Outcomes** (NND vs Alive) for each LBW/SGA category, stratified by specific **Maternal Age Intervals**?

## Script

`Q16_outcomes_by_maternal_age.R`

## Maternal Age Intervals

* `< 16`
* `16 - 19`
* `20 - 24`
* `25 - 34`
* `>= 35`

## What This Script Does

1. **Cleans Maternal Age Data**: Filters for valid ages (10-60 years).
2. **Groups by Interval & Category**: Ensures all possible combinations are listed.
3. **Outputs Wide Format**:
    * Rows: Facility + Age Interval.
    * Columns: Category_NND (Deaths) and Category_DC (Discharged/Alive) for each category.

## Output Files

Saved to `00-simple_questions_DEF/Q16_outputs/`:

1. **01_mat_age_outcomes_LBW_wide.csv**: Wide format table for Birthweight categories (Counts).
2. **02_mat_age_outcomes_SGA_wide.csv**: Wide format table for SGA categories (Counts).
3. **03_avg_mat_age_by_outcome_LBW_wide.csv**: Average maternal age by outcome (LBW categories). Wide format (2 rows for Facility).
4. **04_avg_mat_age_by_outcome_SGA_wide.csv**: Average maternal age by outcome (SGA categories). Wide format.

## Output Variables Description

* **facility**: Facility name (KCH, SMCH).
* **age_interval**: The maternal age range (for `01` and `02` files).
* **[Category]_NND**:
  * For `01/02`: Number of Neonatal Deaths in that category and age range.
  * For `03/04`: **Average Maternal Age** of Neonatal Deaths in that category.
* **[Category]_DC**:
  * For `01/02`: Number of Survivors (Discharged/Alive) in that category and age range.
  * For `03/04`: **Average Maternal Age** of Survivors in that category.


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
