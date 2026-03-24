# Question 18: Outcomes by Gender

## Question

What is the breakdown of **Neonatal Outcomes** (NND vs Alive) for each LBW/SGA category, stratified by **Gender**?

## Script

`Q18_outcomes_by_gender.R`

## Gender Categories

* **Male**
* **Female**
* **Unsure/Missing** (Recoded from NS, U, or NA)

## What This Script Does

1. **Cleans Data**: Maps `M` to Male, `F` to Female, and handles unknown/missing values.
2. **Groups by Gender**: Assigns each baby to one of the above categories.
3. **Outputs Wide Format**:
    * Rows: Facility + Gender.
    * Columns: Category_NND (Deaths) and Category_DC (Discharged/Alive).

## Output Files

Saved to `00-simple_questions_DEF/Q18_outputs/`:

1. **01_gender_outcomes_LBW_wide.csv**: Wide format table for Birthweight categories.
2. **02_gender_outcomes_SGA_wide.csv**: Wide format table for SGA categories.

## Output Variables Description

* **facility**: Facility name (KCH, SMCH).
* **gender_interval**: The gender category.
* **[Category]_NND**: Number of Neonatal Deaths in that category and gender.
* **[Category]_DC**: Number of Survivors (Discharged/Alive) in that category and gender.


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
