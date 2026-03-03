# Question 17: Outcomes by Mode of Delivery

## Question

What is the breakdown of **Neonatal Outcomes** (NND vs Alive) for each LBW/SGA category, stratified by **Mode of Delivery**?

## Script

`Q17_outcomes_by_mode_of_delivery.R`

## Mode of Delivery Categories

1. Spontaneous Vaginal Delivery (SVD)
2. Vacuum extraction
3. Forceps extraction
4. Elective Ceasarian Section (ELCS)
5. Emergency Ceasarian Section (EMCS)
6. Breech extraction (vaginal)
7. Induced Vaginal Delivery

* **Missing** (No indicated mode of delivery)

## What This Script Does

1. **Cleans Data**: Maps numeric codes to descriptions. Maps `NA` to "Missing".
2. **Groups by Mode**: Assigns each baby to one of the above categories.
3. **Outputs Wide Format**:
    * Rows: Facility + Mode of Delivery.
    * Columns: Category_NND (Deaths) and Category_DC (Discharged/Alive).

## Output Files

Saved to `00-simple_questions_DEF/Q17_outputs/`:

1. **01_mod_outcomes_LBW_wide.csv**: Wide format table for Birthweight categories.
2. **02_mod_outcomes_SGA_wide.csv**: Wide format table for SGA categories.

## Output Variables Description

* **facility**: Facility name (KCH, SMCH).
* **mod_interval**: The mode of delivery.
* **[Category]_NND**: Number of Neonatal Deaths in that category and mode.
* **[Category]_DC**: Number of Survivors (Discharged/Alive) in that category and mode.


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
