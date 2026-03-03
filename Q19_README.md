# Question 19: Outcomes by Resuscitation Received

## Question

What is the breakdown of **Neonatal Outcomes** (NND vs Alive) for each LBW/SGA category, stratified by **Resuscitation Received**?

## Script

`Q19_outcomes_by_resuscitation.R`

## Resuscitation Categories (Hierarchical Mapping)

The script maps the `resus` variable to the following mutually exclusive categories, prioritizing the highest level of intervention:

1. **Cardiopulmonary Resuscitation** (Highest)
    * Includes: `CPR`, `CARDIOPULMONARY`
2. **Bag Valve Mask**
    * Includes: `BVM`, `BAG`, `AMBU` (if not CPR)
3. **Stimulation/Oxygen**
    * Includes: `O2`, `OXYGEN`, `STIM`, `SUCTION` (if not BVM/CPR)
4. **None**
    * Includes: `NONE`, `N`, `NO`, `NORM`
5. **Unknown**
    * Includes: `NA`, `UNK`, `?`, or any other unrecognized value.

## What This Script Does

1. **Cleans Data**: Normalizes the text data in `resus`.
2. **Groups by Category**: Assigns each baby to one of the above 5 categories.
3. **Outputs Wide Format**:
    * Rows: Facility + Resuscitation Category.
    * Columns: Category_NND (Deaths) and Category_DC (Discharged/Alive).

## Output Files

Saved to `00-simple_questions_DEF/Q19_outputs/`:

1. **01_resus_outcomes_LBW_wide.csv**: Wide format table for Birthweight categories.
2. **02_resus_outcomes_SGA_wide.csv**: Wide format table for SGA categories.

## Output Variables Description

* **facility**: Facility name (KCH, SMCH).
* **resus_category**: The resuscitation level.
* **[Category]_NND**: Number of Neonatal Deaths in that category and resuscitation level.
* **[Category]_DC**: Number of Survivors (Discharged/Alive) in that category and resuscitation level.


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
