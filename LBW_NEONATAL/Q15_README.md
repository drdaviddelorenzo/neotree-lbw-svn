# Question 15: Outcomes by Temperature Intervals

## Question

What is the breakdown of **Neonatal Outcomes** (NND vs Alive) for each LBW/SGA category, stratified by specific **Temperature Intervals**?

## Script

`Q15_outcomes_by_temperature.R`

## Temperature Intervals

* `< 32.0` (Severe Hypothermia)
* `32.0 - 35.9` (Moderate Hypothermia)
* `36.0 - 36.4` (Mild Hypothermia)
* `36.5 - 37.5` (Normothermia)
* `> 37.5` (Hyperthermia)

## What This Script Does

1. **Cleans Temperature Data**: Filters for valid temperatures (30-45°C).
2. **Groups by Interval & Category**: Ensures all possible combinations are listed (even if 0 counts).
3. **Outputs Wide Format**:
    * Rows: Facility + Temperature Interval.
    * Columns: Category_NND (Deaths) and Category_DC (Discharged/Alive) for each category.

## Output Files

Saved to `00-simple_questions_DEF/Q15_outputs/`:

1. **03_avg_temp_by_outcome_LBW_wide.csv**: Average temperature by outcome (LBW categories). Wide format (2 rows for Facility, columns for Category_NND/Category_DC).
2. **04_avg_temp_by_outcome_SGA_wide.csv**: Average temperature by outcome (SGA categories). Wide format.

## Output Variables Description

* **facility**: Facility name (KCH, SMCH).
* **temp_interval**: The temperature range (for `01` and `02` files).
* **[Category]_NND**:
  * For `01/02`: Number of Neonatal Deaths in that category and temperature range.
  * For `03/04`: **Average Admission Temperature** of Neonatal Deaths in that category.
* **[Category]_DC**:
  * For `01/02`: Number of Survivors (Discharged/Alive) in that category and temp range.
  * For `03/04`: **Average Admission Temperature** of Survivors in that category.


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
