# Q21: Outcomes by Antenatal Steroids

## Overview

This analysis investigates neonatal outcomes (Alive vs Neonatal Death) stratified by the use of antenatal steroids, across standard Birthweight (LBW) and Small-for-Gestational-Age (SGA) categories.

## Variable

- **`ansteroids`**: Antenatal steroids administered to the mother.
- **Values Used**:
  - `Y`: Yes, steroids were used.
  - `N`: No, steroids were not used.
  - `U`: Unsure.
  - `Missing`: Data was missing or empty.

## Methodology

1. **Cleaning**: The `ansteroids` column is normalized to the 4 categories above.
2. **Filtering**: Analysis includes only records with valid birthweight (0.3-7.0 kg) and valid normalized outcomes (Alive or Neonatal Death).
3. **Stratification**: Data is grouped by Facility (KCH, SMCH) and the Steroids Category.
4. **Outcome Aggregation**: Counts of Neonatal Deaths (NND) and Discharges (DC) are calculated for each subgroup.

## Outputs

Files are saved in `Q21_outputs/`:

1. **`01_steroids_outcomes_LBW_wide.csv`**:
    - Rows: Facility, Steroids Group.
    - Columns: Outcomes (NND, DC) for ELBW, VLBW, LBW, NBW, HBW.

2. **`02_steroids_outcomes_SGA_wide.csv`**:
    - Rows: Facility, Steroids Group.
    - Columns: Outcomes (NND, DC) for Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA.


## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
