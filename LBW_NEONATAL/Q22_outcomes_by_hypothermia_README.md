# Q22: Outcomes by Hypothermia Status

## Overview

This analysis investigates neonatal outcomes (Alive vs Neonatal Death) stratified by hypothermia status on admission, across standard Birthweight (LBW) and Small-for-Gestational-Age (SGA) categories.

## Variable

- **Hypothermia** is **derived** from the admission temperature (not a directly recorded field):
  - **KCH (Malawi)**: column `temperatureonarrival`
  - **SMCH (Zimbabwe)**: column `temperature`

- **Groups**:
  | Group | Definition |
  |---|---|
  | `Hypothermia (Yes)` | Admission temperature **< 36.5 °C** (WHO definition) |
  | `No Hypothermia (No)` | Admission temperature **≥ 36.5 °C** |
  | `Missing` | Temperature absent, non-numeric, or outside plausible range (30–45 °C) |

> **Note**: Records with missing temperature are **retained** (as the "Missing" group) rather than excluded, so the denominator is the same as in other Q2x analyses.

## Methodology

1. **Temperature cleaning**: Raw temperature string columns are coerced to numeric. Values outside 30–45 °C are set to `Missing` (same quality filter as Q15).
2. **Hypothermia classification**: Applied via `normalise_hypothermia()` using the 36.5 °C WHO threshold.
3. **Outcome normalisation**: `neotreeoutcome` is mapped to `Alive`, `Neonatal Death`, `Stillbirth`, or `Unknown/Other` via `normalise_outcome()`.
4. **Filtering**: Analysis population = records with valid outcomes (`Alive` or `Neonatal Death`).
5. **Stratification**: Data is grouped by Facility (KCH, SMCH) and Hypothermia Group.
6. **Outcome aggregation**: Counts of Neonatal Deaths (NND) and Discharges (DC) are calculated for each subgroup within each LBW and SGA category.
7. **CFR Statistical Tests**: For each Facility × Birthweight/SGA category combination, the Case Fatality Rate (CFR = NND / total) is compared between `Hypothermia (Yes)` and `No Hypothermia (No)` (**"Missing" excluded**) using:
   - **Overall Pearson chi-square test** (H0: CFR equal across groups) — reports χ², df, and p-value.
   - **Pairwise Fisher's exact test** (one pair only: Hypothermia Yes vs. No) with **Bonferroni correction** (n_pairs = 1, so raw and corrected p-values are identical).
   - A `low_expected_count_warning` flag (`TRUE/FALSE`) is included: if any expected cell count < 5, Fisher's exact p-value should be preferred over chi-square.

## Outputs

Files are saved in `Q22_outputs/`:

1. **`00_sample_size_summary.csv`**: Flow of records from total admissions through analysis population and each hypothermia group.

2. **`01_hypothermia_outcomes_LBW_wide.csv`**:
    - Rows: Facility + Hypothermia Group.
    - Columns: Outcomes (NND, DC) for ELBW, VLBW, LBW, NBW, HBW, Discarded, Total.

3. **`02_hypothermia_outcomes_SGA_wide.csv`**:
    - Rows: Facility + Hypothermia Group.
    - Columns: Outcomes (NND, DC) for Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA, Discarded, Total.

4. **`03_hypothermia_CFR_chisquare_pairwise.csv`** *(new)*:
    - One row per pairwise group comparison, per Facility × category.
    - **Groups compared**: Hypothermia (Yes) vs. No Hypothermia (No) ("Missing" excluded).
    - **Columns**: `facility`, `classification` (Birthweight/SGA), `category`, `exposure_variable`, `comparison`, `group1`, `group2`, `n_group1`, `NND_group1`, `CFR_group1_pct`, `n_group2`, `NND_group2`, `CFR_group2_pct`, `chisq_statistic`, `chisq_df`, `chisq_p_value`, `fisher_p_raw`, `n_pairs_tested`, `fisher_p_bonferroni`, `low_expected_count_warning`.

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold for SGA**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
