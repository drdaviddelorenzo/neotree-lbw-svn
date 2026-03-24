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
2. **Filtering**: Analysis includes only records with valid birthweight (0.3–7.0 kg) and valid normalized outcomes (Alive or Neonatal Death).
3. **Stratification**: Data is grouped by Facility (KCH, SMCH) and the Steroids Category.
4. **Outcome Aggregation**: Counts of Neonatal Deaths (NND) and Discharges (DC) are calculated for each subgroup.
5. **CFR Statistical Tests**: For each Facility × Birthweight/SGA category combination, the Case Fatality Rate (CFR = NND / total) is compared across steroids groups (Y, N, U — **"Missing" excluded**) using:
   - **Overall Pearson chi-square test** (H0: CFR equal across all groups) — reports χ², df, and p-value.
   - **Pairwise Fisher's exact tests** with **Bonferroni correction** — all group pairs tested; both raw and corrected p-values saved.
   - A `low_expected_count_warning` flag (`TRUE/FALSE`) is included: if any expected cell count < 5, the chi-square approximation may be unreliable and Fisher's exact p-values should be preferred.

## Outputs

Files are saved in `Q21_outputs/`:

1. **`00_sample_size_summary.csv`**: Flow of records from total admissions through analysis population and each steroids group.

2. **`01_steroids_outcomes_LBW_wide.csv`**:
    - Rows: Facility, Steroids Group.
    - Columns: Outcomes (NND, DC) for ELBW, VLBW, LBW, NBW, HBW, Discarded, Total.

3. **`02_steroids_outcomes_SGA_wide.csv`**:
    - Rows: Facility, Steroids Group.
    - Columns: Outcomes (NND, DC) for Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA, Discarded, Total.

4. **`03_steroids_CFR_chisquare_pairwise.csv`** *(new)*:
    - One row per pairwise group comparison, per Facility × category.
    - **Groups compared**: Steroids Used (Y) vs. Not Used (N), Y vs. Unsure (U), N vs. U ("Missing" excluded).
    - **Columns**: `facility`, `classification` (Birthweight/SGA), `category`, `exposure_variable`, `comparison`, `group1`, `group2`, `n_group1`, `NND_group1`, `CFR_group1_pct`, `n_group2`, `NND_group2`, `CFR_group2_pct`, `chisq_statistic`, `chisq_df`, `chisq_p_value`, `fisher_p_raw`, `n_pairs_tested`, `fisher_p_bonferroni`, `low_expected_count_warning`.

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
