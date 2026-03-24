# Question 23: Extra Data Requests (Summary Statistics)

## Question

This script addresses specific summary statistic requests for the LBW/SGA neonatal database (stratified by facility: KCH Malawi and SMCH Zimbabwe).

## Script

`Q23_extra_data_requests.R`

## Data Requests & Methodology

The script calculates the following six items:

1.  **Total NNU admissions**: Raw count from the working sample (USS-filtered) CSV files.
2.  **Inborn (%)**: Percentage of admissions delivered at the facility (`inorout` variable).
3.  **Mean age on admission (hours)**:
    *   **Data Cleaning**: Values > 672 hours (28 days) are treated as implausible data-entry errors and excluded from mean/median calculations.
    *   Reported as: Mean (and median) with valid/missing/excluded counts.
4.  **Female (%)**: Percentage of females based on `gender` (KCH) or `sexdis` (SMCH).
5.  **HIV positive (%)**: Percentage of babies with HIV-positive mothers (`mathivtest` is TRUE and `hivtestresult` is "R").
    *   Reported as: % of tested babies AND % of total admissions.
6.  **Total SVN group (%)**: Percentage of babies with valid SGA classification relative to external total admissions (unfiltered raw admissions provided by the user).

## Data Quality Filters

| Variable | Rule / Validation | Threshold |
| :--- | :--- | :--- |
| **Birthweight** | `is.finite(bw_kg) & bw_kg >= 0.3 & bw_kg <= 7.0` | 0.3 – 7.0 kg |
| **Gestational Age** | `gestation >= 24 & gestation <= 42` | 24 – 42 weeks |
| **Age at Admission** | `age_hours_raw <= 672` (Outliers → NA) | ≤ 672 hours (28 days) |

## Output Files

Saved to `Q23_outputs/`:

1.  **01_facility_summary_numeric.csv**: Raw counts and percentages for each metric.
2.  **02_facility_summary_formatted.csv**: A summary table formatted like the original request document.
3.  **03_external_admission_totals.csv**: Reference table of the external admission denominators used for SVN % calculations.

## SGA Methodology

- **P10 references**: INTERGROWTH-21st (14–42 weeks).
- **Classification**: SGA (<10th percentile) vs AGA (≥10th percentile).
- **Category breakdown**: Term-SGA, Term-AGA, Preterm-SGA, Preterm-AGA.
