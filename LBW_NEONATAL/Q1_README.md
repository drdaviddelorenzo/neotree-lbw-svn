# Question 1: Descriptive Study of LBW and SVN Babies

## Question

What are the descriptive characteristics of low birth weight (LBW) babies and small vulnerable newborns (SVN/SGA) admitted to the neonatal unit at KCH (Malawi) and SMCH (Zimbabwe)?

## Script

`Q1_descriptive_study_LBW_SVN.R`

## Data Sources

- **Malawi (KCH)**: `KCH_MWI_COMPLETE_ALL_RECORDS` (Newborn admissions)
- **Zimbabwe (SMCH)**: `SMCH_ZIM_COMPLETE_ALL_RECORDS` (Newborn admissions)

## What This Script Does

Analyzes newborn admission data to produce descriptive statistics for two classification systems:

1. **LBW Classification** — based on absolute birthweight:

| Category | Definition |
|---|---|
| ELBW | < 1.0 kg |
| VLBW | 1.0–1.499 kg |
| LBW | 1.5–2.499 kg |
| NBW | 2.5–4.0 kg |
| HBW | > 4.0 kg |
| Discarded | Missing or invalid birthweight |

1. **SGA Classification** — based on INTERGROWTH-21st gender-specific 10th percentile (see SGA Methodology below). Restricted to USS-confirmed gestational age.

| Category | Definition |
|---|---|
| Term-AGA | GA ≥ 37 wks, BW ≥ 10th percentile |
| Term-SGA | GA ≥ 37 wks, BW < 10th percentile |
| Preterm-AGA | GA < 37 wks, BW ≥ 10th percentile |
| Preterm-SGA | GA < 37 wks, BW < 10th percentile |

## Output Files

Saved to `Q1_outputs/`:

| File | Description |
|---|---|
| `01_overall_distribution.csv` | Distribution by category (LBW and SGA) |
| `02_distribution_by_facility.csv` | Breakdown by facility (KCH vs SMCH) |
| `03_distribution_by_gender.csv` | Breakdown by gender |
| `04_distribution_by_outcome.csv` | Breakdown by outcome (Alive, Death, etc.) |
| `05_summary_statistics.csv` | Mean, SD, median, min, max of birthweight and gestation |

Each file includes a `classification` column: `"Birthweight"` or `"SGA"`.

## Notes

- **SGA analysis** uses only records with USS-confirmed gestational age (`method_gest == "USS"`).
- **LBW analysis** uses all records with valid birthweight (0.3–7.0 kg).
- The two analyses therefore have different sample sizes.

## Usage

```bash
Rscript 00-simple_questions_DEF/Q1_descriptive_study_LBW_SVN.R
```

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
