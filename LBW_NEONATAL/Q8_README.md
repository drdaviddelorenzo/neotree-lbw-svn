# Question 8: Total Neonatal Admissions by Category

## Question

What is the total number of admissions to the neonatal unit, distributed by birthweight categories (LBW) and SGA categories (SVN)?

## Script

`Q8_neonatal_admissions_by_category.R`

## Data Sources

- **Malawi (KCH)**: `KCH_MWI_COMPLETE_ALL_RECORDS` (Newborn admissions)
- **Zimbabwe (SMCH)**: `SMCH_ZIM_COMPLETE_ALL_RECORDS` (Newborn admissions)

## What This Script Does

1. Loads newborn admission data from both facilities
2. Classifies by LBW categories (ELBW, VLBW, LBW, NBW, HBW)
3. Classifies by SGA categories (Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA)
4. Outputs distributions by facility

## Categorisation

### Birthweight (LBW)

| Category | Definition |
|---|---|
| ELBW | < 1.0 kg |
| VLBW | 1.0–1.499 kg |
| LBW | 1.5–2.499 kg |
| NBW | 2.5–4.0 kg |
| HBW | > 4.0 kg |
| Discarded | Missing or invalid birthweight |

### SGA

| Category | Definition |
|---|---|
| Term-AGA | GA ≥ 37 wks, BW ≥ 10th percentile |
| Term-SGA | GA ≥ 37 wks, BW < 10th percentile |
| Preterm-AGA | GA < 37 wks, BW ≥ 10th percentile |
| Preterm-SGA | GA < 37 wks, BW < 10th percentile |
| Missing BW | Valid GA, missing birthweight |
| Missing GA | Valid BW, GA missing or < 24 wks |
| Missing both | Both missing or invalid |

## Output Files

Saved to `Q8_outputs/`:

| File | Description |
|---|---|
| `01_sample_size_summary.csv` | Total admissions and coverage |
| `02_admissions_by_category_overall.csv` | Combined distribution |
| `03_admissions_by_category_and_facility.csv` | Facility-specific breakdown |

## Usage

```bash
Rscript 00-simple_questions_DEF/Q8_neonatal_admissions_by_category.R
```

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
