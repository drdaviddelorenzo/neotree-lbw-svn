# Question 3: Live Births by LBW and SGA Categories

## Question

What is the number of live births distributed by birthweight categories (LBW) and SGA categories (SVN)?

## Script

`Q3_live_births_by_category.R`

## Data Sources

- **Malawi (KCH)**: `KCH_MWI_Combined_Maternity_Outcomes`
- **Zimbabwe (SMCH)**: `SMCH_ZIM_Maternal_Outcomes`

> **Key difference from Q2**: Q3 filters for live births only (`neotreeoutcome == "LB"`), excluding stillbirths.

## Live Birth Identification

- **Variable**: `neotreeoutcome`
- **Live birth value**: `"LB"`
- **Excluded values**: `SBF` (Stillbirth Female), `SBM` (Stillbirth Male), `UNK` (Unknown)

## What This Script Does

1. Filters for live births only
2. Distribution by LBW categories (ELBW, VLBW, LBW, NBW, HBW)
3. Distribution by SGA categories (Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA)
4. Breakdowns by facility and year (2022–2025)

## Output Files

Saved to `Q3_outputs/`:

| File | Description |
|---|---|
| `01_sample_size_summary.csv` | Live birth counts and valid BW/GA coverage |
| `02_distribution_by_category.csv` | Overall LBW and SGA distributions |
| `03_distribution_by_facility.csv` | Breakdown by facility |
| `04_distribution_by_year.csv` | Breakdown by year |
| `05_distribution_by_facility_and_year.csv` | Breakdown by facility and year |

## Notes

- Same birthweight and SGA classification logic as Q2
- Only difference is the `neotreeoutcome == "LB"` filter applied before analysis

## Usage

```bash
Rscript 00-simple_questions_DEF/Q3_live_births_by_category.R
```

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 22 weeks (maternal dataset)
- **Gender variable**: `sexdis` from the maternal dataset
