# Question 2: Birth Insights from Maternal Data

## Question

What is the total number of deliveries and how are they distributed by birthweight categories (LBW) and SGA categories (SVN)?

## Script

`Q2_birth_insights_maternal_data.R`

## Data Sources

- **Malawi (KCH)**: `KCH_MWI_Combined_Maternity_Outcomes` (all births)
- **Zimbabwe (SMCH)**: `SMCH_ZIM_Maternal_Outcomes` (all births)

> **Key difference from Q1**: Q2 analyzes ALL births (maternal data); Q1 analyzes only neonatal ward admissions.

## What This Script Does

1. Total number of deliveries at each facility
2. Distribution by LBW categories (ELBW, VLBW, LBW, NBW, HBW)
3. Distribution by SGA categories (Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA)
4. Breakdowns by facility and year (2022–2025)

## Output Files

Saved to `Q2_outputs/`:

| File | Description |
|---|---|
| `01_sample_size_summary.csv` | Total deliveries, valid BW/GA counts, discarded tracking |
| `02_distribution_by_category.csv` | Overall LBW and SGA distributions |
| `03_distribution_by_facility.csv` | Breakdown by facility |
| `04_distribution_by_year.csv` | Breakdown by year |
| `05_distribution_by_facility_and_year.csv` | Breakdown by facility and year |

> Files 2–5 include an `explanation` column and a `Total` summary row.

## Usage

```bash
Rscript 00-simple_questions_DEF/Q2_birth_insights_maternal_data.R
```

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 22 weeks (maternal dataset)
- **Gender variable**: `sexdis` from the maternal dataset
