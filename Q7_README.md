# Question 7: Average Maternal Age by LBW and SGA Categories

## Question

What is the average maternal age for live births, distributed by birthweight categories (LBW) and SGA categories (SVN)?

## Script

`Q7_maternal_age_by_category.R`

## Data Sources

- **Malawi (KCH)**: `KCH_MWI_Combined_Maternity_Outcomes`
- **Zimbabwe (SMCH)**: `SMCH_ZIM_Maternal_Outcomes`

## What This Script Does

1. Filters for valid maternal age (10–55 years)
2. Calculates Mean, Median, Min, Max, and SD of maternal age
3. Breakdown by LBW categories (ELBW, VLBW, LBW, NBW, HBW)
4. Breakdown by SGA categories (Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA)
5. Breakdown by facility (KCH vs SMCH)

## Output Files

Saved to `Q7_outputs/`:

| File | Description |
|---|---|
| `01_sample_size_summary.csv` | Sample sizes and missingness rates |
| `02_maternal_age_by_category.csv` | Overall mean/median age by category |
| `03_maternal_age_by_category_and_facility.csv` | Breakdown by facility |
| `04_data_quality_missing_age.csv` | Detailed missing data report |

> **Data quality note**: Maternal age has high missingness in both datasets. Check `04_data_quality_missing_age.csv` for completeness rates before interpreting results.

## Usage

```bash
Rscript 00-simple_questions_DEF/Q7_maternal_age_by_category.R
```

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 22 weeks (maternal dataset)
- **Gender variable**: `sexdis` from the maternal dataset
