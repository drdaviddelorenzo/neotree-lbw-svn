# Question 5: Early Neonatal Deaths by LBW and SGA Categories (SMCH Only)

## Question

What is the number of early neonatal deaths (ENND) distributed by birthweight categories (LBW) and SGA categories (SVN)?

## Script

`Q5_neonatal_deaths_by_category.R`

## Data Sources

- **Zimbabwe (SMCH) only**: `SMCH_ZIM_Maternal_Outcomes`

> **Important**: KCH (Malawi) does not have neonatal death codes in the maternal dataset. This analysis is SMCH only.

## Neonatal Death Identification

- **Variable**: `neotreeoutcome`
- **SMCH code**: `ENND` = Early Neonatal Death

## What This Script Does

1. Filters SMCH data for early neonatal deaths (`neotreeoutcome == "ENND"`)
2. Distribution by LBW categories (ELBW, VLBW, LBW, NBW, HBW)
3. Distribution by SGA categories (Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA)
4. Death rate (CFR) per category using all SMCH births as denominator
5. Breakdown by year (2022–2025)

## Output Files

Saved to `Q5_outputs/`:

| File | Description |
|---|---|
| `01_sample_size_summary.csv` | ENND counts and valid BW/GA coverage |
| `02_distribution_by_category.csv` | LBW and SGA distributions of ENND (% of all ENND) |
| `03_death_rate_by_category.csv` | Total births, ENND, and % ENND per birth category |
| `04_distribution_by_year.csv` | Breakdown by year |

## Death Rate Table (`03_death_rate_by_category.csv`)

Unlike `02`, which shows each category's share of all ENND, this table answers:
*"What proportion of babies born in each category died as early neonatal deaths?"*

The denominator is **all SMCH deliveries** (live births + ENND + stillbirths) classified into the same LBW/SGA scheme.

| Column | Description |
|---|---|
| `classification` | `"Birthweight"` or `"SGA"` |
| `category` | LBW or SGA subgroup |
| `explanation` | `"Included in analysis"` or missingness reason |
| `n_total_births` | Total deliveries in this category |
| `n_ennd` | Early neonatal deaths in this category |
| `pct_ennd_of_births` | `n_ennd / n_total_births × 100` (case fatality rate) |

## Notes

- Uses same birthweight and SGA classification logic as Q2/Q3/Q4
- Filter: `neotreeoutcome == "ENND"` for SMCH only

## Usage

```bash
Rscript 00-simple_questions_DEF/Q5_neonatal_deaths_by_category.R
```

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 22 weeks (maternal dataset)
- **Gender variable**: `sexdis` from the maternal dataset
