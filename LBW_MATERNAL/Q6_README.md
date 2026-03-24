# Question 6: Seasonal Distribution of Births by LBW and SGA Categories

## Question

Does the season when the baby is born affect the probability of belonging to one of the LBW or SGA categories?

## Script

`Q6_seasonal_distribution.R`

## Data Sources

- **Malawi (KCH)**: `KCH_MWI_Combined_Maternity_Outcomes` (all births)
- **Zimbabwe (SMCH)**: `SMCH_ZIM_Maternal_Outcomes` (all births)

> Uses the same maternal dataset as Q2–Q5. All 4 years of data (2022–2025) are pooled together — there is no year stratification. Each season therefore represents the cumulative count across all years.

## Season Definitions

| Season | Months | Description |
|---|---|---|
| **Warm and wet** | December – March (12, 1, 2, 3) | Southern African rainy season |
| **Cool and wet** | April – May (4, 5) | Post-rainy shoulder season |
| **Cool and dry** | June – August (6, 7, 8) | Southern African winter |
| **Warm and dry** | September (9) | Pre-summer transition |
| **Hot and dry** | October – November (10, 11) | Pre-rainy hot season |

Season is assigned from the birth date (`dateadmission`). Records without a valid date are excluded from the seasonal analysis.

## What This Script Does

1. Loads maternal delivery data from both facilities
2. Assigns each record to a season based on birth month
3. Classifies each delivery by LBW and SGA categories
4. Computes counts and percentages for each season × birth category combination
5. Runs chi-squared tests of independence with standardised Pearson residuals
6. Runs logistic regression to estimate ORs by season vs a reference season
7. Outputs results globally (pooled) and per facility

## Output Files

Saved to `Q6_outputs/`:

| File | Description |
|---|---|
| `01_sample_size_summary.csv` | Total deliveries, valid BW/GA counts, records missing date |
| `02_births_per_season.csv` | Total births, valid BW, and valid SGA per season × facility |
| `03_distribution_by_season_global.csv` | LBW and SGA distribution per season (all facilities pooled) |
| `04_distribution_by_season_and_facility.csv` | LBW and SGA distribution per season × facility |
| `05_chisq_results.csv` | Chi-squared test summary per scope × classification |
| `06_chisq_residuals.csv` | Standardised Pearson residuals per season × category cell |
| `07_odds_ratios_by_season.csv` | Logistic regression ORs per season vs reference season |

## Distribution Output Variables

Files 03 and 04 share this structure:

| Column | Description |
|---|---|
| `facility` | `"Global"`, `"KCH"`, or `"SMCH"` |
| `season` | One of the 5 season labels |
| `classification` | `"Birthweight"` or `"SGA"` |
| `category` | LBW or SGA subgroup |
| `explanation` | `"Included in analysis"` or reason for exclusion |
| `n` | Count of births in this group |
| `percentage` | % of all births in that season (within facility) |

A `Total` row is appended per facility × season, summing across all categories.

## Statistical Tests

### Chi-Squared Test of Independence (`05_chisq_results.csv`)

Tests whether the distribution of births across LBW/SGA categories is independent of season. Run for each combination of scope (Global, KCH, SMCH) and classification (Birthweight, SGA). Only valid classified records are included (Discarded/Missing rows excluded).

Columns: `scope`, `classification`, `chi_sq`, `df`, `p_value`, `significant`

### Standardised Pearson Residuals (`06_chisq_residuals.csv`)

Companion to the chi-squared test. For each season × category cell:

| Standardised residual | Interpretation |
|---|---|
| > +2 | Cell is **more frequent** than expected under independence |
| < −2 | Cell is **less frequent** than expected under independence |
| −2 to +2 | As expected |

Columns: `scope`, `classification`, `season`, `category`, `observed`, `expected`, `std_residual`, `direction`

### Odds Ratios by Season (`07_odds_ratios_by_season.csv`)

Logistic regression (unadjusted) with **"Cool and dry"** (June–August) as the reference season. Three binary comparisons run globally and per facility:

| Comparison | Case | Control |
|---|---|---|
| Any-LBW vs NBW | ELBW + VLBW + LBW | NBW |
| SGA vs AGA | Term-SGA + Preterm-SGA | Term-AGA + Preterm-AGA |
| Preterm vs Term | Preterm-SGA + Preterm-AGA | Term-SGA + Term-AGA |

Columns: `scope`, `comparison`, `season`, `reference`, `n_total`, `n_outcome`, `OR`, `CI_lower`, `CI_upper`, `p_value`, `significant`

## Notes

- The denominator for `percentage` is all births in that season at that facility — not just valid BW/SGA records.
- Season assignment requires a valid `dateadmission`. Records with missing dates are reported in `01_sample_size_summary.csv` but excluded from all season-based outputs.
- All 4 years (2022–2025) are **pooled** within each season — there is no year stratification.

## Usage

```bash
Rscript 00-simple_questions_DEF/Q6_seasonal_distribution.R
```

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 22 weeks (maternal dataset)
- **Gender variable**: `sexdis` from the maternal dataset
