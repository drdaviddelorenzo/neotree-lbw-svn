# Question 4: Stillbirths by LBW and SGA Categories

## Question

What is the number of stillbirths (≥ 22 weeks gestation) distributed by birthweight categories (LBW) and SGA categories (SVN)? What is the odds ratio of stillbirth for each birth category?

## Script

`Q4_stillbirths_by_category.R`

## Data Sources

- **Malawi (KCH)**: `KCH_MWI_Combined_Maternity_Outcomes`
- **Zimbabwe (SMCH)**: `SMCH_ZIM_Maternal_Outcomes`

## Stillbirth Classification Criteria

- **Identification**: `neotreeoutcome` in `c("SBF", "SBM", "STBF", "STBM")`
- **Gestation requirement**: ≥ 22 weeks
- **Timing classification**:
  - **Early stillbirth**: 22+0 to 27+6 weeks
  - **Late stillbirth**: ≥ 28+0 weeks

## What This Script Does

1. Filters for stillbirths (≥ 22 weeks gestation)
2. Classifies as Early (22–27+6 wks) or Late (≥ 28 wks)
3. Distribution by LBW categories (ELBW, VLBW, LBW, NBW, HBW)
4. Distribution by SGA categories (Term-AGA, Term-SGA, Preterm-AGA, Preterm-SGA)
5. Breakdowns by facility, stillbirth timing, and year (2022–2025)
6. Logistic regression OR analysis of stillbirth risk by birth category
7. Chi-squared tests of independence for stillbirth vs category
8. Pairwise proportion tests and EMMEANS contrasts for pairwise comparisons

## Output Files

Saved to `Q4_outputs/`:

| File | Description |
|---|---|
| `01_sample_size_summary.csv` | Stillbirth counts and valid BW/GA coverage |
| `02_distribution_by_category.csv` | Overall LBW and SGA distributions |
| `03_distribution_by_stillbirth_timing.csv` | Early vs Late stillbirth breakdown |
| `04_distribution_by_timing_and_facility.csv` | Breakdown by timing and facility |
| `05_distribution_by_facility.csv` | Breakdown by facility |
| `06_distribution_by_year.csv` | Breakdown by year |
| `07_distribution_by_facility_and_year.csv` | Breakdown by facility and year |
| `08_odds_ratios.csv` | OR of stillbirth risk per category, by scope and timing |
| `09_chi_squared_tests.csv` | Overall significance of differences across categories |
| `10_pairwise_prop_tests.csv` | Pairwise proportion tests (Holm adjusted) |
| `11_emmeans_pairwise.csv` | Estimated marginal means pairwise contrasts (Tukey adjusted) |

## Odds Ratio Analysis (`08_odds_ratios.csv`)

The OR analysis compares **stillbirth risk** (outcome = 1) vs **live birth** (outcome = 0) across birth categories. The full maternal delivery population is used as the denominator (not just stillbirths).

Three comparisons are run:

| Comparison | Reference |
|---|---|
| LBW categories (ELBW, VLBW, LBW, HBW) | **NBW** |
| SGA categories (Term-SGA, Preterm-AGA, Preterm-SGA) | **Term-AGA** |
| Preterm vs Term | **Term** |

Each comparison is run across three scopes (`sb_scope`):

- **All stillbirths** — all SBs ≥ 22 weeks vs all live births
- **Early stillbirth (22-27+6w)** — early SBs vs all live births
- **Late stillbirth (≥28w)** — late SBs vs all live births

And across three facility scopes (`scope`): `Global`, `KCH`, `SMCH`.

### Output Variables (`08_odds_ratios.csv`)

| Column | Description |
|---|---|
| `scope` | `"Global"`, `"KCH"`, or `"SMCH"` |
| `sb_scope` | Stillbirth type compared (`All stillbirths`, `Early...`, `Late...`) |
| `comparison` | Description of the comparison made |
| `category` | The category being compared |
| `reference` | The reference category |
| `n_total` | Total records in this model |
| `n_outcome` | Number of stillbirths (events) |
| `OR` | Odds ratio |
| `CI_lower` / `CI_upper` | 95% confidence interval (Wald) |
| `p_value` | p-value from Wald z-test |
| `significant` | `"Yes"` if p < 0.05 |

## Statistical Significance Testing

The analysis additionally computes overall and pairwise statistical significance for the distribution of stillbirths across groups:

### Chi-Squared Tests (`09_chi_squared_tests.csv`)
Tests the overall independence between category assignment (e.g., LBW categories or SGA categories) and the outcome (Stillbirth vs Live birth). A significant p-value indicates that the proportion of stillbirths is not uniformly distributed across the categories.

### Pairwise Comparisons
For multiple testing across all category combinations:
1. **Pairwise Proportion Tests (`10_pairwise_prop_tests.csv`)**: Pairwise tests of proportions between all specific categories, adjusted for multiple comparisons using the **Holm** method.
2. **Estimated Marginal Means (`11_emmeans_pairwise.csv`)**: Evaluates pairwise contrasts derived from the logistic regression model probabilities, adjusted using the **Tukey** method.

## Notes

- Same birthweight and SGA classification logic as Q2/Q3
- Missing BW rate is typically higher for stillbirths than live births

## Usage

```bash
Rscript 00-simple_questions_DEF/Q4_stillbirths_by_category.R
```

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 22 weeks (maternal dataset)
- **Gender variable**: `sexdis` from the maternal dataset
