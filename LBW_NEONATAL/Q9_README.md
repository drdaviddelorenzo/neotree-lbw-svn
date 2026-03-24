# Question 9: Neonatal Deaths by Category

## Question

What is the absolute number and percentage of neonatal deaths (Case Fatality Rate) over total admissions, distributed by birthweight categories (LBW) and SGA categories (SVN)?

## Script

`Q9_neonatal_deaths_by_category.R`

## Data Sources

- **Malawi (KCH)**: `KCH_MWI_COMPLETE_ALL_RECORDS`
- **Zimbabwe (SMCH)**: `SMCH_ZIM_COMPLETE_ALL_RECORDS`

## Definition of Neonatal Death

- **Included Codes**: `NND` (SMCH), `NND<24`, `NND>24` (KCH)
- **Excluded Codes**: `BID` (Brought in Dead), `Stillbirths` (handled in Q4)

## Output Files

Saved to `Q9_outputs/`:

| File | Description |
|---|---|
| `01_sample_size_summary.csv` | Total admissions, deaths, and valid analysis populations |
| `02_mortality_by_category_overall.csv` | Mortality rates by category (pooled across facilities) |
| `03_mortality_by_category_and_facility.csv` | Mortality rates by category, stratified by facility |
| `04_odds_ratios.csv` | Odds ratios with 95% CIs and Wald p-values, by facility and globally |

## Categorisation

### Birthweight (LBW) Categories

| Category | Definition |
|---|---|
| ELBW | < 1.0 kg |
| VLBW | 1.0 – 1.499 kg |
| LBW | 1.5 – 2.499 kg |
| NBW | 2.5 – 4.0 kg |
| HBW | > 4.0 kg |
| Discarded | Missing or invalid birthweight |

### SGA Categories

Derived from INTERGROWTH-21st gender-specific 10th percentile thresholds (see SGA Methodology below):

| Category | Definition |
|---|---|
| Term-AGA | GA ≥ 37 wks, BW ≥ 10th percentile |
| Term-SGA | GA ≥ 37 wks, BW < 10th percentile |
| Preterm-AGA | GA < 37 wks, BW ≥ 10th percentile |
| Preterm-SGA | GA < 37 wks, BW < 10th percentile |
| Missing BW | Valid GA but missing/invalid birthweight |
| Missing GA | Valid BW but GA missing or < 24 weeks |
| Missing both BW and GA | Both missing or invalid |

## Usage

```bash
Rscript 00-simple_questions_DEF/Q9_neonatal_deaths_by_category.R
```

## SGA Methodology

SGA classification uses gender-specific INTERGROWTH-21st 10th percentile references:

- `PCT10_BOYS` — used when gender is male
- `PCT10_GIRLS` — used when gender is female
- `PCT10_UNISEX` — fallback when gender is missing

**Gestational age threshold**: GA ≥ 24 weeks required for SGA classification (neonatal admissions dataset). Records below this threshold are classified as `Missing GA`.

**Gender variable**: `gender` column from the newborn admissions dataset.

## Odds Ratio Analysis

The script runs a logistic regression to estimate the odds of neonatal death by birth category, controlling for no covariates (unadjusted ORs). Results are stratified by facility and computed globally.

### Model

- **Type**: Binary logistic regression (`glm`, binomial family, logit link)
- **Outcome**: Neonatal death (`is_death`)
- **CIs**: 95% Wald confidence intervals
- **Test**: Wald z-test p-value

### Three Comparisons

| # | Comparison | Reference | Groups |
|---|---|---|---|
| 1 | LBW category vs NBW | NBW | ELBW, VLBW, LBW, HBW |
| 2 | SGA category vs Term-AGA | Term-AGA | Term-SGA, Preterm-AGA, Preterm-SGA |
| 3 | Preterm vs Term | Term (GA ≥ 37 wks) | Preterm (GA < 37 wks) |

> Discarded entries (missing BW or GA) are excluded from all OR denominators. Only records with valid data for the relevant variable are included in each comparison.
