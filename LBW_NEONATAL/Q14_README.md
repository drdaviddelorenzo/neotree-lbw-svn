# Question 14: Causes of Death

## Question

What are the top 5 causes of death (raw codes) for each facility, and how are deaths distributed across standard clinical categories, stratified by LBW and SGA categories?

## Script

`Q14_causes_of_death.R`

## What This Script Does

1. **Filters for Neonatal Deaths**: Includes outcomes `Neonatal Death`, `NND`, `NND <24`, `NND >24` (and `BID`).
2. **Cleans `causedeath`**: Trims whitespace; empty/missing values mapped to `"Unknown"`.
3. **Output 1 — Raw Top 5**: Ranks the 5 most frequent raw cause codes per group/facility.
4. **Output 2 — Standard Categories**: Maps raw codes to 5 clinical categories (see below) and counts deaths per category.
5. **Stratifies**: By Facility (KCH, SMCH), Birthweight (LBW), and SGA categories.

## Data Sources & Variables

- `neotreeoutcome`: To identify neonatal deaths
- `causedeath`: Primary cause of death code
- `facility`: KCH or SMCH (codes differ between facilities)

## Output Files

Saved to `Q14_outputs/`:

| File | Description |
|---|---|
| `01_top5_causes_death_by_facility.csv` | Wide-format table: top 5 raw cause codes with counts and % per group |
| `02_causes_by_standard_category.csv` | Wide-format table: counts and % for each of the 5 standard categories per group |

### Output 1 Variables (`01_top5_causes_death_by_facility.csv`)

| Column | Description |
|---|---|
| `facility` | KCH or SMCH |
| `classification` | `"Birthweight"` or `"SGA"` |
| `category` | LBW or SGA subgroup |
| `n_total_deaths` | Total deaths in this group |
| `cause_1` … `cause_5` | Raw code for rank 1–5 cause |
| `n_1` … `n_5` | Count for each ranked cause |
| `pct_1` … `pct_5` | Percentage for each ranked cause |

### Output 2 Variables (`02_causes_by_standard_category.csv`)

| Column | Description |
|---|---|
| `facility` | KCH or SMCH |
| `classification` | `"Birthweight"` or `"SGA"` |
| `category` | LBW or SGA subgroup |
| `n_total_deaths` | Total deaths in this group |
| `Prematurity/RDS_n` / `_pct` | n and % in Prematurity/RDS category |
| `Birth Asphyxia/HIE_n` / `_pct` | n and % in Birth Asphyxia/HIE category |
| `Sepsis/Infection_n` / `_pct` | n and % in Sepsis/Infection category |
| `Congenital Anomalies_n` / `_pct` | n and % in Congenital Anomalies category |
| `Other_n` / `_pct` | n and % not matched to any named category |

A **Total** row is always appended per facility, summing across all birth-weight groups.

## Standard Category Mapping

Raw `causedeath` codes are matched case-insensitively:

| Standard Category | Codes Mapped |
|---|---|
| **Prematurity/RDS** | `PRRDS`, `PremRD`, `RDNT`, `Hyaline Membrane Disease`, `VPrem`, `ExPrem` |
| **Birth Asphyxia/HIE** | `BA`, `HIE`, `ASP`, `sHIE` |
| **Sepsis/Infection** | `NS`, `SEPS`, `EONS`, `LONS`, `MEN`, `Pneumonia` |
| **Congenital Anomalies** | `Cong`, `Gastro`, `Omph`, `CHD` |
| **Other** | All remaining codes, including `Unknown` and low-frequency codes |

> Codes not matched to a specific category fall into **Other**. Because coding conventions differ between KCH and SMCH, interpret facility-stratified results together with the raw top-5 output (`01_...`) to understand the underlying codes driving each standard category.

## SGA Methodology

- **P10 references**: `PCT10_BOYS`, `PCT10_GIRLS`, `PCT10_UNISEX` (INTERGROWTH-21st, 14–42 weeks)
- **GA threshold**: ≥ 24 weeks (neonatal admissions dataset)
- **Gender variable**: `gender` from the newborn admissions dataset
