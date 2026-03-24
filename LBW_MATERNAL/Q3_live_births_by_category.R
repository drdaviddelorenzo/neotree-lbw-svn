# Q3_live_births_by_category.R
# Live Births by LBW and SGA Categories
#
# This script analyzes LIVE BIRTHS ONLY from maternal delivery data:
# 1. Filter for live births (neotreeoutcome == "LB")
# 2. Distribution by birthweight categories (LBW classification)
# 3. Distribution by SGA categories (SVN classification)
# 4. Breakdowns by facility and year
#
# Data sources:
# - KCH Malawi maternal outcomes (all births 2022-2025)
# - SMCH Zimbabwe maternal outcomes (all births 2022-2025)

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
})

# ===== CONFIGURATION =====
# Input files - MATERNAL DATA
kch_maternal_file <- "LBW_KCH_MATERNAL.csv"
smch_maternal_file <- "LBW_SMCH_MATERNAL.csv"

# Output directory
output_dir <- "Q3_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS
# Must match the constants in maternal_descriptive_analysis.R and
# Q2_birth_insights_maternal_data.R so that sample sizes are comparable
# across all three scripts.
# ---------------------------------------------------------------------------
BW_MIN_KG <- 0.3 # minimum plausible birth weight (kg) = 300 g
BW_MAX_KG <- 7.0 # maximum plausible birth weight (kg) = 7,000 g
GA_MIN <- 22 # minimum gestational age (weeks)
GA_MAX <- 42 # maximum gestational age (weeks)

# ===== INTERGROWTH-21st 10TH PERCENTILE REFERENCE =====
# UNISEX / SEX-POOLED (14 to 42 weeks)
PCT10_UNISEX <- c(
    "14" = 76, "15" = 98, "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 560, "25" = 640, "26" = 740, "27" = 840, "28" = 940, "29" = 1060, "30" = 1190, "31" = 1340, "32" = 1500,
    "33" = 1580, "34" = 1780, "35" = 2000, "36" = 2220, "37" = 2440, "38" = 2650, "39" = 2850, "40" = 3010, "41" = 3150, "42" = 3260
)

# BOYS (14 to 42 weeks)
PCT10_BOYS <- c(
    "14" = 76, "15" = 98, "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 580, "25" = 660, "26" = 760, "27" = 870, "28" = 980, "29" = 1110, "30" = 1250, "31" = 1410, "32" = 1580,
    "33" = 1600, "34" = 1810, "35" = 2030, "36" = 2250, "37" = 2480, "38" = 2700, "39" = 2900, "40" = 3070, "41" = 3210, "42" = 3320
)

# GIRLS (14 to 42 weeks)
PCT10_GIRLS <- c(
    "14" = 76, "15" = 98, "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 540, "25" = 620, "26" = 720, "27" = 810, "28" = 910, "29" = 1030, "30" = 1150, "31" = 1300, "32" = 1460,
    "33" = 1540, "34" = 1750, "35" = 1960, "36" = 2180, "37" = 2400, "38" = 2610, "39" = 2800, "40" = 2960, "41" = 3090, "42" = 3190
)

# ===== HELPER FUNCTIONS =====

# ---------------------------------------------------------------------------
# COLUMN-SPECIFIC BIRTH WEIGHT PARSERS
# ---------------------------------------------------------------------------
# Raw format audit (confirmed from data):
#
#   KCH (MWI)  bwtdis      — plain numeric, grams     e.g. "2880.0"
#   KCH (MWI)  birthweight — plain numeric, kg        e.g. "2.77"  (all values <= 10)
#   KCH (MWI)  bwt         — comma thousands, grams   e.g. "2,400" (as.numeric() returns NA)
#   SMCH (ZIM) bwtdis      — plain numeric, kg        e.g. "3.0"   (all values <= 10)
#
# Parsing is explicit and column-specific. No median-based unit inference.
# All parsers return values in KILOGRAMS for use in bw_category_from_kg() / classify_sga().

# KCH bwtdis: already in grams -> divide by 1000
parse_kch_bwtdis_kg <- function(x) {
    suppressWarnings(as.numeric(x)) / 1000
}

# KCH birthweight: already in kg -> use directly
parse_kch_birthweight_kg <- function(x) {
    suppressWarnings(as.numeric(x))
}

# KCH bwt: comma is thousands separator (e.g. "2,400" = 2400 g) -> strip comma, divide by 1000
parse_kch_bwt_kg <- function(x) {
    suppressWarnings(as.numeric(gsub(",", "", x))) / 1000
}

# SMCH bwtdis: already in kg -> use directly
parse_smch_bwtdis_kg <- function(x) {
    suppressWarnings(as.numeric(x))
}

# Build composite bw_kg for KCH: bwtdis (P1) > birthweight (P2) > bwt (P3), result in kg.
# bwtdis always overwrites when present; lower-priority columns fill NA slots only.
build_kch_bw_kg <- function(df) {
    result <- rep(NA_real_, nrow(df))

    # 3rd priority — bwt (comma-thousands grams, < 1% of records)
    if ("bwt" %in% names(df)) {
        bwt_val <- parse_kch_bwt_kg(df$bwt)
        result <- ifelse(!is.na(bwt_val), bwt_val, result)
    }

    # 2nd priority — birthweight (kg, ~ 25% of records)
    if ("birthweight" %in% names(df)) {
        bw_val <- parse_kch_birthweight_kg(df$birthweight)
        result <- ifelse(!is.na(bw_val), bw_val, result)
    }

    # 1st priority — bwtdis (grams -> kg, ~ 60% of records, always overwrites)
    if ("bwtdis" %in% names(df)) {
        bwtdis_val <- parse_kch_bwtdis_kg(df$bwtdis)
        result <- ifelse(!is.na(bwtdis_val), bwtdis_val, result)
    }

    result
}

# Get 10th percentile birthweight for gestational age
get_10th_percentile_bw <- function(gestation_weeks, gender_val = NA_character_) {
    if (is.na(gestation_weeks)) {
        return(NA_real_)
    }

    ga <- round(gestation_weeks)
    ga_char <- as.character(ga)

    if (ga < 14 || ga > 42) {
        return(NA_real_)
    }

    gender_clean <- toupper(trimws(gender_val))

    if (!is.na(gender_clean) && gender_clean %in% c("M", "MALE", "BOY")) {
        percentile_g <- PCT10_BOYS[ga_char]
    } else if (!is.na(gender_clean) && gender_clean %in% c("F", "FEMALE", "GIRL")) {
        percentile_g <- PCT10_GIRLS[ga_char]
    } else {
        percentile_g <- PCT10_UNISEX[ga_char]
    }

    if (is.na(percentile_g)) {
        return(NA_real_)
    } else {
        return(as.numeric(percentile_g) / 1000)
    }
}

# Classify SGA
classify_sga <- function(bw_kg, gestation_weeks, gender_val = NA_character_) {
    percentile_10 <- mapply(get_10th_percentile_bw, gestation_weeks, gender_val, USE.NAMES = FALSE)
    ifelse(is.na(bw_kg) | is.na(percentile_10), NA_character_,
        ifelse(bw_kg < percentile_10, "SGA", "AGA")
    )
}

# Create SGA categories
sga_category_from_gestational_age <- function(sga_status, gestation_weeks) {
    dplyr::case_when(
        is.na(sga_status) | is.na(gestation_weeks) ~ NA_character_,
        gestation_weeks >= 37 & sga_status == "SGA" ~ "Term-SGA",
        gestation_weeks >= 37 & sga_status == "AGA" ~ "Term-AGA",
        gestation_weeks < 37 & sga_status == "SGA" ~ "Preterm-SGA",
        gestation_weeks < 37 & sga_status == "AGA" ~ "Preterm-AGA",
        TRUE ~ NA_character_
    )
}

# Birthweight categories
bw_category_from_kg <- function(bw_kg) {
    dplyr::case_when(
        is.na(bw_kg) ~ NA_character_,
        bw_kg < 1.000 ~ "ELBW",
        bw_kg >= 1.000 & bw_kg < 1.500 ~ "VLBW",
        bw_kg >= 1.500 & bw_kg < 2.500 ~ "LBW",
        bw_kg >= 2.500 & bw_kg <= 4.000 ~ "NBW",
        bw_kg > 4.000 ~ "HBW",
        TRUE ~ NA_character_
    )
}

# Add Total row to summarized tables
add_totals <- function(df, group_cols = character(0)) {
    if (nrow(df) == 0) {
        return(df)
    }

    if (length(group_cols) == 0) {
        totals <- tibble(
            category = "Total",
            explanation = "All deliveries considered",
            n = sum(df$n),
            percentage = 100,
            classification = unique(df$classification)
        )
        return(bind_rows(df, totals))
    }

    totals <- df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        mutate(
            category = "Total",
            explanation = "All deliveries considered",
            percentage = 100,
            classification = unique(df$classification)
        )

    bind_rows(df, totals) %>%
        arrange(
            across(all_of(group_cols)),
            match(category, c(
                "ELBW", "VLBW", "LBW", "NBW", "HBW",
                "Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA",
                "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total"
            ))
        )
}

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Maternal Data ===\n")
cat("Reading KCH maternal data...\n")
kch_raw <- suppressMessages(read_csv(kch_maternal_file, guess_max = 50000, col_types = cols(.default = "c")))
cat("Reading SMCH maternal data...\n")
smch_raw <- suppressMessages(read_csv(smch_maternal_file, guess_max = 50000, col_types = cols(.default = "c")))

# Parse birthweight per facility BEFORE combining.
# Column formats differ by site — explicit parsers are used for each.
# KCH:  composite from bwtdis (grams, P1) > birthweight (kg, P2) > bwt (comma-thousands g, P3).
# SMCH: bwtdis only, plain numeric in kg.
# All bw_kg values are in kg after this block.
cat("Parsing birthweight per facility (column-specific parsers)...\n")

# KCH composite bw_kg: bwtdis (P1, grams) > birthweight (P2, kg) > bwt (P3, comma-thousands g)
kch_raw <- kch_raw %>%
    mutate(
        bw_kg    = build_kch_bw_kg(.),
        facility = "KCH"
    )

n_kch_bwtdis <- sum(!is.na(suppressWarnings(as.numeric(kch_raw$bwtdis))), na.rm = TRUE)
n_kch_birthweight <- if ("birthweight" %in% names(kch_raw)) sum(!is.na(suppressWarnings(as.numeric(kch_raw$birthweight))), na.rm = TRUE) else 0L
n_kch_bwt <- if ("bwt" %in% names(kch_raw)) sum(!is.na(suppressWarnings(as.numeric(gsub(",", "", kch_raw$bwt)))), na.rm = TRUE) else 0L
n_kch_composite <- sum(!is.na(kch_raw$bw_kg))
cat(sprintf("  KCH bwtdis      (P1, grams):             %d records\n", n_kch_bwtdis))
cat(sprintf("  KCH birthweight (P2, kg):                %d records\n", n_kch_birthweight))
cat(sprintf("  KCH bwt         (P3, comma-g):           %d records\n", n_kch_bwt))
cat(sprintf(
    "  KCH bw_kg composite:                     %d / %d (%.1f%%)\n",
    n_kch_composite, nrow(kch_raw), 100 * n_kch_composite / nrow(kch_raw)
))

# SMCH: bwtdis is the sole source, plain numeric in kg
smch_raw <- smch_raw %>%
    mutate(
        bw_kg    = parse_smch_bwtdis_kg(bwtdis),
        facility = "SMCH"
    )

n_smch_composite <- sum(!is.na(smch_raw$bw_kg))
cat(sprintf(
    "  SMCH bwtdis (sole source, kg):           %d / %d (%.1f%%)\n",
    n_smch_composite, nrow(smch_raw), 100 * n_smch_composite / nrow(smch_raw)
))

# Combine datasets AFTER conversion
cat("Combining datasets...\n")
all_maternal <- bind_rows(kch_raw, smch_raw)

total_deliveries <- nrow(all_maternal)
cat("Total deliveries:", total_deliveries, "\n")

# ===== FILTER FOR LIVE BIRTHS =====

cat("\n=== Filtering for Live Births ===\n")

# Filter for live births only (neotreeoutcome == "LB")
# Note: SMCH may use different column, check and handle
live_births <- all_maternal %>%
    filter(
        !is.na(neotreeoutcome),
        toupper(trimws(neotreeoutcome)) == "LB"
    )

live_birth_count <- nrow(live_births)
cat("Total live births:", live_birth_count, "\n")
cat("Percentage of total deliveries:", round(100 * live_birth_count / total_deliveries, 1), "%\n")

# Live births by facility
cat("\nLive births by facility:\n")
live_by_facility <- live_births %>%
    group_by(facility) %>%
    summarise(
        live_births = n(),
        .groups = "drop"
    )
print(live_by_facility)

# ===== PROCESS BIRTHWEIGHT DATA FOR LIVE BIRTHS =====

cat("\n=== Processing Birthweight Data (Live Births Only) ===\n")

# Add gestational age and valid categories
process_live <- live_births %>%
    mutate(
        gestation_weeks = suppressWarnings(as.numeric(gestation)),
        date_parsed = suppressWarnings(as.Date(dateadmission)),
        year = as.integer(format(date_parsed, "%Y")),

        # Validations — thresholds from shared constants (match Q2 and maternal_descriptive_analysis.R)
        is_valid_bw = is.finite(bw_kg) & bw_kg >= BW_MIN_KG & bw_kg <= BW_MAX_KG,
        is_invalid_ga = is.na(gestation_weeks) | gestation_weeks < GA_MIN | gestation_weeks > GA_MAX,
        sga_status = dplyr::if_else(gestation_weeks >= 22 & !is.na(gestation_weeks), classify_sga(bw_kg, gestation_weeks, if ("sexdis" %in% names(.)) sexdis else NA_character_), NA_character_),
        sga_cat_temp = sga_category_from_gestational_age(sga_status, gestation_weeks),
        is_valid_sga = is_valid_bw & !is.na(sga_cat_temp),

        # Categorizations including "Discarded"
        bw_category = if_else(is_valid_bw, bw_category_from_kg(bw_kg), "Discarded"),
        lbw_explanation = if_else(is_valid_bw, "Included in analysis", "Missing or invalid birth weight"),
        sga_explanation = case_when(
            is_valid_sga ~ "Included in analysis",
            !is_valid_bw & is_invalid_ga ~ "Missing both BW and GA",
            !is_valid_bw ~ "Missing BW",
            is_invalid_ga ~ "Missing GA",
            TRUE ~ "Missing GA"
        ),
        sga_category = if_else(is_valid_sga, sga_cat_temp, sga_explanation)
    )

# Count valid birthweights in live births
valid_bw_live <- process_live %>% filter(is_valid_bw)

valid_bw_count <- nrow(valid_bw_live)
cat("Live births with valid birthweight:", valid_bw_count, "\n")
cat("Percentage of live births:", round(100 * valid_bw_count / live_birth_count, 1), "%\n")

# ===== LBW CLASSIFICATION (LIVE BIRTHS) =====

cat("\n=== LBW Classification (Live Births Only) ===\n")

lbw_live <- valid_bw_live

cat("Live births with LBW classification:", nrow(lbw_live), "\n")

# Distribution by category
lbw_distribution <- process_live %>%
    count(category = bw_category, explanation = lbw_explanation, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    arrange(match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# Distribution by facility
lbw_by_facility <- process_live %>%
    group_by(facility, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(facility, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

cat("\n=== SVN/SGA Classification (Live Births Only) ===\n")

svn_live <- process_live %>% filter(is_valid_sga)

cat("Live births with SGA classification:", nrow(svn_live), "\n")

# Distribution by category
# Distribution by category
sga_distribution <- process_live %>%
    count(category = sga_category, explanation = sga_explanation, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    arrange(
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# Distribution by facility
sga_by_facility <- process_live %>%
    group_by(facility, category = sga_category, explanation = sga_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    ungroup() %>%
    arrange(
        facility,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# Add totals
lbw_distribution <- add_totals(lbw_distribution)
lbw_by_facility <- add_totals(lbw_by_facility, "facility")
sga_distribution <- add_totals(sga_distribution)
sga_by_facility <- add_totals(sga_by_facility, "facility")

# ===== YEAR-BASED ANALYSIS =====

cat("\n=== Year-Based Analysis (Live Births) ===\n")

# Extract year from dateadmission
# Extract year from dateadmission
process_live_year <- process_live %>%
    filter(!is.na(year), year >= 2022, year <= 2025)

cat("Live births with valid year:", nrow(process_live_year), "\n")

# LBW by year
lbw_by_year <- process_live_year %>%
    group_by(year, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(year, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded", "Total")))

# LBW by facility and year
lbw_by_facility_year <- process_live_year %>%
    group_by(facility, year, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility, year) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(facility, year, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded", "Total")))

# SGA by year
sga_by_year <- process_live_year %>%
    group_by(year, category = sga_category, explanation = sga_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    ungroup() %>%
    arrange(
        year,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# SGA by facility and year
sga_by_facility_year <- process_live_year %>%
    group_by(facility, year, category = sga_category, explanation = sga_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility, year) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    ungroup() %>%
    arrange(
        facility, year,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# Add totals for year-based results
lbw_by_year <- add_totals(lbw_by_year, "year")
lbw_by_facility_year <- add_totals(lbw_by_facility_year, c("facility", "year"))
sga_by_year <- add_totals(sga_by_year, "year")
sga_by_facility_year <- add_totals(sga_by_facility_year, c("facility", "year"))

# Combine results
distribution_combined <- bind_rows(lbw_distribution, sga_distribution)
by_facility_combined <- bind_rows(lbw_by_facility, sga_by_facility)
by_year_combined <- bind_rows(lbw_by_year, sga_by_year)
by_facility_year_combined <- bind_rows(lbw_by_facility_year, sga_by_facility_year)

# ===== SAMPLE SIZE SUMMARY =====

# Build filter-note strings (written once; reused in the tibble and in the CSV)
bw_note <- sprintf(
    "is.finite(bw_kg) & bw_kg >= %.1f kg & bw_kg <= %.1f kg",
    BW_MIN_KG, BW_MAX_KG
)
ga_note <- sprintf(
    "!is.na(gestation) & gestation >= %d wks & gestation <= %d wks",
    GA_MIN, GA_MAX
)
both_note <- paste0(bw_note, " AND ", ga_note)

# Calculate facility-specific sample sizes
kch_lbw_n <- sum(lbw_live$facility == "KCH")
smch_lbw_n <- sum(lbw_live$facility == "SMCH")

kch_sga_n <- sga_by_facility %>%
    filter(facility == "KCH") %>%
    pull(n) %>%
    sum()

smch_sga_n <- sga_by_facility %>%
    filter(facility == "SMCH") %>%
    pull(n) %>%
    sum()

sample_size_summary <- tibble(
    description = c(
        "Total deliveries (all births)",
        "Total live births",
        "Live births with valid birthweight (LBW sample size)",
        "Discarded for LBW (Missing/invalid birthweight)",
        "Live births with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)",
        "",
        "KCH - Total deliveries",
        "KCH - Live births",
        "KCH - LBW sample size",
        "KCH - Discarded for LBW",
        "KCH - SGA sample size",
        "KCH - Discarded for SGA (Missing BW)",
        "KCH - Discarded for SGA (Missing GA)",
        "KCH - Discarded for SGA (Missing both BW and GA)",
        "",
        "SMCH - Total deliveries",
        "SMCH - Live births",
        "SMCH - LBW sample size",
        "SMCH - Discarded for LBW",
        "SMCH - SGA sample size",
        "SMCH - Discarded for SGA (Missing BW)",
        "SMCH - Discarded for SGA (Missing GA)",
        "SMCH - Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        total_deliveries,
        live_birth_count,
        nrow(lbw_live),
        sum(process_live$bw_category == "Discarded"),
        nrow(svn_live),
        sum(process_live$sga_explanation == "Missing BW"),
        sum(process_live$sga_explanation == "Missing GA"),
        sum(process_live$sga_explanation == "Missing both BW and GA"),
        NA,
        sum(all_maternal$facility == "KCH"),
        sum(live_births$facility == "KCH"),
        kch_lbw_n,
        sum(process_live$facility == "KCH" & process_live$bw_category == "Discarded"),
        kch_sga_n,
        sum(process_live$facility == "KCH" & process_live$sga_explanation == "Missing BW"),
        sum(process_live$facility == "KCH" & process_live$sga_explanation == "Missing GA"),
        sum(process_live$facility == "KCH" & process_live$sga_explanation == "Missing both BW and GA"),
        NA,
        sum(all_maternal$facility == "SMCH"),
        sum(live_births$facility == "SMCH"),
        smch_lbw_n,
        sum(process_live$facility == "SMCH" & process_live$bw_category == "Discarded"),
        smch_sga_n,
        sum(process_live$facility == "SMCH" & process_live$sga_explanation == "Missing BW"),
        sum(process_live$facility == "SMCH" & process_live$sga_explanation == "Missing GA"),
        sum(process_live$facility == "SMCH" & process_live$sga_explanation == "Missing both BW and GA")
    ),
    percentage_of_total = c(
        100,
        round(100 * live_birth_count / total_deliveries, 1),
        round(100 * nrow(lbw_live) / live_birth_count, 1),
        round(100 * sum(process_live$bw_category == "Discarded") / live_birth_count, 1),
        round(100 * nrow(svn_live) / live_birth_count, 1),
        round(100 * sum(process_live$sga_explanation == "Missing BW") / live_birth_count, 1),
        round(100 * sum(process_live$sga_explanation == "Missing GA") / live_birth_count, 1),
        round(100 * sum(process_live$sga_explanation == "Missing both BW and GA") / live_birth_count, 1),
        NA,
        round(100 * sum(all_maternal$facility == "KCH") / total_deliveries, 1),
        round(100 * sum(live_births$facility == "KCH") / sum(all_maternal$facility == "KCH"), 1),
        round(100 * kch_lbw_n / sum(live_births$facility == "KCH"), 1),
        round(100 * sum(process_live$facility == "KCH" & process_live$bw_category == "Discarded") / sum(live_births$facility == "KCH"), 1),
        round(100 * kch_sga_n / sum(live_births$facility == "KCH"), 1),
        round(100 * sum(process_live$facility == "KCH" & process_live$sga_explanation == "Missing BW") / sum(live_births$facility == "KCH"), 1),
        round(100 * sum(process_live$facility == "KCH" & process_live$sga_explanation == "Missing GA") / sum(live_births$facility == "KCH"), 1),
        round(100 * sum(process_live$facility == "KCH" & process_live$sga_explanation == "Missing both BW and GA") / sum(live_births$facility == "KCH"), 1),
        NA,
        round(100 * sum(all_maternal$facility == "SMCH") / total_deliveries, 1),
        round(100 * sum(live_births$facility == "SMCH") / sum(all_maternal$facility == "SMCH"), 1),
        round(100 * smch_lbw_n / sum(live_births$facility == "SMCH"), 1),
        round(100 * sum(process_live$facility == "SMCH" & process_live$bw_category == "Discarded") / sum(live_births$facility == "SMCH"), 1),
        round(100 * smch_sga_n / sum(live_births$facility == "SMCH"), 1),
        round(100 * sum(process_live$facility == "SMCH" & process_live$sga_explanation == "Missing BW") / sum(live_births$facility == "SMCH"), 1),
        round(100 * sum(process_live$facility == "SMCH" & process_live$sga_explanation == "Missing GA") / sum(live_births$facility == "SMCH"), 1),
        round(100 * sum(process_live$facility == "SMCH" & process_live$sga_explanation == "Missing both BW and GA") / sum(live_births$facility == "SMCH"), 1)
    ),
    Filter_notes = case_when(
        grepl("valid birthweight|LBW sample", description, ignore.case = TRUE) ~ bw_note,
        grepl("valid SGA", description, ignore.case = TRUE) ~ both_note,
        grepl("Discarded.*LBW|Missing.*invalid birthweight", description, ignore.case = TRUE) ~ paste0("NOT (", bw_note, ")"),
        grepl("Discarded.*SGA.*both|Missing both", description, ignore.case = TRUE) ~ "Both BW and GA invalid / missing",
        grepl("Discarded.*SGA.*Missing BW|SGA.*BW$", description, ignore.case = TRUE) ~ "Birth weight invalid / missing",
        grepl("Discarded.*SGA.*Missing GA|SGA.*GA$", description, ignore.case = TRUE) ~ "Gestational age invalid / missing",
        TRUE ~ NA_character_
    )
)

# ===== SAVE OUTPUTS =====

cat("\n=== Saving Results ===\n")

write_csv(sample_size_summary, file.path(output_dir, "01_sample_size_summary.csv"))
cat("Saved: 01_sample_size_summary.csv\n")

write_csv(distribution_combined, file.path(output_dir, "02_distribution_by_category.csv"))
cat("Saved: 02_distribution_by_category.csv\n")

write_csv(by_facility_combined, file.path(output_dir, "03_distribution_by_facility.csv"))
cat("Saved: 03_distribution_by_facility.csv\n")

write_csv(by_year_combined, file.path(output_dir, "04_distribution_by_year.csv"))
cat("Saved: 04_distribution_by_year.csv\n")

write_csv(by_facility_year_combined, file.path(output_dir, "05_distribution_by_facility_and_year.csv"))
cat("Saved: 05_distribution_by_facility_and_year.csv\n")

# ===== SUMMARY REPORT =====

cat("\n", rep("=", 60), "\n", sep = "")
cat("LIVE BIRTHS SUMMARY BY LBW/SGA CATEGORIES\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Sample Sizes:\n")
cat("  Total deliveries:", total_deliveries, "\n")
cat(
    "  Live births:", live_birth_count,
    sprintf("(%.1f%% of deliveries)", 100 * live_birth_count / total_deliveries), "\n"
)
cat(
    "  Valid birthweight:", valid_bw_count,
    sprintf("(%.1f%% of live births)", 100 * valid_bw_count / live_birth_count), "\n"
)
cat(
    "  LBW classified:", nrow(lbw_live),
    sprintf("(%.1f%% of live births)", 100 * nrow(lbw_live) / live_birth_count), "\n"
)
cat(
    "  SGA classified:", nrow(svn_live),
    sprintf("(%.1f%% of live births)", 100 * nrow(svn_live) / live_birth_count), "\n\n"
)

cat("Facilities:\n")
cat("  KCH:", sum(live_births$facility == "KCH"), "live births\n")
cat("  SMCH:", sum(live_births$facility == "SMCH"), "live births\n\n")

cat("Years Analyzed: 2022-2025\n")
cat("  LBW live births with year:", nrow(process_live_year), "\n")
if (nrow(svn_live) > 0) {
    cat("  SGA live births with year:", nrow(process_live_year), "\n")
}
cat("\n")

cat("Birthweight Categories (LBW) - Live Births Only:\n")
print(lbw_distribution %>% select(category, n, percentage))
cat("\n")

if (nrow(sga_distribution) > 0) {
    cat("SGA Categories (SVN) - Live Births Only:\n")
    print(sga_distribution %>% select(category, n, percentage))
    cat("\n")
}

cat("Output files saved to:", output_dir, "\n")
cat("  - 01_sample_size_summary.csv\n")
cat("  - 02_distribution_by_category.csv\n")
cat("  - 03_distribution_by_facility.csv\n")
cat("  - 04_distribution_by_year.csv\n")
cat("  - 05_distribution_by_facility_and_year.csv\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Analysis complete!\n")
