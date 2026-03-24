# Q7_maternal_age_by_category.R
# Average Maternal Age by LBW and SGA Categories
#
# This script analyzes MATERNAL AGE for live births:
# 1. Distribution of maternal age by birthweight categories (LBW)
# 2. Distribution of maternal age by SGA categories (SVN)
# 3. Calculation of Mean and Median age for each category
# 4. Assessment of missing data (high in both datasets)
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
output_dir <- "Q7_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS
# Must match the constants in maternal_descriptive_analysis.R and Q2–Q6
# so that sample sizes and classification decisions are comparable.
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
#   KCH (MWI)  bwt         — comma thousands, grams   e.g. "2,400" (as.numeric() -> NA)
#   SMCH (ZIM) bwtdis      — plain numeric, kg        e.g. "3.0"   (all values <= 10)
#
# Parsing is explicit and column-specific. No median-based unit inference.
# All parsers return values in KILOGRAMS.

# KCH bwtdis: already in grams -> divide by 1000
parse_kch_bwtdis_kg <- function(x) suppressWarnings(as.numeric(x)) / 1000

# KCH birthweight: already in kg -> use directly
parse_kch_birthweight_kg <- function(x) suppressWarnings(as.numeric(x))

# KCH bwt: comma is thousands separator (e.g. "2,400" = 2400 g) -> strip comma, divide by 1000
parse_kch_bwt_kg <- function(x) suppressWarnings(as.numeric(gsub(",", "", x))) / 1000

# SMCH bwtdis: already in kg -> use directly
parse_smch_bwtdis_kg <- function(x) suppressWarnings(as.numeric(x))

# Build composite bw_kg for KCH: bwtdis (P1) > birthweight (P2) > bwt (P3), result in kg.
# bwtdis always overwrites when present; lower-priority columns fill NA slots only.
build_kch_bw_kg <- function(df) {
    result <- rep(NA_real_, nrow(df))
    if ("bwt" %in% names(df)) {
        v <- parse_kch_bwt_kg(df$bwt)
        result <- ifelse(!is.na(v), v, result)
    }
    if ("birthweight" %in% names(df)) {
        v <- parse_kch_birthweight_kg(df$birthweight)
        result <- ifelse(!is.na(v), v, result)
    }
    if ("bwtdis" %in% names(df)) {
        v <- parse_kch_bwtdis_kg(df$bwtdis)
        result <- ifelse(!is.na(v), v, result)
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
# raw_data: the individual-level data frame with matage; used to compute
#           correct median and SD for the Total row (cannot be derived
#           from per-category summaries).
add_totals <- function(df, group_cols = character(0), raw_data = NULL) {
    if (nrow(df) == 0) {
        return(df)
    }

    if (length(group_cols) == 0) {
        ages <- if (!is.null(raw_data)) raw_data$matage else numeric(0)
        totals <- tibble(
            category       = "Total",
            explanation    = "All deliveries considered",
            n              = sum(df$n),
            classification = unique(df$classification),
            mean_age       = round(sum(df$n * df$mean_age, na.rm = TRUE) / sum(df$n), 1),
            median_age     = if (length(ages) > 0) median(ages, na.rm = TRUE) else NA_real_,
            sd_age         = if (length(ages) > 0) round(sd(ages, na.rm = TRUE), 1) else NA_real_,
            min_age        = min(df$min_age, na.rm = TRUE),
            max_age        = max(df$max_age, na.rm = TRUE)
        )
        return(bind_rows(df, totals))
    }

    totals <- df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
            n        = sum(n),
            mean_age = round(sum(n * mean_age, na.rm = TRUE) / sum(n), 1),
            min_age  = min(min_age, na.rm = TRUE),
            max_age  = max(max_age, na.rm = TRUE),
            .groups  = "drop"
        )

    # Compute correct median and SD per group from raw data if available
    if (!is.null(raw_data) && length(group_cols) > 0) {
        raw_stats <- raw_data %>%
            group_by(across(all_of(group_cols))) %>%
            summarise(
                median_age = median(matage, na.rm = TRUE),
                sd_age     = round(sd(matage, na.rm = TRUE), 1),
                .groups    = "drop"
            )
        totals <- totals %>% left_join(raw_stats, by = group_cols)
    } else {
        totals <- totals %>% mutate(median_age = NA_real_, sd_age = NA_real_)
    }

    totals <- totals %>%
        mutate(
            category       = "Total",
            explanation    = "All deliveries considered",
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

kch_proc <- kch_raw %>%
    mutate(
        bw_kg    = build_kch_bw_kg(.),
        matage   = suppressWarnings(as.numeric(matageyrs)),
        facility = "KCH"
    )
n_kch_bwtdis <- sum(!is.na(suppressWarnings(as.numeric(kch_raw$bwtdis))))
n_kch_birthweight <- sum(!is.na(suppressWarnings(as.numeric(kch_raw$birthweight))))
n_kch_bwt <- sum(!is.na(suppressWarnings(as.numeric(gsub(",", "", kch_raw$bwt)))))
n_kch_composite <- sum(!is.na(kch_proc$bw_kg))
cat(sprintf("  KCH bwtdis      (P1, grams):             %d records\n", n_kch_bwtdis))
cat(sprintf("  KCH birthweight (P2, kg):                %d records\n", n_kch_birthweight))
cat(sprintf("  KCH bwt         (P3, comma-g):           %d records\n", n_kch_bwt))
cat(sprintf(
    "  KCH bw_kg composite: %d / %d (%.1f%%)\n",
    n_kch_composite, nrow(kch_raw), 100 * n_kch_composite / nrow(kch_raw)
))

smch_proc <- smch_raw %>%
    mutate(
        bw_kg    = parse_smch_bwtdis_kg(bwtdis),
        matage   = suppressWarnings(as.numeric(matageyrs)),
        facility = "SMCH"
    )
n_smch_composite <- sum(!is.na(smch_proc$bw_kg))
cat(sprintf(
    "  SMCH bwtdis (sole source, kg): %d / %d (%.1f%%)\n",
    n_smch_composite, nrow(smch_raw), 100 * n_smch_composite / nrow(smch_raw)
))

# Combine datasets
cat("Combining datasets...\n")
all_maternal <- bind_rows(kch_proc, smch_proc)

# Add gestational age
all_maternal <- all_maternal %>%
    mutate(
        gestation_weeks = suppressWarnings(as.numeric(gestation))
    )

total_deliveries <- nrow(all_maternal)
cat("Total deliveries:", total_deliveries, "\n")

# ===== FILTER FOR LIVE BIRTHS =====

cat("\n=== Filtering for Live Births ===\n")

# Filter for live births (LB)
live_births <- all_maternal %>%
    filter(
        !is.na(neotreeoutcome),
        toupper(trimws(neotreeoutcome)) == "LB"
    )

lb_count <- nrow(live_births)
cat("Total live births:", lb_count, "\n")

# ===== MATERNAL AGE DATA QUALITY =====

cat("\n=== Maternal Age Data Quality ===\n")

# Check for valid maternal age (10-55 years reasonable range)
lb_with_age <- live_births %>%
    filter(!is.na(matage) & matage >= 10 & matage <= 55)

valid_age_count <- nrow(lb_with_age)
missing_age_count <- lb_count - valid_age_count

cat("Live births with valid maternal age:", valid_age_count, "\n")
cat("Missing maternal age:", missing_age_count, "\n")
cat("Missingness rate:", round(100 * missing_age_count / lb_count, 1), "%\n")

# Breakdown by facility
missing_by_facility <- live_births %>%
    group_by(facility) %>%
    summarise(
        total_lb = n(),
        valid_age = sum(!is.na(matage) & matage >= 10 & matage <= 55),
        missing_age = sum(is.na(matage) | matage < 10 | matage > 55),
        missing_pct = round(100 * missing_age / n(), 1)
    )
print(missing_by_facility)

# ===== PROCESS BIRTHWEIGHT AND SGA =====

cat("\n=== Processing LBW and SGA Categories ===\n")

analysis_df <- lb_with_age %>%
    mutate(
        # Validations — thresholds from shared constants (match Q2–Q6 and maternal_descriptive_analysis.R)
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

cat("Total records entering categorizations:", nrow(analysis_df), "\n")

# ===== ANALYSIS: MATERNAL AGE BY LBW CATEGORY =====

cat("\n=== Maternal Age by LBW Category ===\n")

lbw_age_stats <- analysis_df %>%
    group_by(category = bw_category, explanation = lbw_explanation) %>%
    summarise(
        n = n(),
        mean_age = round(mean(matage, na.rm = TRUE), 1),
        median_age = median(matage, na.rm = TRUE),
        sd_age = round(sd(matage, na.rm = TRUE), 1),
        min_age = min(matage, na.rm = TRUE),
        max_age = max(matage, na.rm = TRUE),
        classification = "Birthweight",
        .groups = "drop"
    ) %>%
    arrange(match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

print(lbw_age_stats)

# Stratified by facility
lbw_age_facility <- analysis_df %>%
    group_by(facility, category = bw_category, explanation = lbw_explanation) %>%
    summarise(
        n = n(),
        mean_age = round(mean(matage, na.rm = TRUE), 1),
        median_age = median(matage, na.rm = TRUE),
        sd_age = round(sd(matage, na.rm = TRUE), 1),
        min_age = min(matage, na.rm = TRUE),
        max_age = max(matage, na.rm = TRUE),
        classification = "Birthweight",
        .groups = "drop"
    ) %>%
    arrange(facility, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# ===== ANALYSIS: MATERNAL AGE BY SGA CATEGORY =====

cat("\n=== Maternal Age by SGA Category ===\n")

sga_age_stats <- analysis_df %>%
    group_by(category = sga_category, explanation = sga_explanation) %>%
    summarise(
        n = n(),
        mean_age = round(mean(matage, na.rm = TRUE), 1),
        median_age = median(matage, na.rm = TRUE),
        sd_age = round(sd(matage, na.rm = TRUE), 1),
        min_age = min(matage, na.rm = TRUE),
        max_age = max(matage, na.rm = TRUE),
        classification = "SGA",
        .groups = "drop"
    ) %>%
    arrange(
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

print(sga_age_stats)

# Stratified by facility
sga_age_facility <- analysis_df %>%
    group_by(facility, category = sga_category, explanation = sga_explanation) %>%
    summarise(
        n = n(),
        mean_age = round(mean(matage, na.rm = TRUE), 1),
        median_age = median(matage, na.rm = TRUE),
        sd_age = round(sd(matage, na.rm = TRUE), 1),
        min_age = min(matage, na.rm = TRUE),
        max_age = max(matage, na.rm = TRUE),
        classification = "SGA",
        .groups = "drop"
    ) %>%
    arrange(
        facility,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# Apply totals row wrappers to outputs
lbw_age_stats <- add_totals(lbw_age_stats, raw_data = analysis_df)
lbw_age_facility <- add_totals(lbw_age_facility, group_cols = "facility", raw_data = analysis_df)

sga_age_stats <- add_totals(sga_age_stats, raw_data = analysis_df)
sga_age_facility <- add_totals(sga_age_facility, group_cols = "facility", raw_data = analysis_df)

# ===== COMBINE RESULTS =====


all_stats <- bind_rows(lbw_age_stats, sga_age_stats)
all_facility_stats <- bind_rows(lbw_age_facility, sga_age_facility)

# ===== SAMPLE SIZE SUMMARY =====

sample_size_summary <- tibble(
    description = c(
        "Total live births",
        "Live births with valid maternal age",
        "Records with valid age AND birthweight (LBW sample)",
        "Discarded for LBW (Missing/invalid birthweight)",
        "Records with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        lb_count,
        valid_age_count,
        sum(analysis_df$is_valid_bw),
        sum(!analysis_df$is_valid_bw),
        sum(analysis_df$is_valid_sga),
        sum(analysis_df$sga_explanation == "Missing BW"),
        sum(analysis_df$sga_explanation == "Missing GA"),
        sum(analysis_df$sga_explanation == "Missing both BW and GA")
    ),
    pct_of_valid_maternal_age = round(100 * n / valid_age_count, 1),
    Filter_notes = case_when(
        grepl("^Total live births", description, ignore.case = TRUE) ~
            "neotreeoutcome == \"LB\"",
        grepl("valid maternal age", description, ignore.case = TRUE) ~
            "!is.na(matage) & matage >= 10 & matage <= 55",
        grepl("valid age AND birthweight|LBW sample", description, ignore.case = TRUE) ~
            sprintf(
                "matage valid AND is.finite(bw_kg) & bw_kg >= %.1f kg & bw_kg <= %.1f kg",
                BW_MIN_KG, BW_MAX_KG
            ),
        grepl("Discarded.*LBW|missing.*invalid birthweight", description, ignore.case = TRUE) ~
            sprintf(
                "NOT (is.finite(bw_kg) & bw_kg >= %.1f kg & bw_kg <= %.1f kg)",
                BW_MIN_KG, BW_MAX_KG
            ),
        grepl("valid SGA", description, ignore.case = TRUE) ~
            sprintf(
                "BW valid AND !is.na(gestation) & gestation >= %d wks & gestation <= %d wks",
                GA_MIN, GA_MAX
            ),
        grepl("Discarded.*SGA.*both|missing both", description, ignore.case = TRUE) ~
            "Both BW and GA invalid / missing",
        grepl("Discarded.*SGA.*Missing BW|SGA.*BW$", description, ignore.case = TRUE) ~
            "Birth weight invalid / missing",
        grepl("Discarded.*SGA.*Missing GA|SGA.*GA$", description, ignore.case = TRUE) ~
            "Gestational age invalid / missing",
        TRUE ~ NA_character_
    )
)

# ===== SAVE OUTPUTS =====

cat("\n=== Saving Results ===\n")

write_csv(sample_size_summary, file.path(output_dir, "01_sample_size_summary.csv"))
cat("Saved: 01_sample_size_summary.csv\n")

write_csv(all_stats, file.path(output_dir, "02_maternal_age_by_category.csv"))
cat("Saved: 02_maternal_age_by_category.csv\n")

write_csv(all_facility_stats, file.path(output_dir, "03_maternal_age_by_category_and_facility.csv"))
cat("Saved: 03_maternal_age_by_category_and_facility.csv\n")

write_csv(missing_by_facility, file.path(output_dir, "04_data_quality_missing_age.csv"))
cat("Saved: 04_data_quality_missing_age.csv\n")
cat("\nAnalysis complete!\n")
