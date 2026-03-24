# Q12_age_at_death.R
# Analysis of Age at Time of Death by LBW and SGA Categories
#
# This script analyzes the timing of Neonatal Deaths (NND):
# 1. Filters for "Neonatal Death" outcomes.
# 2. Calculates Duration of Stay = datetimedeath - datetimeadmission.
#    (Note: 'lengthofstay' variable is mostly missing for NNDs, so must verify timestamps).
# 3. Calculates Age at Death (days) = (Age at Admission in hours / 24) + Duration of Stay (days).
# 4. Categorizes Age at Death into bins (0-1, 1-3, 3-7, 7-14, 14-21, >21 days).
# 5. Stratifies by LBW and SGA categories.
# 6. Stratifies by Facility.
#
# Data sources:
# - KCH Malawi newborn admissions
# - SMCH Zimbabwe newborn admissions

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
    library(lubridate)
})

# ===== CONFIGURATION =====
# Input files
kch_file <- "LBW_KCH_NNU.csv"
smch_file <- "LBW_SMCH_NNU.csv"

# Output directory
output_dir <- "Q12_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS
# Must match the thresholds used in analysis_df below.
# ---------------------------------------------------------------------------
BW_MIN_KG <- 0.3 # minimum plausible birth weight (kg) — 300 g
BW_MAX_KG <- 7.0 # maximum plausible birth weight (kg) — 7000 g
GA_MIN <- 24 # minimum gestational age for SGA classification (weeks)
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

to_kg <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    pos <- x_num[is.finite(x_num) & x_num > 0]
    if (length(pos) > 0) {
        med <- median(pos, na.rm = TRUE)
        if (is.finite(med) && med > 20 && med <= 7000) {
            return(x_num / 1000)
        }
    }
    x_num
}

get_bw_column <- function(df) {
    result <- rep(NA_character_, nrow(df))
    if ("birthweight" %in% names(df)) {
        result <- ifelse(is.na(result) & !is.na(df$birthweight), df$birthweight, result)
    }
    result
}

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

classify_sga <- function(bw_kg, gestation_weeks, gender_val = NA_character_) {
    percentile_10 <- mapply(get_10th_percentile_bw, gestation_weeks, gender_val, USE.NAMES = FALSE)
    ifelse(is.na(bw_kg) | is.na(percentile_10), NA_character_,
        ifelse(bw_kg < percentile_10, "SGA", "AGA")
    )
}

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

# Normalize outcomes to identify Neonatal Deaths
normalise_outcome <- function(x) {
    y <- tolower(trimws(as.character(x)))
    y <- gsub("[^a-z0-9]+", " ", y)
    y <- gsub("\\s+", " ", y)

    dplyr::case_when(
        grepl("\\bdc\\b|discharged|\\bdcr\\b|\\bdpc\\b|\\bdckmc\\b|kangaroo mother care", y) ~ "Alive",
        grepl("\\babs\\b|abscond|\\bdama\\b|against medical advice", y) ~ "Alive",
        grepl("\\btrh\\b|\\btro\\b|transfer", y) ~ "Alive",
        grepl("\\bnnd\\b|neonatal death|\\bdda\\b|died during admission", y) ~ "Neonatal Death",
        grepl("\\bbid\\b|brought in dead", y) ~ "Neonatal Death",
        grepl("\\bstb\\b|stillbirth|still born", y) ~ "Stillbirth",
        y == "" ~ NA_character_,
        TRUE ~ "Unknown/Other"
    )
}

# Classify Age at Death
classify_age_at_death <- function(days) {
    dplyr::case_when(
        is.na(days) | days < 0 ~ NA_character_,
        days <= 1.0 ~ "0-1 days",
        days > 1.0 & days <= 3.0 ~ "1-3 days",
        days > 3.0 & days <= 7.0 ~ "3-7 days",
        days > 7.0 & days <= 14.0 ~ "7-14 days",
        days > 14.0 & days <= 21.0 ~ "14-21 days",
        days > 21.0 ~ ">21 days",
        TRUE ~ NA_character_
    )
}

# New function: Apply Total Wrapper specific for Age at Death Outputs
add_totals_age <- function(df, raw_df, classification_name, by_facility = FALSE) {
    if (nrow(df) == 0) {
        return(df)
    }

    if (by_facility) {
        totals <- raw_df %>%
            group_by(facility) %>%
            summarise(
                n_total_deaths = n(),
                `0-1 days` = sum(age_at_death_cat == "0-1 days", na.rm = TRUE),
                `1-3 days` = sum(age_at_death_cat == "1-3 days", na.rm = TRUE),
                `3-7 days` = sum(age_at_death_cat == "3-7 days", na.rm = TRUE),
                `7-14 days` = sum(age_at_death_cat == "7-14 days", na.rm = TRUE),
                `14-21 days` = sum(age_at_death_cat == "14-21 days", na.rm = TRUE),
                `>21 days` = sum(age_at_death_cat == ">21 days", na.rm = TRUE),
                `Unknown Age` = sum(is.na(age_at_death_cat), na.rm = TRUE),
                .groups = "drop"
            ) %>%
            mutate(
                category = "Total",
                explanation = "All deaths considered",
                classification = classification_name
            )
        group_cols <- "facility"
    } else {
        totals <- raw_df %>%
            summarise(
                n_total_deaths = n(),
                `0-1 days` = sum(age_at_death_cat == "0-1 days", na.rm = TRUE),
                `1-3 days` = sum(age_at_death_cat == "1-3 days", na.rm = TRUE),
                `3-7 days` = sum(age_at_death_cat == "3-7 days", na.rm = TRUE),
                `7-14 days` = sum(age_at_death_cat == "7-14 days", na.rm = TRUE),
                `14-21 days` = sum(age_at_death_cat == "14-21 days", na.rm = TRUE),
                `>21 days` = sum(age_at_death_cat == ">21 days", na.rm = TRUE),
                `Unknown Age` = sum(is.na(age_at_death_cat), na.rm = TRUE),
                .groups = "drop"
            ) %>%
            mutate(
                category = "Total",
                explanation = "All deaths considered",
                classification = classification_name,
                facility = "Overall"
            )
        group_cols <- character(0)
    }

    bind_rows(df, totals) %>%
        arrange(
            if (by_facility) facility else NULL,
            match(category, c(
                "ELBW", "VLBW", "LBW", "NBW", "HBW",
                "Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA",
                "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total"
            ))
        )
}

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Newborn Admission Data ===\n")
kch_raw <- suppressMessages(read_csv(kch_file, guess_max = 50000, col_types = cols(.default = "c")))
smch_raw <- suppressMessages(read_csv(smch_file, guess_max = 50000, col_types = cols(.default = "c")))

kch_proc <- kch_raw %>% mutate(facility = "KCH")
smch_proc <- smch_raw %>% mutate(facility = "SMCH")
all_admissions <- bind_rows(kch_proc, smch_proc)
cat("Total records loaded:", nrow(all_admissions), "\n")

# Process
cat("Processing variables...\n")
analysis_df <- all_admissions %>%
    mutate(
        bw_raw = get_bw_column(.),
        bw_kg = to_kg(bw_raw),
        gestation_weeks = suppressWarnings(as.numeric(gestation)),

        # Outcome
        outcome_norm = normalise_outcome(neotreeoutcome),

        # Age at Admission
        age_at_admission_hours = suppressWarnings(as.numeric(age)),
        age_at_admission_days = age_at_admission_hours / 24,

        # Timestamps
        adm_ts = ymd_hms(datetimeadmission),
        dth_ts = ymd_hms(datetimedeath),

        # Calculate Duration (LOS equivalent for deaths)
        # Note: lengthofstay variable is mostly NA for NNDs
        duration_death_days = as.numeric(difftime(dth_ts, adm_ts, units = "days")),

        # Calculate Age at Death (days)
        # Age at Admission + Duration of Admission
        age_at_death_days = age_at_admission_days + duration_death_days,

        # Validations
        is_valid_bw = is.finite(bw_kg) & bw_kg >= BW_MIN_KG & bw_kg <= BW_MAX_KG,
        is_invalid_ga = is.na(gestation_weeks) | gestation_weeks < GA_MIN | gestation_weeks > GA_MAX,
        sga_status = dplyr::if_else(gestation_weeks >= GA_MIN & !is.na(gestation_weeks), classify_sga(bw_kg, gestation_weeks, if ("gender" %in% names(.)) gender else NA_character_), NA_character_),
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
        sga_category = if_else(is_valid_sga, sga_cat_temp, sga_explanation),
        age_at_death_cat = classify_age_at_death(age_at_death_days)
    )

# Filter for NNDs with valid Birthweight and valid Age at Death

# 1. Study Population: Total Admissions
n_total_admissions <- nrow(analysis_df)

# 2. Neonatal Deaths
nnd_df <- analysis_df %>% filter(outcome_norm == "Neonatal Death")
n_total_nnd <- nrow(nnd_df)

cat("\n=== Sample Size Summary ===\n")
cat("Total Admissions:", n_total_admissions, "\n")
cat("Total Neonatal Deaths (NND):", n_total_nnd, "\n")
cat("  as % of Admissions:", round(100 * n_total_nnd / n_total_admissions, 2), "%\n")

# ===== ANALYSIS =====

analyze_age_at_death <- function(data, group_var, explanation_var) {
    # 1. Total NNDs per category
    totals <- data %>%
        group_by(category = .data[[group_var]], explanation = .data[[explanation_var]]) %>%
        summarise(n_total_deaths = n(), .groups = "drop")

    # 2. Counts per Age Bin
    bins <- data %>%
        mutate(age_bin = if_else(is.na(age_at_death_cat), "Unknown Age", age_at_death_cat)) %>%
        count(category = .data[[group_var]], age_bin) %>%
        pivot_wider(names_from = age_bin, values_from = n, values_fill = 0)

    # Ensure all bin columns exist
    expected_bins <- c("0-1 days", "1-3 days", "3-7 days", "7-14 days", "14-21 days", ">21 days", "Unknown Age")
    for (bin in expected_bins) {
        if (!bin %in% names(bins)) bins[[bin]] <- 0
    }

    # 3. Combine and sort
    left_join(totals, bins, by = "category") %>%
        select(category, explanation, n_total_deaths, all_of(expected_bins))
}


cat("\n=== Stratified Analysis ===\n")

lbw_levels <- c("ELBW", "VLBW", "LBW", "NBW", "HBW")
sga_levels <- c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")

# --- Overall ---

lbw_overall <- analyze_age_at_death(nnd_df, "bw_category", "lbw_explanation") %>%
    mutate(facility = "Overall", classification = "Birthweight") %>%
    add_totals_age(nnd_df, "Birthweight", by_facility = FALSE)

sga_overall <- analyze_age_at_death(nnd_df, "sga_category", "sga_explanation") %>%
    mutate(facility = "Overall", classification = "SGA") %>%
    add_totals_age(nnd_df, "SGA", by_facility = FALSE)

all_overall <- bind_rows(lbw_overall, sga_overall)

# --- By Facility ---

lbw_facility <- nnd_df %>%
    group_by(facility) %>%
    group_modify(~ analyze_age_at_death(.x, "bw_category", "lbw_explanation")) %>%
    mutate(classification = "Birthweight") %>%
    add_totals_age(nnd_df, "Birthweight", by_facility = TRUE)

sga_facility <- nnd_df %>%
    group_by(facility) %>%
    group_modify(~ analyze_age_at_death(.x, "sga_category", "sga_explanation")) %>%
    mutate(classification = "SGA") %>%
    add_totals_age(nnd_df, "SGA", by_facility = TRUE)

all_facility <- bind_rows(lbw_facility, sga_facility)

# ===== SAVE =====
cat("\n=== Saving Results ===\n")

write_csv(all_overall, file.path(output_dir, "01_age_at_death_stratified.csv"))
cat("Saved: 01_age_at_death_stratified.csv\n")

write_csv(all_facility, file.path(output_dir, "02_age_at_death_by_facility.csv"))
cat("Saved: 02_age_at_death_by_facility.csv\n")

# Save sample size summary
bw_note <- sprintf(
    "is.finite(bw_kg) & bw_kg >= %.1f kg & bw_kg <= %.1f kg",
    BW_MIN_KG, BW_MAX_KG
)
ga_note <- sprintf(
    "!is.na(gestation) & gestation >= %d wks & gestation <= %d wks",
    GA_MIN, GA_MAX
)
sga_note <- paste0(bw_note, " AND ", ga_note, " AND !is.na(sga_category)")
death_note <- "normalise_outcome(neotreeoutcome) == \"Neonatal Death\""
age_note <- "is.finite(age_at_death_days) & age_at_death_days >= 0"

sample_size_summary <- tibble(
    description = c(
        "Total admissions",
        "Total neonatal deaths (NND)",
        "NNDs with valid age at death",
        "NNDs with missing/invalid age at death",
        "Total admissions with valid birthweight (LBW sample)",
        "Discarded for LBW (missing/invalid birthweight)",
        "NNDs with valid birthweight",
        "Total admissions with valid SGA classification",
        "NNDs with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        n_total_admissions,
        n_total_nnd,
        sum(!is.na(nnd_df$age_at_death_cat)),
        sum(is.na(nnd_df$age_at_death_cat)),
        sum(analysis_df$is_valid_bw),
        sum(!analysis_df$is_valid_bw),
        sum(nnd_df$is_valid_bw),
        sum(analysis_df$is_valid_sga),
        sum(nnd_df$is_valid_sga),
        sum(analysis_df$sga_explanation == "Missing BW"),
        sum(analysis_df$sga_explanation == "Missing GA"),
        sum(analysis_df$sga_explanation == "Missing both BW and GA")
    ),
    pct_of_total = round(100 * n / n_total_admissions, 1),
    Filter_notes = c(
        NA_character_,
        death_note,
        paste0(death_note, " AND ", age_note),
        paste0(death_note, " AND NOT (", age_note, ")"),
        bw_note,
        paste0("NOT (", bw_note, ")"),
        paste0(death_note, " AND ", bw_note),
        sga_note,
        paste0(death_note, " AND ", sga_note),
        "Invalid/missing birth weight",
        "Invalid/missing gestational age",
        "Both BW and GA invalid/missing"
    )
)
write_csv(sample_size_summary, file.path(output_dir, "00_sample_size_summary.csv"))
cat("Saved: 00_sample_size_summary.csv\n")

cat("\nAnalysis complete!\n")
