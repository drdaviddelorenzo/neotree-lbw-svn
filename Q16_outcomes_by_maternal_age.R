# Q16_outcomes_by_maternal_age.R
# Analysis of Neonatal Outcomes by Maternal Age Intervals, LBW, and SGA Categories
#
# This script:
# 1. Cleans and processes Maternal Age (variable `matageyrs`).
# 2. Classifies maternal age into user-specified intervals:
#    - < 16
#    - 16 - 19
#    - 20 - 24
#    - 25 - 34
#    - >= 35 (interpreted from >35 to include 35)
# 3. Categorizes Outcomes (Alive vs Neonatal Death).
# 4. Generates a WIDE format table:
#    - Rows: Maternal Age Intervals (per Facility).
#    - Columns: LBW and SGA categories.
#    - Sub-columns: NND (Deaths) and DC (Discharges/Alive).
#    - Ensures ALL categories and intervals are present (even if 0).
#
# Data sources:
# - KCH Malawi: Uses `matageyrs`
# - SMCH Zimbabwe: Uses `matageyrs`

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
    library(stringr)
})

# ===== CONFIGURATION =====
# Input files
kch_file <- "SUBSAMPLE_20220101_to_20251231_KCH_MWI_COMPLETE_ALL_RECORDS_cleaned.csv"
smch_file <- "SUBSAMPLE_20220101_to_20251231_SMCH_ZIM_COMPLETE_ALL_RECORDS_cleaned.csv"

# Output directory
output_dir <- "Q16_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Maternal Age Thresholds (Data Cleaning)
MIN_AGE <- 10
MAX_AGE <- 60

# Intervals
AGE_LEVELS <- c("< 16", "16 - 19", "20 - 24", "25 - 34", ">= 35")

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

# Normalize outcomes
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

# Maternal Age Classification
classify_age_q16 <- function(t) {
    dplyr::case_when(
        is.na(t) ~ NA_character_,
        t < 16 ~ "< 16",
        t >= 16 & t <= 19 ~ "16 - 19",
        t >= 20 & t <= 24 ~ "20 - 24",
        t >= 25 & t <= 34 ~ "25 - 34",
        t >= 35 ~ ">= 35",
        TRUE ~ NA_character_
    )
}

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Newborn Admission Data ===\n")
kch_raw <- suppressMessages(read_csv(kch_file, guess_max = 50000, col_types = cols(.default = "c")))
smch_raw <- suppressMessages(read_csv(smch_file, guess_max = 50000, col_types = cols(.default = "c")))

# Process KCH
kch_proc <- kch_raw %>%
    mutate(
        facility = "KCH",
        mat_age_raw = if ("matageyrs" %in% names(.)) matageyrs else NA_character_
    )

# Process SMCH
smch_proc <- smch_raw %>%
    mutate(
        facility = "SMCH",
        mat_age_raw = if ("matageyrs" %in% names(.)) matageyrs else NA_character_
    )

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

        # Maternal Age
        mat_age_val = suppressWarnings(as.numeric(mat_age_raw)),

        # Validations
        is_valid_bw = is.finite(bw_kg) & bw_kg >= 0.3 & bw_kg <= 7.0,
        is_invalid_ga = is.na(gestation_weeks) | gestation_weeks < 24 | gestation_weeks > 42,
        sga_status = dplyr::if_else(gestation_weeks >= 24 & !is.na(gestation_weeks), classify_sga(bw_kg, gestation_weeks, if ("gender" %in% names(.)) gender else NA_character_), NA_character_),
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

# Filter for valid data (Valid Outcome + Valid Age)
valid_df <- analysis_df %>%
    filter(outcome_norm %in% c("Alive", "Neonatal Death")) %>%
    filter(!is.na(mat_age_val) & mat_age_val >= MIN_AGE & mat_age_val <= MAX_AGE) %>%
    mutate(
        age_interval = classify_age_q16(mat_age_val),
        # Order interval factor
        age_interval = factor(age_interval, levels = AGE_LEVELS)
    )

cat("Total Analysis Population (Valid Outcome, MatAge):", nrow(valid_df), "\n")


# ===== ANALYSIS =====

calc_wide_outcome <- function(data, group_cols, group_levels) {
    category_col <- group_cols[1]

    # 1. Summarize Counts per Facilty/Category/Interval
    summary_long <- data %>%
        filter(!is.na(.data[[category_col]])) %>%
        group_by(facility, category = .data[[category_col]], age_interval) %>%
        summarise(
            NND = sum(outcome_norm == "Neonatal Death"),
            DC = sum(outcome_norm == "Alive"),
            .groups = "drop"
        )

    # Generate Totals (regardless of group)
    totals_long <- data %>%
        group_by(facility, age_interval) %>%
        summarise(
            NND = sum(outcome_norm == "Neonatal Death"),
            DC = sum(outcome_norm == "Alive"),
            .groups = "drop"
        ) %>%
        mutate(category = "Total")

    summary_long <- bind_rows(summary_long, totals_long)

    # 2. Complete the grid
    complete_grid <- expand_grid(
        facility = unique(data$facility),
        category = c(group_levels, "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total"),
        age_interval = AGE_LEVELS
    )

    summary_complete <- complete_grid %>%
        left_join(summary_long, by = c("facility", "category", "age_interval")) %>%
        replace_na(list(NND = 0, DC = 0))

    # 3. Pivot to Wide Format
    summary_wide <- summary_complete %>%
        pivot_wider(
            names_from = category,
            values_from = c(NND, DC),
            names_glue = "{category}_{.value}"
        )

    # 4. Reorder Columns explicitly ensuring everything mapped exists
    col_order <- c("facility", "age_interval")
    for (cat in c(group_levels, "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total")) {
        col_name_nnd <- paste0(cat, "_NND")
        col_name_dc <- paste0(cat, "_DC")
        if (col_name_nnd %in% names(summary_wide)) {
            col_order <- c(col_order, col_name_nnd, col_name_dc)
        }
    }

    summary_wide %>% select(any_of(col_order))
}

calc_avg_mat_age_wide_2rows <- function(data, group_cols, group_levels) {
    category_col <- group_cols[1]

    # Summarize Mean Maternal Age per Facility/Category/Outcome mapped from mat_age_val
    summary_long <- data %>%
        filter(!is.na(.data[[category_col]])) %>%
        group_by(facility, category = .data[[category_col]], outcome = outcome_norm) %>%
        summarise(
            mean_age = round(mean(mat_age_val, na.rm = TRUE), 2),
            .groups = "drop"
        )

    totals_long <- data %>%
        group_by(facility, outcome = outcome_norm) %>%
        summarise(
            mean_age = round(mean(mat_age_val, na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        mutate(category = "Total")

    summary_long <- bind_rows(summary_long, totals_long)

    # Pivot Wide
    summary_wide <- summary_long %>%
        mutate(
            outcome_suffix = if_else(outcome == "Neonatal Death", "NND", "DC"),
            col_name = paste0(category, "_", outcome_suffix)
        ) %>%
        select(facility, col_name, mean_age) %>%
        pivot_wider(
            names_from = col_name,
            values_from = mean_age
        )

    # Generate expected column names
    expected_cols <- c("facility")
    for (cat in c(group_levels, "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total")) {
        col_name_nnd <- paste0(cat, "_NND")
        col_name_dc <- paste0(cat, "_DC")
        # Ensure only active classifications render dynamically, fallback arrays to NAs dynamically
        expected_cols <- c(expected_cols, col_name_nnd, col_name_dc)
    }

    # Add missing columns safely matching previous scripts mappings
    missing_cols <- setdiff(expected_cols, names(summary_wide))
    if (length(missing_cols) > 0) {
        for (col in missing_cols) {
            summary_wide[[col]] <- NA_real_
        }
    }

    # Extract mapped layout matched against explicit totals securely preserving Discard values
    valid_expected <- expected_cols[expected_cols %in% names(summary_wide)]

    summary_wide %>%
        select(any_of(valid_expected)) %>%
        arrange(facility)
}

cat("\n=== Calculating Statistics ===\n")

# LBW Categories
lbw_levels <- c("ELBW", "VLBW", "LBW", "NBW", "HBW")
lbw_wide <- calc_wide_outcome(valid_df, c("bw_category", "lbw_explanation"), lbw_levels) %>%
    mutate(classification = "Birthweight")

lbw_avg_age <- calc_avg_mat_age_wide_2rows(valid_df, c("bw_category", "lbw_explanation"), lbw_levels) %>%
    mutate(classification = "Birthweight")

# SGA Categories
sga_levels <- c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")
sga_wide <- calc_wide_outcome(valid_df, c("sga_category", "sga_explanation"), sga_levels) %>%
    mutate(classification = "SGA")

sga_avg_age <- calc_avg_mat_age_wide_2rows(valid_df, c("sga_category", "sga_explanation"), sga_levels) %>%
    mutate(classification = "SGA")

# ===== SAVE =====

cat("\n=== Saving Results ===\n")

write_csv(lbw_wide, file.path(output_dir, "01_mat_age_outcomes_LBW_wide.csv"))
cat("Saved: 01_mat_age_outcomes_LBW_wide.csv\n")

write_csv(sga_wide, file.path(output_dir, "02_mat_age_outcomes_SGA_wide.csv"))
cat("Saved: 02_mat_age_outcomes_SGA_wide.csv\n")

write_csv(lbw_avg_age, file.path(output_dir, "03_avg_mat_age_by_outcome_LBW_wide.csv"))
cat("Saved: 03_avg_mat_age_by_outcome_LBW_wide.csv\n")

write_csv(sga_avg_age, file.path(output_dir, "04_avg_mat_age_by_outcome_SGA_wide.csv"))
cat("Saved: 04_avg_mat_age_by_outcome_SGA_wide.csv\n")

cat("\nAnalysis complete!\n")
