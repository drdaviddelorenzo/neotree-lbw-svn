# Q11_temperature_on_admission.R
# Analysis of Temperature on Admission by LBW and SGA Categories
#
# This script analyzes Admission Temperature:
# 1. Calculates Mean Admission Temperature.
# 2. Classifies Hypothermia (Mild, Moderate, Severe).
# 3. Stratifies by LBW and SGA categories.
# 4. Stratifies by Facility.
# 5. Reports missing data counts.
#
# Data sources:
# - KCH Malawi: Uses `temperatureonarrival`
# - SMCH Zimbabwe: Uses `temperature`

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
})

# ===== CONFIGURATION =====
# Input files
kch_file <- "SUBSAMPLE_20220101_to_20251231_KCH_MWI_COMPLETE_ALL_RECORDS_cleaned.csv"
smch_file <- "SUBSAMPLE_20220101_to_20251231_SMCH_ZIM_COMPLETE_ALL_RECORDS_cleaned.csv"

# Output directory
output_dir <- "Q11_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Temperature Thresholds
# Valid range for analysis (exclude typos like 0, 360, etc.)
MIN_TEMP <- 30.0
MAX_TEMP <- 45.0

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

# New: Temperature Classification (WHO)
classify_temp <- function(t) {
    dplyr::case_when(
        is.na(t) ~ NA_character_,
        t < 32.0 ~ "Severe Hypothermia (<32.0)",
        t >= 32.0 & t < 36.0 ~ "Moderate Hypothermia (32.0-35.9)",
        t >= 36.0 & t < 36.5 ~ "Mild Hypothermia (36.0-36.4)",
        t >= 36.5 & t <= 37.5 ~ "Normothermia (36.5-37.5)",
        t > 37.5 ~ "Hyperthermia (>37.5)",
        TRUE ~ NA_character_
    )
}

# New: Outcome Normalization
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

# New: Apply Total Wrapper specific for Temperature Outputs
add_totals_temp <- function(df, raw_df, classification_name, by_facility = FALSE) {
    if (nrow(df) == 0) {
        return(df)
    }

    if (by_facility) {
        totals <- raw_df %>%
            group_by(facility) %>%
            summarise(
                n_total = n(),
                n_missing = sum(!is_valid_temp, na.rm = TRUE),
                n_analyzed = sum(is_valid_temp, na.rm = TRUE),
                pct_missing = round(100 * sum(!is_valid_temp, na.rm = TRUE) / n(), 1),
                mean_temp = round(mean(temp_val[is_valid_temp], na.rm = TRUE), 2),
                sd_temp = round(sd(temp_val[is_valid_temp], na.rm = TRUE), 2),
                n_hypo_severe = sum(temp_class == "Severe Hypothermia (<32.0)", na.rm = TRUE),
                n_hypo_mod = sum(temp_class == "Moderate Hypothermia (32.0-35.9)", na.rm = TRUE),
                n_hypo_mild = sum(temp_class == "Mild Hypothermia (36.0-36.4)", na.rm = TRUE),
                pct_hypo_severe = if_else(n_analyzed > 0, round(100 * n_hypo_severe / n_analyzed, 1), 0),
                pct_hypo_mod = if_else(n_analyzed > 0, round(100 * n_hypo_mod / n_analyzed, 1), 0),
                pct_any_hypo = if_else(n_analyzed > 0, round(100 * (n_hypo_severe + n_hypo_mod + n_hypo_mild) / n_analyzed, 1), 0),
                .groups = "drop"
            ) %>%
            mutate(
                category = "Total",
                explanation = "All admissions considered",
                classification = classification_name
            )
        group_cols <- "facility"
    } else {
        totals <- raw_df %>%
            summarise(
                n_total = n(),
                n_missing = sum(!is_valid_temp, na.rm = TRUE),
                n_analyzed = sum(is_valid_temp, na.rm = TRUE),
                pct_missing = round(100 * sum(!is_valid_temp, na.rm = TRUE) / n(), 1),
                mean_temp = round(mean(temp_val[is_valid_temp], na.rm = TRUE), 2),
                sd_temp = round(sd(temp_val[is_valid_temp], na.rm = TRUE), 2),
                n_hypo_severe = sum(temp_class == "Severe Hypothermia (<32.0)", na.rm = TRUE),
                n_hypo_mod = sum(temp_class == "Moderate Hypothermia (32.0-35.9)", na.rm = TRUE),
                n_hypo_mild = sum(temp_class == "Mild Hypothermia (36.0-36.4)", na.rm = TRUE),
                pct_hypo_severe = if_else(n_analyzed > 0, round(100 * n_hypo_severe / n_analyzed, 1), 0),
                pct_hypo_mod = if_else(n_analyzed > 0, round(100 * n_hypo_mod / n_analyzed, 1), 0),
                pct_any_hypo = if_else(n_analyzed > 0, round(100 * (n_hypo_severe + n_hypo_mod + n_hypo_mild) / n_analyzed, 1), 0),
                .groups = "drop"
            ) %>%
            mutate(
                category = "Total",
                explanation = "All admissions considered",
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

# Process KCH
kch_proc <- kch_raw %>%
    mutate(
        facility = "KCH",
        temp_raw = if ("temperatureonarrival" %in% names(.)) temperatureonarrival else NA_character_
    )

# Process SMCH
smch_proc <- smch_raw %>%
    mutate(
        facility = "SMCH",
        temp_raw = if ("temperature" %in% names(.)) temperature else NA_character_
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

        # Temperature (Unified column)
        temp_val = suppressWarnings(as.numeric(temp_raw)),

        # Outcome
        outcome_norm = normalise_outcome(neotreeoutcome),

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

# Identify valid temperature
# We keep the temp_class as NA if temp is invalid/missing
analysis_df <- analysis_df %>%
    mutate(
        is_valid_temp = !is.na(temp_val) & temp_val >= MIN_TEMP & temp_val <= MAX_TEMP,
        temp_class = if_else(is_valid_temp, classify_temp(temp_val), NA_character_)
    )

cat("Total Study Population:", nrow(analysis_df), "\n")
cat("Records with missing/invalid temperature:", sum(!analysis_df$is_valid_temp), "\n")
cat("Records with valid temperature:", sum(analysis_df$is_valid_temp), "\n")


# ===== ANALYSIS =====

calc_temp_stats <- function(data, group_var) {
    data %>%
        filter(!is.na(.data[[group_var]])) %>%
        group_by(across(all_of(group_var))) %>%
        summarise(
            n_total = n(),
            n_missing = sum(!is_valid_temp, na.rm = TRUE),
            n_analyzed = sum(is_valid_temp, na.rm = TRUE),
            pct_missing = round(100 * n_missing / n_total, 1),

            # Statistics (only on analyzed)
            mean_temp = round(mean(temp_val[is_valid_temp], na.rm = TRUE), 2),
            sd_temp = round(sd(temp_val[is_valid_temp], na.rm = TRUE), 2),

            # Hypothermia Rates (denominator = n_analyzed)
            n_hypo_severe = sum(temp_class == "Severe Hypothermia (<32.0)", na.rm = TRUE),
            n_hypo_mod = sum(temp_class == "Moderate Hypothermia (32.0-35.9)", na.rm = TRUE),
            n_hypo_mild = sum(temp_class == "Mild Hypothermia (36.0-36.4)", na.rm = TRUE),
            pct_hypo_severe = round(100 * n_hypo_severe / n_analyzed, 1),
            pct_hypo_mod = round(100 * n_hypo_mod / n_analyzed, 1),
            pct_any_hypo = round(100 * (n_hypo_severe + n_hypo_mod + n_hypo_mild) / n_analyzed, 1),
            .groups = "drop"
        )
}

calc_temp_by_outcome <- function(data, group_var, explanation_var) {
    data %>%
        filter(!is.na(.data[[group_var]])) %>%
        filter(outcome_norm %in% c("Alive", "Neonatal Death")) %>%
        filter(is_valid_temp) %>%
        group_by(facility, outcome = outcome_norm, category = .data[[group_var]], explanation = .data[[explanation_var]]) %>%
        summarise(
            n = n(),
            mean_temp = round(mean(temp_val, na.rm = TRUE), 2),
            .groups = "drop"
        )
}

# New function: Calculate Total Mean Temp by Outcome
add_totals_outcome <- function(df, raw_df, classification_name) {
    totals <- raw_df %>%
        filter(outcome_norm %in% c("Alive", "Neonatal Death")) %>%
        filter(is_valid_temp) %>%
        group_by(facility, outcome = outcome_norm) %>%
        summarise(
            n = n(),
            mean_temp = round(mean(temp_val, na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        mutate(
            category = "Total",
            explanation = "All admissions considered",
            classification = classification_name
        )

    bind_rows(df, totals) %>%
        arrange(
            facility, outcome,
            match(category, c(
                "ELBW", "VLBW", "LBW", "NBW", "HBW",
                "Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA",
                "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total"
            ))
        )
}

cat("\n=== Calculating Statistics ===\n")

# LBW Categories
lbw_overall <- analysis_df %>%
    rename(category = bw_category, explanation = lbw_explanation) %>%
    calc_temp_stats("category") %>%
    mutate(facility = "Overall", classification = "Birthweight") %>%
    add_totals_temp(analysis_df, "Birthweight", by_facility = FALSE)

lbw_facility <- analysis_df %>%
    rename(category = bw_category, explanation = lbw_explanation) %>%
    group_by(facility) %>%
    group_modify(~ calc_temp_stats(.x, "category")) %>%
    mutate(classification = "Birthweight") %>%
    add_totals_temp(analysis_df, "Birthweight", by_facility = TRUE)

# LBW by Outcome
lbw_outcome <- calc_temp_by_outcome(analysis_df, "bw_category", "lbw_explanation") %>%
    mutate(classification = "Birthweight") %>%
    add_totals_outcome(analysis_df, "Birthweight")

# SGA Categories
sga_overall <- analysis_df %>%
    rename(category = sga_category, explanation = sga_explanation) %>%
    calc_temp_stats("category") %>%
    mutate(facility = "Overall", classification = "SGA") %>%
    add_totals_temp(analysis_df, "SGA", by_facility = FALSE) %>%
    arrange(
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total"))
    )

sga_facility <- analysis_df %>%
    rename(category = sga_category, explanation = sga_explanation) %>%
    group_by(facility) %>%
    group_modify(~ calc_temp_stats(.x, "category")) %>%
    mutate(classification = "SGA") %>%
    add_totals_temp(analysis_df, "SGA", by_facility = TRUE) %>%
    arrange(
        facility,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total"))
    )

# SGA by Outcome
sga_outcome <- calc_temp_by_outcome(analysis_df, "sga_category", "sga_explanation") %>%
    mutate(classification = "SGA") %>%
    add_totals_outcome(analysis_df, "SGA") %>%
    arrange(
        facility, outcome,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total"))
    )


# ===== SAVE =====

cat("\n=== Saving Results ===\n")
all_overall <- bind_rows(lbw_overall, sga_overall)
all_facility <- bind_rows(lbw_facility, sga_facility)

write_csv(all_overall, file.path(output_dir, "01_temp_stats_overall.csv"))
cat("Saved: 01_temp_stats_overall.csv\n")

write_csv(all_facility, file.path(output_dir, "02_temp_stats_by_facility.csv"))
cat("Saved: 02_temp_stats_by_facility.csv\n")

write_csv(lbw_outcome, file.path(output_dir, "03_avg_temp_by_outcome_LBW.csv"))
cat("Saved: 03_avg_temp_by_outcome_LBW.csv\n")

write_csv(sga_outcome, file.path(output_dir, "04_avg_temp_by_outcome_SGA.csv"))
cat("Saved: 04_avg_temp_by_outcome_SGA.csv\n")

cat("\nAnalysis complete!\n")
