# Q1_descriptive_study_LBW_SVN.R
# Descriptive Study of Low Birth Weight (LBW) and Small Vulnerable Newborns (SVN/SGA)
#
# This script analyzes newborn admission data for:
# 1. Low Birth Weight babies (ELBW, VLBW, LBW, NBW, HBW)
# 2. Small for Gestational Age babies (Term-SGA, Term-AGA, Preterm-SGA, Preterm-AGA)
#
# Data sources:
# - KCH Malawi newborn admissions (2022-2025)
# - SMCH Zimbabwe newborn admissions (2022-2025)

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
output_dir <- "Q1_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

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

# Convert birthweight to kg
to_kg <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    pos <- x_num[is.finite(x_num) & x_num > 0]
    if (length(pos) > 0) {
        med <- median(pos, na.rm = TRUE)
        if (is.finite(med) && med > 20 && med <= 7000) {
            return(x_num / 1000) # grams -> kg
        }
    }
    x_num
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

# Normalize outcomes
normalise_outcome <- function(x) {
    y <- tolower(trimws(as.character(x)))
    y <- gsub("[^a-z0-9]+", " ", y)
    y <- gsub("\\s+", " ", y)

    dplyr::case_when(
        grepl("\\bdc\\b|discharged|\\bdcr\\b|\\bdpc\\b|\\bdckmc\\b|kangaroo mother care", y) ~ "Alive",
        grepl("\\babs\\b|abscond|\\bdama\\b|against medical advice", y) ~ "Alive",
        grepl("\\btrh\\b|\\btro\\b|transfer", y) ~ "Alive",
        grepl("\\bnnd\\b|neonatal death|\\bdda\\b|died during admission", y) ~ "Neonatal Death (in-facility)",
        grepl("\\bbid\\b|brought in dead", y) ~ "Neonatal Death (pre-facility)",
        grepl("\\bstb\\b|stillbirth|still born", y) ~ "Stillbirth",
        y == "" ~ NA_character_,
        TRUE ~ "Unknown/Other"
    )
}

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Data ===\n")
cat("Reading KCH data...\n")
kch_raw <- suppressMessages(read_csv(kch_file, guess_max = 50000, col_types = cols(.default = "c")))
cat("Reading SMCH data...\n")
smch_raw <- suppressMessages(read_csv(smch_file, guess_max = 50000, col_types = cols(.default = "c")))

# Add facility identifier
kch_raw$facility <- "KCH"
smch_raw$facility <- "SMCH"

# Combine datasets
cat("Combining datasets...\n")
all_data <- bind_rows(kch_raw, smch_raw)

cat("Total records loaded:", nrow(all_data), "\n")

# Process data
cat("\n=== Processing Data ===\n")

# Standardize column names
process_data <- all_data %>%
    mutate(
        # Birthweight
        bw_kg = to_kg(birthweight),

        # Gestational age
        gestation_weeks = suppressWarnings(as.numeric(gestation)),

        # Method of gestational age estimation
        method_gest = toupper(trimws(methodestgest)),

        # Gender - use available columns
        gender_std = if_else(
            !is.na(gender),
            as.character(gender),
            if_else(!is.na(sexdis), as.character(sexdis), NA_character_)
        ),

        # Outcome
        outcome_norm = normalise_outcome(neotreeoutcome)
    ) %>%
    # Filter valid birthweights
    filter(is.finite(bw_kg) & bw_kg >= 0.3 & bw_kg <= 7.0)

cat("After birthweight filtering:", nrow(process_data), "\n")

# Create LBW classifications (for all babies)
lbw_data <- process_data %>%
    mutate(
        bw_category = bw_category_from_kg(bw_kg)
    ) %>%
    filter(!is.na(bw_category))

cat("Records with valid birthweight category:", nrow(lbw_data), "\n")

# Create SVN/SGA classifications (USS-only)
svn_data <- process_data %>%
    filter(method_gest == "USS") %>%
    mutate(
        sga_status = dplyr::if_else(gestation_weeks >= 24 & !is.na(gestation_weeks), classify_sga(bw_kg, gestation_weeks, if ("gender_std" %in% names(.)) gender_std else if ("gender" %in% names(.)) gender else NA_character_), NA_character_),
        sga_category = sga_category_from_gestational_age(sga_status, gestation_weeks)
    ) %>%
    filter(!is.na(sga_category))

cat("USS-confirmed records:", nrow(process_data %>% filter(method_gest == "USS")), "\n")
cat("Records with valid SGA category:", nrow(svn_data), "\n")

# ===== DESCRIPTIVE ANALYSES =====

cat("\n=== Generating Descriptive Statistics ===\n")

# Define category levels for ordering
bw_levels <- c("ELBW", "VLBW", "LBW", "NBW", "HBW")
sga_levels <- c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")

# ----- LBW DESCRIPTIVE STATISTICS -----

# 1. Overall distribution by birthweight category
lbw_overall <- lbw_data %>%
    count(bw_category, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    arrange(match(bw_category, bw_levels)) %>%
    rename(category = bw_category)

# 2. Distribution by facility
lbw_by_facility <- lbw_data %>%
    group_by(facility, bw_category) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(facility, match(bw_category, bw_levels)) %>%
    rename(category = bw_category)

# 3. Distribution by gender
lbw_by_gender <- lbw_data %>%
    filter(!is.na(gender_std)) %>%
    group_by(gender_std, bw_category) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(gender_std) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(gender_std, match(bw_category, bw_levels)) %>%
    rename(category = bw_category, gender = gender_std)

# 4. Distribution by outcome
lbw_by_outcome <- lbw_data %>%
    filter(!is.na(outcome_norm)) %>%
    group_by(outcome_norm, bw_category) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(outcome_norm) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(outcome_norm, match(bw_category, bw_levels)) %>%
    rename(category = bw_category, outcome = outcome_norm)

# 5. Summary statistics by category
lbw_summary <- lbw_data %>%
    group_by(bw_category) %>%
    summarise(
        n = n(),
        mean_bw_kg = mean(bw_kg, na.rm = TRUE),
        sd_bw_kg = sd(bw_kg, na.rm = TRUE),
        median_bw_kg = median(bw_kg, na.rm = TRUE),
        min_bw_kg = min(bw_kg, na.rm = TRUE),
        max_bw_kg = max(bw_kg, na.rm = TRUE),
        mean_gestation_weeks = mean(gestation_weeks, na.rm = TRUE),
        sd_gestation_weeks = sd(gestation_weeks, na.rm = TRUE),
        classification = "Birthweight",
        .groups = "drop"
    ) %>%
    arrange(match(bw_category, bw_levels)) %>%
    rename(category = bw_category)

# ----- SVN/SGA DESCRIPTIVE STATISTICS -----

# 1. Overall distribution by SGA category
svn_overall <- svn_data %>%
    count(sga_category, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    arrange(match(sga_category, sga_levels)) %>%
    rename(category = sga_category)

# 2. Distribution by facility
svn_by_facility <- svn_data %>%
    group_by(facility, sga_category) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    ungroup() %>%
    arrange(facility, match(sga_category, sga_levels)) %>%
    rename(category = sga_category)

# 3. Distribution by gender
svn_by_gender <- svn_data %>%
    filter(!is.na(gender_std)) %>%
    group_by(gender_std, sga_category) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(gender_std) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    ungroup() %>%
    arrange(gender_std, match(sga_category, sga_levels)) %>%
    rename(category = sga_category, gender = gender_std)

# 4. Distribution by outcome
svn_by_outcome <- svn_data %>%
    filter(!is.na(outcome_norm)) %>%
    group_by(outcome_norm, sga_category) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(outcome_norm) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    ungroup() %>%
    arrange(outcome_norm, match(sga_category, sga_levels)) %>%
    rename(category = sga_category, outcome = outcome_norm)

# 5. Summary statistics by category
svn_summary <- svn_data %>%
    group_by(sga_category) %>%
    summarise(
        n = n(),
        mean_bw_kg = mean(bw_kg, na.rm = TRUE),
        sd_bw_kg = sd(bw_kg, na.rm = TRUE),
        median_bw_kg = median(bw_kg, na.rm = TRUE),
        min_bw_kg = min(bw_kg, na.rm = TRUE),
        max_bw_kg = max(bw_kg, na.rm = TRUE),
        mean_gestation_weeks = mean(gestation_weeks, na.rm = TRUE),
        sd_gestation_weeks = sd(gestation_weeks, na.rm = TRUE),
        classification = "SGA",
        .groups = "drop"
    ) %>%
    arrange(match(sga_category, sga_levels)) %>%
    rename(category = sga_category)

# ===== COMBINED OUTPUTS =====

# Combine LBW and SVN results
overall_combined <- bind_rows(lbw_overall, svn_overall)
by_facility_combined <- bind_rows(lbw_by_facility, svn_by_facility)
by_gender_combined <- bind_rows(lbw_by_gender, svn_by_gender)
by_outcome_combined <- bind_rows(lbw_by_outcome, svn_by_outcome)
summary_combined <- bind_rows(lbw_summary, svn_summary)

# ===== SAVE OUTPUTS =====

cat("\n=== Saving Results ===\n")

write_csv(overall_combined, file.path(output_dir, "01_overall_distribution.csv"))
cat("Saved: 01_overall_distribution.csv\n")

write_csv(by_facility_combined, file.path(output_dir, "02_distribution_by_facility.csv"))
cat("Saved: 02_distribution_by_facility.csv\n")

write_csv(by_gender_combined, file.path(output_dir, "03_distribution_by_gender.csv"))
cat("Saved: 03_distribution_by_gender.csv\n")

write_csv(by_outcome_combined, file.path(output_dir, "04_distribution_by_outcome.csv"))
cat("Saved: 04_distribution_by_outcome.csv\n")

write_csv(summary_combined, file.path(output_dir, "05_summary_statistics.csv"))
cat("Saved: 05_summary_statistics.csv\n")

# ===== SUMMARY REPORT =====

cat("\n", rep("=", 60), "\n", sep = "")
cat("DESCRIPTIVE STUDY SUMMARY\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Sample Sizes:\n")
cat("  Total records processed:", nrow(all_data), "\n")
cat("  Valid birthweight records:", nrow(lbw_data), "\n")
cat("  USS-confirmed SGA records:", nrow(svn_data), "\n\n")

cat("Birthweight Categories (LBW):\n")
print(lbw_overall %>% select(category, n, percentage))
cat("\n")

cat("SGA Categories (SVN - USS only):\n")
print(svn_overall %>% select(category, n, percentage))
cat("\n")

cat("Facilities:\n")
cat("  KCH records:", sum(lbw_data$facility == "KCH"), "\n")
cat("  SMCH records:", sum(lbw_data$facility == "SMCH"), "\n\n")

cat("Output files saved to:", output_dir, "\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Analysis complete!\n")
