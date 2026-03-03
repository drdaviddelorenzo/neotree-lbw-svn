# Q2_birth_insights_maternal_data.R
# Birth Insights from Maternal Data (All Deliveries)
#
# This script analyzes maternal delivery data to determine:
# 1. Total number of deliveries
# 2. Number of deliveries with valid birthweight (sample size)
# 3. Distribution by birthweight categories (LBW classification)
# 4. Distribution by SGA categories (SVN classification - USS only)
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
kch_maternal_file <- "SUBSAMPLE_20220101_to_20251231_KCH_MWI_Combined_Maternity_Outcomes_2026-02-11_cleaned_no_gestation_filter.csv"
smch_maternal_file <- "SUBSAMPLE_20220101_to_20251231_SMCH_ZIM_Maternal_Outcomes_2026-02-11_cleaned_no_gestation_filter.csv"

# Output directory
output_dir <- "Q2_outputs"
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

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Maternal Data ===\n")
cat("Reading KCH maternal data...\n")
kch_raw <- suppressMessages(read_csv(kch_maternal_file, guess_max = 50000, col_types = cols(.default = "c")))
cat("Reading SMCH maternal data...\n")
smch_raw <- suppressMessages(read_csv(smch_maternal_file, guess_max = 50000, col_types = cols(.default = "c")))

# IMPORTANT: Convert birthweight to kg BEFORE combining
# KCH has birthweight in grams, SMCH in kg
# Try multiple columns for maximum coverage: birthweight first, then bwtdis
# If we combine first, the median will be wrong and conversion won't work
cat("Converting birthweight to kg per facility...\n")

# Helper function to get birthweight from available columns with priority
# Priority based on completion rate: bwtdis (94.7%) > birthweight (3.2%) > bwt (0.03%)
get_bw_column <- function(df) {
    result <- rep(NA_character_, nrow(df))

    # Start with lowest priority (least complete)
    if ("bwt" %in% names(df)) {
        result <- ifelse(is.na(result) & !is.na(df$bwt), df$bwt, result)
    }

    # Middle priority
    if ("birthweight" %in% names(df)) {
        result <- ifelse(is.na(result) & !is.na(df$birthweight), df$birthweight, result)
    }

    # Highest priority (most complete)
    if ("bwtdis" %in% names(df)) {
        result <- ifelse(is.na(result) & !is.na(df$bwtdis), df$bwtdis, result)
    }

    result
}

# KCH: Convert birthweight from available column
kch_raw <- kch_raw %>%
    mutate(
        bw_raw = get_bw_column(.),
        bw_kg = to_kg(bw_raw),
        facility = "KCH"
    )

# SMCH: Convert birthweight from available column
smch_raw <- smch_raw %>%
    mutate(
        bw_raw = get_bw_column(.),
        bw_kg = to_kg(bw_raw),
        facility = "SMCH"
    )

# Combine datasets AFTER conversion
cat("Combining datasets...\n")
all_maternal <- bind_rows(kch_raw, smch_raw)

total_deliveries <- nrow(all_maternal)
cat("Total deliveries:", total_deliveries, "\n")

# ===== PROCESS BIRTHWEIGHT DATA =====

cat("\n=== Processing Birthweight Data ===\n")

# Process maternal data with enhanced validations and categories
all_maternal_enhanced <- all_maternal %>%
    mutate(
        gestation_weeks = suppressWarnings(as.numeric(gestation)),
        date_parsed = suppressWarnings(as.Date(dateadmission)),
        year = as.integer(format(date_parsed, "%Y")),

        # Validations
        is_valid_bw = is.finite(bw_kg) & bw_kg >= 0.3 & bw_kg <= 7.0,
        is_invalid_ga = is.na(gestation_weeks) | gestation_weeks < 22 | gestation_weeks > 42,
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

# Valid subsets for reporting
valid_bw_data <- all_maternal_enhanced %>% filter(is_valid_bw)
valid_bw_count <- nrow(valid_bw_data)

cat("Deliveries with valid birthweight:", valid_bw_count, "\n")
cat("Percentage with valid birthweight:", round(100 * valid_bw_count / total_deliveries, 1), "%\n")

lbw_maternal <- all_maternal_enhanced %>% filter(is_valid_bw)
svn_maternal <- all_maternal_enhanced %>% filter(is_valid_sga)

cat("\n=== LBW Classification ===\n")
cat("Deliveries with LBW classification:", nrow(lbw_maternal), "\n")

# Distribution by category
lbw_distribution <- all_maternal_enhanced %>%
    count(category = bw_category, explanation = lbw_explanation, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    arrange(match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# Distribution by facility
lbw_by_facility <- all_maternal_enhanced %>%
    group_by(facility, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(facility, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

cat("\n=== SVN/SGA Classification ===\n")
cat("Deliveries with SGA classification:", nrow(svn_maternal), "\n")

# Distribution by category
sga_distribution <- all_maternal_enhanced %>%
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
sga_by_facility <- all_maternal_enhanced %>%
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

# ===== COMBINE RESULTS =====

distribution_combined <- bind_rows(lbw_distribution, sga_distribution)
by_facility_combined <- bind_rows(lbw_by_facility, sga_by_facility)

# ===== YEAR-BASED ANALYSIS =====

cat("\n=== Year-Based Analysis ===\n")

all_maternal_enhanced_year <- all_maternal_enhanced %>%
    filter(!is.na(year), year >= 2022, year <= 2025)

cat("Total records with valid year:", nrow(all_maternal_enhanced_year), "\n")

# LBW by year
lbw_by_year <- all_maternal_enhanced_year %>%
    group_by(year, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(year, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# LBW by facility and year
lbw_by_facility_year <- all_maternal_enhanced_year %>%
    group_by(facility, year, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility, year) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(facility, year, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# SGA by year
sga_by_year <- all_maternal_enhanced_year %>%
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
sga_by_facility_year <- all_maternal_enhanced_year %>%
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

# Combine year-based results
by_year_combined <- bind_rows(lbw_by_year, sga_by_year)
by_facility_year_combined <- bind_rows(lbw_by_facility_year, sga_by_facility_year)

# ===== SAMPLE SIZE SUMMARY =====

sample_size_df <- tibble(
    description = c(
        "Total deliveries (all births)",
        "Deliveries with valid birthweight (LBW sample size)",
        "Discarded for LBW (Missing/invalid birthweight)",
        "Deliveries with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)",
        "",
        "KCH - Total deliveries",
        "KCH - LBW sample size",
        "KCH - Discarded for LBW",
        "KCH - SGA sample size",
        "KCH - Discarded for SGA (Missing BW)",
        "KCH - Discarded for SGA (Missing GA)",
        "KCH - Discarded for SGA (Missing both BW and GA)",
        "",
        "SMCH - Total deliveries",
        "SMCH - LBW sample size",
        "SMCH - Discarded for LBW",
        "SMCH - SGA sample size",
        "SMCH - Discarded for SGA (Missing BW)",
        "SMCH - Discarded for SGA (Missing GA)",
        "SMCH - Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        total_deliveries,
        nrow(lbw_maternal),
        sum(all_maternal_enhanced$bw_category == "Discarded"),
        nrow(svn_maternal),
        sum(all_maternal_enhanced$sga_explanation == "Missing BW"),
        sum(all_maternal_enhanced$sga_explanation == "Missing GA"),
        sum(all_maternal_enhanced$sga_explanation == "Missing both BW and GA"),
        NA,
        sum(all_maternal$facility == "KCH"),
        sum(all_maternal_enhanced$facility == "KCH" & all_maternal_enhanced$bw_category != "Discarded"),
        sum(all_maternal_enhanced$facility == "KCH" & all_maternal_enhanced$bw_category == "Discarded"),
        sum(all_maternal_enhanced$facility == "KCH" & all_maternal_enhanced$sga_category %in% c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")),
        sum(all_maternal_enhanced$facility == "KCH" & all_maternal_enhanced$sga_explanation == "Missing BW"),
        sum(all_maternal_enhanced$facility == "KCH" & all_maternal_enhanced$sga_explanation == "Missing GA"),
        sum(all_maternal_enhanced$facility == "KCH" & all_maternal_enhanced$sga_explanation == "Missing both BW and GA"),
        NA,
        sum(all_maternal$facility == "SMCH"),
        sum(all_maternal_enhanced$facility == "SMCH" & all_maternal_enhanced$bw_category != "Discarded"),
        sum(all_maternal_enhanced$facility == "SMCH" & all_maternal_enhanced$bw_category == "Discarded"),
        sum(all_maternal_enhanced$facility == "SMCH" & all_maternal_enhanced$sga_category %in% c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")),
        sum(all_maternal_enhanced$facility == "SMCH" & all_maternal_enhanced$sga_explanation == "Missing BW"),
        sum(all_maternal_enhanced$facility == "SMCH" & all_maternal_enhanced$sga_explanation == "Missing GA"),
        sum(all_maternal_enhanced$facility == "SMCH" & all_maternal_enhanced$sga_explanation == "Missing both BW and GA")
    )
) %>%
    mutate(
        percentage_of_total = case_when(
            is.na(n) ~ NA_real_,
            grepl("^KCH", description) ~ n / sum(all_maternal$facility == "KCH") * 100,
            grepl("^SMCH", description) ~ n / sum(all_maternal$facility == "SMCH") * 100,
            TRUE ~ n / total_deliveries * 100
        ),
        percentage_of_total = round(percentage_of_total, 1)
    )

sample_size_summary <- sample_size_df

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
cat("BIRTH INSIGHTS SUMMARY (MATERNAL DATA)\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Sample Sizes:\n")
cat("  Total deliveries:", total_deliveries, "\n")
cat(
    "  Valid birthweight:", valid_bw_count,
    sprintf("(%.1f%%)", 100 * valid_bw_count / total_deliveries), "\n"
)
cat(
    "  LBW classified:", nrow(lbw_maternal),
    sprintf("(%.1f%%)", 100 * nrow(lbw_maternal) / total_deliveries), "\n"
)
cat(
    "  SGA classified:", nrow(svn_maternal),
    sprintf("(%.1f%%)", 100 * nrow(svn_maternal) / total_deliveries), "\n\n"
)

cat("Facilities:\n")
cat("  KCH:", sum(all_maternal$facility == "KCH"), "deliveries\n")
cat("  SMCH:", sum(all_maternal$facility == "SMCH"), "deliveries\n\n")

cat("Years Analyzed: 2022-2025\n")
cat("  Total records with valid year:", nrow(all_maternal_enhanced_year), "\n\n")

cat("Birthweight Categories (LBW) - Overall:\n")
print(lbw_distribution %>% select(category, explanation, n, percentage))
cat("\n")

cat("SGA Categories (SVN) - Overall:\n")
print(sga_distribution %>% select(category, explanation, n, percentage))
cat("\n")

cat("Output files saved to:", output_dir, "\n")
cat("  - 01_sample_size_summary.csv\n")
cat("  - 02_distribution_by_category.csv\n")
cat("  - 03_distribution_by_facility.csv\n")
cat("  - 04_distribution_by_year.csv\n")
cat("  - 05_distribution_by_facility_and_year.csv\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Analysis complete!\n")
