# Q5_neonatal_deaths_by_category.R
# Neonatal Deaths by LBW and SGA Categories (SMCH ONLY)
#
# This script analyzes EARLY NEONATAL DEATHS (ENND) from SMCH maternal data:
# 1. Filter for ENND (Early Neonatal Death) from SMCH only
# 2. Exclude stillbirths (STBF, STBM)
# 3. Distribution by birthweight categories (LBW classification)
# 4. Distribution by SGA categories (SVN classification)
# 5. Breakdowns by year
#
# NOTE: KCH does not have neonatal death codes in maternal data, so this
# analysis is limited to SMCH (Zimbabwe) only.
#
# Data source:
# - SMCH Zimbabwe maternal outcomes (all births 2022-2025)

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
})

# ===== CONFIGURATION =====
# Input file - SMCH MATERNAL DATA ONLY
smch_maternal_file <- "SUBSAMPLE_20220101_to_20251231_SMCH_ZIM_Maternal_Outcomes_2026-02-11_cleaned_no_gestation_filter.csv"

# Output directory
output_dir <- "Q5_outputs"
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

# Get birthweight from available columns (SMCH has bwtdis only)
get_bw_column <- function(df) {
    result <- rep(NA_character_, nrow(df))

    if ("bwtdis" %in% names(df)) {
        result <- ifelse(is.na(result) & !is.na(df$bwtdis), df$bwtdis, result)
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

cat("\n=== Loading SMCH Maternal Data ===\n")
cat("Reading SMCH maternal data...\n")
smch_raw <- suppressMessages(read_csv(smch_maternal_file, guess_max = 50000, col_types = cols(.default = "c")))

# Convert birthweight to kg
cat("Converting birthweight to kg...\n")
smch_raw <- smch_raw %>%
    mutate(
        bw_raw = get_bw_column(.),
        bw_kg = to_kg(bw_raw),
        facility = "SMCH"
    )

# Add gestational age
smch_data <- smch_raw %>%
    mutate(
        gestation_weeks = suppressWarnings(as.numeric(gestation))
    )

total_deliveries <- nrow(smch_data)
cat("Total SMCH deliveries:", total_deliveries, "\n")

# ===== FILTER FOR EARLY NEONATAL DEATHS (ENND) =====

cat("\n=== Filtering for Early Neonatal Deaths (ENND) ===\n")

# Filter for ENND (Early Neonatal Death)
# Exclude stillbirths (STBF, STBM)
ennd_all <- smch_data %>%
    filter(
        !is.na(neotreeoutcome),
        toupper(trimws(neotreeoutcome)) == "ENND"
    )

ennd_count <- nrow(ennd_all)
cat("Early neonatal deaths (ENND):", ennd_count, "\n")
cat("Percentage of total deliveries:", round(100 * ennd_count / total_deliveries, 1), "%\n")

# ===== PROCESS BIRTHWEIGHT DATA FOR ENND =====

cat("\n=== Processing Birthweight Data (ENND Only) ===\n")

# Add gestational age and valid categories
process_ennd <- ennd_all %>%
    mutate(
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

# Count valid birthweights in ENND
valid_bw_ennd <- process_ennd %>% filter(is_valid_bw)

valid_bw_count <- nrow(valid_bw_ennd)
cat("ENND with valid birthweight:", valid_bw_count, "\n")
cat("Percentage of ENND:", round(100 * valid_bw_count / ennd_count, 1), "%\n")

# ===== LBW CLASSIFICATION (ENND) =====

cat("\n=== LBW Classification (ENND Only) ===\n")

lbw_ennd <- valid_bw_ennd

cat("ENND with LBW classification:", nrow(lbw_ennd), "\n")

# Distribution by category
lbw_distribution <- process_ennd %>%
    count(category = bw_category, explanation = lbw_explanation, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    arrange(match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# ===== SVN/SGA CLASSIFICATION (ENND) =====

cat("\n=== SVN/SGA Classification (ENND Only) ===\n")

svn_ennd <- valid_bw_ennd %>%
    filter(!is.na(sga_cat_temp))

cat("ENND with SGA classification:", nrow(svn_ennd), "\n")

# Distribution by category
sga_distribution <- process_ennd %>%
    count(category = sga_category, explanation = sga_explanation, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    arrange(
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# ===== YEAR-BASED ANALYSIS =====

cat("\n=== Year-Based Analysis (ENND) ===\n")

# Extract year from dateadmission
process_ennd_year <- process_ennd %>%
    filter(!is.na(year), year >= 2022, year <= 2025)

cat("ENND with valid year:", nrow(process_ennd_year), "\n")

# LBW by year
lbw_by_year <- process_ennd_year %>%
    group_by(year, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(year, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded", "Total")))

# SGA by year
sga_by_year <- process_ennd_year %>%
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

lbw_distribution <- add_totals(lbw_distribution)
lbw_by_year <- add_totals(lbw_by_year, "year")

sga_distribution <- add_totals(sga_distribution)
sga_by_year <- add_totals(sga_by_year, "year")

# Combine results
distribution_combined <- bind_rows(lbw_distribution, sga_distribution)
by_year_combined <- bind_rows(lbw_by_year, sga_by_year)

# ===== SAMPLE SIZE SUMMARY =====

sample_size_summary <- tibble(
    description = c(
        "Total SMCH deliveries",
        "Early neonatal deaths (ENND)",
        "ENND with valid birthweight (LBW sample size)",
        "Discarded for LBW (Missing/invalid birthweight)",
        "ENND with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        total_deliveries,
        ennd_count,
        nrow(lbw_ennd),
        sum(process_ennd$bw_category == "Discarded"),
        nrow(svn_ennd),
        sum(process_ennd$sga_explanation == "Missing BW"),
        sum(process_ennd$sga_explanation == "Missing GA"),
        sum(process_ennd$sga_explanation == "Missing both BW and GA")
    ),
    percentage_of_total = c(
        100,
        round(100 * ennd_count / total_deliveries, 1),
        round(100 * nrow(lbw_ennd) / ennd_count, 1),
        round(100 * sum(process_ennd$bw_category == "Discarded") / ennd_count, 1),
        round(100 * nrow(svn_ennd) / ennd_count, 1),
        round(100 * sum(process_ennd$sga_explanation == "Missing BW") / ennd_count, 1),
        round(100 * sum(process_ennd$sga_explanation == "Missing GA") / ennd_count, 1),
        round(100 * sum(process_ennd$sga_explanation == "Missing both BW and GA") / ennd_count, 1)
    )
)

# ===== DEATH RATE BY CATEGORY (CFR) =====
#
# For each LBW and SGA category: total births, ENND, and % of births
# that ended in early neonatal death within that category.
#
# This is the Case Fatality Rate (CFR) per birth category.
# Denominator = all SMCH deliveries (live + ENND + stillbirths) classified
# into the same LBW/SGA scheme.

cat("\n=== Death Rate by Category (CFR) ===\n")

# Classify ALL SMCH deliveries by LBW and SGA
smch_all_classified <- smch_data %>%
    mutate(
        is_valid_bw = is.finite(bw_kg) & bw_kg >= 0.3 & bw_kg <= 7.0,
        bw_category = dplyr::if_else(is_valid_bw, bw_category_from_kg(bw_kg), "Discarded"),
        lbw_explanation = dplyr::if_else(is_valid_bw, "Included in analysis", "Missing or invalid birth weight"),
        is_invalid_ga = is.na(gestation_weeks) | gestation_weeks < 22 | gestation_weeks > 42,
        sga_status = dplyr::if_else(
            gestation_weeks >= 22 & !is.na(gestation_weeks),
            classify_sga(bw_kg, gestation_weeks, if ("sexdis" %in% names(.)) sexdis else NA_character_),
            NA_character_
        ),
        sga_cat_temp = sga_category_from_gestational_age(sga_status, gestation_weeks),
        is_valid_sga = is_valid_bw & !is.na(sga_cat_temp),
        sga_explanation = case_when(
            is_valid_sga ~ "Included in analysis",
            !is_valid_bw & is_invalid_ga ~ "Missing both BW and GA",
            !is_valid_bw ~ "Missing BW",
            is_invalid_ga ~ "Missing GA",
            TRUE ~ "Missing GA"
        ),
        sga_category = dplyr::if_else(is_valid_sga, sga_cat_temp, sga_explanation)
    )

# Total births per LBW category
lbw_births <- smch_all_classified %>%
    count(
        category = bw_category, explanation = lbw_explanation,
        name = "n_total_births"
    )

# ENND per LBW category
lbw_deaths_cat <- process_ennd %>%
    count(
        category = bw_category, explanation = lbw_explanation,
        name = "n_ennd"
    )

# Combine into CFR table for LBW
lbw_cfr <- lbw_births %>%
    left_join(lbw_deaths_cat, by = c("category", "explanation")) %>%
    mutate(
        n_ennd = tidyr::replace_na(n_ennd, 0L),
        pct_ennd_of_births = round(100 * n_ennd / n_total_births, 2),
        classification = "Birthweight"
    ) %>%
    arrange(match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# Total row for LBW CFR
lbw_cfr <- bind_rows(
    lbw_cfr,
    tibble(
        category           = "Total",
        explanation        = "All deliveries considered",
        n_total_births     = sum(lbw_cfr$n_total_births),
        n_ennd             = sum(lbw_cfr$n_ennd),
        pct_ennd_of_births = round(100 * sum(lbw_cfr$n_ennd) / sum(lbw_cfr$n_total_births), 2),
        classification     = "Birthweight"
    )
)

# Total births per SGA category
sga_births <- smch_all_classified %>%
    count(
        category = sga_category, explanation = sga_explanation,
        name = "n_total_births"
    )

# ENND per SGA category
sga_deaths_cat <- process_ennd %>%
    count(
        category = sga_category, explanation = sga_explanation,
        name = "n_ennd"
    )

# Combine into CFR table for SGA
SGA_CAT_ORDER <- c(
    "Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA",
    "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded"
)

sga_cfr <- sga_births %>%
    left_join(sga_deaths_cat, by = c("category", "explanation")) %>%
    mutate(
        n_ennd             = tidyr::replace_na(n_ennd, 0L),
        pct_ennd_of_births = round(100 * n_ennd / n_total_births, 2),
        classification     = "SGA"
    ) %>%
    arrange(match(category, SGA_CAT_ORDER))

# Total row for SGA CFR
sga_cfr <- bind_rows(
    sga_cfr,
    tibble(
        category           = "Total",
        explanation        = "All deliveries considered",
        n_total_births     = sum(sga_cfr$n_total_births),
        n_ennd             = sum(sga_cfr$n_ennd),
        pct_ennd_of_births = round(100 * sum(sga_cfr$n_ennd) / sum(sga_cfr$n_total_births), 2),
        classification     = "SGA"
    )
)

cfr_combined <- bind_rows(lbw_cfr, sga_cfr)

cat("CFR table (LBW, valid categories only):\n")
print(lbw_cfr %>% filter(category %in% c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Total")) %>%
    select(category, n_total_births, n_ennd, pct_ennd_of_births))
cat("\n")

# ===== SAVE OUTPUTS =====

cat("\n=== Saving Results ===\n")

write_csv(sample_size_summary, file.path(output_dir, "01_sample_size_summary.csv"))
cat("Saved: 01_sample_size_summary.csv\n")

write_csv(distribution_combined, file.path(output_dir, "02_distribution_by_category.csv"))
cat("Saved: 02_distribution_by_category.csv\n")

write_csv(cfr_combined, file.path(output_dir, "03_death_rate_by_category.csv"))
cat("Saved: 03_death_rate_by_category.csv\n")

write_csv(by_year_combined, file.path(output_dir, "04_distribution_by_year.csv"))
cat("Saved: 04_distribution_by_year.csv\n")

# ===== SUMMARY REPORT =====

cat("\n", rep("=", 60), "\n", sep = "")
cat("EARLY NEONATAL DEATHS (ENND) - SMCH ONLY\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Sample Sizes:\n")
cat("  Total SMCH deliveries:", total_deliveries, "\n")
cat(
    "  Early neonatal deaths (ENND):", ennd_count,
    sprintf("(%.1f%% of deliveries)", 100 * ennd_count / total_deliveries), "\n"
)
cat(
    "  Valid birthweight:", valid_bw_count,
    sprintf("(%.1f%% of ENND)", 100 * valid_bw_count / ennd_count), "\n"
)
cat(
    "  LBW classified:", nrow(lbw_ennd),
    sprintf("(%.1f%% of ENND)", 100 * nrow(lbw_ennd) / ennd_count), "\n"
)
cat(
    "  SGA classified:", nrow(svn_ennd),
    sprintf("(%.1f%% of ENND)", 100 * nrow(svn_ennd) / ennd_count), "\n\n"
)

cat("Output files saved to:", output_dir, "\n")
cat("  01_sample_size_summary.csv\n")
cat("  02_distribution_by_category.csv\n")
cat("  03_death_rate_by_category.csv  — total births, ENND, % ENND per category\n")
cat("  04_distribution_by_year.csv\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Analysis complete!\n")
cat("\nNOTE: This analysis includes SMCH (Zimbabwe) data only.\n")
cat("KCH (Malawi) does not have neonatal death codes in maternal data.\n")
