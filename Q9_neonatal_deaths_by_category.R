# Q9_neonatal_deaths_by_category.R
# Neonatal Deaths (Absolute and %) by LBW and SGA Categories
#
# This script analyzes NEONATAL DEATHS in the newborn unit:
# 1. Total number of deaths (NND)
# 2. Death rates (Case Fatality Rate) by birthweight categories (LBW)
# 3. Death rates (Case Fatality Rate) by SGA categories (SVN)
# 4. Stratification by facility
#
# Outcome Codes Used:
# - KCH: "NND<24", "NND>24" (Excluded: "BID" - Brought in Dead)
# - SMCH: "NND"
#
# Data sources:
# - KCH Malawi newborn admissions (COMPLETE_ALL_RECORDS)
# - SMCH Zimbabwe newborn admissions (COMPLETE_ALL_RECORDS)

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
})

# ===== CONFIGURATION =====
# Input files - NEWBORN ADMISSION DATA
kch_file <- "SUBSAMPLE_20220101_to_20251231_KCH_MWI_COMPLETE_ALL_RECORDS_cleaned.csv"
smch_file <- "SUBSAMPLE_20220101_to_20251231_SMCH_ZIM_COMPLETE_ALL_RECORDS_cleaned.csv"

# Output directory
output_dir <- "Q9_outputs"
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

# Get birthweight from available columns
get_bw_column <- function(df) {
    result <- rep(NA_character_, nrow(df))
    if ("birthweight" %in% names(df)) {
        result <- ifelse(is.na(result) & !is.na(df$birthweight), df$birthweight, result)
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
            explanation = "All admissions considered",
            total_admissions = sum(df$total_admissions),
            deaths = sum(df$deaths),
            mortality_rate = round(100 * sum(df$deaths) / sum(df$total_admissions), 1),
            classification = unique(df$classification)
        )
        if ("facility" %in% names(df) && "Overall" %in% df$facility) totals$facility <- "Overall"

        return(bind_rows(df, totals))
    }

    totals <- df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
            total_admissions = sum(total_admissions),
            deaths = sum(deaths),
            mortality_rate = round(100 * sum(deaths) / sum(total_admissions), 1),
            .groups = "drop"
        ) %>%
        mutate(
            category = "Total",
            explanation = "All admissions considered",
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

cat("\n=== Loading Newborn Admission Data ===\n")
cat("Reading KCH newborn data...\n")
kch_raw <- suppressMessages(read_csv(kch_file, guess_max = 50000, col_types = cols(.default = "c")))
cat("Reading SMCH newborn data...\n")
smch_raw <- suppressMessages(read_csv(smch_file, guess_max = 50000, col_types = cols(.default = "c")))

# Add Facility identifiers
kch_proc <- kch_raw %>% mutate(facility = "KCH")
smch_proc <- smch_raw %>% mutate(facility = "SMCH")

# Combine datasets
cat("Combining datasets...\n")
all_admissions <- bind_rows(kch_proc, smch_proc)

total_admissions <- nrow(all_admissions)
cat("Total newborn admissions:", total_admissions, "\n")

# Process key variables
cat("Processing variables and classifications...\n")
analysis_df <- all_admissions %>%
    mutate(
        # Birthweight and Gestation
        bw_raw = get_bw_column(.),
        bw_kg = to_kg(bw_raw),
        gestation_weeks = suppressWarnings(as.numeric(gestation)),

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
        sga_category = if_else(is_valid_sga, sga_cat_temp, sga_explanation),

        # Outcome Standardisation
        # Codes: NND, NND<24, NND>24 -> Death
        outcome_clean = toupper(trimws(neotreeoutcome)),
        is_death = case_when(
            outcome_clean %in% c("NND", "NND<24", "NND>24") ~ TRUE,
            outcome_clean == "BID" ~ FALSE, # Exclude Brought In Dead from ward mortality
            TRUE ~ FALSE
        )
    )

total_deaths <- sum(analysis_df$is_death)
cat("Total neonatal deaths identified:", total_deaths, "\n")
cat("Overall mortality rate:", round(100 * total_deaths / total_admissions, 1), "%\n")

# ===== ANALYSIS: DEATHS BY LBW CATEGORY =====

cat("\n=== Deaths by LBW Category ===\n")

lbw_deaths <- analysis_df %>%
    group_by(facility, category = bw_category, explanation = lbw_explanation) %>%
    summarise(
        total_admissions = n(),
        deaths = sum(is_death),
        mortality_rate = if_else(total_admissions > 0, round(100 * deaths / total_admissions, 1), 0),
        classification = "Birthweight",
        .groups = "drop"
    ) %>%
    arrange(facility, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded", "Total")))

print(lbw_deaths)

# Overall stats
lbw_overall <- analysis_df %>%
    group_by(category = bw_category, explanation = lbw_explanation) %>%
    summarise(
        total_admissions = n(),
        deaths = sum(is_death),
        mortality_rate = if_else(total_admissions > 0, round(100 * deaths / total_admissions, 1), 0),
        classification = "Birthweight",
        facility = "Overall",
        .groups = "drop"
    ) %>%
    arrange(match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded", "Total")))

# ===== ANALYSIS: DEATHS BY SGA CATEGORY =====

cat("\n=== Deaths by SGA Category ===\n")

sga_deaths <- analysis_df %>%
    group_by(facility, category = sga_category, explanation = sga_explanation) %>%
    summarise(
        total_admissions = n(),
        deaths = sum(is_death),
        mortality_rate = if_else(total_admissions > 0, round(100 * deaths / total_admissions, 1), 0),
        classification = "SGA",
        .groups = "drop"
    ) %>%
    arrange(
        facility,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

print(sga_deaths)

# Overall stats
sga_overall <- analysis_df %>%
    group_by(category = sga_category, explanation = sga_explanation) %>%
    summarise(
        total_admissions = n(),
        deaths = sum(is_death),
        mortality_rate = if_else(total_admissions > 0, round(100 * deaths / total_admissions, 1), 0),
        classification = "SGA",
        facility = "Overall",
        .groups = "drop"
    ) %>%
    arrange(
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# Apply totals wrapper functions
lbw_deaths <- add_totals(lbw_deaths, "facility")
sga_deaths <- add_totals(sga_deaths, "facility")
lbw_overall <- add_totals(lbw_overall)
sga_overall <- add_totals(sga_overall)

# ===== COMBINE RESULTS =====

all_facility_stats <- bind_rows(lbw_deaths, sga_deaths)
all_overall_stats <- bind_rows(lbw_overall, sga_overall)

# ===== SAMPLE SIZE SUMMARY =====

sample_size_summary <- tibble(
    description = c(
        "Total newborn admissions",
        "Total neonatal deaths",
        "Admissions with valid birthweight (LBW sample)",
        "Deaths with valid birthweight",
        "Discarded for LBW (Missing/invalid birthweight)",
        "Admissions with valid SGA classification",
        "Deaths with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        total_admissions,
        total_deaths,
        sum(analysis_df$is_valid_bw),
        sum(analysis_df$is_valid_bw & analysis_df$is_death),
        sum(!analysis_df$is_valid_bw),
        sum(analysis_df$is_valid_sga),
        sum(analysis_df$is_valid_sga & analysis_df$is_death),
        sum(analysis_df$sga_explanation == "Missing BW"),
        sum(analysis_df$sga_explanation == "Missing GA"),
        sum(analysis_df$sga_explanation == "Missing both BW and GA")
    ),
    pct = round(100 * n / total_admissions, 1)
)

# ===== ODDS RATIO ANALYSIS =====

cat("\n=== Odds Ratio Analysis ===\n")

# Helper: run logistic regression and extract OR table
calc_or_table <- function(data, category_col, ref_level, label) {
    df <- data %>%
        filter(
            !is.na(.data[[category_col]]),
            .data[[category_col]] %in% names(which(table(.data[[category_col]]) > 0))
        ) %>%
        mutate(
            cat_factor = relevel(factor(.data[[category_col]]), ref = ref_level),
            outcome = as.integer(is_death)
        )

    # Need at least 2 levels and some events
    if (length(unique(df$cat_factor)) < 2 || sum(df$outcome) == 0) {
        return(NULL)
    }

    tryCatch(
        {
            model <- glm(outcome ~ cat_factor, data = df, family = binomial(link = "logit"))
            coefs <- summary(model)$coefficients

            or_rows <- rownames(coefs)[grepl("^cat_factor", rownames(coefs))]
            if (length(or_rows) == 0) {
                return(NULL)
            }

            result <- data.frame(
                comparison = label,
                category = sub("^cat_factor", "", or_rows),
                reference = ref_level,
                OR = round(exp(coefs[or_rows, "Estimate"]), 3),
                CI_lower = round(exp(coefs[or_rows, "Estimate"] - 1.96 * coefs[or_rows, "Std. Error"]), 3),
                CI_upper = round(exp(coefs[or_rows, "Estimate"] + 1.96 * coefs[or_rows, "Std. Error"]), 3),
                p_value = round(coefs[or_rows, "Pr(>|z|)"], 4),
                significant = ifelse(coefs[or_rows, "Pr(>|z|)"] < 0.05, "Yes", "No"),
                stringsAsFactors = FALSE
            )
            result
        },
        error = function(e) NULL
    )
}

# Helper: run OR analysis for one data subset (facility or global)
run_or_analysis <- function(data, scope_label) {
    results <- list()

    # 1. LBW vs NBW reference — only valid BW entries
    lbw_data <- data %>% filter(is_valid_bw, bw_category %in% c("ELBW", "VLBW", "LBW", "NBW", "HBW"))
    r1 <- calc_or_table(lbw_data, "bw_category", ref_level = "NBW", label = "LBW category vs NBW")
    if (!is.null(r1)) results[["lbw"]] <- r1

    # 2. SGA category vs Term-AGA reference — only valid SGA entries
    sga_data <- data %>% filter(is_valid_sga, sga_category %in% c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA"))
    r2 <- calc_or_table(sga_data, "sga_category", ref_level = "Term-AGA", label = "SGA category vs Term-AGA")
    if (!is.null(r2)) results[["sga"]] <- r2

    # 3. Preterm vs Term — derive from gestation_weeks (valid GA only)
    term_data <- data %>%
        filter(!is_invalid_ga) %>%
        mutate(term_group = if_else(gestation_weeks >= 37, "Term", "Preterm"))
    r3 <- calc_or_table(term_data, "term_group", ref_level = "Term", label = "Preterm vs Term")
    if (!is.null(r3)) results[["term"]] <- r3

    if (length(results) == 0) {
        return(NULL)
    }

    bind_rows(results) %>% mutate(scope = scope_label, .before = 1)
}

# Run by facility
or_by_facility <- analysis_df %>%
    group_by(facility) %>%
    group_modify(~ run_or_analysis(.x, scope_label = .y$facility)) %>%
    ungroup()

# Run globally
or_global <- run_or_analysis(analysis_df, scope_label = "Global")

# Combine
or_results <- bind_rows(or_global, or_by_facility) %>%
    select(scope, comparison, category, reference, OR, CI_lower, CI_upper, p_value, significant)

cat("Odds Ratio Results:\n")
print(as.data.frame(or_results))

# ===== SAVE OUTPUTS =====

cat("\n=== Saving Results ===\n")

write_csv(sample_size_summary, file.path(output_dir, "01_sample_size_summary.csv"))
cat("Saved: 01_sample_size_summary.csv\n")

write_csv(all_overall_stats, file.path(output_dir, "02_mortality_by_category_overall.csv"))
cat("Saved: 02_mortality_by_category_overall.csv\n")

write_csv(all_facility_stats, file.path(output_dir, "03_mortality_by_category_and_facility.csv"))
cat("Saved: 03_mortality_by_category_and_facility.csv\n")

write_csv(or_results, file.path(output_dir, "04_odds_ratios.csv"))
cat("Saved: 04_odds_ratios.csv\n")

cat("\nAnalysis complete!\n")
