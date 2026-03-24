# Q13_inborn_outborn.R
# Analysis of Inborn vs Outborn Status by LBW and SGA Categories
#
# This script analyzes the 'inorout' variable:
# 1. Calculates the number and percentage of Inborn vs Outborn babies.
# 2. Stratifies by LBW and SGA categories.
# 3. Stratifies by Facility.
#
# Data sources:
# - KCH Malawi newborn admissions
# - SMCH Zimbabwe newborn admissions
#
# Variable: 'inorout'
# - TRUE = Inborn
# - FALSE = Outborn
# - NA = Missing

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
})

# ===== CONFIGURATION =====
# Input files
kch_file <- "LBW_KCH_NNU.csv"
smch_file <- "LBW_SMCH_NNU.csv"

# Output directory
output_dir <- "Q13_outputs"
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

# Add Totals Function specifically for count aggregations
add_totals_inborn <- function(df, raw_df, classification_name, by_facility = FALSE) {
    if (nrow(df) == 0) {
        return(df)
    }

    if (by_facility) {
        totals <- raw_df %>%
            group_by(facility) %>%
            summarise(
                n_total = n(),
                n_inborn = sum(inornout_cat == "Inborn", na.rm = TRUE),
                n_outborn = sum(inornout_cat == "Outborn", na.rm = TRUE),
                n_unknown = sum(inornout_cat == "Unknown", na.rm = TRUE),
                pct_inborn = if_else(n_total > 0, round(100 * n_inborn / n_total, 1), 0),
                pct_outborn = if_else(n_total > 0, round(100 * n_outborn / n_total, 1), 0),
                pct_unknown = if_else(n_total > 0, round(100 * n_unknown / n_total, 1), 0),
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
                n_inborn = sum(inornout_cat == "Inborn", na.rm = TRUE),
                n_outborn = sum(inornout_cat == "Outborn", na.rm = TRUE),
                n_unknown = sum(inornout_cat == "Unknown", na.rm = TRUE),
                pct_inborn = if_else(n_total > 0, round(100 * n_inborn / n_total, 1), 0),
                pct_outborn = if_else(n_total > 0, round(100 * n_outborn / n_total, 1), 0),
                pct_unknown = if_else(n_total > 0, round(100 * n_unknown / n_total, 1), 0),
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

        # inorout Logic
        # Values are "True" or "False" or "TRUE" or "FALSE" or NA
        is_inborn = case_when(
            toupper(as.character(inorout)) == "TRUE" ~ TRUE,
            toupper(as.character(inorout)) == "FALSE" ~ FALSE,
            TRUE ~ NA
        ),
        inornout_cat = case_when(
            is_inborn == TRUE ~ "Inborn",
            is_inborn == FALSE ~ "Outborn",
            TRUE ~ "Unknown"
        ),

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
        sga_category = if_else(is_valid_sga, sga_cat_temp, sga_explanation)
    )

cat("Total Study Population:", nrow(analysis_df), "\n")
cat("Inborn:", sum(analysis_df$inornout_cat == "Inborn"), "\n")
cat("Outborn:", sum(analysis_df$inornout_cat == "Outborn"), "\n")
cat("Unknown:", sum(analysis_df$inornout_cat == "Unknown"), "\n")


# ===== ANALYSIS =====

calc_inout_stats <- function(data, group_var) {
    # If group_var is length > 1, extract the primary column for checking NAs
    primary_var <- group_var[1]
    data %>%
        filter(!is.na(.data[[primary_var]])) %>%
        group_by(across(all_of(group_var))) %>%
        summarise(
            n_total = n(),
            n_inborn = sum(inornout_cat == "Inborn"),
            n_outborn = sum(inornout_cat == "Outborn"),
            n_unknown = sum(inornout_cat == "Unknown"),
            pct_inborn = round(100 * n_inborn / n_total, 1),
            pct_outborn = round(100 * n_outborn / n_total, 1),
            pct_unknown = round(100 * n_unknown / n_total, 1),
            .groups = "drop"
        )
}

cat("\n=== Calculating Statistics ===\n")

# LBW Categories
# Overall
lbw_overall <- analysis_df %>%
    rename(category = bw_category, explanation = lbw_explanation) %>%
    calc_inout_stats(c("category", "explanation")) %>%
    mutate(facility = "Overall", classification = "Birthweight") %>%
    add_totals_inborn(analysis_df, "Birthweight", by_facility = FALSE)

# By Facility
lbw_facility <- analysis_df %>%
    rename(category = bw_category, explanation = lbw_explanation) %>%
    group_by(facility) %>%
    group_modify(~ calc_inout_stats(.x, c("category", "explanation"))) %>%
    mutate(classification = "Birthweight") %>%
    add_totals_inborn(analysis_df, "Birthweight", by_facility = TRUE)

# SGA Categories
# Overall
sga_overall <- analysis_df %>%
    rename(category = sga_category, explanation = sga_explanation) %>%
    calc_inout_stats(c("category", "explanation")) %>%
    mutate(facility = "Overall", classification = "SGA") %>%
    add_totals_inborn(analysis_df, "SGA", by_facility = FALSE) %>%
    arrange(
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total"))
    )

# By Facility
sga_facility <- analysis_df %>%
    rename(category = sga_category, explanation = sga_explanation) %>%
    group_by(facility) %>%
    group_modify(~ calc_inout_stats(.x, c("category", "explanation"))) %>%
    mutate(classification = "SGA") %>%
    add_totals_inborn(analysis_df, "SGA", by_facility = TRUE) %>%
    arrange(
        facility,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total"))
    )


# ===== SAVE =====

cat("\n=== Saving Results ===\n")
all_overall <- bind_rows(lbw_overall, sga_overall)
all_facility <- bind_rows(lbw_facility, sga_facility)

# Sample size summary
bw_note <- sprintf(
    "is.finite(bw_kg) & bw_kg >= %.1f kg & bw_kg <= %.1f kg",
    BW_MIN_KG, BW_MAX_KG
)
ga_note <- sprintf(
    "!is.na(gestation) & gestation >= %d wks & gestation <= %d wks",
    GA_MIN, GA_MAX
)
sga_note <- paste0(bw_note, " AND ", ga_note, " AND !is.na(sga_category)")
inorout_note <- "toupper(inorout) == \"TRUE\" | toupper(inorout) == \"FALSE\""

n_total <- nrow(analysis_df)

sample_size_summary <- tibble(
    description = c(
        "Total admissions",
        "Inborn",
        "Outborn",
        "Unknown inborn/outborn status",
        "Records with valid birthweight (LBW sample)",
        "Discarded for LBW (missing/invalid birthweight)",
        "Records with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        n_total,
        sum(analysis_df$inornout_cat == "Inborn"),
        sum(analysis_df$inornout_cat == "Outborn"),
        sum(analysis_df$inornout_cat == "Unknown"),
        sum(analysis_df$is_valid_bw),
        sum(!analysis_df$is_valid_bw),
        sum(analysis_df$is_valid_sga),
        sum(analysis_df$sga_explanation == "Missing BW"),
        sum(analysis_df$sga_explanation == "Missing GA"),
        sum(analysis_df$sga_explanation == "Missing both BW and GA")
    ),
    pct_of_total = round(100 * n / n_total, 1),
    Filter_notes = c(
        NA_character_,
        "toupper(inorout) == \"TRUE\"",
        "toupper(inorout) == \"FALSE\"",
        paste0("NOT (", inorout_note, ")"),
        bw_note,
        paste0("NOT (", bw_note, ")"),
        sga_note,
        "Invalid/missing birth weight",
        "Invalid/missing gestational age",
        "Both BW and GA invalid/missing"
    )
)

write_csv(sample_size_summary, file.path(output_dir, "00_sample_size_summary.csv"))
cat("Saved: 00_sample_size_summary.csv\n")

write_csv(all_overall, file.path(output_dir, "01_inborn_outborn_overall.csv"))
cat("Saved: 01_inborn_outborn_overall.csv\n")

write_csv(all_facility, file.path(output_dir, "02_inborn_outborn_by_facility.csv"))
cat("Saved: 02_inborn_outborn_by_facility.csv\n")

cat("\nAnalysis complete!\n")
