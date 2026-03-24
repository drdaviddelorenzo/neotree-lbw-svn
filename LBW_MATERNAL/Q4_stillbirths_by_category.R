# Q4_stillbirths_by_category.R
# Stillbirths by LBW and SGA Categories
#
# This script analyzes STILLBIRTHS ONLY from maternal delivery data:
# 1. Filter for stillbirths (neotreeoutcome in c("SBF","SBM","STBF","STBM")) with gestation >= 22 weeks
# 2. Classify as Early stillbirth (22-27+6 weeks) or Late stillbirth (>=28 weeks)
# 3. Distribution by birthweight categories (LBW classification)
# 4. Distribution by SGA categories (SVN classification)
# 5. Breakdowns by facility and year
#
# Stillbirth classification criteria:
# - Outcome = stillbirth (SBF or SBM)
# - Gestation >= 22+0 weeks
# - Early stillbirth: 22+0 to 27+6 weeks
# - Late stillbirth: >= 28+0 weeks
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
output_dir <- "Q4_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS
# Must match the constants in maternal_descriptive_analysis.R,
# Q2_birth_insights_maternal_data.R, and Q3_live_births_by_category.R
# so that sample sizes are comparable across all scripts.
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

# Classify stillbirth timing
classify_stillbirth_timing <- function(gestation_weeks) {
    dplyr::case_when(
        is.na(gestation_weeks) ~ NA_character_,
        gestation_weeks >= 22 & gestation_weeks < 28 ~ "Early stillbirth (22-27+6w)",
        gestation_weeks >= 28 ~ "Late stillbirth (>=28w)",
        TRUE ~ "Below 22 weeks (excluded)"
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

# Add gestational age early for filtering
all_maternal <- all_maternal %>%
    mutate(
        gestation_weeks = suppressWarnings(as.numeric(gestation))
    )

total_deliveries <- nrow(all_maternal)
cat("Total deliveries:", total_deliveries, "\n")

# ===== FILTER FOR STILLBIRTHS =====

cat("\n=== Filtering for Stillbirths ===\n")

# Filter for stillbirths
# KCH uses: SBF = Stillbirth Female, SBM = Stillbirth Male
# SMCH uses: STBF = Stillbirth Female, STBM = Stillbirth Male
stillbirths_all <- all_maternal %>%
    filter(
        !is.na(neotreeoutcome),
        toupper(trimws(neotreeoutcome)) %in% c("SBF", "SBM", "STBF", "STBM")
    )

cat("Total stillbirths (all gestations):", nrow(stillbirths_all), "\n")

# Filter for gestation >= 22 weeks
stillbirths <- stillbirths_all %>%
    filter(
        !is.na(gestation_weeks),
        gestation_weeks >= 22
    )

stillbirth_count <- nrow(stillbirths)
cat("Stillbirths >= 22 weeks:", stillbirth_count, "\n")
cat("Percentage of total deliveries:", round(100 * stillbirth_count / total_deliveries, 1), "%\n")

# Classify stillbirth timing
stillbirths <- stillbirths %>%
    mutate(
        sb_timing = classify_stillbirth_timing(gestation_weeks)
    )

cat("\nStillbirth timing classification:\n")
sb_timing_summary <- stillbirths %>%
    count(sb_timing)
print(sb_timing_summary)

# Stillbirths by facility
cat("\nStillbirths by facility:\n")
sb_by_facility <- stillbirths %>%
    group_by(facility) %>%
    summarise(
        stillbirths = n(),
        .groups = "drop"
    )
print(sb_by_facility)

# ===== PROCESS BIRTHWEIGHT DATA FOR STILLBIRTHS =====

cat("\n=== Processing Birthweight Data (Stillbirths Only) ===\n")

# Add gestational age and valid categories
process_sb <- stillbirths %>%
    mutate(
        date_parsed = suppressWarnings(as.Date(dateadmission)),
        year = as.integer(format(date_parsed, "%Y")),

        # Validations — thresholds from shared constants (match Q2, Q3 and maternal_descriptive_analysis.R)
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

# Count valid birthweights in stillbirths
valid_bw_sb <- process_sb %>% filter(is_valid_bw)

valid_bw_count <- nrow(valid_bw_sb)
cat("Stillbirths with valid birthweight:", valid_bw_count, "\n")
cat("Percentage of stillbirths:", round(100 * valid_bw_count / stillbirth_count, 1), "%\n")

# ===== LBW CLASSIFICATION (STILLBIRTHS) =====

cat("\n=== LBW Classification (Stillbirths Only) ===\n")

lbw_sb <- valid_bw_sb

cat("Stillbirths with LBW classification:", nrow(lbw_sb), "\n")

# Distribution by category
lbw_distribution <- process_sb %>%
    count(category = bw_category, explanation = lbw_explanation, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    arrange(match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# Distribution by stillbirth timing
lbw_by_timing <- process_sb %>%
    group_by(sb_timing, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(sb_timing) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(sb_timing, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# Distribution by facility
lbw_by_facility <- process_sb %>%
    group_by(facility, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(facility, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# Distribution by stillbirth timing AND facility
lbw_by_timing_facility <- process_sb %>%
    group_by(facility, sb_timing, category = bw_category, explanation = lbw_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility, sb_timing) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "Birthweight"
    ) %>%
    ungroup() %>%
    arrange(facility, sb_timing, match(category, c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")))

# ===== SVN/SGA CLASSIFICATION (STILLBIRTHS) =====

cat("\n=== SVN/SGA Classification (Stillbirths Only) ===\n")

svn_sb <- valid_bw_sb %>%
    filter(!is.na(gestation_weeks), is.finite(gestation_weeks)) %>%
    mutate(
        sga_status = dplyr::if_else(gestation_weeks >= 22 & !is.na(gestation_weeks), classify_sga(bw_kg, gestation_weeks, if ("sexdis" %in% names(.)) sexdis else NA_character_), NA_character_),
        sga_category = sga_category_from_gestational_age(sga_status, gestation_weeks)
    ) %>%
    filter(!is.na(sga_category))

cat("Stillbirths with SGA classification:", nrow(svn_sb), "\n")

# Distribution by category
sga_distribution <- process_sb %>%
    count(category = sga_category, explanation = sga_explanation, name = "n") %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    arrange(
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# Distribution by stillbirth timing
sga_by_timing <- process_sb %>%
    group_by(sb_timing, category = sga_category, explanation = sga_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(sb_timing) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    ungroup() %>%
    arrange(
        sb_timing,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# Distribution by facility
sga_by_facility <- process_sb %>%
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

# Distribution by stillbirth timing AND facility
sga_by_timing_facility <- process_sb %>%
    group_by(facility, sb_timing, category = sga_category, explanation = sga_explanation) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility, sb_timing) %>%
    mutate(
        percentage = n / sum(n) * 100,
        classification = "SGA"
    ) %>%
    ungroup() %>%
    arrange(
        facility, sb_timing,
        match(category, c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA", "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total")),
        match(explanation, c("Included in analysis", "Missing BW", "Missing GA", "Missing both BW and GA"))
    )

# ===== YEAR-BASED ANALYSIS =====

cat("\n=== Year-Based Analysis (Stillbirths) ===\n")

# Extract year from dateadmission
process_sb_year <- process_sb %>%
    filter(!is.na(year), year >= 2022, year <= 2025)

cat("Stillbirths with valid year:", nrow(process_sb_year), "\n")

# LBW by year
lbw_by_year <- process_sb_year %>%
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
lbw_by_facility_year <- process_sb_year %>%
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
sga_by_year <- process_sb_year %>%
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
sga_by_facility_year <- process_sb_year %>%
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

# Add totals for all derivations
lbw_distribution <- add_totals(lbw_distribution)
lbw_by_timing <- add_totals(lbw_by_timing, "sb_timing")
lbw_by_facility <- add_totals(lbw_by_facility, "facility")
lbw_by_timing_facility <- add_totals(lbw_by_timing_facility, c("facility", "sb_timing"))
lbw_by_year <- add_totals(lbw_by_year, "year")
lbw_by_facility_year <- add_totals(lbw_by_facility_year, c("facility", "year"))

sga_distribution <- add_totals(sga_distribution)
sga_by_timing <- add_totals(sga_by_timing, "sb_timing")
sga_by_facility <- add_totals(sga_by_facility, "facility")
sga_by_timing_facility <- add_totals(sga_by_timing_facility, c("facility", "sb_timing"))
sga_by_year <- add_totals(sga_by_year, "year")
sga_by_facility_year <- add_totals(sga_by_facility_year, c("facility", "year"))

# Combine results
distribution_combined <- bind_rows(lbw_distribution, sga_distribution)
by_timing_combined <- bind_rows(lbw_by_timing, sga_by_timing)
by_timing_facility_combined <- bind_rows(lbw_by_timing_facility, sga_by_timing_facility)
by_facility_combined <- bind_rows(lbw_by_facility, sga_by_facility)
by_year_combined <- bind_rows(lbw_by_year, sga_by_year)
by_facility_year_combined <- bind_rows(lbw_by_facility_year, sga_by_facility_year)

# ===== SAMPLE SIZE SUMMARY =====

# Build filter-note strings (written once; stored in CSV for audit trail)
bw_note <- sprintf(
    "is.finite(bw_kg) & bw_kg >= %.1f kg & bw_kg <= %.1f kg",
    BW_MIN_KG, BW_MAX_KG
)
ga_note <- sprintf(
    "!is.na(gestation) & gestation >= %d wks & gestation <= %d wks",
    GA_MIN, GA_MAX
)
both_note <- paste0(bw_note, " AND ", ga_note)
sb_note <- "neotreeoutcome in c(\"SBF\",\"SBM\",\"STBF\",\"STBM\") & gestation >= 22 wks"

# Calculate facility-specific sample sizes
kch_lbw_n <- sum(lbw_sb$facility == "KCH")
smch_lbw_n <- sum(lbw_sb$facility == "SMCH")

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
        "Total stillbirths (all gestations)",
        "Stillbirths >= 22 weeks",
        "Stillbirths with valid birthweight (LBW sample size)",
        "Discarded for LBW (Missing/invalid birthweight)",
        "Stillbirths with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)",
        "",
        "Early stillbirths (22-27+6 weeks)",
        "Late stillbirths (>=28 weeks)",
        "",
        "KCH - Total deliveries",
        "KCH - Stillbirths >= 22w",
        "KCH - LBW sample size",
        "KCH - Discarded for LBW",
        "KCH - SGA sample size",
        "KCH - Discarded for SGA (Missing BW)",
        "KCH - Discarded for SGA (Missing GA)",
        "KCH - Discarded for SGA (Missing both BW and GA)",
        "",
        "SMCH - Total deliveries",
        "SMCH - Stillbirths >= 22w",
        "SMCH - LBW sample size",
        "SMCH - Discarded for LBW",
        "SMCH - SGA sample size",
        "SMCH - Discarded for SGA (Missing BW)",
        "SMCH - Discarded for SGA (Missing GA)",
        "SMCH - Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        total_deliveries,
        nrow(stillbirths_all),
        stillbirth_count,
        nrow(lbw_sb),
        sum(process_sb$bw_category == "Discarded"),
        nrow(svn_sb),
        sum(process_sb$sga_explanation == "Missing BW"),
        sum(process_sb$sga_explanation == "Missing GA"),
        sum(process_sb$sga_explanation == "Missing both BW and GA"),
        NA,
        sum(stillbirths$sb_timing == "Early stillbirth (22-27+6w)", na.rm = TRUE),
        sum(stillbirths$sb_timing == "Late stillbirth (>=28w)", na.rm = TRUE),
        NA,
        sum(all_maternal$facility == "KCH"),
        sum(stillbirths$facility == "KCH"),
        kch_lbw_n,
        sum(process_sb$facility == "KCH" & process_sb$bw_category == "Discarded"),
        kch_sga_n,
        sum(process_sb$facility == "KCH" & process_sb$sga_explanation == "Missing BW"),
        sum(process_sb$facility == "KCH" & process_sb$sga_explanation == "Missing GA"),
        sum(process_sb$facility == "KCH" & process_sb$sga_explanation == "Missing both BW and GA"),
        NA,
        sum(all_maternal$facility == "SMCH"),
        sum(stillbirths$facility == "SMCH"),
        smch_lbw_n,
        sum(process_sb$facility == "SMCH" & process_sb$bw_category == "Discarded"),
        smch_sga_n,
        sum(process_sb$facility == "SMCH" & process_sb$sga_explanation == "Missing BW"),
        sum(process_sb$facility == "SMCH" & process_sb$sga_explanation == "Missing GA"),
        sum(process_sb$facility == "SMCH" & process_sb$sga_explanation == "Missing both BW and GA")
    ),
    percentage_of_total = c(
        100,
        round(100 * nrow(stillbirths_all) / total_deliveries, 1),
        round(100 * stillbirth_count / total_deliveries, 1),
        round(100 * nrow(lbw_sb) / stillbirth_count, 1),
        round(100 * sum(process_sb$bw_category == "Discarded") / stillbirth_count, 1),
        round(100 * nrow(svn_sb) / stillbirth_count, 1),
        round(100 * sum(process_sb$sga_explanation == "Missing BW") / stillbirth_count, 1),
        round(100 * sum(process_sb$sga_explanation == "Missing GA") / stillbirth_count, 1),
        round(100 * sum(process_sb$sga_explanation == "Missing both BW and GA") / stillbirth_count, 1),
        NA,
        round(100 * sum(stillbirths$sb_timing == "Early stillbirth (22-27+6w)", na.rm = TRUE) / stillbirth_count, 1),
        round(100 * sum(stillbirths$sb_timing == "Late stillbirth (>=28w)", na.rm = TRUE) / stillbirth_count, 1),
        NA,
        round(100 * sum(all_maternal$facility == "KCH") / total_deliveries, 1),
        round(100 * sum(stillbirths$facility == "KCH") / sum(all_maternal$facility == "KCH"), 1),
        round(100 * kch_lbw_n / sum(stillbirths$facility == "KCH"), 1),
        round(100 * sum(process_sb$facility == "KCH" & process_sb$bw_category == "Discarded") / sum(stillbirths$facility == "KCH"), 1),
        round(100 * kch_sga_n / sum(stillbirths$facility == "KCH"), 1),
        round(100 * sum(process_sb$facility == "KCH" & process_sb$sga_explanation == "Missing BW") / sum(stillbirths$facility == "KCH"), 1),
        round(100 * sum(process_sb$facility == "KCH" & process_sb$sga_explanation == "Missing GA") / sum(stillbirths$facility == "KCH"), 1),
        round(100 * sum(process_sb$facility == "KCH" & process_sb$sga_explanation == "Missing both BW and GA") / sum(stillbirths$facility == "KCH"), 1),
        NA,
        round(100 * sum(all_maternal$facility == "SMCH") / total_deliveries, 1),
        round(100 * sum(stillbirths$facility == "SMCH") / sum(all_maternal$facility == "SMCH"), 1),
        round(100 * smch_lbw_n / sum(stillbirths$facility == "SMCH"), 1),
        round(100 * sum(process_sb$facility == "SMCH" & process_sb$bw_category == "Discarded") / sum(stillbirths$facility == "SMCH"), 1),
        round(100 * smch_sga_n / sum(stillbirths$facility == "SMCH"), 1),
        round(100 * sum(process_sb$facility == "SMCH" & process_sb$sga_explanation == "Missing BW") / sum(stillbirths$facility == "SMCH"), 1),
        round(100 * sum(process_sb$facility == "SMCH" & process_sb$sga_explanation == "Missing GA") / sum(stillbirths$facility == "SMCH"), 1),
        round(100 * sum(process_sb$facility == "SMCH" & process_sb$sga_explanation == "Missing both BW and GA") / sum(stillbirths$facility == "SMCH"), 1)
    ),
    Filter_notes = case_when(
        grepl("^Total deliveries", description, ignore.case = TRUE) ~ "All records",
        grepl("stillbirths \\(all gestations\\)", description, ignore.case = TRUE) ~ sb_note,
        grepl("stillbirths.*>= 22|Stillbirths >= 22", description, ignore.case = TRUE) ~ paste0(sb_note, " (applied)"),
        grepl("valid birthweight|LBW sample", description, ignore.case = TRUE) ~ bw_note,
        grepl("valid SGA", description, ignore.case = TRUE) ~ both_note,
        grepl("Discarded.*LBW|Missing.*invalid birthweight", description, ignore.case = TRUE) ~ paste0("NOT (", bw_note, ")"),
        grepl("Discarded.*SGA.*both|Missing both", description, ignore.case = TRUE) ~ "Both BW and GA invalid / missing",
        grepl("Discarded.*SGA.*Missing BW|SGA.*BW$", description, ignore.case = TRUE) ~ "Birth weight invalid / missing",
        grepl("Discarded.*SGA.*Missing GA|SGA.*GA$", description, ignore.case = TRUE) ~ "Gestational age invalid / missing",
        grepl("Early stillbirths", description, ignore.case = TRUE) ~ "gestation >= 22 & gestation < 28 weeks",
        grepl("Late stillbirths", description, ignore.case = TRUE) ~ "gestation >= 28 weeks",
        TRUE ~ NA_character_
    )
)

# ===== SAVE OUTPUTS =====

cat("\n=== Saving Results ===\n")

write_csv(sample_size_summary, file.path(output_dir, "01_sample_size_summary.csv"))
cat("Saved: 01_sample_size_summary.csv\n")

write_csv(distribution_combined, file.path(output_dir, "02_distribution_by_category.csv"))
cat("Saved: 02_distribution_by_category.csv\n")

write_csv(by_timing_combined, file.path(output_dir, "03_distribution_by_stillbirth_timing.csv"))
cat("Saved: 03_distribution_by_stillbirth_timing.csv\n")

write_csv(by_timing_facility_combined, file.path(output_dir, "04_distribution_by_timing_and_facility.csv"))
cat("Saved: 04_distribution_by_timing_and_facility.csv\n")

write_csv(by_facility_combined, file.path(output_dir, "05_distribution_by_facility.csv"))
cat("Saved: 05_distribution_by_facility.csv\n")

write_csv(by_year_combined, file.path(output_dir, "06_distribution_by_year.csv"))
cat("Saved: 06_distribution_by_year.csv\n")

write_csv(by_facility_year_combined, file.path(output_dir, "07_distribution_by_facility_and_year.csv"))
cat("Saved: 07_distribution_by_facility_and_year.csv\n")

# ===== ODDS RATIO ANALYSIS =====
#
# Outcome = stillbirth (1) vs live birth (0).
# Denominator = ALL deliveries with valid BW/SGA, not just stillbirths.
# We rebuild a classification table from all_maternal using the same
# classification logic already applied in process_sb.
#
# Three scopes of OR:
#   A. Overall stillbirth risk        → all SBF/SBM/STBF/STBM vs all LB
#   B. Early stillbirth risk          → subset with 22-27+6 w
#   C. Late stillbirth risk           → subset with >=28 w
#
# References: NBW for LBW comparison, Term-AGA for SGA comparison.

cat("\n=== Odds Ratio Analysis ===\n")

# --- Build full delivery dataset with classifications (same logic as process_sb) ---
all_maternal_class <- all_maternal %>%
    mutate(
        # Outcome: 1 = stillbirth (>=22w), 0 = live birth
        outcome_raw = toupper(trimws(neotreeoutcome)),
        is_stillbirth = outcome_raw %in% c("SBF", "SBM", "STBF", "STBM"),
        is_live_birth = outcome_raw == "LB",

        # Stillbirth timing (NA for non-SBs)
        sb_timing = dplyr::case_when(
            is_stillbirth & !is.na(gestation_weeks) & gestation_weeks >= 22 & gestation_weeks < 28 ~ "Early stillbirth (22-27+6w)",
            is_stillbirth & !is.na(gestation_weeks) & gestation_weeks >= 28 ~ "Late stillbirth (>=28w)",
            TRUE ~ NA_character_
        ),

        # Classify BW — thresholds from shared constants
        is_valid_bw = is.finite(bw_kg) & bw_kg >= BW_MIN_KG & bw_kg <= BW_MAX_KG,
        bw_category = dplyr::if_else(is_valid_bw, bw_category_from_kg(bw_kg), NA_character_),

        # Classify SGA — thresholds from shared constants
        is_invalid_ga = is.na(gestation_weeks) | gestation_weeks < GA_MIN | gestation_weeks > GA_MAX,
        sga_status = dplyr::if_else(
            gestation_weeks >= 22 & !is.na(gestation_weeks),
            classify_sga(bw_kg, gestation_weeks, if ("sexdis" %in% names(.)) sexdis else NA_character_),
            NA_character_
        ),
        sga_cat_temp = sga_category_from_gestational_age(sga_status, gestation_weeks),
        is_valid_sga = is_valid_bw & !is.na(sga_cat_temp),
        sga_category = dplyr::if_else(is_valid_sga, sga_cat_temp, NA_character_),

        # Preterm flag
        is_preterm = dplyr::if_else(!is_invalid_ga, gestation_weeks < 37, NA)
    ) %>%
    # Keep only live births and confirmed stillbirths >= 22 w for valid comparisons
    filter(is_live_birth | (!is.na(sb_timing)))

cat("Deliveries used for OR analysis (LB + SB >=22w):", nrow(all_maternal_class), "\n")

# --- Helper: logistic regression → OR table ---
calc_or_table <- function(data, category_col, ref_level, label) {
    df <- data %>%
        filter(!is.na(.data[[category_col]])) %>%
        mutate(
            cat_factor = relevel(factor(.data[[category_col]]), ref = ref_level),
            outcome    = as.integer(is_stillbirth)
        )

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

            data.frame(
                comparison = label,
                category = sub("^cat_factor", "", or_rows),
                reference = ref_level,
                n_total = nrow(df),
                n_outcome = sum(df$outcome),
                OR = round(exp(coefs[or_rows, "Estimate"]), 3),
                CI_lower = round(exp(coefs[or_rows, "Estimate"] - 1.96 * coefs[or_rows, "Std. Error"]), 3),
                CI_upper = round(exp(coefs[or_rows, "Estimate"] + 1.96 * coefs[or_rows, "Std. Error"]), 3),
                p_value = round(coefs[or_rows, "Pr(>|z|)"], 4),
                significant = ifelse(coefs[or_rows, "Pr(>|z|)"] < 0.05, "Yes", "No"),
                stringsAsFactors = FALSE
            )
        },
        error = function(e) NULL
    )
}

# --- Helper: run all comparisons for a dataset subset ---
run_or_analysis <- function(data, scope_label, timing_label = "All stillbirths") {
    results <- list()

    # 1. LBW categories vs NBW
    lbw_data <- data %>% filter(is_valid_bw, bw_category %in% c("ELBW", "VLBW", "LBW", "NBW", "HBW"))
    r1 <- calc_or_table(lbw_data, "bw_category", "NBW", "LBW category vs NBW")
    if (!is.null(r1)) results[["lbw"]] <- r1

    # 2. SGA categories vs Term-AGA
    sga_data <- data %>% filter(is_valid_sga, sga_category %in% c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA"))
    r2 <- calc_or_table(sga_data, "sga_category", "Term-AGA", "SGA category vs Term-AGA")
    if (!is.null(r2)) results[["sga"]] <- r2

    # 3. Preterm vs Term
    term_data <- data %>%
        filter(!is_invalid_ga) %>%
        mutate(term_group = dplyr::if_else(gestation_weeks >= 37, "Term", "Preterm"))
    r3 <- calc_or_table(term_data, "term_group", "Term", "Preterm vs Term")
    if (!is.null(r3)) results[["term"]] <- r3

    if (length(results) == 0) {
        return(NULL)
    }
    bind_rows(results) %>% mutate(scope = scope_label, sb_scope = timing_label, .before = 1)
}

# --- Scope A: Overall stillbirths ---
cat("\n-- A: Overall stillbirth risk --\n")

or_overall_global <- run_or_analysis(all_maternal_class, "Global", "All stillbirths")
or_overall_fac <- all_maternal_class %>%
    group_by(facility) %>%
    group_modify(~ run_or_analysis(.x, scope_label = .y$facility, timing_label = "All stillbirths")) %>%
    ungroup()

# --- Scope B: Early stillbirth risk ---
cat("\n-- B: Early stillbirth (22-27+6w) risk --\n")

# For timing subgroups: keep live births + only that timing of SB
early_data <- all_maternal_class %>%
    filter(is_live_birth | sb_timing == "Early stillbirth (22-27+6w)") %>%
    mutate(is_stillbirth = !is_live_birth) # redefine outcome for this subgroup

or_early_global <- run_or_analysis(early_data, "Global", "Early stillbirth (22-27+6w)")
or_early_fac <- early_data %>%
    group_by(facility) %>%
    group_modify(~ run_or_analysis(.x, scope_label = .y$facility, timing_label = "Early stillbirth (22-27+6w)")) %>%
    ungroup()

# --- Scope C: Late stillbirth risk ---
cat("\n-- C: Late stillbirth (>=28w) risk --\n")

late_data <- all_maternal_class %>%
    filter(is_live_birth | sb_timing == "Late stillbirth (>=28w)") %>%
    mutate(is_stillbirth = !is_live_birth)

or_late_global <- run_or_analysis(late_data, "Global", "Late stillbirth (>=28w)")
or_late_fac <- late_data %>%
    group_by(facility) %>%
    group_modify(~ run_or_analysis(.x, scope_label = .y$facility, timing_label = "Late stillbirth (>=28w)")) %>%
    ungroup()

# --- Combine all OR results ---
or_results <- bind_rows(
    or_overall_global, or_overall_fac,
    or_early_global,   or_early_fac,
    or_late_global,    or_late_fac
) %>%
    select(
        scope, sb_scope, comparison, category, reference, n_total, n_outcome,
        OR, CI_lower, CI_upper, p_value, significant
    )

cat("\nOdds Ratio Results (first 12 rows):\n")
print(head(as.data.frame(or_results), 12))

write_csv(or_results, file.path(output_dir, "08_odds_ratios.csv"))
cat("Saved: 08_odds_ratios.csv\n")

# ===== CHI-SQUARED TESTS =====
# Testing for overall significant differences in the proportion of stillbirths across categories.
# We test independence between category and outcome (Stillbirth vs Live birth).

cat("\n=== Chi-Squared Tests of Independence ===\n")

run_chisq_test <- function(data, category_col, test_name, scope_label, sb_scope) {
    df <- data %>% filter(!is.na(.data[[category_col]]))
    
    # We need variability in both category and outcome
    if(length(unique(df[[category_col]])) > 1 && length(unique(df$is_stillbirth)) > 1) {
        tbl <- table(df[[category_col]], df$is_stillbirth)
        test <- suppressWarnings(chisq.test(tbl))
        
        return(data.frame(
            test_name = test_name,
            scope = scope_label,
            sb_scope = sb_scope,
            x_squared = round(test$statistic, 3),
            df = test$parameter,
            p_value = signif(test$p.value, 4),
            significant = ifelse(test$p.value < 0.05, "Yes", "No"),
            stringsAsFactors = FALSE
        ))
    }
    return(NULL)
}

chisq_results <- list()

# Global tests - All Stillbirths
chisq_results[[1]] <- run_chisq_test(
    all_maternal_class %>% filter(is_valid_bw, bw_category %in% c("ELBW", "VLBW", "LBW", "NBW", "HBW")),
    "bw_category", "LBW Categories", "Global", "All stillbirths"
)
chisq_results[[2]] <- run_chisq_test(
    all_maternal_class %>% filter(is_valid_sga, sga_category %in% c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")),
    "sga_category", "SGA Categories", "Global", "All stillbirths"
)

# Facility-level tests - All Stillbirths
for(fac in unique(all_maternal_class$facility)) {
    fac_data <- all_maternal_class %>% filter(facility == fac)
    chisq_results[[length(chisq_results) + 1]] <- run_chisq_test(
        fac_data %>% filter(is_valid_bw, bw_category %in% c("ELBW", "VLBW", "LBW", "NBW", "HBW")),
        "bw_category", "LBW Categories", fac, "All stillbirths"
    )
    chisq_results[[length(chisq_results) + 1]] <- run_chisq_test(
        fac_data %>% filter(is_valid_sga, sga_category %in% c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")),
        "sga_category", "SGA Categories", fac, "All stillbirths"
    )
}

chisq_df <- bind_rows(chisq_results)
write_csv(chisq_df, file.path(output_dir, "09_chi_squared_tests.csv"))
cat("Saved: 09_chi_squared_tests.csv\n")

if(nrow(chisq_df) > 0) {
    cat("\nChi-Squared Results:\n")
    print(chisq_df)
}

# ===== PAIRWISE COMPARISONS =====
cat("\n=== Pairwise Comparisons ===\n")

run_pairwise_tests <- function(data, category_col, scope_label, sb_scope) {
    df <- data %>% filter(!is.na(.data[[category_col]]))
    
    if(length(unique(df[[category_col]])) < 2 || length(unique(df$is_stillbirth)) < 2) {
        return(list(prop = NULL, emm = NULL))
    }
    
    tbl <- table(df[[category_col]], df$is_stillbirth)
    col_names <- colnames(tbl)
    
    prop_res <- NULL
    if("TRUE" %in% col_names) {
        successes <- tbl[, "TRUE"]
        totals <- rowSums(tbl)
        
        ppt <- suppressWarnings(pairwise.prop.test(successes, totals, p.adjust.method = "holm"))
        
        prop_res <- as.data.frame(as.table(ppt$p.value)) %>%
            filter(!is.na(Freq)) %>%
            rename(group1 = Var1, group2 = Var2, p_value_holm = Freq) %>%
            mutate(
                scope = scope_label,
                sb_scope = sb_scope,
                category_type = category_col,
                significant = dplyr::if_else(p_value_holm < 0.05, "Yes", "No")
            ) %>%
            relocate(scope, sb_scope, category_type)
    }
    
    emm_res <- NULL
    if (requireNamespace("emmeans", quietly = TRUE)) {
        tryCatch({
            df_model <- df %>% mutate(
                outcome = as.integer(is_stillbirth),
                cat_factor = factor(.data[[category_col]])
            )
            model <- glm(outcome ~ cat_factor, data = df_model, family = binomial(link = "logit"))
            em <- emmeans::emmeans(model, "cat_factor", type = "response")
            
            # Use emmeans::contrast directly to avoid generic pairs() issues
            pairs_em <- emmeans::contrast(em, method = "pairwise", adjust = "tukey")
            
            # Ensure p.value column exists in output
            pairs_df <- as.data.frame(pairs_em)
            if ("p.value" %in% names(pairs_df)) {
                emm_res <- pairs_df %>%
                    mutate(
                        scope = scope_label,
                        sb_scope = sb_scope,
                        category_type = category_col,
                        significant = dplyr::if_else(p.value < 0.05, "Yes", "No")
                    ) %>%
                    relocate(scope, sb_scope, category_type)
            }
        }, error = function(e) {
            cat("    Error in emmeans for", category_col, "-", scope_label, ":", e$message, "\n")
        })
    }
    
    return(list(prop = prop_res, emm = emm_res))
}

pairwise_prop_list <- list()
pairwise_emm_list <- list()

append_pairwise <- function(res, name) {
    if (!is.null(res$prop)) pairwise_prop_list[[name]] <<- res$prop
    if (!is.null(res$emm)) pairwise_emm_list[[name]] <<- res$emm
}

append_pairwise(run_pairwise_tests(all_maternal_class %>% filter(is_valid_bw, bw_category %in% c("ELBW", "VLBW", "LBW", "NBW", "HBW")), "bw_category", "Global", "All stillbirths"), "Global_BW")
append_pairwise(run_pairwise_tests(all_maternal_class %>% filter(is_valid_sga, sga_category %in% c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")), "sga_category", "Global", "All stillbirths"), "Global_SGA")

for(fac in unique(all_maternal_class$facility)) {
    fac_data <- all_maternal_class %>% filter(facility == fac)
    append_pairwise(run_pairwise_tests(fac_data %>% filter(is_valid_bw, bw_category %in% c("ELBW", "VLBW", "LBW", "NBW", "HBW")), "bw_category", fac, "All stillbirths"), paste0(fac, "_BW"))
    append_pairwise(run_pairwise_tests(fac_data %>% filter(is_valid_sga, sga_category %in% c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")), "sga_category", fac, "All stillbirths"), paste0(fac, "_SGA"))
}

if (length(pairwise_prop_list) > 0) {
    prop_df <- bind_rows(pairwise_prop_list)
    write_csv(prop_df, file.path(output_dir, "10_pairwise_prop_tests.csv"))
    cat("Saved: 10_pairwise_prop_tests.csv\n")
}

if (length(pairwise_emm_list) > 0) {
    emm_df <- bind_rows(pairwise_emm_list)
    write_csv(emm_df, file.path(output_dir, "11_emmeans_pairwise.csv"))
    cat("Saved: 11_emmeans_pairwise.csv\n")
} else if (!requireNamespace("emmeans", quietly = TRUE)) {
    cat("Note: emmeans package not installed, skipping 11_emmeans_pairwise.csv\n")
}

# ===== SUMMARY REPORT =====

cat("\n", rep("=", 60), "\n", sep = "")
cat("STILLBIRTHS SUMMARY BY LBW/SGA CATEGORIES\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Sample Sizes:\n")
cat("  Total deliveries:", total_deliveries, "\n")
cat(
    "  Stillbirths (all gestations):", nrow(stillbirths_all),
    sprintf("(%.1f%% of deliveries)", 100 * nrow(stillbirths_all) / total_deliveries), "\n"
)
cat(
    "  Stillbirths >= 22 weeks:", stillbirth_count,
    sprintf("(%.1f%% of deliveries)", 100 * stillbirth_count / total_deliveries), "\n"
)
cat(
    "  Valid birthweight:", valid_bw_count,
    sprintf("(%.1f%% of stillbirths)", 100 * valid_bw_count / stillbirth_count), "\n"
)
cat(
    "  LBW classified:", nrow(lbw_sb),
    sprintf("(%.1f%% of stillbirths)", 100 * nrow(lbw_sb) / stillbirth_count), "\n"
)
cat(
    "  SGA classified:", nrow(svn_sb),
    sprintf("(%.1f%% of stillbirths)", 100 * nrow(svn_sb) / stillbirth_count), "\n\n"
)

cat("Stillbirth Timing:\n")
print(sb_timing_summary)
cat("\n")

cat("Odds Ratio Summary (Global, All Stillbirths):\n")
print(
    or_results %>%
        filter(scope == "Global", sb_scope == "All stillbirths") %>%
        select(comparison, category, OR, CI_lower, CI_upper, p_value, significant) %>%
        as.data.frame()
)
cat("\n")

cat("Output files saved to:", output_dir, "\n")
cat("  01_sample_size_summary.csv\n")
cat("  02_distribution_by_category.csv\n")
cat("  03_distribution_by_stillbirth_timing.csv\n")
cat("  04_distribution_by_timing_and_facility.csv\n")
cat("  05_distribution_by_facility.csv\n")
cat("  06_distribution_by_year.csv\n")
cat("  07_distribution_by_facility_and_year.csv\n")
cat("  08_odds_ratios.csv  — OR by category (overall, early SB, late SB) x facility\n")
cat("  09_chi_squared_tests.csv — Overall significance of differences across categories\n")
cat("  10_pairwise_prop_tests.csv — Pairwise proportions (Holm adjusted)\n")
cat("  11_emmeans_pairwise.csv  — Estimated marginal means contrasts (Tukey adjusted)\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Analysis complete!\n")
