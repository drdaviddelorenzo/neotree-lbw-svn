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
kch_maternal_file <- "LBW_KCH_MATERNAL.csv"
smch_maternal_file <- "LBW_SMCH_MATERNAL.csv"

# Output directory
output_dir <- "Q2_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS
# Must match the thresholds in maternal_descriptive_analysis.R.
# ---------------------------------------------------------------------------
BW_MIN_KG <- 0.3 # minimum plausible birth weight (kg) — 300 g
BW_MAX_KG <- 7.0 # maximum plausible birth weight (kg) — 7000 g
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

# -----------------------------------------------------------------------
# COLUMN-SPECIFIC BIRTH WEIGHT PARSERS
# -----------------------------------------------------------------------
# Confirmed formats (from raw data audit):
#
#   KCH (MWI) bwtdis      — grams, plain numeric    e.g. "2880.0", "3500.0"
#   KCH (MWI) birthweight — kg,    plain numeric    e.g. "2.77",   "1.38"   (100% <= 10)
#   KCH (MWI) bwt         — grams, comma as thousands separator e.g. "2,400", "3,520"
#   SMCH (ZIM) bwtdis     — kg,    plain numeric    e.g. "3.0",    "1.6"    (100% <= 10)
#
# Parsing is explicit and column-specific. NO median-based unit guessing.
# All parsers return values in KILOGRAMS (for compatibility with downstream
# LBW/SGA functions that operate in kg).

# KCH bwtdis: plain numeric, already in grams -> divide by 1000 to get kg
parse_kch_bwtdis_kg <- function(x) {
    suppressWarnings(as.numeric(x)) / 1000
}

# KCH birthweight: plain numeric in kg -> use directly
parse_kch_birthweight_kg <- function(x) {
    suppressWarnings(as.numeric(x)) # already kg
}

# KCH bwt: comma is thousands separator (e.g. "2,400" = 2400 g) -> strip comma, divide by 1000
parse_kch_bwt_kg <- function(x) {
    suppressWarnings(as.numeric(gsub(",", "", x))) / 1000
}

# SMCH bwtdis: plain numeric in kg -> use directly
parse_smch_bwtdis_kg <- function(x) {
    suppressWarnings(as.numeric(x)) # already kg
}

# Build composite bw_kg for KCH: bwtdis (P1) > birthweight (P2) > bwt (P3), result in kg.
# bwtdis always overwrites when present; lower-priority columns fill NA slots only.
build_kch_bw_kg <- function(df) {
    result <- rep(NA_real_, nrow(df))

    # 3rd priority — bwt (comma-thousands grams, <1% coverage)
    if ("bwt" %in% names(df)) {
        bwt_val <- parse_kch_bwt_kg(df$bwt)
        result <- ifelse(!is.na(bwt_val), bwt_val, result)
    }

    # 2nd priority — birthweight (kg, fills remaining NAs)
    if ("birthweight" %in% names(df)) {
        bw_val <- parse_kch_birthweight_kg(df$birthweight)
        result <- ifelse(!is.na(bw_val), bw_val, result)
    }

    # 1st priority — bwtdis (grams -> kg, always overwrites when present)
    if ("bwtdis" %in% names(df)) {
        bwtdis_val <- parse_kch_bwtdis_kg(df$bwtdis)
        result <- ifelse(!is.na(bwtdis_val), bwtdis_val, result)
    }

    result
}

# -----------------------------------------------------------------------

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

# IMPORTANT: Parse birthweight per facility BEFORE combining.
# Each site and column has a distinct format — column-specific parsers are used.
# KCH: composite from bwtdis (grams) > birthweight (kg) > bwt (comma-thousands grams).
# SMCH: bwtdis only, plain numeric in kg.
# All bw_kg values are in kg after this block.
cat("Parsing birthweight per facility (column-specific parsers)...\n")

# KCH: composite bw_kg from bwtdis (P1, g) > birthweight (P2, kg) > bwt (P3, comma-g)
# Result is always in kg. See parse_kch_* functions above for format details.
kch_raw <- kch_raw %>%
    mutate(
        bw_kg    = build_kch_bw_kg(.),
        facility = "KCH"
    )

n_kch_bwtdis <- sum(!is.na(suppressWarnings(as.numeric(kch_raw$bwtdis))))
n_kch_birthweight <- if ("birthweight" %in% names(kch_raw)) sum(!is.na(suppressWarnings(as.numeric(kch_raw$birthweight)))) else 0L
n_kch_bwt <- if ("bwt" %in% names(kch_raw)) sum(!is.na(suppressWarnings(as.numeric(gsub(",", "", kch_raw$bwt))))) else 0L
n_kch_composite <- sum(!is.na(kch_raw$bw_kg))
cat(sprintf("  KCH bwtdis      (P1, grams):              %d records\n", n_kch_bwtdis))
cat(sprintf("  KCH birthweight (P2, kg):                 %d records\n", n_kch_birthweight))
cat(sprintf("  KCH bwt         (P3, comma-thousands g):  %d records\n", n_kch_bwt))
cat(sprintf(
    "  KCH bw_kg composite (all in kg):          %d / %d (%.1f%%)\n",
    n_kch_composite, nrow(kch_raw), 100 * n_kch_composite / nrow(kch_raw)
))

# SMCH: bwtdis sole source, plain numeric in kg -> parse_smch_bwtdis_kg() uses directly
smch_raw <- smch_raw %>%
    mutate(
        bw_kg    = parse_smch_bwtdis_kg(bwtdis),
        facility = "SMCH"
    )

n_smch_composite <- sum(!is.na(smch_raw$bw_kg))
cat(sprintf(
    "  SMCH bwtdis (sole source, kg):            %d / %d (%.1f%%)\n",
    n_smch_composite, nrow(smch_raw), 100 * n_smch_composite / nrow(smch_raw)
))

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

        # Validations — thresholds from shared constants (match maternal_descriptive_analysis.R)
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

# Attach filter notes to BW/GA rows for transparency in the CSV output
bw_note <- sprintf(
    "is.finite(bw_kg) & bw_kg >= %.1f kg & bw_kg <= %.1f kg",
    BW_MIN_KG, BW_MAX_KG
)
ga_note <- sprintf(
    "!is.na(gestation) & gestation >= %d wks & gestation <= %d wks",
    GA_MIN, GA_MAX
)
both_note <- paste0(bw_note, " AND ", ga_note)

sample_size_df <- sample_size_df %>%
    mutate(
        Filter_notes = case_when(
            grepl("valid birthweight|LBW sample", description, ignore.case = TRUE) ~ bw_note,
            grepl("valid SGA", description, ignore.case = TRUE) ~ both_note,
            grepl("Discarded.*LBW|Missing.*invalid birthweight", description, ignore.case = TRUE) ~ paste0("NOT (", bw_note, ")"),
            grepl("Discarded.*SGA.*Missing BW", description, ignore.case = TRUE) ~ "Invalid/missing birth weight",
            grepl("Discarded.*SGA.*Missing GA", description, ignore.case = TRUE) ~ "Invalid/missing gestational age",
            grepl("Discarded.*SGA.*both", description, ignore.case = TRUE) ~ "Both BW and GA invalid/missing",
            TRUE ~ NA_character_
        )
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
