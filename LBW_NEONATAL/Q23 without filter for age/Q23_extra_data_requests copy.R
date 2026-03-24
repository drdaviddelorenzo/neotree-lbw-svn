# Q23_extra_data_requests.R
# Extra Data Requests (Q23_Extra_data_requests_David.docx)
#
# This script answers the following data requests from the Q23 docx table,
# stratified by facility (SMCH = Zimbabwe, KCH = Malawi):
#
#   1. Total admissions to neonatal unit  — raw count from the working sample CSV files
#   2. Inborn (%)                         — inorout variable (TRUE = inborn)
#   3. Mean age on admission (hours)      — `age` column (median also reported)
#   4. Female (%)                         — gender / sexdis columns
#   5. HIV positive (%)                   — mathivtest TRUE & hivtestresult == "R"
#   6. Total SVN group (%)                — records with valid SGA classification,
#                                           expressed as % of total *unfiltered*
#                                           neonatal admissions (provided externally)
#
# External admission totals (provided by the user; unfiltered raw admissions):
#   SMCH: 22826 total admissions (20541 matched pairs + 2285 unmatched)
#   KCH : 11797 total admissions (10575 matched pairs + 1222 unmatched)
#
# Data sources (working sample — already USS-filtered):
#   - LBW_KCH_NNU.csv   (KCH Malawi)
#   - LBW_SMCH_NNU.csv  (SMCH Zimbabwe)

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
})

# ===== CONFIGURATION =====
kch_file  <- "LBW_KCH_NNU.csv"
smch_file <- "LBW_SMCH_NNU.csv"

output_dir <- "Q23_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS  (must match other Q-scripts)
# ---------------------------------------------------------------------------
BW_MIN_KG <- 0.3
BW_MAX_KG <- 7.0
GA_MIN    <- 24
GA_MAX    <- 42

# ---------------------------------------------------------------------------
# EXTERNAL ADMISSION TOTALS  (provided by the user; unfiltered)
# Used only for the "Total SVN group (%)" row.
# ---------------------------------------------------------------------------
SMCH_TOTAL_ADMISSIONS     <- 22826
SMCH_MATCHED_ADMISSIONS   <- 20541
SMCH_UNMATCHED_ADMISSIONS <- 2285

KCH_TOTAL_ADMISSIONS      <- 11797
KCH_MATCHED_ADMISSIONS    <- 10575
KCH_UNMATCHED_ADMISSIONS  <- 1222

# ===== INTERGROWTH-21st 10TH PERCENTILE REFERENCE (unisex, boys, girls) =====

PCT10_UNISEX <- c(
    "14" = 76,  "15" = 98,  "16" = 125, "17" = 158, "18" = 198, "19" = 244,
    "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 560, "25" = 640, "26" = 740, "27" = 840, "28" = 940, "29" = 1060,
    "30" = 1190,"31" = 1340,"32" = 1500,"33" = 1580,"34" = 1780,"35" = 2000,
    "36" = 2220,"37" = 2440,"38" = 2650,"39" = 2850,"40" = 3010,"41" = 3150,
    "42" = 3260
)

PCT10_BOYS <- c(
    "14" = 76,  "15" = 98,  "16" = 125, "17" = 158, "18" = 198, "19" = 244,
    "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 580, "25" = 660, "26" = 760, "27" = 870, "28" = 980, "29" = 1110,
    "30" = 1250,"31" = 1410,"32" = 1580,"33" = 1600,"34" = 1810,"35" = 2030,
    "36" = 2250,"37" = 2480,"38" = 2700,"39" = 2900,"40" = 3070,"41" = 3210,
    "42" = 3320
)

PCT10_GIRLS <- c(
    "14" = 76,  "15" = 98,  "16" = 125, "17" = 158, "18" = 198, "19" = 244,
    "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 540, "25" = 620, "26" = 720, "27" = 810, "28" = 910, "29" = 1030,
    "30" = 1150,"31" = 1300,"32" = 1460,"33" = 1540,"34" = 1750,"35" = 1960,
    "36" = 2180,"37" = 2400,"38" = 2610,"39" = 2800,"40" = 2960,"41" = 3090,
    "42" = 3190
)

# ===== HELPER FUNCTIONS =====

# Convert birthweight to kg (auto-detects grams vs kg)
to_kg <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    pos   <- x_num[is.finite(x_num) & x_num > 0]
    if (length(pos) > 0) {
        med <- median(pos, na.rm = TRUE)
        if (is.finite(med) && med > 20 && med <= 7000) {
            return(x_num / 1000) # grams -> kg
        }
    }
    x_num
}

# 10th percentile birthweight for GA (sex-aware)
get_10th_percentile_bw <- function(gestation_weeks, gender_val = NA_character_) {
    if (is.na(gestation_weeks)) return(NA_real_)
    ga      <- round(gestation_weeks)
    ga_char <- as.character(ga)
    if (ga < 14 || ga > 42) return(NA_real_)
    gender_clean <- toupper(trimws(gender_val))
    if (!is.na(gender_clean) && gender_clean %in% c("M", "MALE", "BOY")) {
        pct <- PCT10_BOYS[ga_char]
    } else if (!is.na(gender_clean) && gender_clean %in% c("F", "FEMALE", "GIRL")) {
        pct <- PCT10_GIRLS[ga_char]
    } else {
        pct <- PCT10_UNISEX[ga_char]
    }
    if (is.na(pct)) return(NA_real_)
    as.numeric(pct) / 1000
}

# SGA classification (SGA / AGA)
classify_sga <- function(bw_kg, gestation_weeks, gender_val = NA_character_) {
    pct10 <- mapply(get_10th_percentile_bw, gestation_weeks, gender_val, USE.NAMES = FALSE)
    ifelse(is.na(bw_kg) | is.na(pct10), NA_character_,
           ifelse(bw_kg < pct10, "SGA", "AGA"))
}

# SGA category (Term/Preterm × SGA/AGA)
sga_category_from_gestational_age <- function(sga_status, gestation_weeks) {
    dplyr::case_when(
        is.na(sga_status) | is.na(gestation_weeks) ~ NA_character_,
        gestation_weeks >= 37 & sga_status == "SGA" ~ "Term-SGA",
        gestation_weeks >= 37 & sga_status == "AGA" ~ "Term-AGA",
        gestation_weeks <  37 & sga_status == "SGA" ~ "Preterm-SGA",
        gestation_weeks <  37 & sga_status == "AGA" ~ "Preterm-AGA",
        TRUE ~ NA_character_
    )
}

# Normalise boolean columns stored as "TRUE"/"FALSE" strings
norm_bool <- function(x) {
    x <- toupper(trimws(as.character(x)))
    dplyr::case_when(
        x %in% c("TRUE", "T")  ~ TRUE,
        x %in% c("FALSE", "F") ~ FALSE,
        TRUE ~ NA
    )
}

# ===== LOAD DATA =====

cat("\n=== Loading Newborn Admission Data ===\n")
kch_raw  <- suppressMessages(read_csv(kch_file,  guess_max = 50000, col_types = cols(.default = "c")))
smch_raw <- suppressMessages(read_csv(smch_file, guess_max = 50000, col_types = cols(.default = "c")))
cat("KCH records loaded :", nrow(kch_raw),  "\n")
cat("SMCH records loaded:", nrow(smch_raw), "\n")

kch_raw$facility  <- "KCH"
smch_raw$facility <- "SMCH"

all_admissions <- bind_rows(kch_raw, smch_raw)
cat("Total records      :", nrow(all_admissions), "\n")

# ===== PROCESS VARIABLES =====

cat("\nProcessing variables...\n")

analysis_df <- all_admissions %>%
    mutate(
        # --- Birthweight ---
        bw_raw        = if ("birthweight" %in% names(.)) birthweight else NA_character_,
        bw_kg         = to_kg(bw_raw),
        is_valid_bw   = is.finite(bw_kg) & bw_kg >= BW_MIN_KG & bw_kg <= BW_MAX_KG,

        # --- Gestational age ---
        gestation_weeks = suppressWarnings(as.numeric(gestation)),
        is_invalid_ga   = is.na(gestation_weeks) | gestation_weeks < GA_MIN | gestation_weeks > GA_MAX,

        # --- Gender (use `gender` for KCH; `sexdis` as fallback for SMCH) ---
        gender_std = dplyr::coalesce(
            if ("gender" %in% names(.)) gender else NA_character_,
            if ("sexdis" %in% names(.)) sexdis else NA_character_
        ),

        # --- Inborn/Outborn ---
        is_inborn    = norm_bool(inorout),
        inorout_cat  = dplyr::case_when(
            is_inborn == TRUE  ~ "Inborn",
            is_inborn == FALSE ~ "Outborn",
            TRUE               ~ "Unknown"
        ),

        # --- Age on admission (hours) ---
        # `age` column — units are hours in both KCH and SMCH CSV files
        age_hours = suppressWarnings(as.numeric(age)),

        # --- Maternal HIV ---
        # Tested positive: mathivtest == TRUE AND hivtestresult == "R" (Reactive)
        hiv_tested   = norm_bool(mathivtest),
        hiv_result   = toupper(trimws(as.character(hivtestresult))),
        hiv_positive = hiv_tested == TRUE & hiv_result == "R",

        # --- SGA / SVN ---
        sga_status = dplyr::if_else(
            gestation_weeks >= GA_MIN & !is.na(gestation_weeks),
            classify_sga(bw_kg, gestation_weeks,
                         if ("gender" %in% names(.)) gender else NA_character_),
            NA_character_
        ),
        sga_cat_temp = sga_category_from_gestational_age(sga_status, gestation_weeks),
        is_valid_sga = is_valid_bw & !is.na(sga_cat_temp)
    )

cat("\nData processing complete.\n")

# ===== ANALYSIS PER FACILITY =====

cat("\n=== Computing summary statistics by facility ===\n")

facility_stats <- analysis_df %>%
    group_by(facility) %>%
    summarise(
        # 1. Total NNU admissions in working sample
        n_working_sample = n(),

        # 2. Inborn
        n_inborn           = sum(inorout_cat == "Inborn",  na.rm = TRUE),
        n_inorout_known    = sum(inorout_cat != "Unknown",  na.rm = TRUE),
        pct_inborn_of_known = round(100 * n_inborn / n_inorout_known, 1),
        pct_inborn_of_total = round(100 * n_inborn / n_working_sample, 1),
        n_inorout_unknown  = sum(inorout_cat == "Unknown",  na.rm = TRUE),

        # 3. Age on admission
        n_age_valid        = sum(is.finite(age_hours), na.rm = TRUE),
        mean_age_hours     = round(mean(age_hours, na.rm = TRUE), 1),
        median_age_hours   = round(median(age_hours, na.rm = TRUE), 1),
        sd_age_hours       = round(sd(age_hours,   na.rm = TRUE), 1),
        n_age_missing      = sum(is.na(age_hours)),

        # 4. Female
        n_gender_known = sum(!is.na(gender_std) & toupper(trimws(gender_std)) != "", na.rm = TRUE),
        n_female       = sum(toupper(trimws(gender_std)) %in% c("F", "FEMALE", "GIRL"), na.rm = TRUE),
        pct_female_of_known = round(100 * n_female / n_gender_known, 1),
        pct_female_of_total = round(100 * n_female / n_working_sample, 1),
        n_gender_missing = sum(is.na(gender_std) | toupper(trimws(gender_std)) == ""),

        # 5. HIV positive (mathivtest TRUE & hivtestresult Reactive)
        n_tested           = sum(hiv_tested == TRUE, na.rm = TRUE),
        n_hiv_positive     = sum(hiv_positive == TRUE, na.rm = TRUE),
        pct_hiv_pos_of_tested = round(100 * n_hiv_positive / n_tested, 1),
        pct_hiv_pos_of_total  = round(100 * n_hiv_positive / n_working_sample, 1),
        n_hiv_not_tested   = sum(hiv_tested == FALSE, na.rm = TRUE),
        n_hiv_test_missing = sum(is.na(hiv_tested)),

        # 6. SVN group (valid SGA classification from working sample)
        n_svn = sum(is_valid_sga, na.rm = TRUE),

        .groups = "drop"
    ) %>%
    # Add external total admissions and compute SVN %
    mutate(
        external_total_admissions = dplyr::case_when(
            facility == "SMCH" ~ as.integer(SMCH_TOTAL_ADMISSIONS),
            facility == "KCH"  ~ as.integer(KCH_TOTAL_ADMISSIONS),
            TRUE               ~ NA_integer_
        ),
        external_matched_pairs = dplyr::case_when(
            facility == "SMCH" ~ as.integer(SMCH_MATCHED_ADMISSIONS),
            facility == "KCH"  ~ as.integer(KCH_MATCHED_ADMISSIONS),
            TRUE               ~ NA_integer_
        ),
        external_unmatched = dplyr::case_when(
            facility == "SMCH" ~ as.integer(SMCH_UNMATCHED_ADMISSIONS),
            facility == "KCH"  ~ as.integer(KCH_UNMATCHED_ADMISSIONS),
            TRUE               ~ NA_integer_
        ),
        pct_svn_of_working_sample       = round(100 * n_svn / n_working_sample, 1),
        pct_svn_of_external_total       = round(100 * n_svn / external_total_admissions, 1),
        pct_svn_of_external_matched     = round(100 * n_svn / external_matched_pairs, 1)
    )

print(facility_stats)

# ===== SUMMARY TABLE (mirrors docx layout) =====

cat("\n=== Summary Table (matching Q23 docx layout) ===\n")

# Helper to format "N (X%)" strings
fmt <- function(n, pct) sprintf("%d (%.1f%%)", n, pct)

summary_table <- facility_stats %>%
    mutate(
        # Row 1: Total admissions (working sample CSV)
        row_total_admissions     = as.character(n_working_sample),

        # Row 2: Inborn
        row_inborn               = fmt(n_inborn, pct_inborn_of_known),
        row_inborn_note          = sprintf(
            "%d inborn of %d with known status (%d unknown/missing)",
            n_inborn, n_inorout_known, n_inorout_unknown),

        # Row 3: Mean (and median) age on admission
        row_mean_age             = sprintf("%.1f hours (median: %.1f h)", mean_age_hours, median_age_hours),
        row_age_note             = sprintf("n valid = %d; n missing = %d", n_age_valid, n_age_missing),

        # Row 4: Female
        row_female               = fmt(n_female, pct_female_of_known),
        row_female_note          = sprintf(
            "%d female of %d with known sex (%d missing)",
            n_female, n_gender_known, n_gender_missing),

        # Row 5: HIV positive
        row_hiv_pos              = sprintf(
            "%s of those tested; %s of all admissions",
            sprintf("%.1f%%", pct_hiv_pos_of_tested),
            sprintf("%.1f%%", pct_hiv_pos_of_total)),
        row_hiv_note             = sprintf(
            "%d reactive of %d tested (not tested: %d; missing: %d)",
            n_hiv_positive, n_tested, n_hiv_not_tested, n_hiv_test_missing),

        # Row 6: SVN group
        row_svn                  = sprintf(
            "%d (%.1f%% of working sample; %.1f%% of total external admissions)",
            n_svn, pct_svn_of_working_sample, pct_svn_of_external_total),
        row_svn_note             = sprintf(
            "External total: %d (matched: %d + unmatched: %d)",
            external_total_admissions, external_matched_pairs, external_unmatched)
    ) %>%
    select(
        facility,
        row_total_admissions,
        row_inborn, row_inborn_note,
        row_mean_age, row_age_note,
        row_female, row_female_note,
        row_hiv_pos, row_hiv_note,
        row_svn, row_svn_note
    )

# Print long format for readability
for (fac in summary_table$facility) {
    row <- summary_table %>% filter(facility == fac)
    cat("\n----------------------------------------------------------------------\n")
    cat("Facility:", fac, "\n")
    cat("----------------------------------------------------------------------\n")
    cat("1. Total NNU admissions (working sample):", row$row_total_admissions, "\n")
    cat("2. Inborn:", row$row_inborn, "\n")
    cat("   Note:", row$row_inborn_note, "\n")
    cat("3. Mean age on admission:", row$row_mean_age, "\n")
    cat("   Note:", row$row_age_note, "\n")
    cat("4. Female:", row$row_female, "\n")
    cat("   Note:", row$row_female_note, "\n")
    cat("5. HIV positive:", row$row_hiv_pos, "\n")
    cat("   Note:", row$row_hiv_note, "\n")
    cat("6. Total SVN group:", row$row_svn, "\n")
    cat("   Note:", row$row_svn_note, "\n")
}
cat("\n----------------------------------------------------------------------\n\n")

# ===== SAVE OUTPUTS =====

cat("\n=== Saving Results ===\n")

# 1. Full numeric summary by facility
write_csv(facility_stats, file.path(output_dir, "01_facility_summary_numeric.csv"))
cat("Saved: 01_facility_summary_numeric.csv\n")

# 2. Formatted summary table (docx layout)
write_csv(summary_table, file.path(output_dir, "02_facility_summary_formatted.csv"))
cat("Saved: 02_facility_summary_formatted.csv\n")

# 3. Sample size / notes table
external_totals <- tibble(
    variable              = c(
        "External total admissions - SMCH",
        "External matched pairs - SMCH",
        "External unmatched admissions - SMCH",
        "External total admissions - KCH",
        "External matched pairs - KCH",
        "External unmatched admissions - KCH"
    ),
    value = c(
        SMCH_TOTAL_ADMISSIONS, SMCH_MATCHED_ADMISSIONS, SMCH_UNMATCHED_ADMISSIONS,
        KCH_TOTAL_ADMISSIONS,  KCH_MATCHED_ADMISSIONS,  KCH_UNMATCHED_ADMISSIONS
    ),
    source = "Provided by user (unfiltered neonatal admissions for the studied period)"
)
write_csv(external_totals, file.path(output_dir, "03_external_admission_totals.csv"))
cat("Saved: 03_external_admission_totals.csv\n")

cat("\nAnalysis complete!\n")
