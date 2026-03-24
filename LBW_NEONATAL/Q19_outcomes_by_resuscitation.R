# Q19_outcomes_by_resuscitation.R
# Analysis of Neonatal Outcomes by Resuscitation, LBW, and SGA Categories
#
# This script:
# 1. Cleans and processes Resuscitation (variable `resus`).
# 2. Maps complex combinations to the highest level of intervention:
#    Priority Order (Highest to Lowest):
#    1. Cardiopulmonary resuscitation (CPR) -> Any mention of "CPR"
#    2. Bag Valve Mask (BVM) -> Any mention of "BVM" or "Bag" (if not CPR)
#    3. Stimulant and/or Oxygen -> Any "O2", "Oxygen", "Stim", "Suction" (if not BVM/CPR)
#    4. None -> "None", "N", "No", "Norm"
#    5. Unknown -> Everything else (including NA)
#
# 3. Categorizes Outcomes (Alive vs Neonatal Death).
# 4. Generates a WIDE format table:
#    - Rows: Resuscitation Category (per Facility).
#    - Columns: LBW and SGA categories.
#    - Sub-columns: NND (Deaths) and DC (Discharged/Alive).
#
# Data sources:
# - KCH Malawi: Uses `resus`
# - SMCH Zimbabwe: Uses `resus`

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
    library(stringr)
})

# ===== CONFIGURATION =====
# Input files
kch_file <- "LBW_KCH_NNU.csv"
smch_file <- "LBW_SMCH_NNU.csv"

# Output directory
output_dir <- "Q19_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS
# Must match the thresholds used in analysis_df and valid_df below.
# ---------------------------------------------------------------------------
BW_MIN_KG <- 0.3 # minimum plausible birth weight (kg) — 300 g
BW_MAX_KG <- 7.0 # maximum plausible birth weight (kg) — 7000 g
GA_MIN <- 24 # minimum gestational age for SGA classification (weeks)
GA_MAX <- 42 # maximum gestational age (weeks)

# Resuscitation Categories
RESUS_LEVELS <- c(
    "Cardiopulmonary Resuscitation",
    "Bag Valve Mask",
    "Stimulation/Oxygen",
    "None",
    "Unknown"
)

map_resus <- function(x) {
    # Normalize string
    y <- toupper(trimws(as.character(x)))
    # Remove braces for set notation {O2, BVM}
    y <- gsub("[{}]", "", y)

    dplyr::case_when(
        # 1. CPR (Highest Priority)
        grepl("CPR", y) | grepl("CARDIOPULMONARY", y) ~ "Cardiopulmonary Resuscitation",

        # 2. Bag Valve Mask
        grepl("BVM", y) | grepl("BAG", y) | grepl("AMBU", y) ~ "Bag Valve Mask",

        # 3. Stimulation / Oxygen / Suction
        grepl("O2", y) | grepl("OXYGEN", y) | grepl("STIM", y) | grepl("SUC", y) ~ "Stimulation/Oxygen",

        # 4. None
        y %in% c("NONE", "N", "NO", "NORM") | grepl("NOTHING", y) ~ "None",

        # 5. Unknown (NA or Unrecognized)
        is.na(y) | y == "" | y == "NA" | y == "UNK" | y == "?" ~ "Unknown",

        # Default
        TRUE ~ "Unknown"
    )
}

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

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Newborn Admission Data ===\n")
kch_raw <- suppressMessages(read_csv(kch_file, guess_max = 50000, col_types = cols(.default = "c")))
smch_raw <- suppressMessages(read_csv(smch_file, guess_max = 50000, col_types = cols(.default = "c")))

# Process KCH
kch_proc <- kch_raw %>%
    mutate(
        facility = "KCH",
        resus_raw = if ("resus" %in% names(.)) resus else NA_character_
    )

# Process SMCH
smch_proc <- smch_raw %>%
    mutate(
        facility = "SMCH",
        resus_raw = if ("resus" %in% names(.)) resus else NA_character_
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

        # Resus Classification
        resus_desc = map_resus(as.character(resus_raw)),

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

# Filter for valid data (Valid Outcome) - Birthweight mapped cleanly over subsets natively
valid_df <- analysis_df %>%
    filter(outcome_norm %in% c("Alive", "Neonatal Death")) %>%
    mutate(
        # Order interval factor
        resus_category = factor(resus_desc, levels = RESUS_LEVELS)
    )

cat("Total Analysis Population (Valid Outcome):", nrow(valid_df), "\n")


# ===== ANALYSIS =====

calc_wide_outcome <- function(data, group_cols, group_levels) {
    category_col <- group_cols[1]

    # 1. Summarize Counts
    summary_long <- data %>%
        filter(!is.na(.data[[category_col]])) %>%
        group_by(facility, category = .data[[category_col]], resus_category) %>%
        summarise(
            NND = sum(outcome_norm == "Neonatal Death"),
            DC = sum(outcome_norm == "Alive"),
            .groups = "drop"
        )

    # Generate Totals (regardless of group)
    totals_long <- data %>%
        group_by(facility, resus_category) %>%
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
        resus_category = RESUS_LEVELS
    )

    summary_complete <- complete_grid %>%
        left_join(summary_long, by = c("facility", "category", "resus_category")) %>%
        replace_na(list(NND = 0, DC = 0))

    # 3. Pivot to Wide Format
    summary_wide <- summary_complete %>%
        pivot_wider(
            names_from = category,
            values_from = c(NND, DC),
            names_glue = "{category}_{.value}"
        )

    # 4. Reorder Columns explicitly ensuring everything mapped exists
    col_order <- c("facility", "resus_category")
    for (cat in c(group_levels, "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total")) {
        col_name_nnd <- paste0(cat, "_NND")
        col_name_dc <- paste0(cat, "_DC")
        if (col_name_nnd %in% names(summary_wide)) {
            col_order <- c(col_order, col_name_nnd, col_name_dc)
        }
    }

    summary_wide %>% select(any_of(col_order))
}

cat("\n=== Calculating Statistics ===\n")

# LBW Categories
lbw_levels <- c("ELBW", "VLBW", "LBW", "NBW", "HBW")
lbw_wide <- calc_wide_outcome(valid_df, c("bw_category", "lbw_explanation"), lbw_levels) %>%
    mutate(classification = "Birthweight")

# SGA Categories
sga_levels <- c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")
sga_wide <- calc_wide_outcome(valid_df, c("sga_category", "sga_explanation"), sga_levels) %>%
    mutate(classification = "SGA")

# ===== SAVE =====

cat("\n=== Saving Results ===\n")

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
outcome_note <- "normalise_outcome(neotreeoutcome) %in% c(\"Alive\", \"Neonatal Death\")"
resus_note <- paste0(
    "map_resus(resus): priority CPR > BVM > Stim/O2 > None; ",
    "NA/blank/unrecognised -> \"Unknown\""
)

n_total <- nrow(analysis_df)

sample_size_summary <- tibble(
    description = c(
        "Total admissions",
        "Analysis population (valid outcome)",
        "Excluded: missing/invalid outcome",
        "Analysis population — Cardiopulmonary Resuscitation",
        "Analysis population — Bag Valve Mask",
        "Analysis population — Stimulation/Oxygen",
        "Analysis population — None",
        "Analysis population — Unknown resuscitation",
        "Analysis population with valid birthweight (LBW sample)",
        "Discarded for LBW (missing/invalid birthweight)",
        "Analysis population with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        n_total,
        nrow(valid_df),
        sum(!analysis_df$outcome_norm %in% c("Alive", "Neonatal Death")),
        sum(valid_df$resus_desc == "Cardiopulmonary Resuscitation"),
        sum(valid_df$resus_desc == "Bag Valve Mask"),
        sum(valid_df$resus_desc == "Stimulation/Oxygen"),
        sum(valid_df$resus_desc == "None"),
        sum(valid_df$resus_desc == "Unknown"),
        sum(valid_df$is_valid_bw),
        sum(!valid_df$is_valid_bw),
        sum(valid_df$is_valid_sga),
        sum(valid_df$sga_explanation == "Missing BW"),
        sum(valid_df$sga_explanation == "Missing GA"),
        sum(valid_df$sga_explanation == "Missing both BW and GA")
    ),
    pct_of_total = round(100 * n / n_total, 1),
    Filter_notes = c(
        NA_character_,
        outcome_note,
        paste0("NOT (", outcome_note, ")"),
        paste0(outcome_note, " AND grepl(\"CPR|CARDIOPULMONARY\", toupper(resus))"),
        paste0(outcome_note, " AND grepl(\"BVM|BAG|AMBU\", toupper(resus)) — not CPR"),
        paste0(outcome_note, " AND grepl(\"O2|OXYGEN|STIM|SUC\", toupper(resus)) — not BVM/CPR"),
        paste0(outcome_note, " AND toupper(trimws(resus)) %in% c(\"NONE\",\"N\",\"NO\",\"NORM\") or grepl(\"NOTHING\")"),
        paste0(outcome_note, " AND resus is NA/blank/unrecognised — retained as \"Unknown\" in tables"),
        paste0(outcome_note, " AND ", bw_note),
        paste0(outcome_note, " AND NOT (", bw_note, ")"),
        paste0(outcome_note, " AND ", sga_note),
        "Invalid/missing birth weight",
        "Invalid/missing gestational age",
        "Both BW and GA invalid/missing"
    )
)

write_csv(sample_size_summary, file.path(output_dir, "00_sample_size_summary.csv"))
cat("Saved: 00_sample_size_summary.csv\n")

write_csv(lbw_wide, file.path(output_dir, "01_resus_outcomes_LBW_wide.csv"))
cat("Saved: 01_resus_outcomes_LBW_wide.csv\n")

write_csv(sga_wide, file.path(output_dir, "02_resus_outcomes_SGA_wide.csv"))
cat("Saved: 02_resus_outcomes_SGA_wide.csv\n")

cat("\nAnalysis complete!\n")
