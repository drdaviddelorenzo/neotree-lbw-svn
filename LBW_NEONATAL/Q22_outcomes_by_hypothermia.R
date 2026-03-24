# Q22_outcomes_by_hypothermia.R
# Analysis of Neonatal Outcomes by Hypothermia Status, LBW, and SGA Categories
#
# This script:
# 1. Derives hypothermia status from admission temperature (facility-specific columns).
# 2. Defines 3 specific groups:
#    - Hypothermia (Yes): temperature on admission < 36.5 °C
#    - No Hypothermia (No): temperature on admission ≥ 36.5 °C
#    - Missing: no valid temperature reading (absent, non-numeric, or outside 30–45 °C)
# 3. Categorizes Outcomes (Alive vs Neonatal Death).
# 4. Generates a WIDE format table:
#    - Rows: Hypothermia Group (per Facility).
#    - Columns: LBW and SGA categories.
#    - Sub-columns: NND (Deaths) and DC (Discharged/Alive).
#
# Data sources:
# - KCH Malawi:    Uses `temperatureonarrival`
# - SMCH Zimbabwe: Uses `temperature`
#
# Hypothermia threshold (WHO): < 36.5 °C

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
    library(stringr)
    library(purrr)
})

# ===== CONFIGURATION =====
# Input files
kch_file  <- "LBW_KCH_NNU.csv"
smch_file <- "LBW_SMCH_NNU.csv"

# Output directory
output_dir <- "Q22_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS
# Must match the thresholds used in analysis_df and valid_df below.
# ---------------------------------------------------------------------------
BW_MIN_KG <- 0.3  # minimum plausible birth weight (kg) — 300 g
BW_MAX_KG <- 7.0  # maximum plausible birth weight (kg) — 7000 g
GA_MIN    <- 24   # minimum gestational age for SGA classification (weeks)
GA_MAX    <- 42   # maximum gestational age (weeks)

# Temperature validity range (same as Q15)
MIN_TEMP <- 30.0
MAX_TEMP <- 45.0

# Hypothermia threshold (WHO definition)
HYPOTHERMIA_THRESHOLD <- 36.5

# Hypothermia Groups (ordered for display)
HYPOTHERMIA_GROUPS <- c(
    "Hypothermia (Yes)",
    "No Hypothermia (No)",
    "Missing"
)

# ===== INTERGROWTH-21st 10TH PERCENTILE REFERENCE =====
# UNISEX / SEX-POOLED (14 to 42 weeks)
PCT10_UNISEX <- c(
    "14" = 76,  "15" = 98,  "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 560, "25" = 640, "26" = 740, "27" = 840, "28" = 940, "29" = 1060, "30" = 1190, "31" = 1340, "32" = 1500,
    "33" = 1580, "34" = 1780, "35" = 2000, "36" = 2220, "37" = 2440, "38" = 2650, "39" = 2850, "40" = 3010, "41" = 3150, "42" = 3260
)

# BOYS (14 to 42 weeks)
PCT10_BOYS <- c(
    "14" = 76,  "15" = 98,  "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 580, "25" = 660, "26" = 760, "27" = 870, "28" = 980, "29" = 1110, "30" = 1250, "31" = 1410, "32" = 1580,
    "33" = 1600, "34" = 1810, "35" = 2030, "36" = 2250, "37" = 2480, "38" = 2700, "39" = 2900, "40" = 3070, "41" = 3210, "42" = 3320
)

# GIRLS (14 to 42 weeks)
PCT10_GIRLS <- c(
    "14" = 76,  "15" = 98,  "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
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

    ga      <- round(gestation_weeks)
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
        gestation_weeks <  37 & sga_status == "SGA" ~ "Preterm-SGA",
        gestation_weeks <  37 & sga_status == "AGA" ~ "Preterm-AGA",
        TRUE ~ NA_character_
    )
}

bw_category_from_kg <- function(bw_kg) {
    dplyr::case_when(
        is.na(bw_kg)                               ~ NA_character_,
        bw_kg <  1.000                             ~ "ELBW",
        bw_kg >= 1.000 & bw_kg < 1.500            ~ "VLBW",
        bw_kg >= 1.500 & bw_kg < 2.500            ~ "LBW",
        bw_kg >= 2.500 & bw_kg <= 4.000           ~ "NBW",
        bw_kg >  4.000                             ~ "HBW",
        TRUE                                       ~ NA_character_
    )
}

# Normalize outcomes
normalise_outcome <- function(x) {
    y <- tolower(trimws(as.character(x)))
    y <- gsub("[^a-z0-9]+", " ", y)
    y <- gsub("\\s+", " ", y)

    dplyr::case_when(
        grepl("\\bdc\\b|discharged|\\bdcr\\b|\\bdpc\\b|\\bdckmc\\b|kangaroo mother care", y) ~ "Alive",
        grepl("\\babs\\b|abscond|\\bdama\\b|against medical advice", y)                      ~ "Alive",
        grepl("\\btrh\\b|\\btro\\b|transfer", y)                                             ~ "Alive",
        grepl("\\bnnd\\b|neonatal death|\\bdda\\b|died during admission", y)                 ~ "Neonatal Death",
        grepl("\\bbid\\b|brought in dead", y)                                                ~ "Neonatal Death",
        grepl("\\bstb\\b|stillbirth|still born", y)                                          ~ "Stillbirth",
        y == ""                                                                               ~ NA_character_,
        TRUE                                                                                  ~ "Unknown/Other"
    )
}

# Derive hypothermia group from a numeric temperature value (°C).
# Valid range: MIN_TEMP – MAX_TEMP (same quality filter as Q15).
# - Hypothermia (Yes): temp < 36.5 °C
# - No Hypothermia (No): temp >= 36.5 °C
# - Missing: temp is NA or outside plausible range
normalise_hypothermia <- function(temp_val) {
    dplyr::case_when(
        !is.finite(temp_val) | temp_val < MIN_TEMP | temp_val > MAX_TEMP ~ "Missing",
        temp_val <  HYPOTHERMIA_THRESHOLD                                  ~ "Hypothermia (Yes)",
        temp_val >= HYPOTHERMIA_THRESHOLD                                  ~ "No Hypothermia (No)",
        TRUE                                                               ~ "Missing"
    )
}

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Newborn Admission Data ===\n")
kch_raw  <- suppressMessages(read_csv(kch_file,  guess_max = 50000, col_types = cols(.default = "c")))
smch_raw <- suppressMessages(read_csv(smch_file, guess_max = 50000, col_types = cols(.default = "c")))

# Process KCH — temperature column: `temperatureonarrival`
kch_proc <- kch_raw %>%
    mutate(
        facility = "KCH",
        temp_raw = if ("temperatureonarrival" %in% names(.)) temperatureonarrival else NA_character_
    )

# Process SMCH — temperature column: `temperature`
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
        bw_raw          = get_bw_column(.),
        bw_kg           = to_kg(bw_raw),
        gestation_weeks = suppressWarnings(as.numeric(gestation)),

        # Outcome
        outcome_norm = normalise_outcome(neotreeoutcome),

        # Temperature (numeric)
        temp_val = suppressWarnings(as.numeric(temp_raw)),

        # Hypothermia Categorisation
        hypothermia_cat = normalise_hypothermia(temp_val),

        # Validations
        is_valid_bw  = is.finite(bw_kg) & bw_kg >= BW_MIN_KG & bw_kg <= BW_MAX_KG,
        is_invalid_ga = is.na(gestation_weeks) | gestation_weeks < GA_MIN | gestation_weeks > GA_MAX,
        sga_status   = dplyr::if_else(
            gestation_weeks >= GA_MIN & !is.na(gestation_weeks),
            classify_sga(bw_kg, gestation_weeks, if ("gender" %in% names(.)) gender else NA_character_),
            NA_character_
        ),
        sga_cat_temp = sga_category_from_gestational_age(sga_status, gestation_weeks),
        is_valid_sga = is_valid_bw & !is.na(sga_cat_temp),

        # Categorizations including "Discarded"
        bw_category    = if_else(is_valid_bw, bw_category_from_kg(bw_kg), "Discarded"),
        lbw_explanation = if_else(is_valid_bw, "Included in analysis", "Missing or invalid birth weight"),
        sga_explanation = case_when(
            is_valid_sga              ~ "Included in analysis",
            !is_valid_bw & is_invalid_ga ~ "Missing both BW and GA",
            !is_valid_bw              ~ "Missing BW",
            is_invalid_ga             ~ "Missing GA",
            TRUE                      ~ "Missing GA"
        ),
        sga_category = if_else(is_valid_sga, sga_cat_temp, sga_explanation)
    )

# Filter for valid outcome (Alive or Neonatal Death)
# NOTE: Records with missing temperature are RETAINED and appear in the "Missing" hypothermia group.
valid_df <- analysis_df %>%
    filter(outcome_norm %in% c("Alive", "Neonatal Death"))

cat("Total Analysis Population (Valid Outcome):", nrow(valid_df), "\n")


# ===== ANALYSIS =====

calc_group_stats <- function(data, group_name, filter_expr, group_cols, group_levels) {
    category_col <- group_cols[1]

    stats <- data %>%
        filter(eval(parse(text = filter_expr))) %>%
        filter(!is.na(.data[[category_col]])) %>%
        group_by(facility, category = .data[[category_col]]) %>%
        summarise(
            NND = sum(outcome_norm == "Neonatal Death"),
            DC  = sum(outcome_norm == "Alive"),
            .groups = "drop"
        ) %>%
        mutate(hypothermia_group = group_name)

    totals <- data %>%
        filter(eval(parse(text = filter_expr))) %>%
        group_by(facility) %>%
        summarise(
            NND = sum(outcome_norm == "Neonatal Death"),
            DC  = sum(outcome_norm == "Alive"),
            .groups = "drop"
        ) %>%
        mutate(category = "Total", hypothermia_group = group_name)

    bind_rows(stats, totals)
}

calc_wide_outcome <- function(data, group_cols, group_levels) {
    # Define group definitions
    defs <- list(
        list(name = "Hypothermia (Yes)",   expr = "hypothermia_cat == 'Hypothermia (Yes)'"),
        list(name = "No Hypothermia (No)", expr = "hypothermia_cat == 'No Hypothermia (No)'"),
        list(name = "Missing",             expr = "hypothermia_cat == 'Missing'")
    )

    all_stats <- list()
    for (d in defs) {
        all_stats[[d$name]] <- calc_group_stats(data, d$name, d$expr, group_cols, group_levels)
    }

    summary_long <- bind_rows(all_stats)

    # Complete the grid (ensure all combinations are represented, fill absent cells with 0)
    complete_grid <- expand_grid(
        facility          = unique(data$facility),
        category          = c(group_levels, "Discarded", "Missing BW", "Missing GA",
                              "Missing both BW and GA", "Total"),
        hypothermia_group = HYPOTHERMIA_GROUPS
    )

    summary_complete <- complete_grid %>%
        left_join(summary_long, by = c("facility", "category", "hypothermia_group")) %>%
        replace_na(list(NND = 0, DC = 0))

    # Pivot to Wide Format
    summary_wide <- summary_complete %>%
        pivot_wider(
            names_from  = category,
            values_from = c(NND, DC),
            names_glue  = "{category}_{.value}"
        )

    # Reorder Columns explicitly
    col_order <- c("facility", "hypothermia_group")
    for (cat in c(group_levels, "Discarded", "Missing BW", "Missing GA",
                  "Missing both BW and GA", "Total")) {
        col_name_nnd <- paste0(cat, "_NND")
        col_name_dc  <- paste0(cat, "_DC")
        if (col_name_nnd %in% names(summary_wide)) {
            col_order <- c(col_order, col_name_nnd, col_name_dc)
        }
    }

    summary_wide %>%
        select(any_of(col_order)) %>%
        arrange(facility, match(hypothermia_group, HYPOTHERMIA_GROUPS))
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
sga_note    <- paste0(bw_note, " AND ", ga_note, " AND !is.na(sga_category)")
temp_note   <- sprintf(
    "is.finite(temp_val) & temp_val >= %.1f°C & temp_val <= %.1f°C",
    MIN_TEMP, MAX_TEMP
)
outcome_note <- "normalise_outcome(neotreeoutcome) %in% c(\"Alive\", \"Neonatal Death\")"
hypoth_note  <- paste0(
    temp_note, " — Hypothermia (Yes): temp < ", HYPOTHERMIA_THRESHOLD,
    "°C; No Hypothermia (No): temp >= ", HYPOTHERMIA_THRESHOLD,
    "°C; Missing: absent/invalid temp (retained in tables)"
)

n_total <- nrow(analysis_df)

sample_size_summary <- tibble(
    description = c(
        "Total admissions",
        "Analysis population (valid outcome)",
        "Excluded: missing/invalid outcome",
        "Analysis population — Hypothermia (Yes):   temp < 36.5°C",
        "Analysis population — No Hypothermia (No): temp ≥ 36.5°C",
        "Analysis population — Missing temperature record",
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
        sum(valid_df$hypothermia_cat == "Hypothermia (Yes)"),
        sum(valid_df$hypothermia_cat == "No Hypothermia (No)"),
        sum(valid_df$hypothermia_cat == "Missing"),
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
        paste0(outcome_note, " AND ", temp_note, " AND temp < ", HYPOTHERMIA_THRESHOLD, "°C"),
        paste0(outcome_note, " AND ", temp_note, " AND temp >= ", HYPOTHERMIA_THRESHOLD, "°C"),
        paste0(outcome_note, " AND temp is NA/out-of-range — retained as \"Missing\" in tables"),
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

write_csv(lbw_wide, file.path(output_dir, "01_hypothermia_outcomes_LBW_wide.csv"))
cat("Saved: 01_hypothermia_outcomes_LBW_wide.csv\n")

write_csv(sga_wide, file.path(output_dir, "02_hypothermia_outcomes_SGA_wide.csv"))
cat("Saved: 02_hypothermia_outcomes_SGA_wide.csv\n")

# ===== CFR CHI-SQUARE AND PAIRWISE TESTS =====
# For each facility × category combination (LBW and SGA),
# compare the Case Fatality Rate (CFR = NND / (NND + DC)) across
# hypothermia groups using:
#   1. Overall Pearson chi-square test (H0: CFR is equal across all groups)
#   2. Pairwise Fisher's exact tests with Bonferroni correction
# The "Missing" temperature group is EXCLUDED from statistical tests
# (as it is not a meaningful clinical exposure group).
# Non-analytic rows (Discarded, Missing BW/GA) are also excluded.

calc_cfr_tests <- function(data,
                           category_col,      # "bw_category" or "sga_category"
                           valid_categories,  # only test these category values
                           group_col,         # "hypothermia_cat"
                           group_levels,      # ordered levels to test
                           group_label) {     # label for output column

    # Restrict to testable groups and valid categories
    d <- data %>%
        filter(
            .data[[group_col]] %in% group_levels,
            .data[[category_col]] %in% valid_categories
        )

    facilities <- unique(d$facility)
    categories <- valid_categories

    results <- list()

    for (fac in facilities) {
        for (cat in categories) {
            sub <- d %>%
                filter(facility == fac, .data[[category_col]] == cat)

            # Build a named matrix: rows = hypothermia groups, cols = NND / DC
            ct_df <- sub %>%
                group_by(group = .data[[group_col]]) %>%
                summarise(
                    NND = sum(outcome_norm == "Neonatal Death"),
                    DC  = sum(outcome_norm == "Alive"),
                    .groups = "drop"
                ) %>%
                filter((NND + DC) > 0)  # drop empty groups

            if (nrow(ct_df) < 2) next  # need at least 2 groups

            ct_mat <- as.matrix(ct_df[, c("NND", "DC")])
            rownames(ct_mat) <- ct_df$group

            # CFR per group
            cfr_vals <- ct_df %>%
                mutate(
                    total = NND + DC,
                    CFR   = round(100 * NND / total, 1)
                ) %>%
                select(group, NND, DC, total, CFR)

            # --- Overall chi-square test ---
            chisq_res <- tryCatch(
                suppressWarnings(chisq.test(ct_mat, correct = FALSE)),
                error = function(e) NULL
            )

            overall_p    <- if (!is.null(chisq_res)) chisq_res$p.value    else NA_real_
            overall_stat <- if (!is.null(chisq_res)) chisq_res$statistic  else NA_real_
            overall_df   <- if (!is.null(chisq_res)) chisq_res$parameter  else NA_real_

            # Flag low-expected-count warning
            expected_warn <- if (!is.null(chisq_res)) {
                any(chisq_res$expected < 5)
            } else NA

            # --- Pairwise Fisher's exact tests (Bonferroni-corrected) ---
            grp_names <- rownames(ct_mat)
            pairs     <- combn(grp_names, 2, simplify = FALSE)
            n_pairs   <- length(pairs)

            pairwise_rows <- map_dfr(pairs, function(pair) {
                g1 <- pair[1]; g2 <- pair[2]
                sub_mat <- ct_mat[c(g1, g2), , drop = FALSE]
                fish_p <- tryCatch(
                    fisher.test(sub_mat)$p.value,
                    error = function(e) NA_real_
                )
                tibble(
                    facility        = fac,
                    category        = cat,
                    comparison      = paste0(g1, " vs. ", g2),
                    group1          = g1,
                    group2          = g2,
                    CFR_group1_pct  = cfr_vals$CFR[cfr_vals$group == g1],
                    n_group1        = cfr_vals$total[cfr_vals$group == g1],
                    NND_group1      = cfr_vals$NND[cfr_vals$group == g1],
                    CFR_group2_pct  = cfr_vals$CFR[cfr_vals$group == g2],
                    n_group2        = cfr_vals$total[cfr_vals$group == g2],
                    NND_group2      = cfr_vals$NND[cfr_vals$group == g2],
                    fisher_p_raw    = fish_p,
                    n_pairs_tested  = n_pairs
                )
            })

            # Apply Bonferroni correction per facility × category block
            pairwise_rows <- pairwise_rows %>%
                mutate(
                    fisher_p_bonferroni = pmin(fisher_p_raw * n_pairs_tested, 1),
                    fisher_p_bonferroni = round(fisher_p_bonferroni, 6),
                    fisher_p_raw        = round(fisher_p_raw, 6),
                    chisq_statistic     = round(as.numeric(overall_stat), 4),
                    chisq_df            = as.integer(overall_df),
                    chisq_p_value       = round(overall_p, 6),
                    low_expected_count_warning = expected_warn,
                    exposure_variable   = group_label,
                    classification      = if (category_col == "bw_category") "Birthweight" else "SGA"
                ) %>%
                select(
                    facility, classification, category, exposure_variable,
                    comparison, group1, group2,
                    n_group1, NND_group1, CFR_group1_pct,
                    n_group2, NND_group2, CFR_group2_pct,
                    chisq_statistic, chisq_df, chisq_p_value,
                    fisher_p_raw, n_pairs_tested, fisher_p_bonferroni,
                    low_expected_count_warning
                )

            results[[paste(fac, cat, sep = "_")]] <- pairwise_rows
        }
    }

    bind_rows(results)
}

cat("\n=== CFR Chi-Square and Pairwise Tests ===\n")

# Groups to include (exclude "Missing" from statistical comparisons)
testable_hypothermia <- c("Hypothermia (Yes)", "No Hypothermia (No)")

cfr_tests_lbw <- calc_cfr_tests(
    data             = valid_df,
    category_col     = "bw_category",
    valid_categories = lbw_levels,
    group_col        = "hypothermia_cat",
    group_levels     = testable_hypothermia,
    group_label      = "Hypothermia Status"
)

cfr_tests_sga <- calc_cfr_tests(
    data             = valid_df,
    category_col     = "sga_category",
    valid_categories = sga_levels,
    group_col        = "hypothermia_cat",
    group_levels     = testable_hypothermia,
    group_label      = "Hypothermia Status"
)

cfr_tests_all <- bind_rows(cfr_tests_lbw, cfr_tests_sga) %>%
    arrange(facility, classification, category, comparison)

write_csv(cfr_tests_all, file.path(output_dir, "03_hypothermia_CFR_chisquare_pairwise.csv"))
cat("Saved: 03_hypothermia_CFR_chisquare_pairwise.csv\n")
cat("  Rows written:", nrow(cfr_tests_all), "\n")

cat("\nAnalysis complete!\n")
