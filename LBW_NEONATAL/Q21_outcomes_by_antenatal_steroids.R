# Q21_outcomes_by_antenatal_steroids.R
# Analysis of Neonatal Outcomes by Antenatal Steroids Use, LBW, and SGA Categories
#
# This script:
# 1. Cleans and processes the `ansteroids` variable.
# 2. Defines 4 specific groups:
#    - Steroids Used (Y)
#    - Steroids Not Used (N)
#    - Unsure (U)
#    - Missing (NA or empty)
# 3. Categorizes Outcomes (Alive vs Neonatal Death).
# 4. Generates a WIDE format table:
#    - Rows: Steroids Group (per Facility).
#    - Columns: LBW and SGA categories.
#    - Sub-columns: NND (Deaths) and DC (Discharged/Alive).
#
# Data sources:
# - KCH Malawi: Uses `ansteroids`
# - SMCH Zimbabwe: Uses `ansteroids`

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
    library(stringr)
    library(purrr)
})

# ===== CONFIGURATION =====
# Input files
kch_file <- "LBW_KCH_NNU.csv"
smch_file <- "LBW_SMCH_NNU.csv"

# Output directory
output_dir <- "Q21_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# SHARED VALIDITY THRESHOLDS
# Must match the thresholds used in analysis_df and valid_df below.
# ---------------------------------------------------------------------------
BW_MIN_KG <- 0.3 # minimum plausible birth weight (kg) — 300 g
BW_MAX_KG <- 7.0 # maximum plausible birth weight (kg) — 7000 g
GA_MIN <- 24 # minimum gestational age for SGA classification (weeks)
GA_MAX <- 42 # maximum gestational age (weeks)

# Steroids Groups
STEROIDS_GROUPS <- c(
    "Steroids Used (Y)",
    "Steroids Not Used (N)",
    "Unsure (U)",
    "Missing"
)

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

# Normalize Antenatal Steroids
# Values: Y, N, U, or Missing
normalise_steroids <- function(x) {
    y <- toupper(trimws(as.character(x)))
    dplyr::case_when(
        y == "Y" ~ "Steroids Used (Y)",
        y == "N" ~ "Steroids Not Used (N)",
        y == "U" ~ "Unsure (U)",
        is.na(y) | y == "" | y == "NA" ~ "Missing",
        TRUE ~ "Missing" # Treat unexpected values as missing or check them
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
        steroids_raw = ansteroids
    )

# Process SMCH
smch_proc <- smch_raw %>%
    mutate(
        facility = "SMCH",
        steroids_raw = ansteroids
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

        # Steroids Categorization
        steroids_cat = normalise_steroids(steroids_raw),

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

# Filter for valid data (Valid Outcome) - Birthweight mappings managed securely within explicit matrices natively gracefully natively accurately explicitly
valid_df <- analysis_df %>%
    filter(outcome_norm %in% c("Alive", "Neonatal Death"))

cat("Total Analysis Population (Valid Outcome):", nrow(valid_df), "\n")


# ===== ANALYSIS FUNCTION =====

run_q21_analysis <- function(valid_df, analysis_df, output_dir, label) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    cat(sprintf("\n=== Running Analysis: %s ===\n", label))
    cat("Total Analysis Population (Valid Outcome):", nrow(valid_df), "\n")

    # ----- CALC STATISTICS -----
    cat("Calculating Statistics...\n")

    # Helper for group stats
    calc_group_stats <- function(data, group_name, filter_expr, group_cols, group_levels) {
        category_col <- group_cols[1]
        stats <- data %>%
            filter(eval(parse(text = filter_expr))) %>%
            filter(!is.na(.data[[category_col]])) %>%
            group_by(facility, category = .data[[category_col]]) %>%
            summarise(
                NND = sum(outcome_norm == "Neonatal Death"),
                DC = sum(outcome_norm == "Alive"),
                .groups = "drop"
            ) %>%
            mutate(steroids_group = group_name)

        totals <- data %>%
            filter(eval(parse(text = filter_expr))) %>%
            group_by(facility) %>%
            summarise(
                NND = sum(outcome_norm == "Neonatal Death"),
                DC = sum(outcome_norm == "Alive"),
                .groups = "drop"
            ) %>%
            mutate(category = "Total", steroids_group = group_name)
        bind_rows(stats, totals)
    }

    # Helper for wide outcome
    calc_wide_outcome <- function(data, group_cols, group_levels) {
        defs <- list(
            list(name = "Steroids Used (Y)", expr = "steroids_cat == 'Steroids Used (Y)'"),
            list(name = "Steroids Not Used (N)", expr = "steroids_cat == 'Steroids Not Used (N)'"),
            list(name = "Unsure (U)", expr = "steroids_cat == 'Unsure (U)'"),
            list(name = "Missing", expr = "steroids_cat == 'Missing'")
        )
        all_stats <- list()
        for (d in defs) {
            all_stats[[d$name]] <- calc_group_stats(data, d$name, d$expr, group_cols, group_levels)
        }
        summary_long <- bind_rows(all_stats)
        complete_grid <- expand_grid(
            facility = unique(data$facility),
            category = c(group_levels, "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total"),
            steroids_group = STEROIDS_GROUPS
        )
        summary_complete <- complete_grid %>%
            left_join(summary_long, by = c("facility", "category", "steroids_group")) %>%
            replace_na(list(NND = 0, DC = 0))

        summary_wide <- summary_complete %>%
            pivot_wider(
                names_from = category,
                values_from = c(NND, DC),
                names_glue = "{category}_{.value}"
            )

        col_order <- c("facility", "steroids_group")
        for (cat in c(group_levels, "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total")) {
            col_name_nnd <- paste0(cat, "_NND")
            col_name_dc <- paste0(cat, "_DC")
            if (col_name_nnd %in% names(summary_wide)) {
                col_order <- c(col_order, col_name_nnd, col_name_dc)
            }
        }
        summary_wide %>% select(any_of(col_order)) %>% arrange(facility, match(steroids_group, STEROIDS_GROUPS))
    }

    # LBW Categories
    lbw_levels <- c("ELBW", "VLBW", "LBW", "NBW", "HBW")
    lbw_wide <- calc_wide_outcome(valid_df, c("bw_category", "lbw_explanation"), lbw_levels) %>%
        mutate(classification = "Birthweight")

    # SGA Categories
    sga_levels <- c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")
    sga_wide <- calc_wide_outcome(valid_df, c("sga_category", "sga_explanation"), sga_levels) %>%
        mutate(classification = "SGA")

    # ----- SAVE RESULTS -----
    cat("Saving Results...\n")

    bw_note <- sprintf("is.finite(bw_kg) & bw_kg >= %.1f kg & bw_kg <= %.1f kg", BW_MIN_KG, BW_MAX_KG)
    ga_note <- sprintf("!is.na(gestation) & gestation >= %d wks & gestation <= %d wks", GA_MIN, GA_MAX)
    sga_note <- paste0(bw_note, " AND ", ga_note, " AND !is.na(sga_category)")
    outcome_note <- "normalise_outcome(neotreeoutcome) %in% c(\"Alive\", \"Neonatal Death\")"
    
    n_total <- nrow(analysis_df)
    sample_size_summary <- tibble(
        description = c(
            "Total admissions",
            "Analysis population (valid outcome)",
            "Excluded: missing/invalid outcome",
            "Analysis population — Steroids Used (Y)",
            "Analysis population — Steroids Not Used (N)",
            "Analysis population — Unsure (U)",
            "Analysis population — Missing steroids record",
            "Analysis population (KCH) — Steroids Used (Y)",
            "Analysis population (KCH) — Steroids Not Used (N)",
            "Analysis population (KCH) — Unsure (U)",
            "Analysis population (KCH) — Missing steroids record",
            "Analysis population (SMCH) — Steroids Used (Y)",
            "Analysis population (SMCH) — Steroids Not Used (N)",
            "Analysis population (SMCH) — Unsure (U)",
            "Analysis population (SMCH) — Missing steroids record",
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
            sum(valid_df$steroids_cat == "Steroids Used (Y)"),
            sum(valid_df$steroids_cat == "Steroids Not Used (N)"),
            sum(valid_df$steroids_cat == "Unsure (U)"),
            sum(valid_df$steroids_cat == "Missing"),
            sum(valid_df$facility == "KCH" & valid_df$steroids_cat == "Steroids Used (Y)"),
            sum(valid_df$facility == "KCH" & valid_df$steroids_cat == "Steroids Not Used (N)"),
            sum(valid_df$facility == "KCH" & valid_df$steroids_cat == "Unsure (U)"),
            sum(valid_df$facility == "KCH" & valid_df$steroids_cat == "Missing"),
            sum(valid_df$facility == "SMCH" & valid_df$steroids_cat == "Steroids Used (Y)"),
            sum(valid_df$facility == "SMCH" & valid_df$steroids_cat == "Steroids Not Used (N)"),
            sum(valid_df$facility == "SMCH" & valid_df$steroids_cat == "Unsure (U)"),
            sum(valid_df$facility == "SMCH" & valid_df$steroids_cat == "Missing"),
            sum(valid_df$is_valid_bw),
            sum(!valid_df$is_valid_bw),
            sum(valid_df$is_valid_sga),
            sum(valid_df$sga_explanation == "Missing BW"),
            sum(valid_df$sga_explanation == "Missing GA"),
            sum(valid_df$sga_explanation == "Missing both BW and GA")
        ),
        pct_of_total = round(100 * n / n_total, 1),
        Filter_notes = c(
            NA_character_, outcome_note, paste0("NOT (", outcome_note, ")"),
            paste0(outcome_note, " AND steroids == 'Y'"), paste0(outcome_note, " AND steroids == 'N'"),
            paste0(outcome_note, " AND steroids == 'U'"), paste0(outcome_note, " AND steroids is NA/blank/other"),
            paste0(outcome_note, " AND facility == 'KCH' AND steroids == 'Y'"),
            paste0(outcome_note, " AND facility == 'KCH' AND steroids == 'N'"),
            paste0(outcome_note, " AND facility == 'KCH' AND steroids == 'U'"),
            paste0(outcome_note, " AND facility == 'KCH' AND steroids is Missing"),
            paste0(outcome_note, " AND facility == 'SMCH' AND steroids == 'Y'"),
            paste0(outcome_note, " AND facility == 'SMCH' AND steroids == 'N'"),
            paste0(outcome_note, " AND facility == 'SMCH' AND steroids == 'U'"),
            paste0(outcome_note, " AND facility == 'SMCH' AND steroids is Missing"),
            paste0(outcome_note, " AND ", bw_note),
            paste0(outcome_note, " AND NOT (", bw_note, ")"),
            paste0(outcome_note, " AND ", sga_note),
            "Invalid/missing birth weight", "Invalid/missing gestational age", "Both BW and GA invalid/missing"
        )
    )

    write_csv(sample_size_summary, file.path(output_dir, "00_sample_size_summary.csv"))
    write_csv(lbw_wide, file.path(output_dir, "01_steroids_outcomes_LBW_wide.csv"))
    write_csv(sga_wide, file.path(output_dir, "02_steroids_outcomes_SGA_wide.csv"))

    # ----- CFR TESTS -----
    cat("Running CFR Chi-Square and Pairwise Tests...\n")
    
    calc_cfr_tests <- function(data, category_col, valid_categories, group_col, group_levels, group_label) {
        d <- data %>% filter(.data[[group_col]] %in% group_levels, .data[[category_col]] %in% valid_categories)
        facilities <- unique(d$facility)
        results <- list()
        for (fac in facilities) {
            for (cat in valid_categories) {
                sub <- d %>% filter(facility == fac, .data[[category_col]] == cat)
                ct_df <- sub %>% group_by(group = .data[[group_col]]) %>%
                    summarise(NND = sum(outcome_norm == "Neonatal Death"), DC  = sum(outcome_norm == "Alive"), .groups = "drop") %>%
                    filter((NND + DC) > 0)
                if (nrow(ct_df) < 2) next
                ct_mat <- as.matrix(ct_df[, c("NND", "DC")]); rownames(ct_mat) <- ct_df$group
                cfr_vals <- ct_df %>% mutate(total = NND + DC, CFR = round(100 * NND / total, 1))
                chisq_res <- tryCatch(suppressWarnings(chisq.test(ct_mat, correct = FALSE)), error = function(e) NULL)
                overall_p <- if (!is.null(chisq_res)) chisq_res$p.value else NA_real_
                overall_stat <- if (!is.null(chisq_res)) chisq_res$statistic else NA_real_
                overall_df   <- if (!is.null(chisq_res)) chisq_res$parameter else NA_real_
                expected_warn <- if (!is.null(chisq_res)) any(chisq_res$expected < 5) else NA
                grp_names <- rownames(ct_mat); pairs <- combn(grp_names, 2, simplify = FALSE); n_pairs <- length(pairs)
                pairwise_rows <- map_dfr(pairs, function(pair) {
                    g1 <- pair[1]; g2 <- pair[2]; sub_mat <- ct_mat[c(g1, g2), , drop = FALSE]
                    fish_p <- tryCatch(fisher.test(sub_mat)$p.value, error = function(e) NA_real_)
                    tibble(facility = fac, category = cat, comparison = paste0(g1, " vs. ", g2), group1 = g1, group2 = g2,
                           CFR_group1_pct = cfr_vals$CFR[cfr_vals$group == g1], n_group1 = cfr_vals$total[cfr_vals$group == g1], NND_group1 = cfr_vals$NND[cfr_vals$group == g1],
                           CFR_group2_pct = cfr_vals$CFR[cfr_vals$group == g2], n_group2 = cfr_vals$total[cfr_vals$group == g2], NND_group2 = cfr_vals$NND[cfr_vals$group == g2],
                           fisher_p_raw = fish_p, n_pairs_tested = n_pairs)
                })
                pairwise_rows <- pairwise_rows %>% mutate(
                    fisher_p_bonferroni = pmin(fisher_p_raw * n_pairs_tested, 1),
                    fisher_p_bonferroni = round(fisher_p_bonferroni, 6),
                    fisher_p_raw = round(fisher_p_raw, 6),
                    chisq_statistic = round(as.numeric(overall_stat), 4),
                    chisq_df = as.integer(overall_df),
                    chisq_p_value = round(overall_p, 6),
                    low_expected_count_warning = expected_warn,
                    exposure_variable = group_label,
                    classification = if (category_col == "bw_category") "Birthweight" else "SGA"
                )
                results[[paste(fac, cat, sep = "_")]] <- pairwise_rows
            }
        }
        bind_rows(results)
    }

    testable_steroids <- c("Steroids Used (Y)", "Steroids Not Used (N)", "Unsure (U)")
    cfr_tests_lbw <- calc_cfr_tests(valid_df, "bw_category", lbw_levels, "steroids_cat", testable_steroids, "Antenatal Steroids")
    cfr_tests_sga <- calc_cfr_tests(valid_df, "sga_category", sga_levels, "steroids_cat", testable_steroids, "Antenatal Steroids")
    cfr_tests_all <- bind_rows(cfr_tests_lbw, cfr_tests_sga) %>% arrange(facility, classification, category, comparison)

    write_csv(cfr_tests_all, file.path(output_dir, "03_steroids_CFR_chisquare_pairwise.csv"))
    cat(sprintf("Saved: 03_steroids_CFR_chisquare_pairwise.csv (Rows: %d)\n", nrow(cfr_tests_all)))
}

# ===== EXECUTE ANALYSES =====

# 1. Full Sample Analysis
run_q21_analysis(valid_df, analysis_df, "Q21_outputs", "Full Sample")

# 2. Gestation Subsample Analysis (24 to 34 weeks)
cat("\n=== Creating Gestation Subsample (24 to 34 weeks) ===\n")
sub_analysis_df <- analysis_df %>%
    filter(gestation_weeks >= 24 & gestation_weeks < 35)

sub_valid_df <- sub_analysis_df %>%
    filter(outcome_norm %in% c("Alive", "Neonatal Death"))

run_q21_analysis(sub_valid_df, sub_analysis_df, "Q21_outputs_gestation_24_34", "Gestation Subsample (24 to 34 weeks)")

cat("\nAll analyses complete!\n")
