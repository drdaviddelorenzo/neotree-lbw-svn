# Q6_seasonal_distribution.R
# Seasonal Distribution of Births by LBW and SGA Categories
#
# Question: Does the season when the baby is born affect the probability of
# belonging to one of the LBW or SGA categories?
#
# 5-season classification (Southern African climate):
#   Warm and wet   : December – March  (months 12, 1, 2, 3)
#   Cool and wet   : April – May       (months 4, 5)
#   Cool and dry   : June – August     (months 6, 7, 8)
#   Warm and dry   : September         (month  9)
#   Hot and dry    : October – November (months 10, 11)
#
# Data sources:
#   KCH  Malawi maternal outcomes (all births 2022-2025)
#   SMCH Zimbabwe maternal outcomes (all births 2022-2025)

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tidyr)
})

# ===== CONFIGURATION =====

kch_maternal_file <- "SUBSAMPLE_20220101_to_20251231_KCH_MWI_Combined_Maternity_Outcomes_2026-02-11_cleaned_no_gestation_filter.csv"
smch_maternal_file <- "SUBSAMPLE_20220101_to_20251231_SMCH_ZIM_Maternal_Outcomes_2026-02-11_cleaned_no_gestation_filter.csv"

output_dir <- "Q6_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ===== INTERGROWTH-21st 10TH PERCENTILE REFERENCE =====

PCT10_UNISEX <- c(
    "14" = 76, "15" = 98, "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 560, "25" = 640, "26" = 740, "27" = 840, "28" = 940, "29" = 1060, "30" = 1190, "31" = 1340, "32" = 1500,
    "33" = 1580, "34" = 1780, "35" = 2000, "36" = 2220, "37" = 2440, "38" = 2650, "39" = 2850, "40" = 3010, "41" = 3150, "42" = 3260
)

PCT10_BOYS <- c(
    "14" = 76, "15" = 98, "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 580, "25" = 660, "26" = 760, "27" = 870, "28" = 980, "29" = 1110, "30" = 1250, "31" = 1410, "32" = 1580,
    "33" = 1600, "34" = 1810, "35" = 2030, "36" = 2250, "37" = 2480, "38" = 2700, "39" = 2900, "40" = 3070, "41" = 3210, "42" = 3320
)

PCT10_GIRLS <- c(
    "14" = 76, "15" = 98, "16" = 125, "17" = 158, "18" = 198, "19" = 244, "20" = 298, "21" = 359, "22" = 429, "23" = 508,
    "24" = 540, "25" = 620, "26" = 720, "27" = 810, "28" = 910, "29" = 1030, "30" = 1150, "31" = 1300, "32" = 1460,
    "33" = 1540, "34" = 1750, "35" = 1960, "36" = 2180, "37" = 2400, "38" = 2610, "39" = 2800, "40" = 2960, "41" = 3090, "42" = 3190
)

# Season order for correct sorting in outputs
SEASON_ORDER <- c("Warm and wet", "Cool and wet", "Cool and dry", "Warm and dry", "Hot and dry")

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
    if ("bwt" %in% names(df)) result <- ifelse(is.na(result) & !is.na(df$bwt), df$bwt, result)
    if ("birthweight" %in% names(df)) result <- ifelse(is.na(result) & !is.na(df$birthweight), df$birthweight, result)
    if ("bwtdis" %in% names(df)) result <- ifelse(is.na(result) & !is.na(df$bwtdis), df$bwtdis, result)
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
    percentile_g <- if (!is.na(gender_clean) && gender_clean %in% c("M", "MALE", "BOY")) {
        PCT10_BOYS[ga_char]
    } else if (!is.na(gender_clean) && gender_clean %in% c("F", "FEMALE", "GIRL")) {
        PCT10_GIRLS[ga_char]
    } else {
        PCT10_UNISEX[ga_char]
    }
    if (is.na(percentile_g)) NA_real_ else as.numeric(percentile_g) / 1000
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

# Assign 5-season classification from birth month
assign_season <- function(month) {
    dplyr::case_when(
        month %in% c(12, 1, 2, 3) ~ "Warm and wet",
        month %in% c(4, 5) ~ "Cool and wet",
        month %in% c(6, 7, 8) ~ "Cool and dry",
        month == 9 ~ "Warm and dry",
        month %in% c(10, 11) ~ "Hot and dry",
        TRUE ~ NA_character_
    )
}

# Add "Total" row to a grouped distribution table
add_totals <- function(df, group_cols = character(0)) {
    if (nrow(df) == 0) {
        return(df)
    }

    if (length(group_cols) == 0) {
        totals <- tibble(
            category       = "Total",
            explanation    = "All deliveries considered",
            n              = sum(df$n),
            percentage     = 100,
            classification = unique(df$classification)
        )
        return(bind_rows(df, totals))
    }

    totals <- df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        mutate(
            category       = "Total",
            explanation    = "All deliveries considered",
            percentage     = 100,
            classification = unique(df$classification)
        )

    bind_rows(df, totals) %>%
        arrange(
            across(all_of(group_cols)),
            match(category, c(
                "ELBW", "VLBW", "LBW", "NBW", "HBW",
                "Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA",
                "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total"
            ))
        )
}

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Maternal Data ===\n")
cat("Reading KCH maternal data...\n")
kch_raw <- suppressMessages(read_csv(kch_maternal_file, guess_max = 50000, col_types = cols(.default = "c")))
cat("Reading SMCH maternal data...\n")
smch_raw <- suppressMessages(read_csv(smch_maternal_file, guess_max = 50000, col_types = cols(.default = "c")))

# Convert birthweight to kg BEFORE combining (facility-specific unit detection)
cat("Converting birthweight to kg per facility...\n")

kch_raw <- kch_raw %>% mutate(bw_raw = get_bw_column(.), bw_kg = to_kg(bw_raw), facility = "KCH")
smch_raw <- smch_raw %>% mutate(bw_raw = get_bw_column(.), bw_kg = to_kg(bw_raw), facility = "SMCH")

cat("Combining datasets...\n")
all_maternal <- bind_rows(kch_raw, smch_raw)

total_deliveries <- nrow(all_maternal)
cat("Total deliveries:", total_deliveries, "\n")

# ===== PROCESS VARIABLES =====

cat("\n=== Processing Variables ===\n")

all_maternal_enhanced <- all_maternal %>%
    mutate(
        gestation_weeks = suppressWarnings(as.numeric(gestation)),
        date_parsed = suppressWarnings(as.Date(dateadmission)),
        birth_month = as.integer(format(date_parsed, "%m")),
        season = assign_season(birth_month),
        season = factor(season, levels = SEASON_ORDER),

        # Validations
        is_valid_bw = is.finite(bw_kg) & bw_kg >= 0.3 & bw_kg <= 7.0,
        is_invalid_ga = is.na(gestation_weeks) | gestation_weeks < 22 | gestation_weeks > 42,
        sga_status = dplyr::if_else(
            gestation_weeks >= 22 & !is.na(gestation_weeks),
            classify_sga(bw_kg, gestation_weeks, if ("sexdis" %in% names(.)) sexdis else NA_character_),
            NA_character_
        ),
        sga_cat_temp = sga_category_from_gestational_age(sga_status, gestation_weeks),
        is_valid_sga = is_valid_bw & !is.na(sga_cat_temp),

        # Birth-weight category
        bw_category = if_else(is_valid_bw, bw_category_from_kg(bw_kg), "Discarded"),
        lbw_explanation = if_else(is_valid_bw, "Included in analysis", "Missing or invalid birth weight"),

        # SGA category / explanation
        sga_explanation = case_when(
            is_valid_sga ~ "Included in analysis",
            !is_valid_bw & is_invalid_ga ~ "Missing both BW and GA",
            !is_valid_bw ~ "Missing BW",
            is_invalid_ga ~ "Missing GA",
            TRUE ~ "Missing GA"
        ),
        sga_category = if_else(is_valid_sga, sga_cat_temp, sga_explanation)
    )

# Records with a valid season (requires a parseable admission date)
seasonal_data <- all_maternal_enhanced %>% filter(!is.na(season))

cat("Records with valid season:", nrow(seasonal_data), "\n")
cat(
    "Records missing date (excluded from seasonal analysis):",
    total_deliveries - nrow(seasonal_data), "\n"
)

# ===== SAMPLE SIZE SUMMARY =====

cat("\n=== Sample Size Summary ===\n")

lbw_count <- sum(all_maternal_enhanced$is_valid_bw)
sga_count <- sum(all_maternal_enhanced$is_valid_sga)

sample_size_summary <- tibble(
    description = c(
        "Total deliveries",
        "Records with valid season (date available)",
        "Records missing date (excluded)",
        "Deliveries with valid birthweight (LBW sample)",
        "Discarded for LBW (missing/invalid birthweight)",
        "Deliveries with valid SGA classification",
        "Discarded for SGA (missing BW)",
        "Discarded for SGA (missing GA)",
        "Discarded for SGA (missing both BW and GA)"
    ),
    n = c(
        total_deliveries,
        nrow(seasonal_data),
        total_deliveries - nrow(seasonal_data),
        lbw_count,
        sum(all_maternal_enhanced$bw_category == "Discarded"),
        sga_count,
        sum(all_maternal_enhanced$sga_explanation == "Missing BW"),
        sum(all_maternal_enhanced$sga_explanation == "Missing GA"),
        sum(all_maternal_enhanced$sga_explanation == "Missing both BW and GA")
    )
) %>%
    mutate(pct_of_total = round(100 * n / total_deliveries, 1))

# ===== SEASON-BASED ANALYSIS (helpers) =====

LBW_CAT_ORDER <- c("ELBW", "VLBW", "LBW", "NBW", "HBW", "Discarded")
SGA_CAT_ORDER <- c(
    "Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA",
    "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total"
)

# Compute distribution of birth-weight category by season (± facility)
lbw_by_season_facility <- function(data) {
    data %>%
        filter(!is.na(season)) %>%
        group_by(facility, season, category = bw_category, explanation = lbw_explanation) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(facility, season) %>%
        mutate(percentage = round(100 * n / sum(n), 1), classification = "Birthweight") %>%
        ungroup() %>%
        arrange(facility, season, match(category, LBW_CAT_ORDER))
}

sga_by_season_facility <- function(data) {
    data %>%
        filter(!is.na(season)) %>%
        group_by(facility, season, category = sga_category, explanation = sga_explanation) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(facility, season) %>%
        mutate(percentage = round(100 * n / sum(n), 1), classification = "SGA") %>%
        ungroup() %>%
        arrange(facility, season, match(category, SGA_CAT_ORDER))
}

# ===== OUTPUT 1: BY SEASON (GLOBAL) =====

cat("\n=== Seasonal Analysis — Global ===\n")

# Treat all facilities as one pool, add pseudo-facility "Global"
global_data <- seasonal_data %>% mutate(facility = "Global")

lbw_global <- lbw_by_season_facility(global_data)
sga_global <- sga_by_season_facility(global_data)

lbw_global <- add_totals(lbw_global, c("facility", "season"))
sga_global <- add_totals(sga_global, c("facility", "season"))

seasonal_global <- bind_rows(lbw_global, sga_global)

# ===== OUTPUT 2: BY SEASON AND FACILITY =====

cat("\n=== Seasonal Analysis — By Facility ===\n")

lbw_fac <- lbw_by_season_facility(seasonal_data)
sga_fac <- sga_by_season_facility(seasonal_data)

lbw_fac <- add_totals(lbw_fac, c("facility", "season"))
sga_fac <- add_totals(sga_fac, c("facility", "season"))

seasonal_by_facility <- bind_rows(lbw_fac, sga_fac)

# ===== OUTPUT 3: BIRTHS PER SEASON (denominator summary) =====

births_per_season <- seasonal_data %>%
    group_by(facility, season) %>%
    summarise(
        n_total = n(),
        n_valid_bw = sum(is_valid_bw),
        n_valid_sga = sum(is_valid_sga),
        .groups = "drop"
    ) %>%
    bind_rows(
        seasonal_data %>%
            group_by(season) %>%
            summarise(
                n_total = n(),
                n_valid_bw = sum(is_valid_bw),
                n_valid_sga = sum(is_valid_sga),
                .groups = "drop"
            ) %>%
            mutate(facility = "Global")
    ) %>%
    arrange(facility, season)

# ===== SAVE DESCRIPTIVE OUTPUTS =====

cat("\n=== Saving Descriptive Results ===\n")

write_csv(sample_size_summary, file.path(output_dir, "01_sample_size_summary.csv"))
cat("Saved: 01_sample_size_summary.csv\n")

write_csv(births_per_season, file.path(output_dir, "02_births_per_season.csv"))
cat("Saved: 02_births_per_season.csv\n")

write_csv(seasonal_global, file.path(output_dir, "03_distribution_by_season_global.csv"))
cat("Saved: 03_distribution_by_season_global.csv\n")

write_csv(seasonal_by_facility, file.path(output_dir, "04_distribution_by_season_and_facility.csv"))
cat("Saved: 04_distribution_by_season_and_facility.csv\n")

# ===== CHI-SQUARED TEST OF INDEPENDENCE (season × category) =====
#
# Tests whether the distribution of births across LBW / SGA categories
# is independent of season. Run separately for each scope (Global, KCH, SMCH)
# and each classification (Birthweight, SGA).
#
# Standardised Pearson residuals = (observed - expected) / sqrt(expected)
# Residual >  2 : cell significantly MORE frequent than expected
# Residual < -2 : cell significantly LESS  frequent than expected

cat("\n=== Chi-Squared Tests of Independence ===\n")

# Only valid (classified) categories — exclude "Discarded" / missing rows
VALID_LBW_CATS <- c("ELBW", "VLBW", "LBW", "NBW", "HBW")
VALID_SGA_CATS <- c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")

run_chisq <- function(data, cat_col, valid_cats, classification_label, scope_label) {
    sub <- data %>%
        filter(!is.na(season), .data[[cat_col]] %in% valid_cats) %>%
        mutate(
            season = factor(as.character(season), levels = SEASON_ORDER),
            cat    = factor(.data[[cat_col]], levels = valid_cats)
        )

    if (nrow(sub) == 0) {
        return(NULL)
    }

    tbl <- table(sub$season, sub$cat)

    # Suppress warning when expected < 5 (we report it in the output instead)
    test <- suppressWarnings(chisq.test(tbl, correct = FALSE))
    stdres <- test$stdres # standardised Pearson residuals matrix

    # --- Summary row ---
    summary_row <- tibble(
        scope          = scope_label,
        classification = classification_label,
        chi_sq         = round(test$statistic, 3),
        df             = test$parameter,
        p_value        = round(test$p.value, 5),
        significant    = test$p.value < 0.05
    )

    # --- Residuals (long format) ---
    resid_df <- as.data.frame(stdres) %>%
        rename(season = Var1, category = Var2, std_residual = Freq) %>%
        mutate(
            scope = scope_label,
            classification = classification_label,
            observed = as.integer(tbl[cbind(as.character(season), as.character(category))]),
            expected = round(test$expected[cbind(as.character(season), as.character(category))], 1),
            std_residual = round(std_residual, 3),
            direction = case_when(
                std_residual > 2 ~ "More than expected",
                std_residual < -2 ~ "Less than expected",
                TRUE ~ "As expected"
            )
        ) %>%
        select(scope, classification, season, category, observed, expected, std_residual, direction)

    list(summary = summary_row, residuals = resid_df)
}

# Define scopes: Global + per facility
scopes <- list(
    list(label = "Global", data = seasonal_data),
    list(label = "KCH", data = seasonal_data %>% filter(facility == "KCH")),
    list(label = "SMCH", data = seasonal_data %>% filter(facility == "SMCH"))
)

chisq_summaries <- list()
chisq_residuals <- list()

for (sc in scopes) {
    for (cls in list(
        list(col = "bw_category", valid = VALID_LBW_CATS, label = "Birthweight"),
        list(col = "sga_category", valid = VALID_SGA_CATS, label = "SGA")
    )) {
        res <- run_chisq(sc$data, cls$col, cls$valid, cls$label, sc$label)
        if (!is.null(res)) {
            chisq_summaries <- c(chisq_summaries, list(res$summary))
            chisq_residuals <- c(chisq_residuals, list(res$residuals))
        }
    }
}

chisq_summary_df <- bind_rows(chisq_summaries)
chisq_residuals_df <- bind_rows(chisq_residuals)

cat("Chi-squared results:\n")
print(chisq_summary_df)

write_csv(chisq_summary_df, file.path(output_dir, "05_chisq_results.csv"))
write_csv(chisq_residuals_df, file.path(output_dir, "06_chisq_residuals.csv"))
cat("Saved: 05_chisq_results.csv\n")
cat("Saved: 06_chisq_residuals.csv\n")

# ===== LOGISTIC REGRESSION: ODDS RATIOS BY SEASON =====
#
# Three binary comparisons (reference season = "Cool and dry"):
#   1. Any-LBW (ELBW + VLBW + LBW) vs NBW  — sample: valid LBW records only
#   2. SGA vs AGA (regardless of term status) — sample: valid SGA records only
#   3. Preterm vs Term                         — sample: valid SGA records only
#
# Run globally and per facility.

cat("\n=== Logistic Regression: Odds Ratios by Season ===\n")

# Reference season
REF_SEASON <- "Cool and dry"

run_logreg <- function(data, outcome_col, label, scope_label) {
    sub <- data %>%
        filter(!is.na(season), !is.na(.data[[outcome_col]])) %>%
        mutate(
            season  = relevel(factor(as.character(season), levels = SEASON_ORDER), ref = REF_SEASON),
            outcome = as.integer(.data[[outcome_col]])
        )

    if (nrow(sub) < 10 || length(unique(sub$outcome)) < 2) {
        return(NULL)
    }

    tryCatch(
        {
            fit <- glm(outcome ~ season, data = sub, family = binomial())
            coefs <- summary(fit)$coefficients
            ci <- suppressMessages(confint(fit))

            or_df <- coefs[-1, , drop = FALSE] %>%
                as.data.frame() %>%
                tibble::rownames_to_column("term") %>%
                mutate(
                    season         = gsub("^season", "", term),
                    scope          = scope_label,
                    comparison     = label,
                    reference      = REF_SEASON,
                    n_total        = nrow(sub),
                    n_outcome      = sum(sub$outcome),
                    OR             = round(exp(Estimate), 3),
                    CI_lower       = round(exp(ci[term, 1]), 3),
                    CI_upper       = round(exp(ci[term, 2]), 3),
                    p_value        = round(`Pr(>|z|)`, 5),
                    significant    = `Pr(>|z|)` < 0.05
                ) %>%
                select(
                    scope, comparison, season, reference, n_total, n_outcome,
                    OR, CI_lower, CI_upper, p_value, significant
                )

            or_df
        },
        error = function(e) {
            message("  Logistic regression failed for ", scope_label, " / ", label, ": ", e$message)
            NULL
        }
    )
}

# Prepare binary outcome columns
seasonal_data <- seasonal_data %>%
    mutate(
        # 1. Any-LBW vs NBW (exclude VLBW/ELBW edge cases? no — all LBW vs NBW)
        is_lbw_vs_nbw = case_when(
            bw_category %in% c("ELBW", "VLBW", "LBW") ~ 1L,
            bw_category == "NBW" ~ 0L,
            TRUE ~ NA_integer_
        ),
        # 2. SGA vs AGA (pooled across term/preterm)
        is_sga_vs_aga = case_when(
            sga_category %in% c("Term-SGA", "Preterm-SGA") ~ 1L,
            sga_category %in% c("Term-AGA", "Preterm-AGA") ~ 0L,
            TRUE ~ NA_integer_
        ),
        # 3. Preterm vs Term
        is_preterm_vs_term = case_when(
            sga_category %in% c("Preterm-SGA", "Preterm-AGA") ~ 1L,
            sga_category %in% c("Term-SGA", "Term-AGA") ~ 0L,
            TRUE ~ NA_integer_
        )
    )

# Rebuild global_data with the new columns
global_data <- seasonal_data %>% mutate(facility = "Global")

comparisons <- list(
    list(col = "is_lbw_vs_nbw", label = "Any-LBW vs NBW"),
    list(col = "is_sga_vs_aga", label = "SGA vs AGA"),
    list(col = "is_preterm_vs_term", label = "Preterm vs Term")
)

or_results <- list()
for (sc in list(
    list(label = "Global", data = global_data),
    list(label = "KCH", data = seasonal_data %>% filter(facility == "KCH") %>% mutate(facility = "KCH")),
    list(label = "SMCH", data = seasonal_data %>% filter(facility == "SMCH") %>% mutate(facility = "SMCH"))
)) {
    for (cmp in comparisons) {
        res <- run_logreg(sc$data, cmp$col, cmp$label, sc$label)
        if (!is.null(res)) or_results <- c(or_results, list(res))
    }
}

or_df <- bind_rows(or_results)

cat("Odds ratio results (first rows):\n")
print(head(or_df, 10))

write_csv(or_df, file.path(output_dir, "07_odds_ratios_by_season.csv"))
cat("Saved: 07_odds_ratios_by_season.csv\n")

# ===== CONSOLE SUMMARY =====

cat("\n", rep("=", 60), "\n", sep = "")
cat("SEASONAL DISTRIBUTION SUMMARY\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Births per season (Global):\n")
print(births_per_season %>% filter(facility == "Global") %>% select(season, n_total, n_valid_bw, n_valid_sga))
cat("\n")

cat("Chi-squared independence tests:\n")
print(chisq_summary_df %>% select(scope, classification, chi_sq, df, p_value, significant))
cat("\n")

cat("Output files saved to:", output_dir, "\n")
cat("  01_sample_size_summary.csv\n")
cat("  02_births_per_season.csv\n")
cat("  03_distribution_by_season_global.csv\n")
cat("  04_distribution_by_season_and_facility.csv\n")
cat("  05_chisq_results.csv          — chi-sq statistic + p-value per scope/classification\n")
cat("  06_chisq_residuals.csv        — standardised Pearson residuals per cell\n")
cat("  07_odds_ratios_by_season.csv  — OR (95% CI, p) per season vs 'Cool and dry'\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Analysis complete!\n")
