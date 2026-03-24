# Q24_methods_est_gestation.R
# Analysis of frequencies and percentages for methods to estimate gestational age
# Variable: methodestgest
# Stratified by facility.

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
output_dir <- "Q24_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ===== LOAD AND PROCESS DATA =====

cat("\n=== Loading Newborn Admission Data ===\n")
kch_raw <- suppressMessages(read_csv(kch_file, guess_max = 50000, col_types = cols(.default = "c")))
smch_raw <- suppressMessages(read_csv(smch_file, guess_max = 50000, col_types = cols(.default = "c")))

kch_proc <- kch_raw %>% mutate(facility = "KCH")
smch_proc <- smch_raw %>% mutate(facility = "SMCH")
all_admissions <- bind_rows(kch_proc, smch_proc)

cat("Total records loaded:", nrow(all_admissions), "\n")

# Ensure variable exists
if (!"methodestgest" %in% names(all_admissions)) {
    stop("Column 'methodestgest' not found in the datasets.")
}

# ===== ANALYSIS =====
cat("\n=== Calculating Frequencies and Percentages ===\n")

# Process variable: handle NAs and blanks
methods_summary_df <- all_admissions %>%
    mutate(methodestgest_clean = ifelse(is.na(methodestgest) | trimws(methodestgest) == "", 
                                        "Missing/Unknown", 
                                        trimws(methodestgest)))

# 1. By Facility
facility_summary <- methods_summary_df %>%
    group_by(facility, methodestgest_clean) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(facility) %>%
    mutate(
        total_facility = sum(n),
        percentage = round(100 * n / total_facility, 1)
    ) %>%
    ungroup() %>%
    arrange(facility, desc(n))

# 2. Overall
overall_summary <- methods_summary_df %>%
    group_by(methodestgest_clean) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
        facility = "Overall",
        total_facility = sum(n),
        percentage = round(100 * n / total_facility, 1)
    ) %>%
    arrange(desc(n))

# Combine Long Format
final_summary_long <- bind_rows(overall_summary, facility_summary) %>%
    select(facility, methodestgest = methodestgest_clean, n, percentage, total_facility)

# 3. Create a Wide Format for easier reading
wide_summary <- final_summary_long %>%
    mutate(val = sprintf("%d (%.1f%%)", n, percentage)) %>%
    select(methodestgest, facility, val) %>%
    pivot_wider(names_from = facility, values_from = val, values_fill = "0 (0.0%)")

# Add Totals row to the wide summary
totals_wide <- final_summary_long %>%
    select(facility, total_facility) %>%
    distinct() %>%
    mutate(methodestgest = "Total", val = as.character(total_facility)) %>%
    select(methodestgest, facility, val) %>%
    pivot_wider(names_from = facility, values_from = val, values_fill = "0")

wide_final <- bind_rows(
    wide_summary,
    totals_wide
)

# ===== SAVE =====
cat("\n=== Saving Results ===\n")

write_csv(final_summary_long, file.path(output_dir, "01_methodestgest_frequencies_long.csv"))
cat("Saved: 01_methodestgest_frequencies_long.csv\n")

write_csv(wide_final, file.path(output_dir, "02_methodestgest_frequencies_wide.csv"))
cat("Saved: 02_methodestgest_frequencies_wide.csv\n")

cat("\nAnalysis complete!\n")
