# Q14_causes_of_death.R
# Analysis of Top 5 Causes of Death by LBW and SGA Categories
#
# This script:
# 1. Filters for Neonatal Deaths.
# 2. Identifies the Top 5 reported causes of death (raw codes) for each group.
# 3. Stratifies by Facility (KCH vs SMCH) since coding excludes differ.
# 4. Stratifies by LBW and SGA categories.
#
# Data sources:
# - KCH Malawi newborn admissions
# - SMCH Zimbabwe newborn admissions

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
output_dir <- "Q14_outputs"
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

# New function: Apply Total Wrapper specific for Outputting Ranked Top 5 Output
add_totals_causes <- function(df, raw_df, classification_name) {
    if (nrow(df) == 0) {
        return(df)
    }

    # Calculates Total over all rows directly
    # Since cause analysis already pivots wide dynamically, we manually recreate the ranking steps for 'Overall'

    group_totals <- raw_df %>%
        count(facility, name = "n_total_deaths")

    totals <- raw_df %>%
        count(facility, cause = cod_clean) %>%
        group_by(facility) %>%
        arrange(desc(n)) %>%
        mutate(rank = row_number()) %>%
        filter(rank <= 5) %>%
        ungroup() %>%
        left_join(group_totals, by = "facility") %>%
        mutate(pct = round(100 * n / n_total_deaths, 1)) %>%
        pivot_wider(
            id_cols = c(facility, n_total_deaths),
            names_from = rank,
            values_from = c(cause, n, pct),
            names_glue = "rank_{rank}_{.value}"
        ) %>%
        mutate(
            category = "Total",
            explanation = "All deaths considered",
            classification = classification_name
        )

    bind_rows(df, totals) %>%
        arrange(
            facility,
            match(category, c(
                "ELBW", "VLBW", "LBW", "NBW", "HBW",
                "Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA",
                "Discarded", "Missing BW", "Missing GA", "Missing both BW and GA", "Total"
            ))
        )
}

# Normalize outcomes to identify Neonatal Deaths
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

        # Outcome
        outcome_norm = normalise_outcome(neotreeoutcome),

        # Clean Cause of Death
        cod_clean = str_trim(causedeath),
        cod_clean = ifelse(cod_clean == "" | is.na(cod_clean) | cod_clean == "NA", "Unknown", cod_clean),

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

# Filter for Neonatal Deaths
valid_nnd_df <- analysis_df %>% filter(outcome_norm == "Neonatal Death")

cat("Total NNDs for analysis:", nrow(valid_nnd_df), "\n")


# ===== ANALYSIS =====

get_top_causes <- function(data, group_cols) {
    category_col <- group_cols[1]
    explanation_col <- group_cols[2]

    # Calculate totals per group
    group_totals <- data %>%
        filter(!is.na(.data[[category_col]])) %>%
        count(facility, category = .data[[category_col]], explanation = .data[[explanation_col]], name = "n_total_deaths")

    # Calculate Top 5 Causes per group
    top_causes <- data %>%
        filter(!is.na(.data[[category_col]])) %>%
        count(facility, category = .data[[category_col]], explanation = .data[[explanation_col]], cause = cod_clean) %>%
        group_by(facility, category) %>%
        arrange(desc(n)) %>%
        mutate(rank = row_number()) %>%
        filter(rank <= 5) %>%
        ungroup() %>%
        # Calculate Percentage
        left_join(group_totals, by = c("facility", "category", "explanation")) %>%
        mutate(pct = round(100 * n / n_total_deaths, 1)) %>%
        # Pivot Wide
        pivot_wider(
            id_cols = c(facility, category, explanation, n_total_deaths),
            names_from = rank,
            values_from = c(cause, n, pct),
            names_glue = "rank_{rank}_{.value}"
        )

    return(top_causes)
}

# ===== NORMALISE CAUSE CODES (group variants into canonical names) =====
#
# This runs BEFORE the top-5 ranking so that variants of the same condition
# are counted together in output 01 (specific causes) as well as output 02
# (standard categories).
#
# Canonical names map:
#   Prematurity/RDS variants  → "Prematurity/RDS"
#   Birth Asphyxia variants   → "Birth Asphyxia/HIE"
#   Neonatal Sepsis variants   → grouped under their type (EONS/LONS/NS→"Neonatal Sepsis")
#   Gastroschisis variants     → "Gastroschisis"
#   Omphalocele variants       → "Omphalocele"
#   Congenital Anomaly (generic) variants → "Congenital Anomaly"

normalise_cause <- function(code) {
    code_up <- toupper(trimws(code))
    dplyr::case_when(
        # --- Prematurity / RDS group ---
        code_up %in% c(
            "PRRDS", "PREMRD", "EXPREM", "VPREM",
            "PR", "RDS", "EXLBW", "VLBW", "LBW",
            "TERMRD", "RDNT", "TTN", "CLEFTRD"
        ) ~ "Prematurity/RDS",

        # --- Birth Asphyxia / HIE group ---
        code_up %in% c(
            "BA", "HIE", "SHIE", "MAS", "MA",
            "ASP", "LAS", "AP", "BBA"
        ) ~ "Birth Asphyxia/HIE",

        # --- Neonatal Sepsis (keep EONS/LONS distinct, merge NS/SEPS) ---
        code_up %in% c("NS", "SEPS", "HYPOGSY") ~ "Neonatal Sepsis",
        code_up == "EONS" ~ "Neonatal Sepsis (Early Onset)",
        code_up == "LONS" ~ "Neonatal Sepsis (Late Onset)",

        # --- Gastroschisis variants ---
        code_up %in% c("G", "GSCH", "GSCHIS", "GASTRO") ~ "Gastroschisis",

        # --- Omphalocele variants ---
        code_up %in% c("OMPH", "OM", "EXOM") ~ "Omphalocele",

        # --- Generic Congenital Anomaly ---
        code_up %in% c("CONG", "CA", "OCA") ~ "Congenital Anomaly",

        # --- All other codes kept as-is (their own names in output 01) ---
        code_up %in% c("OTH") ~ "Cause Not Specified",
        TRUE ~ code
    )
}

valid_nnd_df <- valid_nnd_df %>%
    mutate(cod_clean = normalise_cause(cod_clean))

cat("Sample of normalised cause codes:\n")
print(sort(table(valid_nnd_df$cod_clean), decreasing = TRUE)[1:15])

cat("\n=== Calculating Top Causes ===\n")

# LBW Categories
lbw_top <- get_top_causes(valid_nnd_df, c("bw_category", "lbw_explanation")) %>%
    mutate(classification = "Birthweight") %>%
    add_totals_causes(valid_nnd_df, "Birthweight")

# SGA Categories
sga_top <- get_top_causes(valid_nnd_df, c("sga_category", "sga_explanation")) %>%
    mutate(classification = "SGA") %>%
    add_totals_causes(valid_nnd_df, "SGA")

# ===== FORMATTING OUTPUT =====
# Reorder columns to be reader-friendly: Cause, N, Pct blocks
combine_and_format <- function(df) {
    df %>%
        select(
            facility, classification, category, explanation, n_total_deaths,
            cause_1 = rank_1_cause, n_1 = rank_1_n, pct_1 = rank_1_pct,
            cause_2 = rank_2_cause, n_2 = rank_2_n, pct_2 = rank_2_pct,
            cause_3 = rank_3_cause, n_3 = rank_3_n, pct_3 = rank_3_pct,
            cause_4 = rank_4_cause, n_4 = rank_4_n, pct_4 = rank_4_pct,
            cause_5 = rank_5_cause, n_5 = rank_5_n, pct_5 = rank_5_pct
        )
}

final_output <- bind_rows(
    combine_and_format(lbw_top),
    combine_and_format(sga_top)
)

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
death_note <- "normalise_outcome(neotreeoutcome) == \"Neonatal Death\""
cod_note <- "causedeath is not NA/empty — mapped via normalise_cause() and map_cause_to_category()"

n_total <- nrow(analysis_df)
n_nnd <- nrow(valid_nnd_df)

sample_size_summary <- tibble(
    description = c(
        "Total admissions",
        "Total neonatal deaths (NND)",
        "NNDs with known cause of death",
        "NNDs with unknown/missing cause of death",
        "Total admissions with valid birthweight (LBW sample)",
        "Discarded for LBW (missing/invalid birthweight)",
        "NNDs with valid birthweight",
        "Total admissions with valid SGA classification",
        "NNDs with valid SGA classification",
        "Discarded for SGA (Missing BW)",
        "Discarded for SGA (Missing GA)",
        "Discarded for SGA (Missing both BW and GA)"
    ),
    n = c(
        n_total,
        n_nnd,
        sum(valid_nnd_df$cod_clean != "Unknown"),
        sum(valid_nnd_df$cod_clean == "Unknown"),
        sum(analysis_df$is_valid_bw),
        sum(!analysis_df$is_valid_bw),
        sum(valid_nnd_df$is_valid_bw),
        sum(analysis_df$is_valid_sga),
        sum(valid_nnd_df$is_valid_sga),
        sum(analysis_df$sga_explanation == "Missing BW"),
        sum(analysis_df$sga_explanation == "Missing GA"),
        sum(analysis_df$sga_explanation == "Missing both BW and GA")
    ),
    pct_of_total = round(100 * n / n_total, 1),
    Filter_notes = c(
        NA_character_,
        death_note,
        paste0(death_note, " AND ", cod_note),
        paste0(death_note, " AND (causedeath is NA or empty)"),
        bw_note,
        paste0("NOT (", bw_note, ")"),
        paste0(death_note, " AND ", bw_note),
        sga_note,
        paste0(death_note, " AND ", sga_note),
        "Invalid/missing birth weight",
        "Invalid/missing gestational age",
        "Both BW and GA invalid/missing"
    )
)

write_csv(sample_size_summary, file.path(output_dir, "00_sample_size_summary.csv"))
cat("Saved: 00_sample_size_summary.csv\n")
write_csv(final_output, file.path(output_dir, "01_top5_causes_death_by_facility.csv"))
cat("Saved: 01_top5_causes_death_by_facility.csv\n")

# ===== STANDARD CAUSE CATEGORY MAPPING =====

cat("\n=== Mapping Causes to Standard Categories ===\n")

# Map raw codes to 5 standard categories (case-insensitive)
# Sources:
#   MWI — PHC-Discharges sheet of Dictionary_MWI.xlsx
#   ZIM  — BPH-Discharges sheet of Dictionary_ZIM.xlsx
#          + additional free-text variants observed in the SMCH data
#
# Category definitions:
#
#   Prematurity/RDS
#     MWI: PRRDS, PR, RDNT, TTN, LBW (cause=LBW), RDS
#     ZIM: PremRD, TermRD, ExPrem, VPrem, PRRDS, PR, LBW, ExLBW,
#          VLBW, RDS, TTN, CleftRD (cleft-related respiratory distress)
#
#   Birth Asphyxia/HIE
#     MWI: BA, HIE, ASP, MA, LAS, AP
#     ZIM: BA, HIE, sHIE (suspected HIE), MA, MAS (Meconium Asp. Syndrome),
#          ASP, BBA (Born Before Arrival — typically presents as asphyxia)
#
#   Sepsis/Infection
#     MWI: NS, EONS, LONS, MEN, PN, NT, NEOT, BRON, GA, GE
#     ZIM: SEPS, EONS, LONS, PN, MEN, HypogSy (sepsis with hypoglycaemia)
#
#   Congenital Anomalies
#     MWI: CONG, CHD, BI, BDN, HYDR, BO, EXOM, G, IA, NEC, J
#     ZIM: Cong, CA, CHD, GSch, GSchis, Gastro (Gastroschisis), Omph,
#          NEC, BI, MD (Musculoskeletal), Myelo (Myelomeningocele),
#          CleftLipPalate, OM, OCA
#
#   Other
#     Everything else: AN, HYP, HYGL, CONV, DEHY, SH, MAL, UNK, OTH,
#     RF, CAI, HYPG, SHypo, MiHypo, ModHypo, RiHypog, HypogAs,
#     DJ, DF, Risk, Safe, MJ, PJaundice, Hyperth, An, HBW, Dhyd, MecEx

map_cause_to_category <- function(code) {
    code_up <- toupper(trimws(code))
    dplyr::case_when(
        # ---- Prematurity / RDS ----
        # Canonical name (from normalise_cause) and raw codes
        code_up %in% c(
            "PREMATURITY/RDS",
            "PRRDS", "PR", "RDNT", "TTN", "LBW", "RDS",
            "PREMRD", "TERMRD", "EXPREM", "VPREM", "EXLBW", "VLBW",
            "CLEFTRD"
        ) ~ "Prematurity/RDS",

        # ---- Birth Asphyxia / HIE ----
        code_up %in% c(
            "BIRTH ASPHYXIA/HIE",
            "BA", "HIE", "ASP", "SHIE", "MA", "LAS", "AP",
            "MAS", "BBA"
        ) ~ "Birth Asphyxia/HIE",

        # ---- Sepsis / Infection ----
        code_up %in% c(
            "NEONATAL SEPSIS",
            "NEONATAL SEPSIS (EARLY ONSET)",
            "NEONATAL SEPSIS (LATE ONSET)",
            "NS", "EONS", "LONS", "SEPS", "HYPOGSY",
            "MEN", "PN", "NT", "NEOT",
            "BRON", "GA", "GE"
        ) ~ "Sepsis/Infection",

        # ---- Congenital Anomalies ----
        code_up %in% c(
            "CONGENITAL ANOMALY", "GASTROSCHISIS", "OMPHALOCELE",
            "CONG", "CHD", "BI", "BDN", "HYDR",
            "BO", "EXOM", "G", "IA", "NEC", "J",
            "CA", "GASTRO", "GSCH", "GSCHIS",
            "OMPH", "OM", "OCA", "MD", "MYELO",
            "CLEFTLIPPALATE"
        ) ~ "Congenital Anomalies",

        # ---- Other (catch-all) ----
        TRUE ~ "Other"
    )
}

# Add mapped category to deaths dataframe
nnd_mapped <- valid_nnd_df %>%
    mutate(cause_category = map_cause_to_category(cod_clean))

# Reuse get_top_causes but with cause_category instead of cod_clean.
# We temporarily rename the column so the existing function works as-is.
nnd_mapped_for_rank <- nnd_mapped %>%
    mutate(cod_clean = cause_category) # swap raw code for category name

# Get ranked output (mirrors output 01 exactly)
get_mapped_top_causes <- function(data, group_cols) {
    category_col <- group_cols[1]
    explanation_col <- group_cols[2]

    group_totals <- data %>%
        filter(!is.na(.data[[category_col]])) %>%
        count(facility,
            category = .data[[category_col]], explanation = .data[[explanation_col]],
            name = "n_total_deaths"
        )

    top_causes <- data %>%
        filter(!is.na(.data[[category_col]])) %>%
        count(facility,
            category    = .data[[category_col]],
            explanation = .data[[explanation_col]],
            cause       = cod_clean
        ) %>% # cod_clean is now the category name
        group_by(facility, category) %>%
        arrange(desc(n)) %>%
        mutate(rank = row_number()) %>%
        filter(rank <= 5) %>%
        ungroup() %>%
        left_join(group_totals, by = c("facility", "category", "explanation")) %>%
        mutate(pct = round(100 * n / n_total_deaths, 1)) %>%
        pivot_wider(
            id_cols     = c(facility, category, explanation, n_total_deaths),
            names_from  = rank,
            values_from = c(cause, n, pct),
            names_glue  = "rank_{rank}_{.value}"
        )

    top_causes
}

# Total row builder (same logic as add_totals_causes but using category names)
add_totals_mapped <- function(df, raw_data, classification_name) {
    group_totals <- raw_data %>%
        count(facility, name = "n_total_deaths")

    totals <- raw_data %>%
        count(facility, cause = cod_clean) %>% # cod_clean = category name here
        group_by(facility) %>%
        arrange(desc(n)) %>%
        mutate(rank = row_number()) %>%
        filter(rank <= 5) %>%
        ungroup() %>%
        left_join(group_totals, by = "facility") %>%
        mutate(pct = round(100 * n / n_total_deaths, 1)) %>%
        pivot_wider(
            id_cols     = c(facility, n_total_deaths),
            names_from  = rank,
            values_from = c(cause, n, pct),
            names_glue  = "rank_{rank}_{.value}"
        ) %>%
        mutate(
            category = "Total", explanation = "All deaths considered",
            classification = classification_name
        )

    bind_rows(df, totals) %>%
        arrange(
            facility,
            match(category, c(
                "ELBW", "VLBW", "LBW", "NBW", "HBW",
                "Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA",
                "Missing BW", "Missing GA", "Missing both BW and GA", "Discarded", "Total"
            ))
        )
}

# Run for LBW and SGA
lbw_mapped <- get_mapped_top_causes(nnd_mapped_for_rank, c("bw_category", "lbw_explanation")) %>%
    mutate(classification = "Birthweight") %>%
    add_totals_mapped(nnd_mapped_for_rank, "Birthweight")

sga_mapped <- get_mapped_top_causes(nnd_mapped_for_rank, c("sga_category", "sga_explanation")) %>%
    mutate(classification = "SGA") %>%
    add_totals_mapped(nnd_mapped_for_rank, "SGA")

# Format to match output 01 column names — use any_of() so missing rank columns
# don't crash the select (can happen when a group has <5 distinct standard categories)
mapped_output <- bind_rows(lbw_mapped, sga_mapped) %>%
    rename_with(~ sub("^rank_(\\d+)_cause$", "cause_\\1", .x), starts_with("rank_") & ends_with("_cause")) %>%
    rename_with(~ sub("^rank_(\\d+)_n$", "n_\\1", .x), starts_with("rank_") & ends_with("_n")) %>%
    rename_with(~ sub("^rank_(\\d+)_pct$", "pct_\\1", .x), starts_with("rank_") & ends_with("_pct")) %>%
    select(
        facility, classification, category, explanation, n_total_deaths,
        any_of(c(
            "cause_1", "n_1", "pct_1",
            "cause_2", "n_2", "pct_2",
            "cause_3", "n_3", "pct_3",
            "cause_4", "n_4", "pct_4",
            "cause_5", "n_5", "pct_5"
        ))
    )

write_csv(mapped_output, file.path(output_dir, "02_causes_by_standard_category.csv"))
cat("Saved: 02_causes_by_standard_category.csv\n")

cat("\nAnalysis complete!\n")
