source("00_config_and_themes.R")
steroids_raw <- read_csv(
  nnu_path("Q21_outputs_gestation_24_34", "02_steroids_outcomes_SGA_wide.csv"),
  show_col_types = FALSE
)
steroids_long <- steroids_raw %>%
  filter(classification == "SGA") %>%
  rename_with(~ gsub("-", "_", .x)) %>%
  pivot_longer(
    cols = matches("^(Term_AGA|Term_SGA|Preterm_AGA|Preterm_SGA)_(NND|DC)$"),
    names_to = c("category", "outcome"),
    names_pattern = "^(.+)_(NND|DC)$",
    values_to = "n"
  ) %>%
  mutate(
    category = gsub("_", "-", category),
    outcome  = recode(outcome, NND = "Neonatal death", DC = "Survived")
  ) %>%
  filter(category %in% SVN_CATS) %>%
  pivot_wider(names_from = outcome, values_from = n) %>%
  mutate(
    total = `Neonatal death` + Survived,
    mortality_pct = `Neonatal death` / total * 100,
    svn_cat = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    steroids_grp = recode(steroids_group,
      "Steroids Used (Y)"     = "Yes",
      "Steroids Not Used (N)" = "No",
      "Unsure (U)"            = "Unsure",
      "Missing"               = "Missing"
    )
  )

cat("\nSummary of results for ALL categories:\n")
print(steroids_long %>% group_by(svn_cat, steroids_grp) %>% summarise(total_n = sum(total), .groups = 'drop'))

panel_d_data <- steroids_long %>%
  filter(
    svn_cat %in% c("Preterm AGA", "Preterm SGA"),
    steroids_grp %in% c("Yes", "No")
  )
cat("\nSummary of panel_d_data:\n")
print(panel_d_data %>% select(facility_lbl, svn_cat, steroids_grp, total, mortality_pct))
