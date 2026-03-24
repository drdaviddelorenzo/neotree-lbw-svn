# ==============================================================================
# 00_config_and_themes.R
# Shared configuration, colour palette, ggplot2 theme, and helper functions
# for all SVN graph scripts (Figures 1–7).
#
# SOURCE THIS FILE at the top of each figure script:
#   source("00_config_and_themes.R")
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(scales)
  library(forcats)
  library(purrr)
})

# ==============================================================================
# PATHS
# Each script lives in:  .../00-simple_questions_DEF_DEF_for_Graphs/graphs/
# Data directories live in: ../01-MATERNAL_ANALYSIS/ and ../02-NNU_ANALYSIS_USING_ADM_AND_DIS/
# ==============================================================================
GRAPHS_DIR <- "." # current working directory (graphs/)
MATERNAL_DIR <- "Results_maternal_analyses"
NNU_DIR <- "Results_NNU_analyses"
OUTPUT_DIR <- "outputs"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Convenience path builders
maternal_path <- function(...) file.path(MATERNAL_DIR, ...)
nnu_path <- function(...) file.path(NNU_DIR, ...)
out_path <- function(...) file.path(OUTPUT_DIR, ...)

# ==============================================================================
# SVN CATEGORY DEFINITIONS
# All analyses use SGA/SVN classification (not LBW).
# The four main categories used throughout all figures:
# ==============================================================================
SVN_CATS <- c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA")
SVN_LABELS <- c(
  "Term-AGA"    = "Term AGA",
  "Term-SGA"    = "Term SGA",
  "Preterm-AGA" = "Preterm AGA",
  "Preterm-SGA" = "Preterm SGA"
)

# Ordered factor helper
svn_factor <- function(x) {
  factor(x, levels = SVN_CATS, labels = unname(SVN_LABELS))
}

# ==============================================================================
# COLOUR PALETTE  (publication-friendly, colourblind-safe)
# ==============================================================================
SVN_COLOURS <- c(
  "Term-AGA"    = "#2166AC", # deep blue
  "Term-SGA"    = "#74ADD1", # light blue
  "Preterm-AGA" = "#D73027", # red
  "Preterm-SGA" = "#FDAE61" # amber/orange
)

# Named by the relabelled values (for use after svn_factor())
SVN_COLOURS_LABELLED <- c(
  "Term AGA"    = "#2166AC",
  "Term SGA"    = "#74ADD1",
  "Preterm AGA" = "#D73027",
  "Preterm SGA" = "#FDAE61"
)

# Facility colours
FACILITY_COLOURS <- c(
  "KCH"  = "#1B7837", # forest green  (Malawi / KCH)
  "SMCH" = "#762A83" # purple        (Zimbabwe / SMCH)
)

FACILITY_LABELS <- c(KCH = "KCH (Malawi)", SMCH = "SMCH (Zimbabwe)")

# ==============================================================================
# GGPLOT2 THEME  — clean, publication-ready
# ==============================================================================
theme_svn <- function(base_size = 11, legend_position = "right") {
  theme_classic(base_size = base_size) %+replace%
    theme(
      # Axes
      axis.line = element_line(colour = "grey30", linewidth = 0.5),
      axis.ticks = element_line(colour = "grey50", linewidth = 0.4),
      axis.text = element_text(colour = "grey20"),
      axis.title = element_text(colour = "grey10", face = "bold"),
      # Grid
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      # Legend
      legend.position = legend_position,
      legend.title = element_text(face = "bold", size = rel(0.9)),
      legend.text = element_text(size = rel(0.85)),
      legend.key.size = unit(0.45, "cm"),
      legend.background = element_rect(fill = "white", colour = "grey80"),
      # Facets
      strip.background = element_rect(fill = "grey94", colour = "grey70"),
      strip.text = element_text(face = "bold", colour = "grey15"),
      # Titles
      plot.title = element_text(
        face = "bold", size = rel(1.15),
        colour = "grey10", hjust = 0,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = rel(0.95), colour = "grey30",
        hjust = 0, margin = margin(b = 6)
      ),
      plot.caption = element_text(
        size = rel(0.75), colour = "grey50",
        hjust = 1, margin = margin(t = 6)
      ),
      plot.margin = margin(10, 14, 8, 10)
    )
}

theme_set(theme_svn())

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Save a ggplot as both PDF and PNG
save_figure <- function(plot, name, width = 10, height = 7, dpi = 300) {
  pdf_file <- out_path(paste0(name, ".pdf"))
  png_file <- out_path(paste0(name, ".png"))
  ggsave(pdf_file, plot = plot, width = width, height = height, device = "pdf")
  ggsave(png_file, plot = plot, width = width, height = height, dpi = dpi, device = "png")
  cat(sprintf("  Saved: %s  [%.0f x %.0f in]\n", name, width, height))
}

#' Compute 95% confidence interval for a proportion (Wilson method)
prop_ci <- function(x, n, conf = 0.95) {
  if (is.na(n) || n == 0) {
    return(list(lower = NA_real_, upper = NA_real_))
  }
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  list(lower = pmax(0, centre - margin), upper = pmin(1, centre + margin))
}

#' Add Wilson CIs to a data frame with columns n_events and n_total
add_prop_ci <- function(df, n_col = "deaths", total_col = "total_admissions",
                        rate_col = "mortality_rate") {
  df %>%
    rowwise() %>%
    mutate(
      ci_res   = list(prop_ci(.data[[n_col]], .data[[total_col]])),
      ci_lower = ci_res$lower * 100,
      ci_upper = ci_res$upper * 100
    ) %>%
    ungroup() %>%
    select(-ci_res)
}

#' Standard caption for all figures
svn_caption <- function(extra = NULL) {
  base <- "KCH = Kamuzu Central Hospital, Malawi; SMCH = Sally Mugabe Children's Hospital, Zimbabwe. 2022-2025."
  if (!is.null(extra)) paste0(base, "\n", extra) else base
}

cat("00_config_and_themes.R loaded successfully.\n")

# Suppress default Rplots.pdf generation
pdf(file = NULL)

# Suppress default Rplots.pdf generation
options(device = function() { pdf(file = NULL) })
