# ==============================================================================
# run_all_graphs.R
# MASTER RUNNER — SVN Figure Series
# KCH (Malawi) and SMCH (Zimbabwe), 2022–2025
#
# Usage:
#   Run from within the graphs/ directory:
#     setwd(".../00-simple_questions_DEF_DEF_for_Graphs/graphs")
#     source("run_all_graphs.R")
#
#   Or from the terminal:
#     Rscript run_all_graphs.R
#
# Outputs: PDF and PNG figures saved to graphs/outputs/
# ==============================================================================

cat("==============================================================\n")
cat("  SVN Figure Series — Master Runner\n")
cat("  KCH (Malawi) and SMCH (Zimbabwe), 2022–2025\n")
cat("==============================================================\n\n")

# ------------------------------------------------------------------------------
# SET WORKING DIRECTORY
# Ensure we are in the graphs/ directory regardless of how the script is invoked
# ------------------------------------------------------------------------------
this_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) {
    # When run via Rscript, use the script's own path
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- args[grepl("--file=", args)]
    if (length(file_arg) > 0) {
      dirname(normalizePath(sub("--file=", "", file_arg)))
    } else {
      getwd()  # fallback
    }
  }
)

if (!is.null(this_dir) && nzchar(this_dir) && this_dir != ".") {
  setwd(this_dir)
}

cat("Working directory:", getwd(), "\n\n")

# ------------------------------------------------------------------------------
# REQUIRED PACKAGES — install any that are missing
# ------------------------------------------------------------------------------
required_packages <- c(
  "tidyverse",   # ggplot2, dplyr, tidyr, readr, stringr, forcats, purrr
  "scales",
  "patchwork"
)

missing_pkg <- required_packages[!sapply(required_packages, requireNamespace,
                                          quietly = TRUE)]
if (length(missing_pkg) > 0) {
  cat("Installing missing packages:", paste(missing_pkg, collapse = ", "), "\n")
  install.packages(missing_pkg, repos = "https://cloud.r-project.org")
}

# ------------------------------------------------------------------------------
# FIGURE SCRIPTS
# ------------------------------------------------------------------------------
figure_scripts <- c(
  "fig1_maternal_svn_zim.R",
  "fig2_stillbirth_risk_zim.R",
  "fig3_nnu_svn_prevalence.R",
  "fig4_length_of_stay.R",
  "fig5_case_fatality_rates.R",
  "fig6_day_of_death.R",
  "fig7_risk_factors.R"
)

# ------------------------------------------------------------------------------
# RUN EACH FIGURE IN SEQUENCE
# Errors in one figure are caught and reported, but do not abort the rest
# ------------------------------------------------------------------------------
results <- list()

for (script in figure_scripts) {
  cat("\n--------------------------------------------------------------\n")
  cat("Running:", script, "\n")
  cat("--------------------------------------------------------------\n")

  result <- tryCatch({
    source(script, local = FALSE)
    list(script = script, status = "OK", error = NULL)
  }, error = function(e) {
    cat("\n*** ERROR in", script, "***\n")
    cat(conditionMessage(e), "\n")
    list(script = script, status = "ERROR", error = conditionMessage(e))
  }, warning = function(w) {
    # Re-run with warnings suppressed for cleaner output — warnings are
    # non-fatal and typically relate to missing values in scales / CI
    suppressWarnings(source(script, local = FALSE))
    list(script = script, status = "OK (with warnings)", error = conditionMessage(w))
  })

  results[[script]] <- result
}

# ------------------------------------------------------------------------------
# SUMMARY REPORT
# ------------------------------------------------------------------------------
cat("\n\n==============================================================\n")
cat("  SUMMARY\n")
cat("==============================================================\n")

ok    <- sum(sapply(results, function(r) r$status %in% c("OK", "OK (with warnings)")))
errs  <- sum(sapply(results, function(r) r$status == "ERROR"))

for (r in results) {
  icon <- if (r$status == "ERROR") "[FAIL]" else "[OK]  "
  cat(sprintf("  %s %s\n", icon, r$script))
  if (!is.null(r$error)) {
    cat(sprintf("         Note: %s\n", r$error))
  }
}

cat("\n")
cat(sprintf("  %d / %d figures completed successfully.\n", ok, length(results)))

if (errs > 0) {
  cat(sprintf("  %d figure(s) encountered errors — see messages above.\n", errs))
}

output_path <- normalizePath("outputs", mustWork = FALSE)
cat(sprintf("\n  Output folder: %s\n", output_path))
cat("==============================================================\n\n")
