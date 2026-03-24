# ==============================================================================
# 00_setup_packages.R
# Run this script ONCE to install all required packages for this project.
# It checks your system and only downloads packages you don't already have.
# ==============================================================================

cat("Checking for missing packages...\n")

# List of all packages used across the graph scripts
required_packages <- c(
    "dplyr",
    "forcats",
    "ggplot2",
    "gtsummary",
    "patchwork",
    "readr",
    "scales",
    "survival",
    "tidyr",
    "purrr"
)

# Identify which packages are not currently installed
installed <- rownames(installed.packages())
missing_packages <- required_packages[!(required_packages %in% installed)]

# Install missing packages
if (length(missing_packages) > 0) {
    cat(sprintf("Installing %d missing packages. This may take a moment...\n", length(missing_packages)))
    cat(paste("  -", missing_packages, collapse = "\n"), "\n")

    install.packages(missing_packages)

    cat("\nInstallation complete!\n")
} else {
    cat("All required packages are already installed. You're good to go!\n")
}
