# SVN Figure Generation Scripts: User Guide

This directory contains the necessary R scripts to generate the complete series of Small Vulnerable Newborn (SVN) figures (Figures 1-7) for KCH (Malawi) and SMCH (Zimbabwe).

## Prerequisites

Before generating any figures, you must ensure that your R environment has the correct packages installed.

1. Open your R console, RStudio, or terminal.
2. Set your working directory to the `graphs` folder:

   ```R
   setwd("/path/to/.../graphs")
   ```

3. Run the setup script to automatically install and load all required packages (such as `ggplot2`, `dplyr`, `patchwork`, `purrr`, etc.):

   ```R
   source("00_setup_packages.R")
   ```

## Expected Data Structure

The scripts rely on input data generated from upstream analytical pipelines. By default, `00_config_and_themes.R` expects the following aligned directories to exist relative to your working directory:

- `Results_maternal_analyses/` (contains maternal outcomes and births data)
- `Results_NNU_analyses/` (contains neonatal admission and mortality data)

*Note: The figure scripts are configured to use SVN classifications derived from **all** available gestational age estimation methods (there are no longer separate ultrasound-only comparisons).*

## How to Generate All Figures at Once (Recommended)

The easiest way to generate the entire suite of figures (1 through 7) cleanly is to use the master runner script. This script executes each figure file sequentially and outputs a summary log.

In your R console, run:

```R
source("run_all_graphs.R")
```

**What this does:**

- It loads `00_config_and_themes.R` which sets up the standard colors, themes, font settings, and data paths.
- It iterates through `fig1` to `fig7`, generating each plot.
- It prints a final summary to the console confirming which figures completed successfully and if there were any issues.
- It automatically saves all generated graphs to the `outputs/` folder.

## How to Generate Individual Figures

If you only need to update or review a specific figure, you can run its standalone script directly.

For example, to generate only Figure 6:

```R
source("fig6_day_of_death.R")
```

Each individual script automatically sources the configuration file (`00_config_and_themes.R`) and saves its own results directly to the `outputs/` folder upon completion.

## Understanding the Outputs

All generated figures are saved in the **`outputs/`** directory.

For every figure script run, two high-resolution file formats are created automatically:

1. **PNG (`.png`)**: Best for quickly previewing the charts or embedding them in presentations (e.g., PowerPoint).
2. **PDF (`.pdf`)**: A vector-based format ideal for publication, printing, or zooming in without losing any quality or sharpness.

*Note: The dimensions (width x height) and theme configurations for each output file are carefully predefined within the individual `figX_` scripts.*

## Troubleshooting

- **Missing Data or Path Errors:** If you encounter `file not found` errors, verify that the `00_config_and_themes.R` script is pointing to the correct root data directory.
- **Missing Packages:** Ensure you have run `source("00_setup_packages.R")` first. If an error persists regarding a missing package like `purrr` or `patchwork`, simply run `install.packages("package_name")`.
