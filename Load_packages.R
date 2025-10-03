## ---------------------------
##
## Script name: Load_Packages
##
## Purpose of script: Install & load packages needed to run the analyses
##
## Author: Owen Williams
## Date Created: 19/09/2025
## Last Updated: 22/09/2025
##
## ---------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))

# Minimal top-level packages referenced by ui.R / server.R
# (Dependencies like rlang/scales come in automatically via tidyverse/ggplot2)

packages = c(
  # "formattable",      # dynamic tables
  "tidyverse",  # tidy data.
  "readxl",     # read_excel for .xls/.xlsx
  "afcharts",   # standardise figures
  "tsibble"   # standardise figures
)

# Install any that are missing
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

message("All required packages are installed. You can now run the reports.")
