# ============================================================
# 00_packages.R
# Purpose:
#   Check, load, and report all R packages required for the
#   BIM / PPP financial analysis project.
#
# Notes:
#   - This script should be sourced at the beginning of each
#     main analysis file.
#   - If packages are missing, the script stops and shows
#     which ones must be installed.
# ============================================================

# ------------------------------------------------------------
# 1. Define required packages
# ------------------------------------------------------------
required_packages <- c(
  "readxl",
  "dplyr",
  "janitor",
  "ggplot2",
  "lubridate",
  "kableExtra",
  "tidyr",
  "scales",
  "stringr",
  "knitr",
  "broom",
  "purrr",
  "tibble",
  "forcats",
  "MASS"
)

# ------------------------------------------------------------
# 2. Check whether all packages are installed
# ------------------------------------------------------------
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "The following required packages are not installed:",
      paste(missing_packages, collapse = ", "),
      "\n\nPlease install them first, for example:\n",
      "install.packages(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))",
      sep = " "
    )
  )
}

# ------------------------------------------------------------
# 3. Load packages
# ------------------------------------------------------------
invisible(lapply(required_packages, library, character.only = TRUE))

# ------------------------------------------------------------
# 4. Session options for cleaner output
# ------------------------------------------------------------
options(
  stringsAsFactors = FALSE,
  scipen = 999
)

message("All required packages loaded successfully.")
