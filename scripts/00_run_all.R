# ============================================================
# 00_run_all.R
# Purpose:
#   Run the full BIM / PPP research pipeline from raw-data
#   loading to final value decomposition.
#
# Notes:
#   Execute this script from the project root directory.
#   Expected project structure:
#   - R/
#   - scripts/
#   - data/raw/
#   - outputs/
# ============================================================

# ------------------------------------------------------------
# 0. Core setup
# ------------------------------------------------------------
source("R/00_packages.R")
source("R/01_functions_general.R")
source("R/02_functions_dcf.R")
source("R/03_functions_plots.R")
source("R/04_theme.R")

message("Core project functions loaded successfully.")

# ------------------------------------------------------------
# 1. Data loading and cleaning
# ------------------------------------------------------------
source("scripts/01_load_and_clean_data.R")

# ------------------------------------------------------------
# 2. Descriptive layer
# ------------------------------------------------------------
source("scripts/02_descriptive_analysis.R")

# ------------------------------------------------------------
# 3. Baseline DCF construction
# ------------------------------------------------------------
source("scripts/03_project_level_opex_and_dcf.R")

# ------------------------------------------------------------
# 4. BIM effect calibration
# ------------------------------------------------------------
source("scripts/04_bim_effect_calibration.R")

# ------------------------------------------------------------
# 5. Deterministic scenario analysis
# ------------------------------------------------------------
source("scripts/05_scenario_analysis.R")

# ------------------------------------------------------------
# 6. Monte Carlo: OPEX only
# ------------------------------------------------------------
source("scripts/06_monte_carlo_opex.R")

# ------------------------------------------------------------
# 7. Monte Carlo: joint OPEX + WACC
# ------------------------------------------------------------
source("scripts/07_monte_carlo_opex_wacc.R")

# ------------------------------------------------------------
# 8. WACC sensitivity layer
# ------------------------------------------------------------
source("scripts/08_wacc_sensitivity.R")

# ------------------------------------------------------------
# 9. Meta-regression layer
# ------------------------------------------------------------
source("scripts/09_meta_regression.R")

# ------------------------------------------------------------
# 10. Real-options layer
# ------------------------------------------------------------
source("scripts/10_real_options.R")

# ------------------------------------------------------------
# 11. Final value decomposition
# ------------------------------------------------------------
source("scripts/11_value_decomposition.R")

# ------------------------------------------------------------
# 12. Completion message
# ------------------------------------------------------------
message("Full research pipeline finished successfully.")
