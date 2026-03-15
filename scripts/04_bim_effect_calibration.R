# ============================================================
# 04_bim_effect_calibration.R
# Purpose:
#   Calibrate BIM-related effect sizes from the survey-based
#   and literature-based evidence datasets, with a primary
#   focus on OPEX-relevant effects for later DCF scenarios.
#
# Inputs:
#   outputs/rds/ds3_clean.rds
#   outputs/rds/ds4_clean.rds
#
# Outputs:
#   outputs/rds/effects_ds3_summary.rds
#   outputs/rds/effects_ds4_summary.rds
#   outputs/rds/opex_effects_literature.rds
#   outputs/rds/opex_scenarios.rds
#   outputs/tables/table_09_ds3_effects_summary.csv
#   outputs/tables/table_10_ds4_effects_summary.csv
#   outputs/tables/table_11_opex_effect_distribution.csv
#   outputs/tables/table_12_opex_scenarios.csv
#
# Notes:
#   - Dataset 3 contains company survey evidence.
#   - Dataset 4 contains literature-based evidence.
#   - The primary calibration target in this script is the
#     relative BIM effect on OPEX-related DCF channels.
#   - These calibrated scenarios will later feed into
#     deterministic and stochastic DCF analysis.
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
source("R/00_packages.R")
source("R/01_functions_general.R")

ensure_dir("outputs/rds")
ensure_dir("outputs/tables")

# ------------------------------------------------------------
# 1. Load processed evidence datasets
# ------------------------------------------------------------
ds3_clean <- readRDS("outputs/rds/ds3_clean.rds")
ds4_clean <- readRDS("outputs/rds/ds4_clean.rds")

data_overview(ds3_clean, "ds3_clean")
data_overview(ds4_clean, "ds4_clean")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars_ds3 <- c(
  "id", "company", "country", "stage", "stage_clean",
  "unified_metric", "score_to_the_unified_metric", "dcf_influence"
)

required_vars_ds4 <- c(
  "id", "country", "stage", "stage_clean",
  "unified_metric", "score_to_the_unified_metric", "dcf_influence"
)

missing_ds3 <- required_vars_ds3[!required_vars_ds3 %in% names(ds3_clean)]
missing_ds4 <- required_vars_ds4[!required_vars_ds4 %in% names(ds4_clean)]

if (length(missing_ds3) > 0) {
  stop(
    "The following required variables are missing in ds3_clean:\n",
    paste0("- ", missing_ds3, collapse = "\n")
  )
}

if (length(missing_ds4) > 0) {
  stop(
    "The following required variables are missing in ds4_clean:\n",
    paste0("- ", missing_ds4, collapse = "\n")
  )
}

message("All required variables for BIM effect calibration are available.")

# ------------------------------------------------------------
# 3. Summary of BIM effects: company survey evidence (ds3)
# ------------------------------------------------------------
effects_ds3_summary <- ds3_clean %>%
  dplyr::group_by(stage_clean, dcf_influence, unified_metric) %>%
  dplyr::summarise(
    n_obs        = dplyr::n(),
    mean_score   = mean(score_to_the_unified_metric, na.rm = TRUE),
    median_score = median(score_to_the_unified_metric, na.rm = TRUE),
    sd_score     = sd(score_to_the_unified_metric, na.rm = TRUE),
    min_score    = min(score_to_the_unified_metric, na.rm = TRUE),
    max_score    = max(score_to_the_unified_metric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(stage_clean, dcf_influence, unified_metric)

saveRDS(effects_ds3_summary, "outputs/rds/effects_ds3_summary.rds")

print(knitr::kable(
  effects_ds3_summary,
  digits = 4,
  caption = "Summary of BIM effect estimates from the company survey dataset"
))

save_csv(
  effects_ds3_summary,
  "outputs/tables/table_09_ds3_effects_summary.csv"
)

# ------------------------------------------------------------
# 4. Summary of BIM effects: literature evidence (ds4)
# ------------------------------------------------------------
effects_ds4_summary <- ds4_clean %>%
  dplyr::group_by(stage_clean, dcf_influence, unified_metric) %>%
  dplyr::summarise(
    n_obs        = dplyr::n(),
    mean_score   = mean(score_to_the_unified_metric, na.rm = TRUE),
    median_score = median(score_to_the_unified_metric, na.rm = TRUE),
    sd_score     = sd(score_to_the_unified_metric, na.rm = TRUE),
    min_score    = min(score_to_the_unified_metric, na.rm = TRUE),
    max_score    = max(score_to_the_unified_metric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(stage_clean, dcf_influence, unified_metric)

saveRDS(effects_ds4_summary, "outputs/rds/effects_ds4_summary.rds")

print(knitr::kable(
  effects_ds4_summary,
  digits = 4,
  caption = "Summary of BIM effect estimates from the literature-based dataset"
))

save_csv(
  effects_ds4_summary,
  "outputs/tables/table_10_ds4_effects_summary.csv"
)

# ------------------------------------------------------------
# 5. Extract OPEX-relevant BIM effects from the literature
# ------------------------------------------------------------
# The thesis DCF scenario later uses OPEX savings as the main
# transmission channel from BIM to operating cash flows.
#
# We therefore isolate literature-based evidence linked to:
#   - Maintenance obligations
#   - Salary
# within the Operation stage.

opex_effects_literature <- ds4_clean %>%
  dplyr::filter(
    stage_clean == "Operation",
    dcf_influence %in% c("Maintenance obligations", "Salary")
  ) %>%
  dplyr::filter(!is.na(score_to_the_unified_metric)) %>%
  dplyr::arrange(dcf_influence, unified_metric)

if (nrow(opex_effects_literature) == 0) {
  stop("No literature-based OPEX effect observations were found in ds4_clean.")
}

saveRDS(
  opex_effects_literature,
  "outputs/rds/opex_effects_literature.rds"
)

# ------------------------------------------------------------
# 6. Distribution diagnostics for OPEX-related BIM effects
# ------------------------------------------------------------
table_11_opex_effect_distribution <- opex_effects_literature %>%
  dplyr::summarise(
    n_obs         = dplyr::n(),
    mean_effect   = mean(score_to_the_unified_metric, na.rm = TRUE),
    median_effect = median(score_to_the_unified_metric, na.rm = TRUE),
    sd_effect     = sd(score_to_the_unified_metric, na.rm = TRUE),
    min_effect    = min(score_to_the_unified_metric, na.rm = TRUE),
    p25_effect    = quantile(score_to_the_unified_metric, 0.25, na.rm = TRUE),
    p50_effect    = quantile(score_to_the_unified_metric, 0.50, na.rm = TRUE),
    p75_effect    = quantile(score_to_the_unified_metric, 0.75, na.rm = TRUE),
    max_effect    = max(score_to_the_unified_metric, na.rm = TRUE)
  )

print(knitr::kable(
  table_11_opex_effect_distribution,
  digits = 4,
  caption = "Distribution of literature-based BIM effects linked to OPEX channels"
))

save_csv(
  table_11_opex_effect_distribution,
  "outputs/tables/table_11_opex_effect_distribution.csv"
)

# ------------------------------------------------------------
# 7. Calibrate low / base / high OPEX scenarios
# ------------------------------------------------------------
# Calibration logic:
#   - low  = 25th percentile
#   - base = median
#   - high = 75th percentile
#
# This yields a simple, transparent scenario family for later
# DCF and Monte Carlo modeling.

delta_opex_low <- as.numeric(
  quantile(
    opex_effects_literature$score_to_the_unified_metric,
    probs = 0.25,
    na.rm = TRUE
  )
)

delta_opex_base <- as.numeric(
  median(
    opex_effects_literature$score_to_the_unified_metric,
    na.rm = TRUE
  )
)

delta_opex_high <- as.numeric(
  quantile(
    opex_effects_literature$score_to_the_unified_metric,
    probs = 0.75,
    na.rm = TRUE
  )
)

opex_scenarios <- tibble::tibble(
  scenario = c("low", "base", "high"),
  delta_opex = c(delta_opex_low, delta_opex_base, delta_opex_high),
  interpretation = c(
    "Conservative OPEX effect (25th percentile)",
    "Central OPEX effect (median)",
    "Optimistic OPEX effect (75th percentile)"
  )
)

saveRDS(opex_scenarios, "outputs/rds/opex_scenarios.rds")

print(knitr::kable(
  opex_scenarios,
  digits = 4,
  caption = "Calibrated BIM effect scenarios for OPEX reduction"
))

save_csv(
  opex_scenarios,
  "outputs/tables/table_12_opex_scenarios.csv"
)

# ------------------------------------------------------------
# 8. Optional pooled comparison: survey vs literature
# ------------------------------------------------------------
# This is a compact diagnostic layer showing how the two
# evidence bases compare at a high level.

source_comparison <- dplyr::bind_rows(
  ds3_clean %>%
    dplyr::transmute(
      source_type = "Company survey",
      stage_clean,
      dcf_influence,
      unified_metric,
      score_to_the_unified_metric
    ),
  ds4_clean %>%
    dplyr::transmute(
      source_type = "Literature",
      stage_clean,
      dcf_influence,
      unified_metric,
      score_to_the_unified_metric
    )
) %>%
  dplyr::group_by(source_type) %>%
  dplyr::summarise(
    n_obs        = dplyr::n(),
    mean_effect  = mean(score_to_the_unified_metric, na.rm = TRUE),
    median_effect = median(score_to_the_unified_metric, na.rm = TRUE),
    sd_effect    = sd(score_to_the_unified_metric, na.rm = TRUE),
    .groups = "drop"
  )

print(knitr::kable(
  source_comparison,
  digits = 4,
  caption = "High-level comparison of BIM effect distributions across evidence sources"
))

save_csv(
  source_comparison,
  "outputs/tables/table_13_source_comparison.csv"
)

# ------------------------------------------------------------
# 9. Compact calibration memo
# ------------------------------------------------------------
calibration_memo <- tibble::tibble(
  item = c(
    "Number of survey-based observations",
    "Number of literature-based observations",
    "Number of OPEX-relevant literature observations",
    "Calibrated low OPEX effect",
    "Calibrated base OPEX effect",
    "Calibrated high OPEX effect"
  ),
  value = c(
    nrow(ds3_clean),
    nrow(ds4_clean),
    nrow(opex_effects_literature),
    delta_opex_low,
    delta_opex_base,
    delta_opex_high
  )
)

print(knitr::kable(
  calibration_memo,
  digits = 4,
  caption = "Compact memo of the BIM effect calibration stage"
))

save_csv(
  calibration_memo,
  "outputs/tables/table_14_calibration_memo.csv"
)

# ------------------------------------------------------------
# 10. Session end message
# ------------------------------------------------------------
message("04_bim_effect_calibration.R finished successfully.")
