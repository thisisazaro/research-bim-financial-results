# ============================================================
# 11_value_decomposition.R
# Purpose:
#   Decompose total BIM-related project value into:
#   (1) baseline NPV,
#   (2) value efficiency from OPEX improvements,
#   (3) value of risk reduction through lower WACC, and
#   (4) value of managerial flexibility (real option).
#
# Inputs:
#   outputs/rds/projects_dcf_base.rds
#   outputs/rds/value_efficiency_df.rds
#   outputs/rds/projects_wacc_actual.rds
#   outputs/rds/realopt_bounds.rds
#
# Outputs:
#   outputs/rds/total_value_decomposition.rds
#   outputs/tables/table_38_total_value_decomposition.csv
#   outputs/tables/table_39_total_value_summary.csv
#   outputs/tables/table_40_value_decomposition_memo.csv
#   outputs/figures/figure_26_value_decomposition_by_project.png
#   outputs/figures/figure_27_total_value_vs_baseline.png
#
# Notes:
#   - This script is the final synthesis layer of the thesis.
#   - It combines deterministic and stochastic valuation outputs
#     into a compact project-level decomposition.
#   - The central real-option estimate is used as the default
#     managerial flexibility component.
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
source("R/00_packages.R")
source("R/01_functions_general.R")
source("R/04_theme.R")

ensure_dir("outputs/rds")
ensure_dir("outputs/tables")
ensure_dir("outputs/figures")

# ------------------------------------------------------------
# 1. Load valuation layers
# ------------------------------------------------------------
projects_dcf_base <- readRDS("outputs/rds/projects_dcf_base.rds")
value_efficiency_df <- readRDS("outputs/rds/value_efficiency_df.rds")
projects_wacc_actual <- readRDS("outputs/rds/projects_wacc_actual.rds")
realopt_bounds <- readRDS("outputs/rds/realopt_bounds.rds")

data_overview(projects_dcf_base, "projects_dcf_base")
data_overview(value_efficiency_df, "value_efficiency_df")
data_overview(projects_wacc_actual, "projects_wacc_actual")
data_overview(realopt_bounds, "realopt_bounds")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars_base <- c("id", "project_name", "npv_base")
required_vars_eff  <- c("project_id", "value_efficiency")
required_vars_wacc <- c("id", "npv_gain_wacc")
required_vars_real <- c("project_id", "option_central")

missing_base <- required_vars_base[!required_vars_base %in% names(projects_dcf_base)]
missing_eff  <- required_vars_eff[!required_vars_eff %in% names(value_efficiency_df)]
missing_wacc <- required_vars_wacc[!required_vars_wacc %in% names(projects_wacc_actual)]
missing_real <- required_vars_real[!required_vars_real %in% names(realopt_bounds)]

if (length(missing_base) > 0) {
  stop(
    "The following required variables are missing in projects_dcf_base:\n",
    paste0("- ", missing_base, collapse = "\n")
  )
}

if (length(missing_eff) > 0) {
  stop(
    "The following required variables are missing in value_efficiency_df:\n",
    paste0("- ", missing_eff, collapse = "\n")
  )
}

if (length(missing_wacc) > 0) {
  stop(
    "The following required variables are missing in projects_wacc_actual:\n",
    paste0("- ", missing_wacc, collapse = "\n")
  )
}

if (length(missing_real) > 0) {
  stop(
    "The following required variables are missing in realopt_bounds:\n",
    paste0("- ", missing_real, collapse = "\n")
  )
}

message("All required valuation layers are available for decomposition.")

# ------------------------------------------------------------
# 3. Build project-level decomposition table
# ------------------------------------------------------------
# Components:
#   - baseline NPV
#   - value_efficiency: from stochastic OPEX analysis
#   - value_risk_reduction: from actual lower-WACC scenario
#   - value_realoption_central: central scenario from real-options

total_value_decomposition <- projects_dcf_base %>%
  dplyr::select(
    project_id = id,
    project_name,
    npv_base
  ) %>%
  dplyr::left_join(
    value_efficiency_df,
    by = c("project_id", "project_name")
  ) %>%
  dplyr::left_join(
    projects_wacc_actual %>%
      dplyr::transmute(
        project_id = id,
        value_risk_reduction = npv_gain_wacc
      ),
    by = "project_id"
  ) %>%
  dplyr::left_join(
    realopt_bounds %>%
      dplyr::transmute(
        project_id,
        value_realoption_central = option_central,
        value_realoption_min = option_min,
        value_realoption_max = option_max
      ),
    by = "project_id"
  ) %>%
  dplyr::mutate(
    value_efficiency = dplyr::coalesce(value_efficiency, 0),
    value_risk_reduction = dplyr::coalesce(value_risk_reduction, 0),
    value_realoption_central = dplyr::coalesce(value_realoption_central, 0),
    value_realoption_min = dplyr::coalesce(value_realoption_min, 0),
    value_realoption_max = dplyr::coalesce(value_realoption_max, 0),
    
    total_value_central = npv_base +
      value_efficiency +
      value_risk_reduction +
      value_realoption_central,
    
    total_value_min = npv_base +
      value_efficiency +
      value_risk_reduction +
      value_realoption_min,
    
    total_value_max = npv_base +
      value_efficiency +
      value_risk_reduction +
      value_realoption_max
  ) %>%
  dplyr::arrange(project_id)

saveRDS(
  total_value_decomposition,
  "outputs/rds/total_value_decomposition.rds"
)

# ------------------------------------------------------------
# 4. Main decomposition table
# ------------------------------------------------------------
table_38_total_value_decomposition <- total_value_decomposition %>%
  dplyr::select(
    project_id,
    project_name,
    npv_base,
    value_efficiency,
    value_risk_reduction,
    value_realoption_central,
    total_value_central,
    total_value_min,
    total_value_max
  )

print(knitr::kable(
  table_38_total_value_decomposition,
  digits = 2,
  caption = "Decomposition of BIM-related project value"
))

save_csv(
  table_38_total_value_decomposition,
  "outputs/tables/table_38_total_value_decomposition.csv"
)

# ------------------------------------------------------------
# 5. Summary across projects
# ------------------------------------------------------------
table_39_total_value_summary <- tibble::tibble(
  metric = c(
    "Mean baseline NPV",
    "Mean value efficiency",
    "Mean value of risk reduction",
    "Mean central real-option value",
    "Mean total value (central)",
    "Mean total value (lower bound)",
    "Mean total value (upper bound)"
  ),
  value = c(
    mean(total_value_decomposition$npv_base, na.rm = TRUE),
    mean(total_value_decomposition$value_efficiency, na.rm = TRUE),
    mean(total_value_decomposition$value_risk_reduction, na.rm = TRUE),
    mean(total_value_decomposition$value_realoption_central, na.rm = TRUE),
    mean(total_value_decomposition$total_value_central, na.rm = TRUE),
    mean(total_value_decomposition$total_value_min, na.rm = TRUE),
    mean(total_value_decomposition$total_value_max, na.rm = TRUE)
  )
)

print(knitr::kable(
  table_39_total_value_summary,
  digits = 2,
  caption = "Summary of BIM value decomposition across PPP projects"
))

save_csv(
  table_39_total_value_summary,
  "outputs/tables/table_39_total_value_summary.csv"
)

# ------------------------------------------------------------
# 6. Figure: decomposition by project
# ------------------------------------------------------------
# We plot only the incremental BIM-related value components
# stacked above zero, separately from baseline NPV.

decomp_plot_long <- total_value_decomposition %>%
  dplyr::select(
    project_name,
    value_efficiency,
    value_risk_reduction,
    value_realoption_central
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      value_efficiency,
      value_risk_reduction,
      value_realoption_central
    ),
    names_to = "component",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    component = factor(
      component,
      levels = c(
        "value_efficiency",
        "value_risk_reduction",
        "value_realoption_central"
      ),
      labels = c(
        "Efficiency value",
        "Risk reduction value",
        "Real-option value"
      )
    )
  )

figure_26_value_decomposition_by_project <- ggplot2::ggplot(
  decomp_plot_long,
  ggplot2::aes(x = project_name, y = value, fill = component)
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Decomposition of incremental BIM-related value by project",
    x = "PPP project",
    y = "Incremental value",
    fill = NULL
  ) +
  theme_article +
  ggplot2::theme(legend.position = "bottom")

print(figure_26_value_decomposition_by_project)

save_plot(
  figure_26_value_decomposition_by_project,
  "outputs/figures/figure_26_value_decomposition_by_project.png",
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 7. Figure: total value versus baseline NPV
# ------------------------------------------------------------
total_vs_base_long <- total_value_decomposition %>%
  dplyr::select(
    project_name,
    npv_base,
    total_value_central
  ) %>%
  tidyr::pivot_longer(
    cols = c(npv_base, total_value_central),
    names_to = "value_type",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    value_type = factor(
      value_type,
      levels = c("npv_base", "total_value_central"),
      labels = c("Baseline NPV", "Total value (central)")
    )
  )

figure_27_total_value_vs_baseline <- ggplot2::ggplot(
  total_vs_base_long,
  ggplot2::aes(
    x = reorder(project_name, value),
    y = value,
    fill = value_type
  )
) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Baseline NPV versus total BIM-related project value",
    x = "PPP project",
    y = "Value",
    fill = NULL
  ) +
  theme_article +
  ggplot2::theme(legend.position = "bottom")

print(figure_27_total_value_vs_baseline)

save_plot(
  figure_27_total_value_vs_baseline,
  "outputs/figures/figure_27_total_value_vs_baseline.png",
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 8. Compact memo of the final synthesis layer
# ------------------------------------------------------------
table_40_value_decomposition_memo <- tibble::tibble(
  item = c(
    "Number of projects in final decomposition",
    "Mean baseline NPV",
    "Mean efficiency value",
    "Mean value of risk reduction",
    "Mean central real-option value",
    "Mean total value (central)"
  ),
  value = c(
    nrow(total_value_decomposition),
    mean(total_value_decomposition$npv_base, na.rm = TRUE),
    mean(total_value_decomposition$value_efficiency, na.rm = TRUE),
    mean(total_value_decomposition$value_risk_reduction, na.rm = TRUE),
    mean(total_value_decomposition$value_realoption_central, na.rm = TRUE),
    mean(total_value_decomposition$total_value_central, na.rm = TRUE)
  )
)

print(knitr::kable(
  table_40_value_decomposition_memo,
  digits = 2,
  caption = "Compact memo of the final BIM value synthesis layer"
))

save_csv(
  table_40_value_decomposition_memo,
  "outputs/tables/table_40_value_decomposition_memo.csv"
)

# ------------------------------------------------------------
# 9. Session end message
# ------------------------------------------------------------
message("11_value_decomposition.R finished successfully.")

