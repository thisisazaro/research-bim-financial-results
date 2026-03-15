# ============================================================
# 10_real_options.R
# Purpose:
#   Estimate the value of managerial flexibility as a stylized
#   real option to implement an advanced BIM module at a future
#   decision date under joint OPEX-WACC uncertainty.
#
# Inputs:
#   outputs/rds/projects_mc_joint.rds
#   outputs/rds/projects_dcf_base.rds
#
# Outputs:
#   outputs/rds/projects_real_options.rds
#   outputs/rds/realopt_summary.rds
#   outputs/rds/realopt_bounds.rds
#   outputs/tables/table_35_real_option_summary.csv
#   outputs/tables/table_36_real_option_bounds.csv
#   outputs/tables/table_37_real_option_memo.csv
#   outputs/figures/figure_25_real_option_value_by_scenario.png
#
# Notes:
#   - This is a stylized real-options layer rather than a full
#     contingent-claims model.
#   - The option is modeled as the right, but not the obligation,
#     to install an advanced BIM module at year t_decision.
#   - Option payoff is floored at zero:
#       max(PV of extra savings - PV of upgrade cost, 0)
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
source("R/00_packages.R")
source("R/01_functions_general.R")
source("R/02_functions_dcf.R")
source("R/04_theme.R")

ensure_dir("outputs/rds")
ensure_dir("outputs/tables")
ensure_dir("outputs/figures")

# ------------------------------------------------------------
# 1. Load inputs
# ------------------------------------------------------------
projects_mc_joint <- readRDS("outputs/rds/projects_mc_joint.rds")
projects_dcf_base <- readRDS("outputs/rds/projects_dcf_base.rds")

data_overview(projects_mc_joint, "projects_mc_joint")
data_overview(projects_dcf_base, "projects_dcf_base")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars_mc <- c(
  "id", "project_name", "sim_id",
  "delta_opex", "wacc", "npv_sim"
)

required_vars_base <- c(
  "id", "capex_planned_musd", "term_ppp_years", "npv_base"
)

missing_mc <- required_vars_mc[!required_vars_mc %in% names(projects_mc_joint)]
missing_base <- required_vars_base[!required_vars_base %in% names(projects_dcf_base)]

if (length(missing_mc) > 0) {
  stop(
    "The following required variables are missing in projects_mc_joint:\n",
    paste0("- ", missing_mc, collapse = "\n")
  )
}

if (length(missing_base) > 0) {
  stop(
    "The following required variables are missing in projects_dcf_base:\n",
    paste0("- ", missing_base, collapse = "\n")
  )
}

message("All required variables for real-option valuation are available.")

# ------------------------------------------------------------
# 3. Decision time and scenario design
# ------------------------------------------------------------
# Stylized managerial interpretation:
# the project may decide in year 5 whether to install a more
# advanced BIM module / analytics layer.
#
# lambda:
#   additional OPEX gain as a share of the baseline BIM effect
# cost_share:
#   upgrade cost as a share of planned CAPEX

t_decision <- 5

realopt_scenarios <- tibble::tibble(
  scenario   = c("Conservative", "Central", "Optimistic"),
  lambda     = c(0.30, 0.50, 0.70),
  cost_share = c(0.03, 0.02, 0.01)
)

# ------------------------------------------------------------
# 4. Merge project characteristics into the joint MC layer
# ------------------------------------------------------------
projects_mc_joint_ext <- projects_mc_joint %>%
  dplyr::left_join(
    projects_dcf_base %>%
      dplyr::select(
        id,
        capex_planned_musd,
        term_ppp_years,
        annual_opex,
        npv_base
      ),
    by = "id"
  )
# ------------------------------------------------------------
# 5. Real-option payoff construction
# ------------------------------------------------------------
# The extra savings are modeled as an incremental stream:
#   lambda * baseline BIM OPEX effect
#
# We convert the incremental annual gain into a deferred annuity
# beginning at t_decision and compare it with the discounted
# upgrade cost.
#
# The option value per draw is:
#   payoff0 = max(PV_extra_savings0 - PV_upgrade_cost0, 0)

projects_real_options <- projects_mc_joint_ext %>%
  tidyr::crossing(realopt_scenarios) %>%
  dplyr::mutate(
    # stylized incremental annual gain
    extra_annual_gain = lambda * annual_opex * pmax(delta_opex, 0),
    
    # present value at t=0 of future extra savings
    pv_extra_savings0 = pv_deferred_annuity(
      cf_annual = extra_annual_gain,
      r = wacc,
      T_total = term_ppp_years,
      t_start = t_decision
    ),
    
    # present value at t=0 of future upgrade cost
    pv_upgrade_cost0 = cost_share * capex_planned_musd / ((1 + wacc)^t_decision),
    
    # option payoff today
    payoff0 = pmax(pv_extra_savings0 - pv_upgrade_cost0, 0)
  )

saveRDS(projects_real_options, "outputs/rds/projects_real_options.rds")

# ------------------------------------------------------------
# 6. Project-scenario summary
# ------------------------------------------------------------
realopt_summary <- projects_real_options %>%
  dplyr::group_by(id, project_name, scenario, lambda, cost_share) %>%
  dplyr::summarise(
    option_mean = mean(payoff0, na.rm = TRUE),
    option_median = median(payoff0, na.rm = TRUE),
    option_sd = sd(payoff0, na.rm = TRUE),
    option_p05 = quantile(payoff0, 0.05, na.rm = TRUE),
    option_p50 = quantile(payoff0, 0.50, na.rm = TRUE),
    option_p95 = quantile(payoff0, 0.95, na.rm = TRUE),
    prob_positive = mean(payoff0 > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(id, scenario)

saveRDS(realopt_summary, "outputs/rds/realopt_summary.rds")

print(knitr::kable(
  realopt_summary,
  digits = 3,
  caption = "Estimated real-option value by project and scenario"
))

save_csv(
  realopt_summary,
  "outputs/tables/table_35_real_option_summary.csv"
)

# ------------------------------------------------------------
# 7. Lower / central / upper bounds by project
# ------------------------------------------------------------
realopt_bounds <- realopt_summary %>%
  dplyr::group_by(project_id = id, project_name) %>%
  dplyr::summarise(
    option_min = option_mean[scenario == "Conservative"],
    option_central = option_mean[scenario == "Central"],
    option_max = option_mean[scenario == "Optimistic"],
    .groups = "drop"
  ) %>%
  dplyr::arrange(project_id)

saveRDS(realopt_bounds, "outputs/rds/realopt_bounds.rds")

print(knitr::kable(
  realopt_bounds,
  digits = 3,
  caption = "Range of real-option values across scenarios"
))

save_csv(
  realopt_bounds,
  "outputs/tables/table_36_real_option_bounds.csv"
)

# ------------------------------------------------------------
# 8. Figure: real-option value across scenarios
# ------------------------------------------------------------
figure_25_real_option_value_by_scenario <- ggplot2::ggplot(
  realopt_summary,
  ggplot2::aes(
    x = scenario,
    y = option_mean,
    ymin = option_p05,
    ymax = option_p95,
    colour = project_name
  )
) +
  ggplot2::geom_pointrange(
    position = ggplot2::position_dodge(width = 0.4)
  ) +
  ggplot2::labs(
    title = "Real-option value of managerial flexibility across projects",
    x = "Scenario for advanced BIM module",
    y = "Option value (mean, 5–95% interval)",
    colour = "PPP project"
  ) +
  theme_article +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)
  )

print(figure_25_real_option_value_by_scenario)

save_plot(
  figure_25_real_option_value_by_scenario,
  "outputs/figures/figure_25_real_option_value_by_scenario.png",
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 9. Compact memo
# ------------------------------------------------------------
realopt_memo <- tibble::tibble(
  item = c(
    "Decision time (years)",
    "Number of projects in real-option layer",
    "Mean option value: conservative scenario",
    "Mean option value: central scenario",
    "Mean option value: optimistic scenario"
  ),
  value = c(
    t_decision,
    dplyr::n_distinct(realopt_summary$id),
    mean(realopt_summary$option_mean[realopt_summary$scenario == "Conservative"], na.rm = TRUE),
    mean(realopt_summary$option_mean[realopt_summary$scenario == "Central"], na.rm = TRUE),
    mean(realopt_summary$option_mean[realopt_summary$scenario == "Optimistic"], na.rm = TRUE)
  )
)

print(knitr::kable(
  realopt_memo,
  digits = 3,
  caption = "Compact memo of the real-options layer"
))

save_csv(
  realopt_memo,
  "outputs/tables/table_37_real_option_memo.csv"
)

# ------------------------------------------------------------
# 10. Session end message
# ------------------------------------------------------------
message("10_real_options.R finished successfully.")
