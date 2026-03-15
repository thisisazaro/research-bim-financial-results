# ============================================================
# 05_scenario_analysis.R
# Purpose:
#   Apply calibrated BIM-related OPEX reduction scenarios to
#   baseline PPP project DCFs and estimate value effects under:
#   (1) actual BIM adoption in operation, and
#   (2) hypothetical BIM adoption across all PPP projects.
#
# Inputs:
#   outputs/rds/projects_dcf_base.rds
#   outputs/rds/opex_scenarios.rds
#
# Outputs:
#   outputs/rds/projects_dcf_actual_scenarios.rds
#   outputs/rds/projects_dcf_hypothetical_scenarios.rds
#   outputs/tables/table_15_actual_scenario_project_results.csv
#   outputs/tables/table_16_hypothetical_scenario_project_results.csv
#   outputs/tables/table_17_actual_scenario_summary.csv
#   outputs/tables/table_18_hypothetical_scenario_summary.csv
#   outputs/figures/figure_04_npv_actual_scenarios.png
#   outputs/figures/figure_05_npv_hypothetical_scenarios.png
#
# Notes:
#   - "Actual" scenario:
#       OPEX reduction is applied only to projects with
#       BIM in the operation phase.
#   - "Hypothetical" scenario:
#       OPEX reduction is applied to all PPP projects with
#       available operating data.
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
# 1. Load project-level DCF data and calibrated scenarios
# ------------------------------------------------------------
projects_dcf_base <- readRDS("outputs/rds/projects_dcf_base.rds")
opex_scenarios <- readRDS("outputs/rds/opex_scenarios.rds")

data_overview(projects_dcf_base, "projects_dcf_base")
data_overview(opex_scenarios, "opex_scenarios")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars_projects <- c(
  "id", "project_name", "bim_operation",
  "capex_planned_musd", "term_ppp_years",
  "annual_revenue", "annual_opex", "annual_net_cf",
  "npv_base"
)

required_vars_scenarios <- c(
  "scenario", "delta_opex"
)

missing_projects <- required_vars_projects[
  !required_vars_projects %in% names(projects_dcf_base)
]

missing_scenarios <- required_vars_scenarios[
  !required_vars_scenarios %in% names(opex_scenarios)
]

if (length(missing_projects) > 0) {
  stop(
    "The following required variables are missing in projects_dcf_base:\n",
    paste0("- ", missing_projects, collapse = "\n")
  )
}

if (length(missing_scenarios) > 0) {
  stop(
    "The following required variables are missing in opex_scenarios:\n",
    paste0("- ", missing_scenarios, collapse = "\n")
  )
}

message("All required variables for scenario analysis are available.")

# ------------------------------------------------------------
# 3. Restrict to valid DCF observations
# ------------------------------------------------------------
# We keep only projects with non-missing annual OPEX, net CF,
# CAPEX, and PPP term, since these are required for scenario NPV.

projects_input <- projects_dcf_base %>%
  dplyr::filter(
    !is.na(annual_opex),
    !is.na(annual_revenue),
    !is.na(annual_net_cf),
    !is.na(capex_planned_musd),
    !is.na(term_ppp_years),
    !is.na(npv_base)
  )

if (nrow(projects_input) == 0) {
  stop("No valid project observations are available for scenario analysis.")
}

# ------------------------------------------------------------
# 4. Baseline discount rate
# ------------------------------------------------------------
# For consistency with the baseline DCF layer, we use the same
# fixed discount rate as in 03_project_level_opex_and_dcf.R.

r_base <- 0.08

# ------------------------------------------------------------
# 5. Actual scenario:
#    apply OPEX savings only where BIM is present in operation
# ------------------------------------------------------------
projects_dcf_actual_scenarios <- projects_input %>%
  tidyr::crossing(opex_scenarios) %>%
  dplyr::mutate(
    annual_opex_scenario = dplyr::if_else(
      bim_operation & !is.na(annual_opex),
      annual_opex * (1 - delta_opex),
      annual_opex
    ),
    annual_net_cf_scenario = annual_revenue - annual_opex_scenario,
    npv_scenario = npv_annuity(
      capex         = capex_planned_musd,
      annual_net_cf = annual_net_cf_scenario,
      T_years       = term_ppp_years,
      r             = r_base
    ),
    npv_gain = npv_scenario - npv_base,
    scenario_type = "Actual BIM adoption"
  )

saveRDS(
  projects_dcf_actual_scenarios,
  "outputs/rds/projects_dcf_actual_scenarios.rds"
)

table_15_actual_scenario_project_results <- projects_dcf_actual_scenarios %>%
  dplyr::select(
    id, project_name, scenario, interpretation,
    bim_operation,
    annual_revenue, annual_opex, annual_opex_scenario,
    annual_net_cf, annual_net_cf_scenario,
    capex_planned_musd, term_ppp_years,
    npv_base, npv_scenario, npv_gain
  ) %>%
  dplyr::arrange(scenario, project_name)

print(knitr::kable(
  table_15_actual_scenario_project_results,
  digits = 2,
  caption = "Project-level results under actual BIM adoption scenarios"
))

save_csv(
  table_15_actual_scenario_project_results,
  "outputs/tables/table_15_actual_scenario_project_results.csv"
)

# ------------------------------------------------------------
# 6. Actual scenario summary
# ------------------------------------------------------------
table_17_actual_scenario_summary <- projects_dcf_actual_scenarios %>%
  dplyr::group_by(scenario, bim_operation) %>%
  dplyr::summarise(
    n_projects = dplyr::n(),
    mean_npv_base = mean(npv_base, na.rm = TRUE),
    mean_npv_scenario = mean(npv_scenario, na.rm = TRUE),
    mean_npv_gain = mean(npv_gain, na.rm = TRUE),
    median_npv_gain = median(npv_gain, na.rm = TRUE),
    min_npv_gain = min(npv_gain, na.rm = TRUE),
    max_npv_gain = max(npv_gain, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(scenario, bim_operation)

print(knitr::kable(
  table_17_actual_scenario_summary,
  digits = 2,
  caption = "Summary of actual BIM adoption scenarios"
))

save_csv(
  table_17_actual_scenario_summary,
  "outputs/tables/table_17_actual_scenario_summary.csv"
)

# ------------------------------------------------------------
# 7. Hypothetical scenario:
#    apply OPEX savings to all PPP projects with valid data
# ------------------------------------------------------------
projects_dcf_hypothetical_scenarios <- projects_input %>%
  tidyr::crossing(opex_scenarios) %>%
  dplyr::mutate(
    annual_opex_scenario = annual_opex * (1 - delta_opex),
    annual_net_cf_scenario = annual_revenue - annual_opex_scenario,
    npv_scenario = npv_annuity(
      capex         = capex_planned_musd,
      annual_net_cf = annual_net_cf_scenario,
      T_years       = term_ppp_years,
      r             = r_base
    ),
    npv_gain = npv_scenario - npv_base,
    scenario_type = "Hypothetical full BIM adoption"
  )

saveRDS(
  projects_dcf_hypothetical_scenarios,
  "outputs/rds/projects_dcf_hypothetical_scenarios.rds"
)

table_16_hypothetical_scenario_project_results <- projects_dcf_hypothetical_scenarios %>%
  dplyr::select(
    id, project_name, scenario, interpretation,
    bim_operation,
    annual_revenue, annual_opex, annual_opex_scenario,
    annual_net_cf, annual_net_cf_scenario,
    capex_planned_musd, term_ppp_years,
    npv_base, npv_scenario, npv_gain
  ) %>%
  dplyr::arrange(scenario, project_name)

print(knitr::kable(
  table_16_hypothetical_scenario_project_results,
  digits = 2,
  caption = "Project-level results under hypothetical BIM adoption scenarios"
))

save_csv(
  table_16_hypothetical_scenario_project_results,
  "outputs/tables/table_16_hypothetical_scenario_project_results.csv"
)

# ------------------------------------------------------------
# 8. Hypothetical scenario summary
# ------------------------------------------------------------
table_18_hypothetical_scenario_summary <- projects_dcf_hypothetical_scenarios %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(
    n_projects = dplyr::n(),
    mean_npv_base = mean(npv_base, na.rm = TRUE),
    mean_npv_scenario = mean(npv_scenario, na.rm = TRUE),
    mean_npv_gain = mean(npv_gain, na.rm = TRUE),
    median_npv_gain = median(npv_gain, na.rm = TRUE),
    min_npv_gain = min(npv_gain, na.rm = TRUE),
    max_npv_gain = max(npv_gain, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(scenario)

print(knitr::kable(
  table_18_hypothetical_scenario_summary,
  digits = 2,
  caption = "Summary of hypothetical BIM adoption scenarios"
))

save_csv(
  table_18_hypothetical_scenario_summary,
  "outputs/tables/table_18_hypothetical_scenario_summary.csv"
)

# ------------------------------------------------------------
# 9. Figure: actual scenario comparison
# ------------------------------------------------------------
figure_actual_plot_data <- projects_dcf_actual_scenarios %>%
  dplyr::filter(!is.na(npv_base), !is.na(npv_scenario)) %>%
  dplyr::select(id, project_name, scenario, npv_base, npv_scenario) %>%
  tidyr::pivot_longer(
    cols = c(npv_base, npv_scenario),
    names_to = "npv_type",
    values_to = "npv"
  ) %>%
  dplyr::mutate(
    npv_type = dplyr::recode(
      npv_type,
      npv_base = "Baseline NPV",
      npv_scenario = "Scenario NPV"
    ),
    scenario = factor(scenario, levels = c("low", "base", "high"))
  )

figure_04_npv_actual_scenarios <- ggplot(
  figure_actual_plot_data,
  aes(x = reorder(project_name, npv), y = npv, fill = npv_type)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ scenario, scales = "free_y") +
  labs(
    title = "NPV under actual BIM adoption scenarios",
    x = "Project",
    y = "NPV (million USD)",
    fill = NULL
  ) +
  theme_article +
  theme(
    legend.position = "bottom"
  )

print(figure_04_npv_actual_scenarios)

save_plot(
  figure_04_npv_actual_scenarios,
  "outputs/figures/figure_04_npv_actual_scenarios.png",
  width = 11,
  height = 7
)

# ------------------------------------------------------------
# 10. Figure: hypothetical scenario comparison
# ------------------------------------------------------------
figure_hypothetical_plot_data <- projects_dcf_hypothetical_scenarios %>%
  dplyr::filter(!is.na(npv_base), !is.na(npv_scenario)) %>%
  dplyr::select(id, project_name, scenario, npv_base, npv_scenario) %>%
  tidyr::pivot_longer(
    cols = c(npv_base, npv_scenario),
    names_to = "npv_type",
    values_to = "npv"
  ) %>%
  dplyr::mutate(
    npv_type = dplyr::recode(
      npv_type,
      npv_base = "Baseline NPV",
      npv_scenario = "Scenario NPV"
    ),
    scenario = factor(scenario, levels = c("low", "base", "high"))
  )

figure_05_npv_hypothetical_scenarios <- ggplot(
  figure_hypothetical_plot_data,
  aes(x = reorder(project_name, npv), y = npv, fill = npv_type)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ scenario, scales = "free_y") +
  labs(
    title = "NPV under hypothetical BIM adoption scenarios",
    x = "Project",
    y = "NPV (million USD)",
    fill = NULL
  ) +
  theme_article +
  theme(
    legend.position = "bottom"
  )

print(figure_05_npv_hypothetical_scenarios)

save_plot(
  figure_05_npv_hypothetical_scenarios,
  "outputs/figures/figure_05_npv_hypothetical_scenarios.png",
  width = 11,
  height = 7
)

# ------------------------------------------------------------
# 11. Compact scenario memo
# ------------------------------------------------------------
scenario_memo <- tibble::tibble(
  item = c(
    "Number of valid PPP projects in scenario layer",
    "Mean baseline NPV",
    "Mean actual-scenario NPV gain (base scenario)",
    "Mean hypothetical-scenario NPV gain (base scenario)"
  ),
  value = c(
    nrow(projects_input),
    mean(projects_input$npv_base, na.rm = TRUE),
    projects_dcf_actual_scenarios %>%
      dplyr::filter(scenario == "base") %>%
      dplyr::summarise(x = mean(npv_gain, na.rm = TRUE)) %>%
      dplyr::pull(x),
    projects_dcf_hypothetical_scenarios %>%
      dplyr::filter(scenario == "base") %>%
      dplyr::summarise(x = mean(npv_gain, na.rm = TRUE)) %>%
      dplyr::pull(x)
  )
)

print(knitr::kable(
  scenario_memo,
  digits = 2,
  caption = "Compact memo of deterministic BIM scenario analysis"
))

save_csv(
  scenario_memo,
  "outputs/tables/table_19_scenario_memo.csv"
)

# ------------------------------------------------------------
# 12. Session end message
# ------------------------------------------------------------
message("05_scenario_analysis.R finished successfully.")
