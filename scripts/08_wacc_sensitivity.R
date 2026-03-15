# ============================================================
# 08_wacc_sensitivity.R
# Purpose:
#   Evaluate how a stylized BIM-related reduction in WACC
#   affects PPP project valuation under:
#   (1) actual BIM adoption in operation, and
#   (2) hypothetical full BIM adoption.
#
# Inputs:
#   outputs/rds/projects_dcf_base.rds
#
# Outputs:
#   outputs/rds/projects_wacc_actual.rds
#   outputs/rds/projects_wacc_hypothetical.rds
#   outputs/tables/table_25_wacc_actual_project_results.csv
#   outputs/tables/table_26_wacc_hypothetical_project_results.csv
#   outputs/tables/table_27_wacc_summary.csv
#   outputs/tables/table_28_wacc_memo.csv
#   outputs/figures/figure_15_wacc_scenarios_comparison.png
#   outputs/figures/figure_16_npv_vs_wacc_ru001.png
#   outputs/figures/figure_17_npv_vs_wacc_ru002.png
#   outputs/figures/figure_18_npv_vs_wacc_ru005.png
#
# Notes:
#   - This script isolates the risk / discount-rate channel.
#   - It does not model stochastic OPEX effects.
#   - The WACC reduction is treated as a stylized valuation
#     assumption, not as an estimated financing model.
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
source("R/00_packages.R")
source("R/01_functions_general.R")
source("R/02_functions_dcf.R")
source("R/03_functions_plots.R")
source("R/04_theme.R")

ensure_dir("outputs/rds")
ensure_dir("outputs/tables")
ensure_dir("outputs/figures")

# ------------------------------------------------------------
# 1. Load baseline DCF data
# ------------------------------------------------------------
projects_dcf_base <- readRDS("outputs/rds/projects_dcf_base.rds")

data_overview(projects_dcf_base, "projects_dcf_base")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars <- c(
  "id", "project_name", "bim_operation",
  "capex_planned_musd", "term_ppp_years",
  "annual_net_cf", "npv_base"
)

missing_vars <- required_vars[!required_vars %in% names(projects_dcf_base)]

if (length(missing_vars) > 0) {
  stop(
    "The following required variables are missing in projects_dcf_base:\n",
    paste0("- ", missing_vars, collapse = "\n")
  )
}

message("All required variables for WACC sensitivity analysis are available.")

# ------------------------------------------------------------
# 3. Restrict to valid DCF observations
# ------------------------------------------------------------
projects_wacc_input <- projects_dcf_base %>%
  dplyr::filter(
    !is.na(capex_planned_musd),
    !is.na(term_ppp_years),
    !is.na(annual_net_cf),
    !is.na(npv_base)
  )

if (nrow(projects_wacc_input) == 0) {
  stop("No valid project observations are available for WACC sensitivity analysis.")
}

# ------------------------------------------------------------
# 4. WACC assumptions
# ------------------------------------------------------------
# Baseline:
#   r_base = 8%
# Stylized lower WACC under BIM:
#   r_lower = 7.5%
#
# Interpretation:
#   BIM is assumed to modestly reduce uncertainty, improve
#   transparency, and support better lifecycle risk control,
#   resulting in a 0.5 p.p. lower discount rate.

r_base  <- 0.08
r_lower <- 0.075

# ------------------------------------------------------------
# 5. Actual WACC scenario:
#    lower WACC only for projects with BIM in operation
# ------------------------------------------------------------
projects_wacc_actual <- projects_wacc_input %>%
  dplyr::mutate(
    r_actual = dplyr::if_else(
      bim_operation,
      r_lower,
      r_base
    ),
    npv_wacc_actual = npv_annuity(
      capex         = capex_planned_musd,
      annual_net_cf = annual_net_cf,
      T_years       = term_ppp_years,
      r             = r_actual
    ),
    npv_gain_wacc = npv_wacc_actual - npv_base
  )

saveRDS(projects_wacc_actual, "outputs/rds/projects_wacc_actual.rds")

table_25_wacc_actual_project_results <- projects_wacc_actual %>%
  dplyr::select(
    id, project_name, bim_operation,
    capex_planned_musd, term_ppp_years,
    annual_net_cf,
    npv_base, r_actual, npv_wacc_actual, npv_gain_wacc
  ) %>%
  dplyr::arrange(project_name)

print(knitr::kable(
  table_25_wacc_actual_project_results,
  digits = 2,
  caption = "Project-level valuation under the actual WACC scenario"
))

save_csv(
  table_25_wacc_actual_project_results,
  "outputs/tables/table_25_wacc_actual_project_results.csv"
)

# ------------------------------------------------------------
# 6. Hypothetical WACC scenario:
#    lower WACC for all PPP projects
# ------------------------------------------------------------
projects_wacc_hypothetical <- projects_wacc_input %>%
  dplyr::mutate(
    r_hypothetical = r_lower,
    npv_wacc_hypothetical = npv_annuity(
      capex         = capex_planned_musd,
      annual_net_cf = annual_net_cf,
      T_years       = term_ppp_years,
      r             = r_hypothetical
    ),
    npv_gain_wacc_hypothetical = npv_wacc_hypothetical - npv_base
  )

saveRDS(projects_wacc_hypothetical, "outputs/rds/projects_wacc_hypothetical.rds")

table_26_wacc_hypothetical_project_results <- projects_wacc_hypothetical %>%
  dplyr::select(
    id, project_name,
    capex_planned_musd, term_ppp_years,
    annual_net_cf,
    npv_base, r_hypothetical, npv_wacc_hypothetical,
    npv_gain_wacc_hypothetical
  ) %>%
  dplyr::arrange(project_name)

print(knitr::kable(
  table_26_wacc_hypothetical_project_results,
  digits = 2,
  caption = "Project-level valuation under the hypothetical WACC scenario"
))

save_csv(
  table_26_wacc_hypothetical_project_results,
  "outputs/tables/table_26_wacc_hypothetical_project_results.csv"
)

# ------------------------------------------------------------
# 7. WACC summary table
# ------------------------------------------------------------
table_27_wacc_summary <- tibble::tibble(
  scenario = c(
    "Baseline",
    "Actual: lower WACC only with BIM in operation",
    "Hypothetical: lower WACC for all PPP projects"
  ),
  mean_npv = c(
    mean(projects_wacc_input$npv_base, na.rm = TRUE),
    mean(projects_wacc_actual$npv_wacc_actual, na.rm = TRUE),
    mean(projects_wacc_hypothetical$npv_wacc_hypothetical, na.rm = TRUE)
  ),
  mean_npv_gain = c(
    0,
    mean(projects_wacc_actual$npv_gain_wacc, na.rm = TRUE),
    mean(projects_wacc_hypothetical$npv_gain_wacc_hypothetical, na.rm = TRUE)
  )
)

print(knitr::kable(
  table_27_wacc_summary,
  digits = 2,
  caption = "Summary of NPV under alternative WACC assumptions"
))

save_csv(
  table_27_wacc_summary,
  "outputs/tables/table_27_wacc_summary.csv"
)

# ------------------------------------------------------------
# 8. Comparison plot across WACC scenarios
# ------------------------------------------------------------
wacc_plot_df <- projects_wacc_input %>%
  dplyr::select(id, project_name, npv_base) %>%
  dplyr::left_join(
    projects_wacc_actual %>%
      dplyr::select(id, npv_wacc_actual),
    by = "id"
  ) %>%
  dplyr::left_join(
    projects_wacc_hypothetical %>%
      dplyr::select(id, npv_wacc_hypothetical),
    by = "id"
  ) %>%
  tidyr::pivot_longer(
    cols = c(npv_base, npv_wacc_actual, npv_wacc_hypothetical),
    names_to = "scenario",
    values_to = "npv"
  ) %>%
  dplyr::mutate(
    scenario = factor(
      scenario,
      levels = c("npv_base", "npv_wacc_actual", "npv_wacc_hypothetical"),
      labels = c(
        "Base WACC",
        "Lower WACC with BIM in operation",
        "Lower WACC for all PPP projects"
      )
    )
  )

figure_15_wacc_scenarios_comparison <- ggplot2::ggplot(
  wacc_plot_df,
  ggplot2::aes(x = project_name, y = npv, fill = scenario)
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.7),
    width = 0.6,
    colour = "black"
  ) +
  ggplot2::coord_flip() +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.3
  ) +
  ggplot2::scale_fill_brewer(
    type = "qual",
    palette = "Set2",
    name = NULL
  ) +
  ggplot2::labs(
    title = "Net Present Value under different WACC scenarios",
    x = NULL,
    y = "NPV (million USD)"
  ) +
  theme_article +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major.y = ggplot2::element_blank()
  )

print(figure_15_wacc_scenarios_comparison)

save_plot(
  figure_15_wacc_scenarios_comparison,
  "outputs/figures/figure_15_wacc_scenarios_comparison.png",
  width = 10,
  height = 7
)

# ------------------------------------------------------------
# 9. NPV(r) curves for selected PPP projects
# ------------------------------------------------------------
if ("RU-001" %in% projects_wacc_input$id) {
  figure_16_npv_vs_wacc_ru001 <- plot_npv_vs_wacc(
    projects_data = projects_wacc_input,
    proj_id = "RU-001",
    r_base = r_base,
    r_lower = r_lower
  )
  print(figure_16_npv_vs_wacc_ru001)
  save_plot(
    figure_16_npv_vs_wacc_ru001,
    "outputs/figures/figure_16_npv_vs_wacc_ru001.png",
    width = 8,
    height = 5
  )
}

if ("RU-002" %in% projects_wacc_input$id) {
  figure_17_npv_vs_wacc_ru002 <- plot_npv_vs_wacc(
    projects_data = projects_wacc_input,
    proj_id = "RU-002",
    r_base = r_base,
    r_lower = r_lower
  )
  print(figure_17_npv_vs_wacc_ru002)
  save_plot(
    figure_17_npv_vs_wacc_ru002,
    "outputs/figures/figure_17_npv_vs_wacc_ru002.png",
    width = 8,
    height = 5
  )
}

if ("RU-005" %in% projects_wacc_input$id) {
  figure_18_npv_vs_wacc_ru005 <- plot_npv_vs_wacc(
    projects_data = projects_wacc_input,
    proj_id = "RU-005",
    r_base = r_base,
    r_lower = r_lower
  )
  print(figure_18_npv_vs_wacc_ru005)
  save_plot(
    figure_18_npv_vs_wacc_ru005,
    "outputs/figures/figure_18_npv_vs_wacc_ru005.png",
    width = 8,
    height = 5
  )
}

# ------------------------------------------------------------
# 10. Compact memo of the WACC sensitivity layer
# ------------------------------------------------------------
table_28_wacc_memo <- tibble::tibble(
  item = c(
    "Number of PPP projects in WACC sensitivity layer",
    "Baseline WACC",
    "Lower WACC assumption",
    "Mean baseline NPV",
    "Mean NPV gain under actual WACC scenario",
    "Mean NPV gain under hypothetical WACC scenario"
  ),
  value = c(
    nrow(projects_wacc_input),
    r_base,
    r_lower,
    mean(projects_wacc_input$npv_base, na.rm = TRUE),
    mean(projects_wacc_actual$npv_gain_wacc, na.rm = TRUE),
    mean(projects_wacc_hypothetical$npv_gain_wacc_hypothetical, na.rm = TRUE)
  )
)

print(knitr::kable(
  table_28_wacc_memo,
  digits = 3,
  caption = "Compact memo of WACC sensitivity assumptions and valuation effects"
))

save_csv(
  table_28_wacc_memo,
  "outputs/tables/table_28_wacc_memo.csv"
)

# ------------------------------------------------------------
# 11. Session end message
# ------------------------------------------------------------
message("08_wacc_sensitivity.R finished successfully.")

