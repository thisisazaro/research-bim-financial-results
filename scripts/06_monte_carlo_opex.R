# ============================================================
# 06_monte_carlo_opex.R
# Purpose:
#   Run Monte Carlo simulation for BIM-related OPEX effects
#   only, keeping the discount rate fixed at the baseline level.
#
# Inputs:
#   outputs/rds/projects_dcf_base.rds
#   outputs/rds/opex_effects_literature.rds
#
# Outputs:
#   outputs/rds/projects_mc_opex.rds
#   outputs/rds/mc_opex_summary.rds
#   outputs/rds/value_efficiency_df.rds
#   outputs/tables/table_20_mc_opex_summary.csv
#   outputs/tables/table_21_value_efficiency.csv
#   outputs/figures/figure_06_mc_opex_density_ru001.png
#   outputs/figures/figure_07_mc_opex_density_ru002.png
#   outputs/figures/figure_08_mc_opex_density_ru005.png
#
# Notes:
#   - This script models uncertainty in BIM-induced OPEX savings
#     while holding WACC constant.
#   - It provides the "value efficiency" component later used
#     in the total BIM value decomposition.
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
# 1. Load baseline DCF data and calibrated OPEX effects
# ------------------------------------------------------------
projects_dcf_base <- readRDS("outputs/rds/projects_dcf_base.rds")
opex_effects_literature <- readRDS("outputs/rds/opex_effects_literature.rds")

data_overview(projects_dcf_base, "projects_dcf_base")
data_overview(opex_effects_literature, "opex_effects_literature")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars_projects <- c(
  "id", "project_name", "capex_planned_musd",
  "term_ppp_years", "annual_net_cf", "npv_base"
)

required_vars_opex <- c(
  "score_to_the_unified_metric"
)

missing_projects <- required_vars_projects[
  !required_vars_projects %in% names(projects_dcf_base)
]

missing_opex <- required_vars_opex[
  !required_vars_opex %in% names(opex_effects_literature)
]

if (length(missing_projects) > 0) {
  stop(
    "The following required variables are missing in projects_dcf_base:\n",
    paste0("- ", missing_projects, collapse = "\n")
  )
}

if (length(missing_opex) > 0) {
  stop(
    "The following required variables are missing in opex_effects_literature:\n",
    paste0("- ", missing_opex, collapse = "\n")
  )
}

message("All required variables for Monte Carlo OPEX analysis are available.")

# ------------------------------------------------------------
# 3. Restrict to valid projects for simulation
# ------------------------------------------------------------
projects_mc_input <- projects_dcf_base %>%
  dplyr::filter(
    !is.na(capex_planned_musd),
    !is.na(term_ppp_years),
    !is.na(annual_net_cf),
    !is.na(npv_base)
  )

if (nrow(projects_mc_input) == 0) {
  stop("No valid project observations are available for Monte Carlo OPEX analysis.")
}

# ------------------------------------------------------------
# 4. Empirical distribution of BIM-induced OPEX effects
# ------------------------------------------------------------
# We use the literature-based empirical distribution directly.
# This preserves the observed evidence structure and avoids
# imposing a parametric assumption at this stage.

delta_opex_emp <- opex_effects_literature$score_to_the_unified_metric
delta_opex_emp <- delta_opex_emp[!is.na(delta_opex_emp)]

if (length(delta_opex_emp) == 0) {
  stop("The empirical OPEX effect distribution is empty.")
}

# ------------------------------------------------------------
# 5. Simulation settings
# ------------------------------------------------------------
set.seed(123)
n_sims <- 10000
r_base <- 0.08

message("Monte Carlo settings: ", n_sims, " simulations per project.")

# ------------------------------------------------------------
# 6. Monte Carlo simulation: OPEX effect only
# ------------------------------------------------------------
# Logic:
#   - sample a BIM-related OPEX saving from the empirical
#     literature-based distribution
#   - map it into an improvement in annual net cash flow
#   - revalue the project using the baseline discount rate

projects_mc_opex <- projects_mc_input %>%
  dplyr::select(
    id,
    project_name,
    capex_planned_musd,
    term_ppp_years,
    annual_net_cf,
    npv_base
  ) %>%
  tidyr::uncount(weights = n_sims, .id = "sim_id") %>%
  dplyr::mutate(
    delta_opex = sample(delta_opex_emp, size = dplyr::n(), replace = TRUE),
    annual_net_cf_sim = annual_net_cf * (1 + delta_opex),
    npv_sim = npv_annuity(
      capex         = capex_planned_musd,
      annual_net_cf = annual_net_cf_sim,
      T_years       = term_ppp_years,
      r             = r_base
    ),
    npv_gain = npv_sim - npv_base
  )

saveRDS(projects_mc_opex, "outputs/rds/projects_mc_opex.rds")

# ------------------------------------------------------------
# 7. Project-level Monte Carlo summary
# ------------------------------------------------------------
mc_opex_summary <- projects_mc_opex %>%
  dplyr::group_by(id, project_name) %>%
  dplyr::summarise(
    n_sims         = dplyr::n(),
    npv_base       = dplyr::first(npv_base),
    mean_npv_sim   = mean(npv_sim, na.rm = TRUE),
    median_npv_sim = median(npv_sim, na.rm = TRUE),
    sd_npv_sim     = sd(npv_sim, na.rm = TRUE),
    p05_npv_sim    = quantile(npv_sim, 0.05, na.rm = TRUE),
    p50_npv_sim    = quantile(npv_sim, 0.50, na.rm = TRUE),
    p95_npv_sim    = quantile(npv_sim, 0.95, na.rm = TRUE),
    prob_npv_neg   = mean(npv_sim < 0, na.rm = TRUE),
    mean_npv_gain  = mean(npv_gain, na.rm = TRUE),
    median_npv_gain = median(npv_gain, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(id)

saveRDS(mc_opex_summary, "outputs/rds/mc_opex_summary.rds")

print(knitr::kable(
  mc_opex_summary,
  digits = 2,
  caption = "Monte Carlo summary under stochastic BIM-induced OPEX effects"
))

save_csv(
  mc_opex_summary,
  "outputs/tables/table_20_mc_opex_summary.csv"
)

# ------------------------------------------------------------
# 8. Value efficiency component
# ------------------------------------------------------------
# This is the expected NPV gain attributable to BIM-related
# operational efficiency improvements alone.

value_efficiency_df <- projects_mc_opex %>%
  dplyr::group_by(project_id = id, project_name) %>%
  dplyr::summarise(
    value_efficiency = mean(npv_gain, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(project_id)

saveRDS(value_efficiency_df, "outputs/rds/value_efficiency_df.rds")

print(knitr::kable(
  value_efficiency_df,
  digits = 2,
  caption = "Estimated value efficiency component from Monte Carlo OPEX analysis"
))

save_csv(
  value_efficiency_df,
  "outputs/tables/table_21_value_efficiency.csv"
)

# ------------------------------------------------------------
# 9. Compact simulation memo
# ------------------------------------------------------------
mc_opex_memo <- tibble::tibble(
  item = c(
    "Number of projects in Monte Carlo OPEX layer",
    "Simulations per project",
    "Mean baseline NPV",
    "Mean simulated NPV",
    "Mean value efficiency"
  ),
  value = c(
    dplyr::n_distinct(projects_mc_opex$id),
    n_sims,
    mean(mc_opex_summary$npv_base, na.rm = TRUE),
    mean(mc_opex_summary$mean_npv_sim, na.rm = TRUE),
    mean(value_efficiency_df$value_efficiency, na.rm = TRUE)
  )
)

print(knitr::kable(
  mc_opex_memo,
  digits = 2,
  caption = "Compact memo of Monte Carlo OPEX results"
))

save_csv(
  mc_opex_memo,
  "outputs/tables/table_22_mc_opex_memo.csv"
)

# ------------------------------------------------------------
# 10. Helper plot function for project-level densities
# ------------------------------------------------------------
plot_mc_opex_density <- function(mc_data, proj_id, proj_title = NULL) {
  df_plot <- mc_data %>%
    dplyr::filter(id == proj_id)
  
  if (nrow(df_plot) == 0) {
    stop("No Monte Carlo observations found for project: ", proj_id)
  }
  
  base_npv <- unique(df_plot$npv_base)
  
  if (length(base_npv) != 1) {
    stop("Expected a unique baseline NPV for project: ", proj_id)
  }
  
  if (is.null(proj_title)) {
    proj_title <- unique(df_plot$project_name)
  }
  
  ggplot(df_plot, aes(x = npv_sim)) +
    geom_density() +
    geom_vline(xintercept = base_npv, linetype = "dashed") +
    labs(
      title = paste("NPV distribution under stochastic OPEX effects –", proj_id),
      subtitle = proj_title,
      x = "NPV (million USD)",
      y = "Density"
    ) +
    theme_article
}

# ------------------------------------------------------------
# 11. Density plots for key PPP projects
# ------------------------------------------------------------
# Adjust or extend this list later if needed.

key_projects <- c("RU-001", "RU-002", "RU-005")

if ("RU-001" %in% projects_mc_opex$id) {
  figure_06_mc_opex_density_ru001 <- plot_mc_opex_density(projects_mc_opex, "RU-001")
  print(figure_06_mc_opex_density_ru001)
  save_plot(
    figure_06_mc_opex_density_ru001,
    "outputs/figures/figure_06_mc_opex_density_ru001.png",
    width = 8,
    height = 5
  )
}

if ("RU-002" %in% projects_mc_opex$id) {
  figure_07_mc_opex_density_ru002 <- plot_mc_opex_density(projects_mc_opex, "RU-002")
  print(figure_07_mc_opex_density_ru002)
  save_plot(
    figure_07_mc_opex_density_ru002,
    "outputs/figures/figure_07_mc_opex_density_ru002.png",
    width = 8,
    height = 5
  )
}

if ("RU-005" %in% projects_mc_opex$id) {
  figure_08_mc_opex_density_ru005 <- plot_mc_opex_density(projects_mc_opex, "RU-005")
  print(figure_08_mc_opex_density_ru005)
  save_plot(
    figure_08_mc_opex_density_ru005,
    "outputs/figures/figure_08_mc_opex_density_ru005.png",
    width = 8,
    height = 5
  )
}

# ------------------------------------------------------------
# 12. Session end message
# ------------------------------------------------------------
message("06_monte_carlo_opex.R finished successfully.")
