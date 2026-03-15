# ============================================================
# 07_monte_carlo_opex_wacc.R
# Purpose:
#   Run joint Monte Carlo simulation for BIM-related OPEX
#   effects and discount-rate (WACC) uncertainty, allowing
#   for correlation between operating efficiency gains and
#   risk reduction.
#
# Inputs:
#   outputs/rds/projects_dcf_base.rds
#   outputs/rds/opex_effects_literature.rds
#
# Outputs:
#   outputs/rds/projects_mc_joint.rds
#   outputs/rds/mc_joint_summary.rds
#   outputs/tables/table_23_mc_joint_summary.csv
#   outputs/tables/table_24_mc_joint_memo.csv
#   outputs/figures/figure_09_mc_joint_hist_ru001.png
#   outputs/figures/figure_10_mc_joint_hist_ru002.png
#   outputs/figures/figure_11_mc_joint_hist_ru005.png
#   outputs/figures/figure_12_mc_joint_heatmap_ru001.png
#   outputs/figures/figure_13_mc_joint_heatmap_ru002.png
#   outputs/figures/figure_14_mc_joint_heatmap_ru005.png
#
# Notes:
#   - This script extends the OPEX-only Monte Carlo layer by
#     adding stochastic WACC.
#   - The joint simulation is used later for valuation under
#     combined efficiency and risk uncertainty.
#   - Correlation is imposed at the latent level between
#     log(OPEX effect) and WACC draws.
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
# 1. Load baseline DCF data and calibrated OPEX evidence
# ------------------------------------------------------------
projects_dcf_base <- readRDS("outputs/rds/projects_dcf_base.rds")
opex_effects_literature <- readRDS("outputs/rds/opex_effects_literature.rds")

data_overview(projects_dcf_base, "projects_dcf_base")
data_overview(opex_effects_literature, "opex_effects_literature")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars_projects <- c(
  "id", "project_name",
  "capex_planned_musd", "term_ppp_years",
  "annual_revenue", "annual_opex", "annual_net_cf",
  "npv_base"
)

required_vars_opex <- c("score_to_the_unified_metric")

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

message("All required variables for joint Monte Carlo analysis are available.")

# ------------------------------------------------------------
# 3. Restrict to valid projects for simulation
# ------------------------------------------------------------
projects_mc_input <- projects_dcf_base %>%
  dplyr::filter(
    !is.na(capex_planned_musd),
    !is.na(term_ppp_years),
    !is.na(annual_revenue),
    !is.na(annual_opex),
    !is.na(annual_net_cf),
    !is.na(npv_base)
  )

if (nrow(projects_mc_input) == 0) {
  stop("No valid project observations are available for joint Monte Carlo analysis.")
}

# ------------------------------------------------------------
# 4. Calibrate the latent OPEX distribution
# ------------------------------------------------------------
# We work with log(1 + effect) to keep the simulated OPEX
# improvement economically meaningful after back-transformation.

delta_opex_emp <- opex_effects_literature$score_to_the_unified_metric
delta_opex_emp <- delta_opex_emp[!is.na(delta_opex_emp)]

if (length(delta_opex_emp) == 0) {
  stop("The empirical OPEX effect distribution is empty.")
}

log_opex_emp <- log1p(delta_opex_emp)

mu_log_opex <- mean(log_opex_emp, na.rm = TRUE)
sd_log_opex <- sd(log_opex_emp, na.rm = TRUE)

# ------------------------------------------------------------
# 5. WACC assumptions
# ------------------------------------------------------------
# Baseline WACC is fixed at 8%.
# We model stochastic WACC around this benchmark with a
# standard deviation of 0.5 percentage points.

r_base <- 0.08
sd_r <- 0.005

# Correlation assumption:
# better BIM efficiency effects are assumed to be associated
# with slightly lower project risk / WACC.
rho <- -0.30

Sigma_rw <- matrix(
  c(
    sd_log_opex^2,
    rho * sd_log_opex * sd_r,
    rho * sd_log_opex * sd_r,
    sd_r^2
  ),
  nrow = 2,
  byrow = TRUE
)

# ------------------------------------------------------------
# 6. Simulation settings
# ------------------------------------------------------------
set.seed(123)
n_sims <- 20000

message("Joint Monte Carlo settings: ", n_sims, " simulations per project.")

# ------------------------------------------------------------
# 7. Helper function: joint simulation for one project
# ------------------------------------------------------------
simulate_mc_opex_wacc <- function(proj_row,
                                  n_sim,
                                  mu_log_opex,
                                  Sigma_rw,
                                  r_base) {
  draws <- MASS::mvrnorm(
    n = n_sim,
    mu = c(mu_log_opex, r_base),
    Sigma = Sigma_rw
  )
  
  log_opex_draw <- draws[, 1]
  r_draw <- draws[, 2]
  
  # Bound WACC to a reasonable range for stability
  r_draw <- pmax(0.03, pmin(0.12, r_draw))
  
  # Back-transform latent OPEX effect:
  # log(1 + effect) -> effect
  delta_opex_draw <- exp(log_opex_draw) - 1
  
  # Economically cleaner formulation:
  # OPEX changes, revenue stays fixed.
  annual_opex_sim <- proj_row$annual_opex * (1 - delta_opex_draw)
  annual_net_cf_sim <- proj_row$annual_revenue - annual_opex_sim
  
  npv_sim <- npv_annuity(
    capex         = proj_row$capex_planned_musd,
    annual_net_cf = annual_net_cf_sim,
    T_years       = proj_row$term_ppp_years,
    r             = r_draw
  )
  
  tibble::tibble(
    sim_id = seq_len(n_sim),
    id = proj_row$id,
    project_name = proj_row$project_name,
    delta_opex = delta_opex_draw,
    wacc = r_draw,
    annual_opex_sim = annual_opex_sim,
    annual_net_cf_sim = annual_net_cf_sim,
    npv_base = proj_row$npv_base,
    npv_sim = npv_sim,
    npv_gain = npv_sim - proj_row$npv_base
  )
}

# ------------------------------------------------------------
# 8. Run the joint Monte Carlo simulation
# ------------------------------------------------------------
projects_mc_joint <- projects_mc_input %>%
  dplyr::group_split(id) %>%
  purrr::map_dfr(
    ~ simulate_mc_opex_wacc(
      proj_row = .x[1, ],
      n_sim = n_sims,
      mu_log_opex = mu_log_opex,
      Sigma_rw = Sigma_rw,
      r_base = r_base
    )
  )

saveRDS(projects_mc_joint, "outputs/rds/projects_mc_joint.rds")

# ------------------------------------------------------------
# 9. Project-level joint Monte Carlo summary
# ------------------------------------------------------------
mc_joint_summary <- projects_mc_joint %>%
  dplyr::group_by(id, project_name) %>%
  dplyr::summarise(
    n_sims          = dplyr::n(),
    npv_base        = dplyr::first(npv_base),
    mean_npv_sim    = mean(npv_sim, na.rm = TRUE),
    median_npv_sim  = median(npv_sim, na.rm = TRUE),
    sd_npv_sim      = sd(npv_sim, na.rm = TRUE),
    p05_npv_sim     = quantile(npv_sim, 0.05, na.rm = TRUE),
    p50_npv_sim     = quantile(npv_sim, 0.50, na.rm = TRUE),
    p95_npv_sim     = quantile(npv_sim, 0.95, na.rm = TRUE),
    prob_npv_neg    = mean(npv_sim < 0, na.rm = TRUE),
    mean_npv_gain   = mean(npv_gain, na.rm = TRUE),
    median_npv_gain = median(npv_gain, na.rm = TRUE),
    mean_wacc       = mean(wacc, na.rm = TRUE),
    sd_wacc         = sd(wacc, na.rm = TRUE),
    mean_delta_opex = mean(delta_opex, na.rm = TRUE),
    sd_delta_opex   = sd(delta_opex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(id)

saveRDS(mc_joint_summary, "outputs/rds/mc_joint_summary.rds")

print(knitr::kable(
  mc_joint_summary,
  digits = 2,
  caption = "Joint Monte Carlo summary under stochastic OPEX and WACC"
))

save_csv(
  mc_joint_summary,
  "outputs/tables/table_23_mc_joint_summary.csv"
)

# ------------------------------------------------------------
# 10. Compact joint simulation memo
# ------------------------------------------------------------
mc_joint_memo <- tibble::tibble(
  item = c(
    "Number of projects in joint Monte Carlo layer",
    "Simulations per project",
    "Baseline WACC",
    "WACC standard deviation",
    "Latent correlation (log OPEX effect, WACC)",
    "Mean baseline NPV",
    "Mean simulated NPV",
    "Mean simulated NPV gain"
  ),
  value = c(
    dplyr::n_distinct(projects_mc_joint$id),
    n_sims,
    r_base,
    sd_r,
    rho,
    mean(mc_joint_summary$npv_base, na.rm = TRUE),
    mean(mc_joint_summary$mean_npv_sim, na.rm = TRUE),
    mean(mc_joint_summary$mean_npv_gain, na.rm = TRUE)
  )
)

print(knitr::kable(
  mc_joint_memo,
  digits = 3,
  caption = "Compact memo of joint OPEX-WACC Monte Carlo assumptions and results"
))

save_csv(
  mc_joint_memo,
  "outputs/tables/table_24_mc_joint_memo.csv"
)

# ------------------------------------------------------------
# 11. Histogram helper for project-level joint simulation
# ------------------------------------------------------------
plot_mc_joint_hist <- function(mc_data, proj_id) {
  df_plot <- mc_data %>%
    dplyr::filter(id == proj_id)
  
  if (nrow(df_plot) == 0) {
    stop("No Monte Carlo observations found for project: ", proj_id)
  }
  
  base_npv <- unique(df_plot$npv_base)
  
  if (length(base_npv) != 1) {
    stop("Expected a unique baseline NPV for project: ", proj_id)
  }
  
  project_title <- unique(df_plot$project_name)[1]
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = npv_sim)) +
    ggplot2::geom_histogram(
      bins = 40,
      fill = "grey30",
      color = "white"
    ) +
    ggplot2::geom_vline(
      xintercept = base_npv,
      linetype = "dashed"
    ) +
    ggplot2::geom_vline(
      xintercept = stats::median(df_plot$npv_sim, na.rm = TRUE),
      linetype = "dotted"
    ) +
    ggplot2::labs(
      title = paste("Joint Monte Carlo (OPEX + WACC) –", proj_id),
      subtitle = project_title,
      x = "NPV (million USD)",
      y = "Frequency"
    ) +
    theme_article
}

# ------------------------------------------------------------
# 12. Heatmap helper for OPEX-WACC simulation surface
# ------------------------------------------------------------
plot_mc_joint_heatmap <- function(mc_data, proj_id) {
  df_plot <- mc_data %>%
    dplyr::filter(id == proj_id)
  
  if (nrow(df_plot) == 0) {
    stop("No Monte Carlo observations found for project: ", proj_id)
  }
  
  wacc_breaks <- pretty(range(df_plot$wacc, na.rm = TRUE), n = 6)
  opex_breaks <- pretty(range(df_plot$delta_opex, na.rm = TRUE), n = 10)
  
  surface_data <- df_plot %>%
    dplyr::mutate(
      wacc_bin = cut(wacc, breaks = wacc_breaks, include.lowest = TRUE),
      opex_bin = cut(delta_opex, breaks = opex_breaks, include.lowest = TRUE)
    ) %>%
    dplyr::group_by(wacc_bin, opex_bin) %>%
    dplyr::summarise(
      mean_npv = mean(npv_sim, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot2::ggplot(
    surface_data,
    ggplot2::aes(x = wacc_bin, y = opex_bin, fill = mean_npv)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(
      option = "magma",
      direction = -1,
      name = "Mean NPV\n(million USD)"
    ) +
    ggplot2::labs(
      title = paste("NPV surface under joint OPEX-WACC uncertainty –", proj_id),
      x = "WACC (bins)",
      y = "OPEX effect (bins)"
    ) +
    theme_article +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

# ------------------------------------------------------------
# 13. Figures for key PPP projects
# ------------------------------------------------------------
if ("RU-001" %in% projects_mc_joint$id) {
  figure_09_mc_joint_hist_ru001 <- plot_mc_joint_hist(projects_mc_joint, "RU-001")
  print(figure_09_mc_joint_hist_ru001)
  save_plot(
    figure_09_mc_joint_hist_ru001,
    "outputs/figures/figure_09_mc_joint_hist_ru001.png",
    width = 8,
    height = 5
  )
  
  figure_12_mc_joint_heatmap_ru001 <- plot_mc_joint_heatmap(projects_mc_joint, "RU-001")
  print(figure_12_mc_joint_heatmap_ru001)
  save_plot(
    figure_12_mc_joint_heatmap_ru001,
    "outputs/figures/figure_12_mc_joint_heatmap_ru001.png",
    width = 8,
    height = 5
  )
}

if ("RU-002" %in% projects_mc_joint$id) {
  figure_10_mc_joint_hist_ru002 <- plot_mc_joint_hist(projects_mc_joint, "RU-002")
  print(figure_10_mc_joint_hist_ru002)
  save_plot(
    figure_10_mc_joint_hist_ru002,
    "outputs/figures/figure_10_mc_joint_hist_ru002.png",
    width = 8,
    height = 5
  )
  
  figure_13_mc_joint_heatmap_ru002 <- plot_mc_joint_heatmap(projects_mc_joint, "RU-002")
  print(figure_13_mc_joint_heatmap_ru002)
  save_plot(
    figure_13_mc_joint_heatmap_ru002,
    "outputs/figures/figure_13_mc_joint_heatmap_ru002.png",
    width = 8,
    height = 5
  )
}

if ("RU-005" %in% projects_mc_joint$id) {
  figure_11_mc_joint_hist_ru005 <- plot_mc_joint_hist(projects_mc_joint, "RU-005")
  print(figure_11_mc_joint_hist_ru005)
  save_plot(
    figure_11_mc_joint_hist_ru005,
    "outputs/figures/figure_11_mc_joint_hist_ru005.png",
    width = 8,
    height = 5
  )
  
  figure_14_mc_joint_heatmap_ru005 <- plot_mc_joint_heatmap(projects_mc_joint, "RU-005")
  print(figure_14_mc_joint_heatmap_ru005)
  save_plot(
    figure_14_mc_joint_heatmap_ru005,
    "outputs/figures/figure_14_mc_joint_heatmap_ru005.png",
    width = 8,
    height = 5
  )
}

# ------------------------------------------------------------
# 14. Session end message
# ------------------------------------------------------------
message("07_monte_carlo_opex_wacc.R finished successfully.")
