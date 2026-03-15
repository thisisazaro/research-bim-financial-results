# ============================================================
# 03_functions_plots.R
# Purpose:
#   Store reusable plotting helpers for the BIM / PPP research
#   repository.
#
# Notes:
#   - These functions are intentionally lightweight and focused
#     on repeated visual tasks in the project.
#   - The file complements 04_theme.R, which defines the common
#     publication-style theme.
# ============================================================

# ------------------------------------------------------------
# 1. Plot Monte Carlo NPV density for a single project
# ------------------------------------------------------------
plot_mc_npv_density <- function(mc_data,
                                proj_id,
                                npv_col = "npv_sim",
                                base_col = "npv_base",
                                id_col = "id",
                                name_col = "project_name",
                                title_prefix = "NPV distribution under Monte Carlo simulation") {
  df_plot <- mc_data %>%
    dplyr::filter(.data[[id_col]] == proj_id)
  
  if (nrow(df_plot) == 0) {
    stop("No observations found for project: ", proj_id)
  }
  
  base_npv <- unique(df_plot[[base_col]])
  
  if (length(base_npv) != 1) {
    stop("Expected a unique baseline NPV for project: ", proj_id)
  }
  
  proj_name <- unique(df_plot[[name_col]])
  proj_name <- proj_name[!is.na(proj_name)][1]
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[npv_col]])) +
    ggplot2::geom_density() +
    ggplot2::geom_vline(xintercept = base_npv, linetype = "dashed") +
    ggplot2::labs(
      title = paste(title_prefix, "–", proj_id),
      subtitle = proj_name,
      x = "NPV (million USD)",
      y = "Density"
    ) +
    theme_article
}

# ------------------------------------------------------------
# 2. Plot NPV as a function of WACC for a single project
# ------------------------------------------------------------
plot_npv_vs_wacc <- function(projects_data,
                             proj_id,
                             r_base,
                             r_lower = NA_real_,
                             r_min = 0.05,
                             r_max = 0.10,
                             n_grid = 50) {
  proj <- projects_data %>%
    dplyr::filter(id == proj_id) %>%
    dplyr::slice(1)
  
  if (nrow(proj) == 0) {
    stop("Project not found: ", proj_id)
  }
  
  r_grid <- seq(r_min, r_max, length.out = n_grid)
  
  npv_grid <- sapply(
    r_grid,
    function(rr) npv_annuity(
      capex         = proj$capex_planned_musd,
      annual_net_cf = proj$annual_net_cf,
      T_years       = proj$term_ppp_years,
      r             = rr
    )
  )
  
  df_curve <- tibble::tibble(
    r = r_grid,
    npv = npv_grid
  )
  
  p <- ggplot2::ggplot(df_curve, ggplot2::aes(x = r * 100, y = npv)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = r_base * 100, linetype = "dashed") +
    ggplot2::labs(
      title = paste("NPV as a function of WACC –", proj_id),
      subtitle = proj$project_name,
      x = "WACC, % per year",
      y = "NPV (million USD)"
    ) +
    theme_article
  
  if (!is.na(r_lower)) {
    p <- p + ggplot2::geom_vline(xintercept = r_lower * 100, linetype = "dotted")
  }
  
  return(p)
}

# ------------------------------------------------------------
# 3. Plot comparison of baseline vs scenario NPV by project
# ------------------------------------------------------------
plot_npv_comparison <- function(data,
                                project_col = "project_name",
                                baseline_col = "npv_base",
                                scenario_col = "npv_scenario",
                                scenario_label = "Scenario NPV",
                                title = "Baseline vs scenario NPV") {
  df_plot <- data %>%
    dplyr::select(
      project_name = .data[[project_col]],
      baseline = .data[[baseline_col]],
      scenario = .data[[scenario_col]]
    ) %>%
    tidyr::pivot_longer(
      cols = c(baseline, scenario),
      names_to = "npv_type",
      values_to = "npv"
    ) %>%
    dplyr::mutate(
      npv_type = dplyr::recode(
        npv_type,
        baseline = "Baseline NPV",
        scenario = scenario_label
      )
    )
  
  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = reorder(project_name, npv), y = npv, fill = npv_type)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      x = "Project",
      y = "NPV (million USD)",
      fill = NULL
    ) +
    theme_article +
    ggplot2::theme(legend.position = "bottom")
}

# ------------------------------------------------------------
# 4. Plot point estimates with confidence intervals
# ------------------------------------------------------------
plot_coef_intervals <- function(data,
                                estimate_col = "estimate",
                                lower_col = "conf_low",
                                upper_col = "conf_high",
                                term_col = "term",
                                title = "Coefficient estimates with 95% confidence intervals",
                                xlab = "Estimate",
                                ylab = NULL) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[[estimate_col]],
      y = forcats::fct_reorder(.data[[term_col]], .data[[estimate_col]])
    )
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = .data[[lower_col]],
        xmax = .data[[upper_col]]
      ),
      height = 0.15
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_article
}

# ------------------------------------------------------------
# 5. Plot heatmap for binned simulation surfaces
# ------------------------------------------------------------
plot_surface_heatmap <- function(data,
                                 x_col,
                                 y_col,
                                 fill_col,
                                 title = "Simulation surface",
                                 xlab = NULL,
                                 ylab = NULL,
                                 fill_lab = "Value") {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[y_col]],
      fill = .data[[fill_col]]
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(name = fill_lab) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_article +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
