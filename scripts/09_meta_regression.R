# ============================================================
# 09_meta_regression.R
# Purpose:
#   Estimate meta-regression models for BIM effect sizes using
#   the literature-based evidence dataset and evaluate how
#   project stage, effect type, DCF channel, country, and time
#   are associated with the magnitude of reported BIM effects.
#
# Inputs:
#   outputs/rds/ds4_clean.rds
#
# Outputs:
#   outputs/rds/ds4_meta.rds
#   outputs/rds/ds4_meta_grouped.rds
#   outputs/rds/meta_models.rds
#   outputs/rds/meta_tidy_all.rds
#   outputs/rds/meta_glance_all.rds
#   outputs/rds/pred_meta_stage_effect.rds
#   outputs/rds/pred_meta_country.rds
#   outputs/rds/pred_meta_mechanism.rds
#   outputs/tables/table_29_meta_model_fit.csv
#   outputs/tables/table_30_meta_coefficients_long.csv
#   outputs/tables/table_31_meta_stage_predictions.csv
#   outputs/tables/table_32_meta_country_predictions.csv
#   outputs/tables/table_33_meta_mechanism_predictions.csv
#   outputs/figures/figure_19_meta_stage_effects.png
#   outputs/figures/figure_20_meta_country_effects.png
#   outputs/figures/figure_21_meta_mechanism_effects.png
#   outputs/figures/figure_22_meta_model4_coefficients.png
#   outputs/figures/figure_23_meta_model5_coefficients.png
#   outputs/figures/figure_24_meta_stage_coefficients_across_models.png
#
# Notes:
#   - This script uses the literature-based dataset only.
#   - The dependent variable is log(1 + effect), where effect
#     is the mapped BIM effect size.
#   - The goal is explanatory structure and calibration, not
#     causal identification.
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
source("R/00_packages.R")
source("R/01_functions_general.R")
source("R/03_functions_plots.R")
source("R/04_theme.R")

ensure_dir("outputs/rds")
ensure_dir("outputs/tables")
ensure_dir("outputs/figures")

# ------------------------------------------------------------
# 1. Load literature-based evidence
# ------------------------------------------------------------
ds4_clean <- readRDS("outputs/rds/ds4_clean.rds")

data_overview(ds4_clean, "ds4_clean")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars <- c(
  "stage_clean",
  "dcf_influence",
  "unified_metric",
  "score_to_the_unified_metric",
  "country",
  "year"
)

missing_vars <- required_vars[!required_vars %in% names(ds4_clean)]

if (length(missing_vars) > 0) {
  stop(
    "The following required variables are missing in ds4_clean:\n",
    paste0("- ", missing_vars, collapse = "\n")
  )
}

message("All required variables for meta-regression are available.")

# ------------------------------------------------------------
# 3. Prepare meta-regression dataset
# ------------------------------------------------------------
# Main transformations:
#   - classify effect type: time vs cost
#   - convert stage / country / dcf influence to factors
#   - convert year to numeric
#   - define log-transformed dependent variable:
#       log(1 + effect)

ds4_meta <- ds4_clean %>%
  dplyr::filter(!is.na(score_to_the_unified_metric)) %>%
  dplyr::mutate(
    effect_type = dplyr::case_when(
      stringr::str_detect(
        unified_metric,
        stringr::regex("time|schedule|duration|delay", ignore_case = TRUE)
      ) ~ "time",
      TRUE ~ "cost"
    ),
    effect_type = factor(effect_type),
    
    stage_clean = factor(stage_clean),
    dcf_influence = factor(dcf_influence),
    country = factor(country),
    year = suppressWarnings(as.numeric(year)),
    
    log_effect = log1p(score_to_the_unified_metric)
  )

saveRDS(ds4_meta, "outputs/rds/ds4_meta.rds")

# ------------------------------------------------------------
# 4. Group DCF channels into broader categories
# ------------------------------------------------------------
# The goal is to move from very granular DCF influence labels
# to cleaner mechanism groups for interpretable specifications.

ds4_meta_grouped <- ds4_meta %>%
  dplyr::mutate(
    dcf_group = dplyr::case_when(
      stringr::str_detect(dcf_influence, "schedule") ~ "Schedule",
      stringr::str_detect(dcf_influence, "CAPEX") ~ "CAPEX",
      stringr::str_detect(dcf_influence, "Maintenance|Salary") ~ "OPEX",
      TRUE ~ "Other"
    ),
    dcf_group = factor(dcf_group),
    
    dcf_group2 = dplyr::if_else(
      dcf_group == "Schedule",
      "Other",
      as.character(dcf_group)
    ),
    dcf_group2 = factor(dcf_group2)
  )

saveRDS(ds4_meta_grouped, "outputs/rds/ds4_meta_grouped.rds")

# ------------------------------------------------------------
# 5. Estimate meta-regression specifications
# ------------------------------------------------------------
# Model logic:
#   M1: stage only
#   M2: + effect type
#   M3: + country
#   M4: + country + year
#   M5: + grouped DCF channels + year

meta_mod_1 <- stats::lm(
  log_effect ~ stage_clean,
  data = ds4_meta_grouped
)

meta_mod_2 <- stats::lm(
  log_effect ~ stage_clean + effect_type,
  data = ds4_meta_grouped
)

meta_mod_3 <- stats::lm(
  log_effect ~ stage_clean + effect_type + country,
  data = ds4_meta_grouped
)

meta_mod_4 <- stats::lm(
  log_effect ~ stage_clean + effect_type + country + year,
  data = ds4_meta_grouped
)

meta_mod_5 <- stats::lm(
  log_effect ~ stage_clean + effect_type + dcf_group2 + year,
  data = ds4_meta_grouped
)

meta_models <- list(
  "M1: Stage only" = meta_mod_1,
  "M2: Stage + effect type" = meta_mod_2,
  "M3: + country" = meta_mod_3,
  "M4: + country + year" = meta_mod_4,
  "M5: + DCF groups + year" = meta_mod_5
)

saveRDS(meta_models, "outputs/rds/meta_models.rds")

# ------------------------------------------------------------
# 6. Tidy coefficients and model fit statistics
# ------------------------------------------------------------
meta_tidy_all <- purrr::imap_dfr(
  meta_models,
  ~ broom::tidy(.x) %>%
    dplyr::mutate(model = .y)
)

meta_glance_all <- purrr::imap_dfr(
  meta_models,
  ~ broom::glance(.x) %>%
    dplyr::mutate(model = .y)
)

saveRDS(meta_tidy_all, "outputs/rds/meta_tidy_all.rds")
saveRDS(meta_glance_all, "outputs/rds/meta_glance_all.rds")

table_29_meta_model_fit <- meta_glance_all %>%
  dplyr::select(
    model,
    nobs,
    r.squared,
    adj.r.squared,
    AIC,
    BIC
  ) %>%
  dplyr::arrange(model)

print(knitr::kable(
  table_29_meta_model_fit,
  digits = 3,
  caption = "Summary of meta-regression model fit statistics"
))

save_csv(
  table_29_meta_model_fit,
  "outputs/tables/table_29_meta_model_fit.csv"
)

table_30_meta_coefficients_long <- meta_tidy_all %>%
  dplyr::select(
    model,
    term,
    estimate,
    std.error,
    statistic,
    p.value
  ) %>%
  dplyr::arrange(term, model)

print(knitr::kable(
  table_30_meta_coefficients_long,
  digits = 3,
  caption = "Meta-regression coefficients across all specifications"
))

save_csv(
  table_30_meta_coefficients_long,
  "outputs/tables/table_30_meta_coefficients_long.csv"
)

# ------------------------------------------------------------
# 7. Predicted BIM effect by stage and effect type
# ------------------------------------------------------------
grid_meta <- expand.grid(
  stage_clean = levels(ds4_meta_grouped$stage_clean),
  effect_type = levels(ds4_meta_grouped$effect_type)
)

pred_meta_stage_effect <- cbind(
  grid_meta,
  stats::predict(meta_mod_2, newdata = grid_meta, se.fit = TRUE)
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    log_fit = fit,
    log_lwr = fit - 1.96 * se.fit,
    log_upr = fit + 1.96 * se.fit,
    
    eff_hat = exp(log_fit) - 1,
    eff_lwr = exp(log_lwr) - 1,
    eff_upr = exp(log_upr) - 1,
    
    eff_hat_pct = eff_hat * 100,
    eff_lwr_pct = eff_lwr * 100,
    eff_upr_pct = eff_upr * 100
  )

saveRDS(
  pred_meta_stage_effect,
  "outputs/rds/pred_meta_stage_effect.rds"
)

save_csv(
  pred_meta_stage_effect,
  "outputs/tables/table_31_meta_stage_predictions.csv"
)

figure_19_meta_stage_effects <- ggplot2::ggplot(
  pred_meta_stage_effect,
  ggplot2::aes(
    x = stage_clean,
    y = eff_hat_pct,
    colour = effect_type,
    group = effect_type
  )
) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.4),
    size = 2.3
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = eff_lwr_pct, ymax = eff_upr_pct),
    position = ggplot2::position_dodge(width = 0.4),
    width = 0.2
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Meta-regression: predicted BIM effect by project stage and metric type",
    x = "Project stage",
    y = "Predicted BIM effect (%)",
    colour = "Effect type"
  ) +
  theme_article +
  ggplot2::theme(legend.position = "bottom")

print(figure_19_meta_stage_effects)

save_plot(
  figure_19_meta_stage_effects,
  "outputs/figures/figure_19_meta_stage_effects.png",
  width = 9,
  height = 6
)

# ------------------------------------------------------------
# 8. Predicted BIM effect by country
# ------------------------------------------------------------
ref_stage <- names(sort(table(ds4_meta_grouped$stage_clean), decreasing = TRUE))[1]
ref_effect <- names(sort(table(ds4_meta_grouped$effect_type), decreasing = TRUE))[1]
ref_year <- mean(ds4_meta_grouped$year, na.rm = TRUE)

newdata_country <- expand.grid(
  stage_clean = ref_stage,
  effect_type = ref_effect,
  country = levels(ds4_meta_grouped$country),
  year = ref_year
)

pred_meta_country <- cbind(
  newdata_country,
  stats::predict(meta_mod_4, newdata = newdata_country, se.fit = TRUE)
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    log_fit = fit,
    log_lwr = fit - 1.96 * se.fit,
    log_upr = fit + 1.96 * se.fit,
    
    eff_hat = exp(log_fit) - 1,
    eff_lwr = exp(log_lwr) - 1,
    eff_upr = exp(log_upr) - 1,
    
    eff_hat_pct = eff_hat * 100,
    eff_lwr_pct = eff_lwr * 100,
    eff_upr_pct = eff_upr * 100,
    
    country = forcats::fct_reorder(country, eff_hat_pct)
  )

saveRDS(pred_meta_country, "outputs/rds/pred_meta_country.rds")

save_csv(
  pred_meta_country,
  "outputs/tables/table_32_meta_country_predictions.csv"
)

figure_20_meta_country_effects <- ggplot2::ggplot(
  pred_meta_country,
  ggplot2::aes(x = eff_hat_pct, y = country)
) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = eff_lwr_pct, xmax = eff_upr_pct),
    height = 0.2
  ) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Meta-regression (extended): predicted BIM effect by country",
    subtitle = paste0(
      "Stage: ", ref_stage,
      ", effect type: ", ref_effect,
      ", year ≈ ", round(ref_year)
    ),
    x = "Predicted BIM effect (%)",
    y = "Country"
  ) +
  theme_article

print(figure_20_meta_country_effects)

save_plot(
  figure_20_meta_country_effects,
  "outputs/figures/figure_20_meta_country_effects.png",
  width = 9,
  height = 6
)

# ------------------------------------------------------------
# 9. Predicted BIM effect by stage and DCF mechanism
# ------------------------------------------------------------
ref_effect_m <- names(sort(table(ds4_meta_grouped$effect_type), decreasing = TRUE))[1]
ref_year_m <- mean(ds4_meta_grouped$year, na.rm = TRUE)

newdata_mechanism <- expand.grid(
  stage_clean = levels(ds4_meta_grouped$stage_clean),
  dcf_group2 = levels(ds4_meta_grouped$dcf_group2),
  effect_type = ref_effect_m,
  year = ref_year_m
)

pred_meta_mechanism <- cbind(
  newdata_mechanism,
  stats::predict(meta_mod_5, newdata = newdata_mechanism, se.fit = TRUE)
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    log_fit = fit,
    log_lwr = fit - 1.96 * se.fit,
    log_upr = fit + 1.96 * se.fit,
    
    eff_hat = exp(log_fit) - 1,
    eff_lwr = exp(log_lwr) - 1,
    eff_upr = exp(log_upr) - 1,
    
    eff_hat_pct = eff_hat * 100,
    eff_lwr_pct = eff_lwr * 100,
    eff_upr_pct = eff_upr * 100
  )

saveRDS(pred_meta_mechanism, "outputs/rds/pred_meta_mechanism.rds")

save_csv(
  pred_meta_mechanism,
  "outputs/tables/table_33_meta_mechanism_predictions.csv"
)

figure_21_meta_mechanism_effects <- ggplot2::ggplot(
  pred_meta_mechanism,
  ggplot2::aes(
    x = stage_clean,
    y = eff_hat_pct,
    colour = dcf_group2
  )
) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.4),
    size = 2
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = eff_lwr_pct, ymax = eff_upr_pct),
    position = ggplot2::position_dodge(width = 0.4),
    width = 0.2
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Meta-regression (grouped): predicted BIM effect by project stage and DCF channel",
    subtitle = paste0(
      "Effect type: ", ref_effect_m,
      ", year ≈ ", round(ref_year_m)
    ),
    x = "Project stage",
    y = "Predicted BIM effect (%)",
    colour = "DCF impact group"
  ) +
  theme_article +
  ggplot2::theme(legend.position = "bottom")

print(figure_21_meta_mechanism_effects)

save_plot(
  figure_21_meta_mechanism_effects,
  "outputs/figures/figure_21_meta_mechanism_effects.png",
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 10. Coefficient plots for key specifications
# ------------------------------------------------------------
coef_m4 <- broom::tidy(meta_mod_4) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    conf_low = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error
  )

figure_22_meta_model4_coefficients <- plot_coef_intervals(
  data = coef_m4,
  estimate_col = "estimate",
  lower_col = "conf_low",
  upper_col = "conf_high",
  term_col = "term",
  title = "Meta-regression coefficients (Model 4: stage, effect type, country, year)",
  xlab = "Coefficient estimate (log(1 + effect))",
  ylab = NULL
)

print(figure_22_meta_model4_coefficients)

save_plot(
  figure_22_meta_model4_coefficients,
  "outputs/figures/figure_22_meta_model4_coefficients.png",
  width = 10,
  height = 6
)

coef_m5 <- broom::tidy(meta_mod_5) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    conf_low = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error
  )

figure_23_meta_model5_coefficients <- plot_coef_intervals(
  data = coef_m5,
  estimate_col = "estimate",
  lower_col = "conf_low",
  upper_col = "conf_high",
  term_col = "term",
  title = "Meta-regression coefficients (Model 5: stage, effect type, DCF groups, year)",
  xlab = "Coefficient estimate (log(1 + effect))",
  ylab = NULL
)

print(figure_23_meta_model5_coefficients)

save_plot(
  figure_23_meta_model5_coefficients,
  "outputs/figures/figure_23_meta_model5_coefficients.png",
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 11. Stage coefficients across model specifications
# ------------------------------------------------------------
coef_stage_all <- meta_tidy_all %>%
  dplyr::filter(
    term %in% c(
      "stage_cleanConstruction",
      "stage_cleanDesign",
      "stage_cleanOperation"
    )
  ) %>%
  dplyr::mutate(
    conf_low = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error,
    term_label = dplyr::recode(
      term,
      "stage_cleanConstruction" = "Construction",
      "stage_cleanDesign" = "Design",
      "stage_cleanOperation" = "Operation"
    ),
    term_label = factor(
      term_label,
      levels = c("Design", "Construction", "Operation")
    ),
    model = factor(
      model,
      levels = c(
        "M1: Stage only",
        "M2: Stage + effect type",
        "M3: + country",
        "M4: + country + year",
        "M5: + DCF groups + year"
      )
    )
  )

figure_24_meta_stage_coefficients_across_models <- ggplot2::ggplot(
  coef_stage_all,
  ggplot2::aes(x = model, y = estimate, colour = term_label)
) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.5),
    size = 2
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = conf_low, ymax = conf_high),
    position = ggplot2::position_dodge(width = 0.5),
    width = 0.2
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  ggplot2::labs(
    title = "Stage effects across meta-regression specifications",
    x = "Model specification",
    y = "Coefficient estimate (log(1 + effect))",
    colour = "Project stage"
  ) +
  theme_article +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, size = 8),
    legend.position = "bottom"
  )

print(figure_24_meta_stage_coefficients_across_models)

save_plot(
  figure_24_meta_stage_coefficients_across_models,
  "outputs/figures/figure_24_meta_stage_coefficients_across_models.png",
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 12. Compact memo for the meta-regression layer
# ------------------------------------------------------------
meta_memo <- tibble::tibble(
  item = c(
    "Number of literature-based observations",
    "Mean raw BIM effect",
    "Median raw BIM effect",
    "Mean log(1 + effect)",
    "Best adjusted R-squared across models",
    "Reference model for country effects",
    "Reference model for mechanism effects"
  ),
  value = c(
    nrow(ds4_meta_grouped),
    mean(ds4_meta_grouped$score_to_the_unified_metric, na.rm = TRUE),
    median(ds4_meta_grouped$score_to_the_unified_metric, na.rm = TRUE),
    mean(ds4_meta_grouped$log_effect, na.rm = TRUE),
    max(meta_glance_all$adj.r.squared, na.rm = TRUE),
    "M4: + country + year",
    "M5: + DCF groups + year"
  )
)

print(knitr::kable(
  meta_memo,
  digits = 3,
  caption = "Compact memo of the meta-regression layer"
))

save_csv(
  meta_memo,
  "outputs/tables/table_34_meta_memo.csv"
)

# ------------------------------------------------------------
# 13. Session end message
# ------------------------------------------------------------
message("09_meta_regression.R finished successfully.")

