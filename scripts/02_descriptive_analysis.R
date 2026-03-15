# ============================================================
# 02_descriptive_analysis.R
# Purpose:
#   Produce descriptive statistics and first-look visualizations
#   for PPP projects, BIM adoption, construction delays, and
#   CAPEX overruns.
#
# Inputs:
#   outputs/rds/ds1_clean.rds
#
# Outputs:
#   outputs/tables/table_01_ppp_distribution.csv
#   outputs/tables/table_02_bim_stage_shares_ppp.csv
#   outputs/tables/table_03_capex_overrun_by_bim_construction.csv
#   outputs/tables/table_04_delay_by_bim_construction.csv
#   outputs/figures/figure_01_bim_adoption_across_ppp_stages.png
#   outputs/figures/figure_02_schedule_delays_vs_bim_construction.png
#   outputs/figures/figure_03_capex_overrun_vs_bim_construction.png
#
# Notes:
#   - This script focuses on descriptive evidence only.
#   - No causal claims are made at this stage.
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
source("R/00_packages.R")
source("R/01_functions_general.R")
source("R/04_theme.R")

ensure_dir("outputs/tables")
ensure_dir("outputs/figures")

# ------------------------------------------------------------
# 1. Load processed data
# ------------------------------------------------------------
ds1_clean <- readRDS("outputs/rds/ds1_clean.rds")

data_overview(ds1_clean, "ds1_clean")

# ------------------------------------------------------------
# 2. Basic validation checks
# ------------------------------------------------------------
required_vars <- c(
  "id", "project_name", "is_ppp",
  "bim_design", "bim_construction", "bim_operation",
  "construction_stage_schedule_delay", "capex_overrun_pct"
)

missing_vars <- required_vars[!required_vars %in% names(ds1_clean)]

if (length(missing_vars) > 0) {
  stop(
    "The following required variables are missing in ds1_clean:\n",
    paste0("- ", missing_vars, collapse = "\n")
  )
}

message("All required variables for descriptive analysis are available.")

# ------------------------------------------------------------
# 3. PPP vs non-PPP distribution
# ------------------------------------------------------------
table_01_ppp_distribution <- ds1_clean %>%
  mutate(
    project_type = if_else(is_ppp, "PPP", "Non-PPP")
  ) %>%
  count(project_type, name = "n_projects") %>%
  mutate(
    share = n_projects / sum(n_projects)
  )

print(knitr::kable(
  table_01_ppp_distribution,
  digits = 3,
  caption = "Distribution of projects by PPP status"
))

save_csv(
  table_01_ppp_distribution,
  "outputs/tables/table_01_ppp_distribution.csv"
)

# ------------------------------------------------------------
# 4. BIM adoption across project stages (PPP only)
# ------------------------------------------------------------
table_02_bim_stage_shares_ppp <- ds1_clean %>%
  filter(is_ppp) %>%
  summarise(
    n_projects             = n(),
    share_bim_design       = mean(bim_design, na.rm = TRUE),
    share_bim_construction = mean(bim_construction, na.rm = TRUE),
    share_bim_operation    = mean(bim_operation, na.rm = TRUE)
  )

print(knitr::kable(
  table_02_bim_stage_shares_ppp,
  digits = 3,
  caption = "BIM adoption rates across PPP project stages"
))

save_csv(
  table_02_bim_stage_shares_ppp,
  "outputs/tables/table_02_bim_stage_shares_ppp.csv"
)

# Long format for plotting
bim_stage_summary <- ds1_clean %>%
  filter(is_ppp) %>%
  summarise(
    Design       = mean(bim_design, na.rm = TRUE),
    Construction = mean(bim_construction, na.rm = TRUE),
    Operation    = mean(bim_operation, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "stage",
    values_to = "share"
  )

figure_01_bim_adoption_across_ppp_stages <- ggplot(
  bim_stage_summary,
  aes(x = stage, y = share)
) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "BIM adoption across PPP project stages",
    x = "Project stage",
    y = "Share of PPP projects with BIM"
  ) +
  theme_article

print(figure_01_bim_adoption_across_ppp_stages)

save_plot(
  figure_01_bim_adoption_across_ppp_stages,
  "outputs/figures/figure_01_bim_adoption_across_ppp_stages.png",
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# 5. CAPEX overrun by BIM use during construction (PPP only)
# ------------------------------------------------------------
table_03_capex_overrun_by_bim_construction <- ds1_clean %>%
  filter(is_ppp) %>%
  mutate(
    bim_construction_label = factor(
      bim_construction,
      levels = c(FALSE, TRUE),
      labels = c("No BIM", "BIM")
    )
  ) %>%
  group_by(bim_construction_label) %>%
  summarise(
    n_projects      = n(),
    mean_overrun    = mean(capex_overrun_pct, na.rm = TRUE),
    median_overrun  = median(capex_overrun_pct, na.rm = TRUE),
    sd_overrun      = sd(capex_overrun_pct, na.rm = TRUE),
    min_overrun     = min(capex_overrun_pct, na.rm = TRUE),
    max_overrun     = max(capex_overrun_pct, na.rm = TRUE),
    .groups = "drop"
  )

print(knitr::kable(
  table_03_capex_overrun_by_bim_construction,
  digits = 2,
  caption = "CAPEX overrun by BIM use during construction (PPP projects)"
))

save_csv(
  table_03_capex_overrun_by_bim_construction,
  "outputs/tables/table_03_capex_overrun_by_bim_construction.csv"
)

figure_03_capex_overrun_vs_bim_construction <- ds1_clean %>%
  filter(is_ppp, !is.na(capex_overrun_pct)) %>%
  mutate(
    bim_construction_label = factor(
      bim_construction,
      levels = c(FALSE, TRUE),
      labels = c("No BIM", "BIM")
    )
  ) %>%
  ggplot(aes(x = bim_construction_label, y = capex_overrun_pct)) +
  geom_boxplot() +
  labs(
    title = "CAPEX overruns and BIM use during construction",
    x = "BIM during construction phase",
    y = "CAPEX overrun (%)"
  ) +
  theme_article

print(figure_03_capex_overrun_vs_bim_construction)

save_plot(
  figure_03_capex_overrun_vs_bim_construction,
  "outputs/figures/figure_03_capex_overrun_vs_bim_construction.png",
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# 6. Construction schedule delay by BIM use during construction
#    (PPP only)
# ------------------------------------------------------------
table_04_delay_by_bim_construction <- ds1_clean %>%
  filter(is_ppp) %>%
  mutate(
    bim_construction_label = factor(
      bim_construction,
      levels = c(FALSE, TRUE),
      labels = c("No BIM", "BIM")
    )
  ) %>%
  group_by(bim_construction_label) %>%
  summarise(
    n_projects           = n(),
    mean_delay_months    = mean(construction_stage_schedule_delay, na.rm = TRUE),
    median_delay_months  = median(construction_stage_schedule_delay, na.rm = TRUE),
    sd_delay_months      = sd(construction_stage_schedule_delay, na.rm = TRUE),
    min_delay_months     = min(construction_stage_schedule_delay, na.rm = TRUE),
    max_delay_months     = max(construction_stage_schedule_delay, na.rm = TRUE),
    .groups = "drop"
  )

print(knitr::kable(
  table_04_delay_by_bim_construction,
  digits = 2,
  caption = "Construction schedule delay by BIM use during construction (PPP projects)"
))

save_csv(
  table_04_delay_by_bim_construction,
  "outputs/tables/table_04_delay_by_bim_construction.csv"
)

figure_02_schedule_delays_vs_bim_construction <- ds1_clean %>%
  filter(is_ppp, !is.na(construction_stage_schedule_delay)) %>%
  mutate(
    bim_construction_label = factor(
      bim_construction,
      levels = c(FALSE, TRUE),
      labels = c("No BIM", "BIM")
    )
  ) %>%
  ggplot(aes(x = bim_construction_label, y = construction_stage_schedule_delay)) +
  geom_boxplot() +
  labs(
    title = "Construction schedule delays and BIM use (PPP projects)",
    x = "BIM during construction phase",
    y = "Schedule delay (months)"
  ) +
  theme_article

print(figure_02_schedule_delays_vs_bim_construction)

save_plot(
  figure_02_schedule_delays_vs_bim_construction,
  "outputs/figures/figure_02_schedule_delays_vs_bim_construction.png",
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# 7. Optional: compact descriptive memo table for quick review
# ------------------------------------------------------------
descriptive_memo <- tibble::tibble(
  metric = c(
    "Total number of projects",
    "Number of PPP projects",
    "Share of PPP projects with BIM at design stage",
    "Share of PPP projects with BIM at construction stage",
    "Share of PPP projects with BIM at operation stage"
  ),
  value = c(
    nrow(ds1_clean),
    sum(ds1_clean$is_ppp, na.rm = TRUE),
    round(table_02_bim_stage_shares_ppp$share_bim_design, 3),
    round(table_02_bim_stage_shares_ppp$share_bim_construction, 3),
    round(table_02_bim_stage_shares_ppp$share_bim_operation, 3)
  )
)

print(knitr::kable(
  descriptive_memo,
  caption = "Compact descriptive summary"
))

save_csv(
  descriptive_memo,
  "outputs/tables/table_00_descriptive_memo.csv"
)

# ------------------------------------------------------------
# 8. Session end message
# ------------------------------------------------------------
message("02_descriptive_analysis.R finished successfully.")
