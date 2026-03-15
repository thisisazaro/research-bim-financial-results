# ============================================================
# 03_project_level_opex_and_dcf.R
# Purpose:
#   Build project-level operating and financial summaries from
#   annual observations, merge them with PPP project metadata,
#   and compute baseline DCF / NPV estimates.
#
# Inputs:
#   outputs/rds/ds1_clean.rds
#   outputs/rds/ds2_clean.rds
#
# Outputs:
#   outputs/rds/ds2_with_meta.rds
#   outputs/rds/ds2_project_summary.rds
#   outputs/rds/projects_dcf_base.rds
#   outputs/tables/table_05_project_year_operating_summary.csv
#   outputs/tables/table_06_project_level_opex_summary.csv
#   outputs/tables/table_07_projects_dcf_base.csv
#
# Notes:
#   - This script does not yet apply BIM effect scenarios.
#   - The goal here is to establish the baseline financial
#     structure of PPP projects before scenario analysis.
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
source("R/00_packages.R")
source("R/01_functions_general.R")
source("R/02_functions_dcf.R")

ensure_dir("outputs/rds")
ensure_dir("outputs/tables")

# ------------------------------------------------------------
# 1. Load processed data
# ------------------------------------------------------------
ds1_clean <- readRDS("outputs/rds/ds1_clean.rds")
ds2_clean <- readRDS("outputs/rds/ds2_clean.rds")

data_overview(ds1_clean, "ds1_clean")
data_overview(ds2_clean, "ds2_clean")

# ------------------------------------------------------------
# 2. Validation checks
# ------------------------------------------------------------
required_vars_ds1 <- c(
  "id", "project_name", "is_ppp", "country", "sector",
  "bim_design", "bim_construction", "bim_operation",
  "capex_planned_musd", "term_ppp_years"
)

required_vars_ds2 <- c(
  "id", "project_name", "year", "revenue",
  "construction_stage_capex", "maintenance_obligations", "salary"
)

missing_ds1 <- required_vars_ds1[!required_vars_ds1 %in% names(ds1_clean)]
missing_ds2 <- required_vars_ds2[!required_vars_ds2 %in% names(ds2_clean)]

if (length(missing_ds1) > 0) {
  stop(
    "The following required variables are missing in ds1_clean:\n",
    paste0("- ", missing_ds1, collapse = "\n")
  )
}

if (length(missing_ds2) > 0) {
  stop(
    "The following required variables are missing in ds2_clean:\n",
    paste0("- ", missing_ds2, collapse = "\n")
  )
}

message("All required variables for project-level DCF construction are available.")

# ------------------------------------------------------------
# 3. Merge annual operating data with project metadata
# ------------------------------------------------------------
# We use Dataset 1 as the source of project metadata
# and Dataset 2 as the source of annual operating observations.

ds2_with_meta <- ds2_clean %>%
  left_join(
    ds1_clean %>%
      dplyr::select(
        id, project_name, is_ppp, country, sector,
        bim_design, bim_construction, bim_operation,
        capex_planned_musd, term_ppp_years
      ),
    by = "id",
    suffix = c("_ds2", "_ds1")
  ) %>%
  mutate(
    project_name = dplyr::coalesce(project_name_ds1, project_name_ds2)
  ) %>%
  dplyr::select(
    no,
    id,
    project_name,
    spv_name,
    source,
    year,
    revenue,
    construction_stage_capex,
    maintenance_obligations,
    salary,
    is_ppp,
    country,
    sector,
    bim_design,
    bim_construction,
    bim_operation,
    capex_planned_musd,
    term_ppp_years
  )

saveRDS(ds2_with_meta, "outputs/rds/ds2_with_meta.rds")

# ------------------------------------------------------------
# 4. Annual operating summary check
# ------------------------------------------------------------
# This is still panel-like data (project-year level),
# but already enriched with project metadata.

table_05_project_year_operating_summary <- ds2_with_meta %>%
  group_by(id, project_name, is_ppp, country, sector) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    years_obs  = n(),
    total_revenue = sum(revenue, na.rm = TRUE),
    total_maintenance = sum(maintenance_obligations, na.rm = TRUE),
    total_salary = sum(salary, na.rm = TRUE),
    .groups = "drop"
  )

print(knitr::kable(
  table_05_project_year_operating_summary,
  digits = 2,
  caption = "Project-year operating summary before DCF aggregation"
))

save_csv(
  table_05_project_year_operating_summary,
  "outputs/tables/table_05_project_year_operating_summary.csv"
)

# ------------------------------------------------------------
# 5. Aggregate annual operating data to project level
# ------------------------------------------------------------
# We collapse annual observations into project-level operating
# summaries that can be used as DCF inputs.

ds2_project_summary <- ds2_with_meta %>%
  group_by(
    id, project_name, is_ppp, country, sector,
    bim_design, bim_construction, bim_operation,
    capex_planned_musd, term_ppp_years
  ) %>%
  summarise(
    years_obs = n(),
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    
    total_revenue = sum(revenue, na.rm = TRUE),
    total_construction_stage_capex = sum(construction_stage_capex, na.rm = TRUE),
    total_maintenance = sum(maintenance_obligations, na.rm = TRUE),
    total_salary = sum(salary, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    total_opex = total_maintenance + total_salary,
    opex_to_revenue = if_else(
      !is.na(total_revenue) & total_revenue > 0,
      total_opex / total_revenue,
      NA_real_
    ),
    annual_revenue = if_else(
      !is.na(years_obs) & years_obs > 0,
      total_revenue / years_obs,
      NA_real_
    ),
    annual_opex = if_else(
      !is.na(years_obs) & years_obs > 0,
      total_opex / years_obs,
      NA_real_
    ),
    annual_net_cf = annual_revenue - annual_opex
  )

saveRDS(ds2_project_summary, "outputs/rds/ds2_project_summary.rds")

table_06_project_level_opex_summary <- ds2_project_summary %>%
  dplyr::select(
    id, project_name, is_ppp, country, sector,
    years_obs, first_year, last_year,
    total_revenue, total_maintenance, total_salary,
    total_opex, opex_to_revenue,
    annual_revenue, annual_opex, annual_net_cf
  )

print(knitr::kable(
  table_06_project_level_opex_summary,
  digits = 2,
  caption = "Project-level operating and OPEX summary"
))

save_csv(
  table_06_project_level_opex_summary,
  "outputs/tables/table_06_project_level_opex_summary.csv"
)

# ------------------------------------------------------------
# 6. Restrict to PPP projects for baseline DCF construction
# ------------------------------------------------------------
projects_dcf_base <- ds2_project_summary %>%
  filter(is_ppp) %>%
  mutate(
    npv_base = NA_real_
  )

# ------------------------------------------------------------
# 7. Baseline discount rate
# ------------------------------------------------------------
# The baseline rate is fixed at 8% per year for the initial
# thesis DCF specification.

r_base <- 0.08

# ------------------------------------------------------------
# 8. Compute baseline NPV
# ------------------------------------------------------------
projects_dcf_base <- projects_dcf_base %>%
  mutate(
    npv_base = npv_annuity(
      capex         = capex_planned_musd,
      annual_net_cf = annual_net_cf,
      T_years       = term_ppp_years,
      r             = r_base
    )
  )

saveRDS(projects_dcf_base, "outputs/rds/projects_dcf_base.rds")

table_07_projects_dcf_base <- projects_dcf_base %>%
  dplyr::select(
    id, project_name, country, sector,
    bim_design, bim_construction, bim_operation,
    capex_planned_musd, term_ppp_years,
    annual_revenue, annual_opex, annual_net_cf,
    npv_base
  )

print(knitr::kable(
  table_07_projects_dcf_base,
  digits = 2,
  caption = "Baseline project-level DCF inputs and NPV estimates"
))

save_csv(
  table_07_projects_dcf_base,
  "outputs/tables/table_07_projects_dcf_base.csv"
)

# ------------------------------------------------------------
# 9. Compact diagnostic summary
# ------------------------------------------------------------
dcf_diagnostics <- tibble::tibble(
  metric = c(
    "Number of project-year observations",
    "Number of unique projects in ds2",
    "Number of PPP projects in baseline DCF sample",
    "Mean baseline annual revenue",
    "Mean baseline annual OPEX",
    "Mean baseline annual net cash flow",
    "Mean baseline NPV"
  ),
  value = c(
    nrow(ds2_with_meta),
    dplyr::n_distinct(ds2_with_meta$id),
    nrow(projects_dcf_base),
    mean(projects_dcf_base$annual_revenue, na.rm = TRUE),
    mean(projects_dcf_base$annual_opex, na.rm = TRUE),
    mean(projects_dcf_base$annual_net_cf, na.rm = TRUE),
    mean(projects_dcf_base$npv_base, na.rm = TRUE)
  )
)

print(knitr::kable(
  dcf_diagnostics,
  digits = 2,
  caption = "Compact diagnostics for the baseline project-level DCF sample"
))

save_csv(
  dcf_diagnostics,
  "outputs/tables/table_08_dcf_diagnostics.csv"
)

# ------------------------------------------------------------
# 10. Session end message
# ------------------------------------------------------------
message("03_project_level_opex_and_dcf.R finished successfully.")
