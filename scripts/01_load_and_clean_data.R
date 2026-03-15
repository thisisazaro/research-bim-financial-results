
# ============================================================
# 01_load_and_clean_data.R
# Purpose:
#   Load the four thesis datasets, standardize variable names,
#   clean key fields, derive analytical variables, and save
#   processed objects for downstream analysis.
#
# Outputs:
#   outputs/rds/ds1_clean.rds
#   outputs/rds/ds2_clean.rds
#   outputs/rds/ds3_clean.rds
#   outputs/rds/ds4_clean.rds
#
# Notes:
#   - Dataset 1: PPP / infrastructure project-level data
#   - Dataset 2: project-year operational / financial data
#   - Dataset 3: company survey / expert mapping
#   - Dataset 4: literature-based BIM effect mapping
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
rm(list = ls())

source("R/00_packages.R")
source("R/01_functions_general.R")

options(stringsAsFactors = FALSE)
theme_set(theme_bw())

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------
path_ds1 <- "data/raw/Dataset 1_Thesis.xlsx"
path_ds2 <- "data/raw/Dataset 2_Thesis.xlsx"
path_ds3 <- "data/raw/Dataset 3_Thesis.xlsx"
path_ds4 <- "data/raw/Dataset 4_Thesis.xlsx"

dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/rds", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Input checks
# ------------------------------------------------------------
required_files <- c(path_ds1, path_ds2, path_ds3, path_ds4)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "The following required input files are missing:\n",
    paste0("- ", missing_files, collapse = "\n")
  )
}

message("All raw input files found. Starting data import...")

# ------------------------------------------------------------
# 3. Load raw Excel datasets
# ------------------------------------------------------------
ds1_raw <- readxl::read_excel(path_ds1)
ds2_raw <- readxl::read_excel(path_ds2)
ds3_raw <- readxl::read_excel(path_ds3)
ds4_raw <- readxl::read_excel(path_ds4)

# ------------------------------------------------------------
# 4. Standardize column names
# ------------------------------------------------------------
ds1 <- ds1_raw %>% janitor::clean_names()
ds2 <- ds2_raw %>% janitor::clean_names()
ds3 <- ds3_raw %>% janitor::clean_names()
ds4 <- ds4_raw %>% janitor::clean_names()

message("Column names standardized to snake_case.")

# ------------------------------------------------------------
# 5. Dataset 1: PPP / infrastructure project data
# ------------------------------------------------------------
# Main goals:
#   - convert binary indicators into logical variables
#   - extract construction years
#   - standardize PPP term variables
#   - derive CAPEX planned / actual / overrun metrics
#   - preserve original variables while adding analysis-ready fields

ds1_clean <- ds1 %>%
  mutate(
    # Binary flags -> logical indicators
    is_ppp             = ppp_yes_or_no == 1,
    has_delay          = construction_delay_yes_or_no == 1,
    has_budget_overrun = budget_overrun_yes_or_no == 1,
    bim_design         = bim_at_design_stage_yes_or_no == 1,
    bim_construction   = bim_at_construction_stage_yes_or_no == 1,
    bim_operation      = bim_at_operation_stage_yes_or_no == 1
  ) %>%
  mutate(
    # Extract construction years where source fields are date-like
    construction_start_year = suppressWarnings(
      lubridate::year(start_of_the_construction_year)
    ),
    construction_end_year = suppressWarnings(
      lubridate::year(end_of_the_construction_year)
    ),
    construction_planned_end_year = suppressWarnings(
      lubridate::year(proposed_date_of_the_end_of_the_construction_year)
    )
  ) %>%
  mutate(
    # PPP term:
    # despite source name "..._month", the thesis notes indicate
    # that the actual source values are in years (e.g., 25, 27, 30)
    term_ppp_years_raw = suppressWarnings(
      as.numeric(term_of_the_ppp_agreement_month)
    ),
    term_ppp_years  = term_ppp_years_raw,
    term_ppp_months = term_ppp_years_raw * 12,
    ppp_end_year    = suppressWarnings(
      as.numeric(date_of_the_end_of_the_ppp_year)
    )
  ) %>%
  mutate(
    # CAPEX variables
    capex_planned_musd = suppressWarnings(as.numeric(capex_mln)),
    capex_overrun_musd = suppressWarnings(as.numeric(construction_stage_capex_overrun)),
    capex_actual_musd  = dplyr::if_else(
      !is.na(capex_overrun_musd),
      capex_planned_musd + capex_overrun_musd,
      capex_planned_musd
    ),
    capex_overrun_pct = dplyr::if_else(
      !is.na(capex_overrun_musd) & !is.na(capex_planned_musd) & capex_planned_musd > 0,
      100 * capex_overrun_musd / capex_planned_musd,
      NA_real_
    )
  ) %>%
  mutate(
    # Harmonize core character fields if present
    id           = as.character(id),
    project_name = as.character(project_name),
    country      = as.character(country),
    sector       = as.character(sector)
  )

# ------------------------------------------------------------
# 6. Dataset 2: project-year operational / financial data
# ------------------------------------------------------------
# Main goals:
#   - standardize identifiers and text fields
#   - harmonize project naming conventions
#   - convert annual financial variables to numeric format
#   - retain a clean panel-like structure for later aggregation
#
# Notes:
#   - Dataset 2 contains yearly operating observations by project
#   - Country, sector, and BIM stage indicators are joined later
#     from Dataset 1 using the project identifier `id`

ds2_clean <- ds2 %>%
  mutate(
    no = suppressWarnings(as.integer(no)),
    id = as.character(id),
    source = as.character(source),
    project = as.character(project),
    spv = stringr::str_squish(as.character(spv)),
    year = suppressWarnings(as.integer(year)),
    revenue = suppressWarnings(as.numeric(revenue)),
    construction_stage_capex = suppressWarnings(as.numeric(construction_stage_capex)),
    maintenance_obligations = suppressWarnings(as.numeric(maintenance_obligations)),
    salary = suppressWarnings(as.numeric(salary))
  ) %>%
  rename(
    project_name = project,
    spv_name = spv
  )

# ------------------------------------------------------------
# 7. Dataset 3: company survey / expert mapping
# ------------------------------------------------------------
# Main goals:
#   - standardize identifiers and text fields
#   - recode BIM stages into clean analytical categories
#   - preserve the survey-based mapping between BIM effects
#     and DCF-relevant financial channels
#
# Notes:
#   - Dataset 3 contains company / expert-based evidence
#   - No explicit study year column is available in this dataset

ds3_clean <- ds3 %>%
  mutate(
    no = suppressWarnings(as.integer(no)),
    id = as.character(id),
    company = stringr::str_squish(as.character(company)),
    country = stringr::str_squish(as.character(country)),
    stage = as.character(stage),
    stage_clean = recode_stage(stage),
    source_type = "Company survey",
    effects_per_project = stringr::str_squish(as.character(effects_per_project)),
    estimation = suppressWarnings(as.numeric(estimation)),
    unified_metric = stringr::str_squish(as.character(unified_metric)),
    score_to_the_unified_metric = suppressWarnings(as.numeric(score_to_the_unified_metric)),
    calculations = as.character(calculations),
    dcf_influence = stringr::str_squish(as.character(dcf_influence)),
    comment = as.character(comment)
  )

# ------------------------------------------------------------
# 8. Dataset 4: literature-based BIM effect mapping
# ------------------------------------------------------------
# Main goals:
#   - same standardization logic as ds3
#   - analytical compatibility for later pooling / comparison

ds4_clean <- ds4 %>%
  mutate(
    id                           = as.character(id),
    stage                        = as.character(stage),
    stage_clean                  = recode_stage(stage),
    source_type                  = "Literature",
    dcf_influence                = as.character(dcf_influence),
    unified_metric               = as.character(unified_metric),
    score_to_the_unified_metric  = suppressWarnings(as.numeric(score_to_the_unified_metric)),
    country                      = as.character(country),
    year                         = suppressWarnings(as.numeric(year))
  )

# ------------------------------------------------------------
# 9. Lightweight validation summary
# ------------------------------------------------------------
validation_summary <- tibble::tibble(
  dataset = c("ds1_clean", "ds2_clean", "ds3_clean", "ds4_clean"),
  n_rows  = c(nrow(ds1_clean), nrow(ds2_clean), nrow(ds3_clean), nrow(ds4_clean)),
  n_cols  = c(ncol(ds1_clean), ncol(ds2_clean), ncol(ds3_clean), ncol(ds4_clean))
)

print(validation_summary)

message("Validation summary completed.")

# Optional: brief diagnostics for key fields
message("PPP projects identified in Dataset 1: ",
        sum(ds1_clean$is_ppp, na.rm = TRUE))

message("Rows with non-missing revenue in Dataset 2: ",
        sum(!is.na(ds2_clean$revenue)))

message("Rows with non-missing BIM score in Dataset 3: ",
        sum(!is.na(ds3_clean$score_to_the_unified_metric)))

message("Rows with non-missing BIM score in Dataset 4: ",
        sum(!is.na(ds4_clean$score_to_the_unified_metric)))

# ------------------------------------------------------------
# 10. Save processed data objects
# ------------------------------------------------------------
saveRDS(ds1_clean, "outputs/rds/ds1_clean.rds")
saveRDS(ds2_clean, "outputs/rds/ds2_clean.rds")
saveRDS(ds3_clean, "outputs/rds/ds3_clean.rds")
saveRDS(ds4_clean, "outputs/rds/ds4_clean.rds")

message("Processed datasets saved to outputs/rds/")

# ------------------------------------------------------------
# 11. Session end message
# ------------------------------------------------------------
message("01_load_and_clean_data.R finished successfully.")
