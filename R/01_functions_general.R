# ============================================================
# 01_functions_general.R
# Purpose:
#   Store general helper functions used across the project.
#
# Notes:
#   These functions are intentionally lightweight and reusable.
#   Project-specific financial functions should be placed in
#   a separate file such as 02_functions_dcf.R
# ============================================================

# ------------------------------------------------------------
# 1. Recode BIM / project stage labels into clean categories
# ------------------------------------------------------------
recode_stage <- function(x) {
  dplyr::case_when(
    stringr::str_starts(x, "1_") ~ "Design",
    stringr::str_starts(x, "2_") ~ "Construction",
    stringr::str_starts(x, "3_") ~ "Operation",
    TRUE ~ x
  )
}

# ------------------------------------------------------------
# 2. Create directory safely if it does not exist
# ------------------------------------------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

# ------------------------------------------------------------
# 3. Save a data frame as CSV with consistent defaults
# ------------------------------------------------------------
save_csv <- function(data, file_path) {
  ensure_dir(dirname(file_path))
  write.csv(data, file = file_path, row.names = FALSE, na = "")
  message("Saved table: ", file_path)
}

# ------------------------------------------------------------
# 4. Save a ggplot object with consistent defaults
# ------------------------------------------------------------
save_plot <- function(plot_object,
                      file_path,
                      width = 8,
                      height = 5,
                      dpi = 300,
                      bg = "white") {
  ensure_dir(dirname(file_path))
  ggplot2::ggsave(
    filename = file_path,
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg
  )
  message("Saved figure: ", file_path)
}

# ------------------------------------------------------------
# 5. Print a compact dataset overview
# ------------------------------------------------------------
data_overview <- function(data, data_name = deparse(substitute(data))) {
  overview <- tibble::tibble(
    dataset = data_name,
    n_rows = nrow(data),
    n_cols = ncol(data)
  )
  print(overview)
  invisible(overview)
}
