# ============================================================
# 02_functions_dcf.R
# Purpose:
#   Store financial helper functions used in DCF, NPV,
#   sensitivity analysis, and real-option calculations.
# ============================================================

# ------------------------------------------------------------
# 1. Net Present Value of an annuity-like annual cash flow
# ------------------------------------------------------------
npv_annuity <- function(capex, annual_net_cf, T_years, r) {
  T <- round(T_years)
  
  pv_cf <- ifelse(
    !is.na(T) & !is.na(annual_net_cf) & !is.na(r) & T > 0 & r > 0,
    annual_net_cf * (1 - (1 + r)^(-T)) / r,
    NA_real_
  )
  
  npv <- -capex + pv_cf
  return(npv)
}

# ------------------------------------------------------------
# 2. Present value of a deferred annuity
# ------------------------------------------------------------
pv_deferred_annuity <- function(cf_annual, r, T_total, t_start) {
  n <- pmax(T_total - t_start, 0)
  
  pv_at_t <- ifelse(
    n > 0 & !is.na(cf_annual) & !is.na(r) & r > 0,
    cf_annual * (1 - (1 + r)^(-n)) / r,
    0
  )
  
  pv0 <- pv_at_t / ((1 + r)^t_start)
  return(pv0)
}
