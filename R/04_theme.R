# ============================================================
# 04_theme.R
# Purpose:
#   Define a consistent publication-style ggplot theme.
# ============================================================

theme_article <- ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title       = ggplot2::element_text(hjust = 0.5, face = "bold"),
    axis.title       = ggplot2::element_text(face = "bold")
  )
