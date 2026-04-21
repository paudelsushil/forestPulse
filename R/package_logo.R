# Developer helper to regenerate package logo files without side effects at load.
generate_package_logo <- function(
  output_png = "man/figures/logo.png",
  output_pdf = "man/figures/logo_print.pdf",
  register_logo = FALSE
) {
  required_pkgs <- c("hexSticker", "ggplot2", "showtext", "sysfonts")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_pkgs, collapse = ", "),
      ". Install them first with install.packages().",
      call. = FALSE
    )
  }

  sysfonts::font_add_google("Inter", "inter")
  showtext::showtext_auto()

  col_bg_dark <- "#1B4332"
  col_bg_mid <- "#2D6A4F"
  col_pulse <- "#D8F3DC"
  col_text_main <- "#F1FAEE"
  col_text_sub <- "#B7E4C7"
  col_tree_back <- "#1B4332"
  col_tree_front <- "#081C17"

  pulse_df <- data.frame(
    x = c(0.10, 0.38, 0.43, 0.46, 0.49, 0.52, 0.55, 0.58, 0.61, 0.90),
    y = c(0.50, 0.50, 0.60, 0.38, 0.72, 0.33, 0.70, 0.45, 0.50, 0.50)
  )

  trees_back <- data.frame(
    id = rep(1:6, each = 3),
    x = c(
      0.26, 0.34, 0.30, 0.36, 0.44, 0.40, 0.46, 0.54, 0.50,
      0.56, 0.64, 0.60, 0.66, 0.74, 0.70, 0.76, 0.82, 0.79
    ),
    y = c(
      0.15, 0.15, 0.40, 0.15, 0.15, 0.42, 0.15, 0.15, 0.45,
      0.15, 0.15, 0.42, 0.15, 0.15, 0.40, 0.15, 0.15, 0.35
    )
  )

  trees_front <- data.frame(
    id = rep(1:7, each = 3),
    x = c(
      0.22, 0.30, 0.26, 0.30, 0.38, 0.34, 0.40, 0.46, 0.43,
      0.50, 0.56, 0.53, 0.58, 0.66, 0.62, 0.68, 0.74, 0.71,
      0.76, 0.82, 0.79
    ),
    y = c(
      0.15, 0.15, 0.30, 0.15, 0.15, 0.32, 0.15, 0.15, 0.28,
      0.15, 0.15, 0.30, 0.15, 0.15, 0.30, 0.15, 0.15, 0.27,
      0.15, 0.15, 0.22
    )
  )

  logo_plot <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = pulse_df,
      ggplot2::aes(x = x, y = y),
      colour = col_pulse,
      linewidth = 1.2,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = pulse_df[c(1, nrow(pulse_df)), ],
      ggplot2::aes(x = x, y = y),
      colour = col_pulse,
      size = 1.5
    ) +
    ggplot2::geom_polygon(
      data = trees_back,
      ggplot2::aes(x = x, y = y, group = id),
      fill = col_tree_back
    ) +
    ggplot2::geom_polygon(
      data = trees_front,
      ggplot2::aes(x = x, y = y, group = id),
      fill = col_tree_front
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0.15, xend = 0.85, y = 0.15, yend = 0.15),
      colour = col_tree_front,
      linewidth = 0.8
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )

  hexSticker::sticker(
    subplot = logo_plot,
    package = "forestPulse",
    p_size = 22,
    p_y = 1.45,
    p_color = col_text_main,
    p_family = "inter",
    p_fontface = "bold",
    s_x = 1,
    s_y = 0.95,
    s_width = 1.6,
    s_height = 1.6,
    h_fill = col_bg_mid,
    h_color = col_bg_dark,
    h_size = 1.8,
    url = "https://github.com/paudelsushil/forestPulse",
    u_color = col_text_sub,
    u_size = 4,
    filename = output_png,
    dpi = 600
  )

  hexSticker::sticker(
    subplot = logo_plot,
    package = "forestPulse",
    p_size = 22,
    p_y = 1.45,
    p_color = col_text_main,
    p_family = "inter",
    p_fontface = "bold",
    s_x = 1,
    s_y = 0.95,
    s_width = 1.6,
    s_height = 1.6,
    h_fill = col_bg_mid,
    h_color = col_bg_dark,
    h_size = 1.8,
    url = "https://github.com/paudelsushil/forestPulse",
    u_color = col_text_sub,
    u_size = 4,
    filename = output_pdf,
    dpi = 1200
  )

  if (isTRUE(register_logo) && requireNamespace("usethis", quietly = TRUE)) {
    usethis::use_logo(output_png)
  }

  invisible(list(png = output_png, pdf = output_pdf))
}
