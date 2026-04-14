# ============================================================
# forestpulseR — Hex Sticker (Option 2: Pure R with hexSticker)
# ============================================================

install.packages(c("hexSticker", "ggplot2", "showtext", "sysfonts"))

library(hexSticker)
library(ggplot2)
library(showtext)
library(sysfonts)

# Load a clean sans font
sysfonts::font_add_google("Inter", "inter")
showtext::showtext_auto()

# Colour palette (forest greens)
col_bg_dark    <- "#1B4332"  # border
col_bg_mid     <- "#2D6A4F"  # fill
col_accent     <- "#52B788"  # inner line
col_pulse      <- "#D8F3DC"  # EKG line
col_text_main  <- "#F1FAEE"  # title
col_text_sub   <- "#B7E4C7"  # tagline
col_tree_back  <- "#1B4332"  # back row pines
col_tree_front <- "#081C17"  # front row pines

# ---- Build the inner artwork with ggplot2 ----

# Pulse line coordinates (EKG waveform)
pulse_df <- data.frame(
  x = c(0.10, 0.38, 0.43, 0.46, 0.49, 0.52, 0.55, 0.58, 0.61, 0.90),
  y = c(0.50, 0.50, 0.60, 0.38, 0.72, 0.33, 0.70, 0.45, 0.50, 0.50)
)

# Back row pine trees (6 trees)
trees_back <- data.frame(
  id = rep(1:6, each = 3),
  x  = c(0.26,0.34,0.30,  0.36,0.44,0.40,  0.46,0.54,0.50,
         0.56,0.64,0.60,  0.66,0.74,0.70,  0.76,0.82,0.79),
  y  = c(0.15,0.15,0.40,  0.15,0.15,0.42,  0.15,0.15,0.45,
         0.15,0.15,0.42,  0.15,0.15,0.40,  0.15,0.15,0.35)
)

# Front row pine trees (7 darker, shorter trees — offset)
trees_front <- data.frame(
  id = rep(1:7, each = 3),
  x  = c(0.22,0.30,0.26,  0.30,0.38,0.34,  0.40,0.46,0.43,
         0.50,0.56,0.53,  0.58,0.66,0.62,  0.68,0.74,0.71,
         0.76,0.82,0.79),
  y  = c(0.15,0.15,0.30,  0.15,0.15,0.32,  0.15,0.15,0.28,
         0.15,0.15,0.30,  0.15,0.15,0.30,  0.15,0.15,0.27,
         0.15,0.15,0.22)
)

p <- ggplot() +
  # Pulse line
  geom_path(data = pulse_df, aes(x = x, y = y),
            colour = col_pulse, linewidth = 1.2, lineend = "round") +
  geom_point(data = pulse_df[c(1, nrow(pulse_df)), ],
             aes(x = x, y = y), colour = col_pulse, size = 1.5) +
  # Back row trees
  geom_polygon(data = trees_back, aes(x = x, y = y, group = id),
               fill = col_tree_back) +
  # Front row trees (darker, overlapping)
  geom_polygon(data = trees_front, aes(x = x, y = y, group = id),
               fill = col_tree_front) +
  # Ground line
  geom_segment(aes(x = 0.15, xend = 0.85, y = 0.15, yend = 0.15),
               colour = col_tree_front, linewidth = 0.8) +
  xlim(0, 1) + ylim(0, 1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

# ---- Assemble the hex sticker ----

hexSticker::sticker(
  subplot   = p,
  package   = "forestPulse",
  p_size    = 22,
  p_y       = 1.45,
  p_color   = col_text_main,
  p_family  = "inter",
  p_fontface = "bold",
  s_x       = 1,
  s_y       = 0.95,
  s_width   = 1.6,
  s_height  = 1.6,
  h_fill    = col_bg_mid,
  h_color   = col_bg_dark,
  h_size    = 1.8,
  url       = "https://github.com/paudelsushil/forestPulse",
  u_color   = col_text_sub,
  u_size    = 4,
  filename  = "man/figures/logo.png",
  dpi       = 600
)

# Also save a high-res print version
hexSticker::sticker(
  subplot = p, package = "forestPulse",
  p_size = 22, p_y = 1.45, p_color = col_text_main,
  p_family = "inter", p_fontface = "bold",
  s_x = 1, s_y = 0.95, s_width = 1.6, s_height = 1.6,
  h_fill = col_bg_mid, h_color = col_bg_dark, h_size = 1.8,
  url = "https://github.com/paudelsushil/forestPulse",
  u_color = col_text_sub, u_size = 4,
  filename = "man/figures/logo_print.pdf", dpi = 1200
)

# Register with usethis
usethis::use_logo("man/figures/logo.png")

cat("✔ forestpulseR hex sticker generated successfully!\n")
cat("  - man/figures/logo.png       (README)\n")
cat("  - man/figures/logo_print.pdf (print/stickers)\n")
