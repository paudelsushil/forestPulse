library(gridExtra)

# Panel A: Drought
pdsi_p <- ggplot() +
  geom_raster(data = pdsi_df, aes(x = x,
                                  y = y,
                                  fill = factor(pdsi_class)),
              show.legend = FALSE) +
  scale_fill_manual(values = c("transparent", "darkgoldenrod", "gold",
                               "yellow", "orange", "red3"))  +
  geom_polygon(data = nd_df, aes(x = long, y = lat, group = group),
               color='black', fill = "transparent", size = 0.15) +
  geom_point(data = nks_df,  aes(x = long, y = lat, group = id), size = 3,
             colour='#000000', shape = 18) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_map()

# Panel B: Bark Beetle
ads_p <- ggplot() +
  geom_polygon(data = mpb_df, aes(x = long, y = lat, group = group),
               color='saddlebrown', fill = "saddlebrown", size = 0) +
  geom_polygon(data = sb_df, aes(x = long, y = lat, group = group),
               color='darkturquoise', fill = "darkturquoise", size = 0) +

  geom_polygon(data = nd_df, aes(x = long, y = lat, group = group),
               color='black', fill = "transparent", size = 0.15) +
  geom_point(data = nks_df,  aes(x = long, y = lat, group = id), size = 3,
             colour='#000000', shape = 18) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_map()

# Panel C: Wildfire
mtbs_p <- ggplot() +
  geom_raster(data = mtbs_df, aes(x = x, y = y, fill = factor(fire_only)),
              show.legend = FALSE) +
  scale_fill_manual(values = "#D62728") +
  geom_polygon(data=nd_df, aes(x = long, y = lat, group = group),
               color='black', fill = "transparent", size = 0.15) +
  geom_point(data = nks_df,  aes(x = long, y = lat, group = id), size = 3,
             colour='#000000', shape = 18) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_map()

# Panel D: Combo
combo_p <- ggplot(iskm.df, aes(long, lat)) +
  geom_raster(aes(fill = interactions), show.legend = FALSE) +
  scale_fill_manual(values = my_colors, labels = my_labels,
                    name = "Interactions") +
  geom_polygon(data=nd_df, aes(x = long, y = lat, group = group),
               color='black', fill = "transparent", size = 0.15) +
  geom_point(data = nks_df,  aes(x = long, y = lat, group = id), size = 3,
             colour='#000000', shape = 18) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_equal() +
  theme_map()

g <- arrangeGrob(pdsi_p, ads_p, mtbs_p, combo_p, nrow = 1)

ggsave(file = "results/disturbaces.eps", g, width = 6, height = 2,
       scale = 4, dpi = 600, units = "cm") #saves p



