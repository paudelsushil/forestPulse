plot_theme <- function() {
  theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.line = element_line(colour="black"),
      axis.ticks = element_line(),

      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10, face = "bold"),


      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90"),
      # panel.border = element_rect(colour = "black", fill=NA, size=0.5),

      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 10, face = "bold"),
      # plot.title.position = "panel", 
      # plot.margin = margin(t = 5, r = 0, b = 5, l = 0),
      
      
     )


}
  
pdsi_plot_theme <- function() {
  theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      axis.line = element_line(colour="black"),
      axis.ticks = element_line(),

      axis.title.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),


      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90"),
      # panel.border = element_rect(colour = "black", fill=NA, size=0.5),

      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 10, face = "bold"),
      plot.title.position = "panel", 
      plot.margin = margin(t = 5, r = 0, b = 5, l = 0),

      legend.position = "topleft",
      legend.direction = "horizontal",
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
              
      # Adjust spacing and margins for the legend
      legend.box = "horizontal",
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.spacing.x = unit(0.2, 'cm')


     )


}  
  
    
