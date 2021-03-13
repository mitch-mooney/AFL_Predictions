theme_AFL <- function(base_size = 15, 
                          base_family = "Arial",
                          base_line_size = base_size/22, 
                          base_rect_size = base_size/22,
                          background_hex = "#8cbaa3"){
  theme_grey(
    base_size = base_size, 
    base_family = base_family, 
    base_line_size = base_line_size, 
    base_rect_size = base_rect_size) +
    theme(panel.background = element_rect(fill = "#404241", colour = "#404241"), 
          # added the same color argument to apply to the entire plot
          plot.background = element_rect(fill = background_hex, colour = background_hex),
          panel.border = element_rect(fill = NA, colour = NA), 
          panel.grid = element_line(colour = background_hex, size = 0.3), 
          panel.grid.minor = element_line(size = rel(0.5)), 
          strip.background = element_rect(fill = background_hex, colour = background_hex), 
          legend.position = "none",
          # remove the x title: might not want to make this a global setting!!
          axis.line = element_line(size = 1, colour = background_hex),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(color = "#575958", size = base_size, family = base_family, face = "bold"),
          plot.subtitle = element_text(color = "#575958", size = base_size/1.5, family = base_family, face = "italic"),
          axis.title.x = element_text(color = "#575958", size = base_size/1.5, family = base_family, face = "bold"),
          # add an accent color for the plot text and increase size
          axis.title.y = element_text(color = "#575958", size = base_size/1.5, angle = 90, family = base_family, face = "bold"),
          axis.text.x = element_text(color = "#575958", size = base_size, family = base_family),
          axis.text.y = element_text(color = "#575958", size = base_size, family = base_family),
          complete = TRUE
    )
}
