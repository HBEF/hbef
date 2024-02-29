library(ggthemes)

#DATA

imported_data <- readRDS("precip_stream_data_long.rds")
imported_diff_data <- readRDS("precip_stream_diff_data_long.rds")

my_theme <- theme_fivethirtyeight() + 
  theme(rect = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#dddddd"), 
        text = element_text(family = "Helvetica", size = 12), 
        legend.position = "none", legend.direction = "vertical", legend.title = element_blank(),
        strip.text = element_text(hjust = 1, size = 20, face = "bold"), 
        axis.title= element_text(NULL), axis.title.x= element_blank(), 
        axis.title.y= element_text(hjust = 1, angle = 90, margin = margin(r=20)))

cation <- c("K" = "#95AFDD", "Na" = "#7195D2", "NH4" = "#4E7AC7" , "Ca" = "#3B5C95", "Mg" = "#273D64", "Al" = "#162338")
anion <- c("PO4" = "#600B0B", "SO4" = "#8F1010", "NO3" = "#BF1616", "SiO2"= "#CC4545", "Cl" = "#D97373", "HCO3" = "#E5A2A2")
hydro <- c("pH" = "#FFC408", "H" = "#FFE79C")

solute_palette <- c(cation, anion, hydro)
source_shapes <- c("flow" = 16, "precip"= 21)


ggplot_function <- function(data, x, y, color, facet, ncol = NULL, nrow = NULL){
  ggplotly(  
    (ggplot(data=data, aes(x = get(x), y = get(y), color = solute, shape = source, alpha = ws)) + my_theme +
       geom_line(size = 1)+ 
       geom_point(size = 1.5, fill = "white", stroke = 0.5) + 
       facet_wrap(~get(facet) , ncol = ncol)+ 
       xlim(min(input$timeframe[1]), max(input$timeframe[2]))+ 
       labs(x = "Water Year", y = units())+ 
       scale_shape_manual(values = source_shapes) +
       scale_color_manual(values = solute_palette) +
       scale_alpha_discrete(range = c(0.9, 0.5))), 
    width = 900) %>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE)  
}