library(readxl)
library(ggplot2)

source("setup.R")

# Import data 
habitat <- readxl::read_excel("C:\\Users\\ganda\\Downloads\\hr_landcover_proportion_new.xlsx")

lulc_color <- c("#006400", "#FFBB22", "#FFFF4C", "#F09BFF", "#FA0000",
                "#B4B4B4", "#0064C8", "#0096A0")	

habitat$Class <- factor(habitat$Class_name, levels = unique(habitat$Class_name))

## Plot
strip_data <- data.frame(Species = unique(habitat$Species), 
                         Prop = rep(round(max(habitat$Prop) + 1), length(unique(habitat$Species))))

prop_ylim <- max(strip_data$Prop)[1]
legend_tile <- 18*4
legend_text <- 14*4
species_text <- 8*3
prop_text <- 6*3

calc_angle <- function(x) {
  angle <- 90 - 360 * (x - 0.5) / length(x)
  ifelse(angle < -90, angle + 180, angle)
  print(ifelse(angle < -90, angle + 180, angle))
}

ggplot(data = habitat)+
  geom_col(data = strip_data, mapping = aes(x = Species, y = Prop),
           fill = "gray85")+
  ## Start Bar and Percentage text
  geom_col(mapping = aes(x = Species, y = Prop - Prop + prop_ylim, 
                         group = Class, fill = Class), 
           color = NA, width = 0.03,
           position = position_dodge(width = 1))+
  geom_point(mapping = aes(x = Species, y = Prop - Prop + prop_ylim, 
                           group = Class, color = Class), 
             size = 3, position = position_dodge(width = 1))+
  geom_point(mapping = aes(x = Species, y = Prop - Prop + prop_ylim, group = Class), 
             size = 1, position = position_dodge(width = 1), color = "white")+
  ## End Bar and Percentage text
  geom_col(mapping = aes(x = Species, y = Prop, 
                         fill = Class),
           position = position_dodge())+
  geom_text(mapping = aes(x = Species, y = Prop - Prop + prop_ylim + 0.2*prop_ylim,
                          label = paste0(round(Prop, 1), "%"), 
                          group = Class),
            position = position_dodge(width = 1),
            vjust = 1, hjust = .5, family = "mr", size = prop_text)+
  # geomtextpath::geom_textpath(data = strip_data,
  #                             mapping = aes(x = Species, y = Prop - Prop + prop_ylim + 0.4*prop_ylim,
  #                                           label = gsub("\\s", "\n", Species)),
  #                             vjust = 1.1, hjust = .5,  family = "msbi", size = species_text,
  #                             color = "gray30", upright = T, halign = 'center',
  #                             spacing = -150, kerning = 5)+
  ylim(c(-prop_ylim*0.5, prop_ylim + prop_ylim*0.8))+
  coord_polar()+
  scale_fill_manual(values = lulc_color)+
  scale_color_manual(values = lulc_color)+
  labs(fill = "Habitat type", color = "Habitat type")+
  theme_void()+
  theme(
    plot.margin = margin(b = 0.5, t = 0.5, unit = "lines"),
    legend.title = element_text(size = legend_tile, family = "mm", hjust = .5),
    legend.text = element_text(size = legend_text, family = "mr"),
    legend.title.position = "top",
    legend.position = "bottom",
    legend.byrow = TRUE,
    legend.key.height = unit(1.1, units = "lines"),
    legend.key.width = unit(0.1, units = "lines")
  )

ggsave("C:\\Users\\ganda\\Downloads\\hr_landcover_proportion_new.jpeg", 
       width = 30, height = 30, dpi = 300, units = "cm")

