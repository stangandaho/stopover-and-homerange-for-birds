# Load setup
source("scripts/utils.R")
source("setup.R")
class_name_fr <- c("Eau", "Arbres", "Herbe", "Végétation inondée", "Cultures", 
                   "Arbustes et broussailles", "Zone bâtie", "Sol nu", "Neige et glace")
lulc_color <- c('#419bdf', '#397d49', '#88b053', '#7a87c6', '#e49635', 
                '#dfc35a', '#c4281b', '#a59b8f', '#b39fe1')

## Import Home range and relevant LULC data
# This was processed in Google Earth Engine at this 
# link: https://code.earthengine.google.com/?accept_repo=users/gandahostanmah/bird_home_range_habitat
lulc <- read.csv("datasets/overallHabitatProp.csv") %>% 
  select(individual, class_0:class_8) %>% 
  rename(Species = individual) %>% 
  fix_name(species_col = "Species") %>% 
  dplyr::mutate(Species = gsub("_Hivernage", "", Species)) %>% 
  tidyr::pivot_longer(cols = !Species, names_to = "Class", values_to = "prop") %>% 
  tidyr::separate(col = Class, into = c("pre", "Class"), sep = "_") %>% 
  dplyr::select(-pre) %>% 
  left_join(y = data.frame(Class = as.character(0:8), Class_name = class_name_fr),
            by = "Class") %>% 
  summarise(prop = mean(prop), .by = c("Species", "Class_name"))

lulc$Class_name <- factor(lulc$Class_name, levels = class_name_fr)

## Plot
strip_data <- data.frame(Species = unique(lulc$Species), 
                         prop = rep(93, length(unique(lulc$Species))))
ggplot(data = lulc %>% rename(Class = Class_name))+
  geom_col(data = strip_data, mapping = aes(x = Species, y = prop),
           fill = "gray90")+
  ## Start Bar and Percentage text
  geom_col(mapping = aes(x = Species, y = prop-prop+93, 
                         group = Class, fill = Class), 
           color = NA, width = 0.03,
           position = position_dodge(width = 1))+
  geom_point(mapping = aes(x = Species, y = prop-prop+93, 
                           group = Class, color = Class), 
             size = 3, position = position_dodge(width = 1))+
  geom_point(mapping = aes(x = Species, y = prop-prop+93, group = Class), 
             size = 1, position = position_dodge(width = 1), color = "white")+
  ## End Bar and Percentage text
  geom_col(mapping = aes(x = Species, y = prop, 
                         fill = Class),
           position = position_dodge())+
  geom_text(mapping = aes(x = Species, y = prop - prop + 105,
                          label = paste0(round(prop, 1), "%"), 
                          group = Class),
            position = position_dodge(width = 1),
            vjust = 1, hjust = .5, family = "mr", size = 6)+
  geomtextpath::geom_textline(data = strip_data,
                              mapping = aes(x = Species, y = prop - prop + 115,
                                            label = gsub("\\s", "\n", Species)),
                              vjust = 1, hjust = .5, family = "msbi", size = 8,
                              color = "gray30")+
  ylim(c(-50, 120))+
  coord_polar()+
  scale_fill_manual(values = lulc_color)+
  scale_color_manual(values = lulc_color)+
  labs(fill = "Type d'habitat", color = "Type d'habitat")+
  theme_void()+
  theme(
    plot.margin = margin(b = 1, t = 1, unit = "lines"),
    legend.title = element_text(size = 18, family = "mm", hjust = .5),
    legend.text = element_text(size = 14, family = "mr"),
    legend.title.position = "top",
    legend.position = "bottom",
    legend.byrow = TRUE,
    legend.key.height = unit(1.1, units = "lines"),
    legend.key.width = unit(0.1, units = "lines")
  )

ggsave("plots/habitat_type2.jpeg", width = 25, height = 25, dpi = 150, units = "cm")
