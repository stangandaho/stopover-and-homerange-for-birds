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
lulc <- read.csv("datasets/BirdHomeRangeLULC.csv") %>% 
  select(4:6) %>% 
  rowwise() %>% 
  mutate(lulc_buffer = class_name_fr[lulc_buffer + 1],
         lulc_hr = class_name_fr[lulc_hr + 1]) %>% 
  rename(Species = individual) %>% 
  fix_name(species_col = "Species") %>% 
  dplyr::mutate(Species = gsub("_Hivernage", "", Species)) %>% 
  ungroup()


hr_lulc <- lulc %>% 
  dplyr::select(Species, lulc_hr) %>% 
  dplyr::count(Species, lulc_hr) %>% 
  dplyr::reframe(prop = round(n*100/sum(n), 2), .by = "Species", lulc_hr, n) %>% 
  dplyr::arrange(Species, lulc_hr, prop, .by_group = TRUE) %>% 
  dplyr::rename(Class = lulc_hr)
write.csv(hr_lulc, "tables/hr_lulc.csv", row.names = FALSE)

hr_lulc$Class <- factor(hr_lulc$Class, levels = class_name_fr)

## Plot
strip_data <- data.frame(Species = unique(hr_lulc$Species), 
                         prop = rep(93, length(unique(hr_lulc$Species))))
ggplot(data = hr_lulc)+
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

ggsave("plots/habitat_type.jpeg", width = 25, height = 25, dpi = 150, units = "cm")



## Dependence analysis
# H0 : No independce => p-value >= 0.05
fisher_extact_test <- maimer::mm_to_community(data = hr_lulc, species_column = Species,
                        site_column = Class, size_column = n,
                        values_fill = 0) %>% 
  tibble::column_to_rownames("Class") 
## Write contingency table
write.csv(fisher_extact_test, "tables/contengency_table_species_habitat.csv",
          fileEncoding = "ISO-8859-1")

# Fisher Exact test
fisher_extact_test %>%
  fisher.test(simulate.p.value=TRUE)
# p-value = 0.0004998

## Jacob
buffer_lulc <- lulc %>% 
  dplyr::select(Species, lulc_buffer) %>% 
  dplyr::count(Species, lulc_buffer) %>% 
  dplyr::reframe(prop = round(n*100/sum(n), 2), .by = "Species", lulc_buffer, n) %>% 
  dplyr::arrange(Species, lulc_buffer, prop, .by_group = TRUE) %>% 
  dplyr::rename(Class = lulc_buffer)

## 
all_spec <- unique(buffer_lulc$Species)
## home range
sing_spec_hr <- hr_lulc %>% dplyr::filter(Species == all_spec[1]) %>% 
  dplyr::select(-n) %>% 
  arrange(Class) %>% 
  rename(hr_prop = prop)

## Buffer
sing_spec_buffer <- buffer_lulc %>% dplyr::filter(Species == all_spec[1]) %>% 
  dplyr::select(-n, -Species) %>%
  arrange(Class) %>% 
  rename(buffer_prop = prop)

sing_spec_hr$Class %in% sing_spec_buffer$Class
sing_spec_buffer$Class %in% sing_spec_hr$Class

##
sing_spec_buffer %>% 
  right_join(y = sing_spec_hr, by = "Class")
