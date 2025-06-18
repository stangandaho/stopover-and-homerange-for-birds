# Load setup
source("scripts/utils.R")
source("setup.R")
class_name_fr <- c("Eau", "Arbres", "Herbe", "Végétation inondée", "Cultures", 
  "Arbustes et broussailles", "Zone bâtie", "Sol nu", "Neige et glace")
lulc_color <- c('#419bdf', '#397d49', '#88b053', '#7a87c6', '#e49635', 
                '#dfc35a', '#c4281b', '#a59b8f', '#b39fe1')

## Impor Land Use Land Cover metadata
lulc <- read.csv("datasets/LULC.csv") %>% 
  dplyr::mutate(Class_fr = class_name_fr) %>% 
  dplyr::relocate(Class_fr, .before = 2)
write.csv(lulc, "tables/LULC_FR.csv", row.names = FALSE)

## Import Home range and relevant LULC data
# This was processed in Google Earth Engine at this 
# link: https://code.earthengine.google.com/87c86b7a44746bf9ad4bd009b4ace56f
hr_lulc <- read.csv("tables/hrLULC.csv") %>% 
  mutate(individual = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$|_Hivernage",
                           "", individual),
         individual = case_when(grepl('Tha', individual) ~ "Thalasseus sandvicensis",
                                TRUE ~ individual)) %>%
  dplyr::select(LULC, individual) %>% 
  count(individual, LULC) %>% 
  slice(-c(1, 2)) %>% 
  dplyr::rename(Value = LULC) %>% 
  dplyr::mutate(Value = as.numeric(Value)) %>% 
  dplyr::left_join(x = ., y = lulc %>% dplyr::select(-c(Class, Description)), by = "Value") %>% 
  dplyr::reframe(prop = round(n*100/sum(n), 2), .by = "individual", Value, Class_fr, n) %>% 
  dplyr::arrange(individual, prop, Class_fr, .by_group = TRUE)

hr_lulc$Class_fr <- factor(hr_lulc$Class_fr, levels = class_name_fr)
## Plot
strip_data <- data.frame(individual = unique(hr_lulc$individual), 
                         prop = rep(93, length(unique(hr_lulc$individual))))
ggplot(data = hr_lulc)+
  geom_col(data = strip_data, mapping = aes(x = individual, y = prop),
           fill = "gray90")+
  ## Start Bar and Percentage text
  geom_col(mapping = aes(x = individual, y = prop-prop+93, 
                         group = Class_fr, fill = Class_fr), 
           color = NA, width = 0.03,
           position = position_dodge(width = 1))+
  geom_point(mapping = aes(x = individual, y = prop-prop+93, 
                         group = Class_fr, color = Class_fr), 
           size = 3, position = position_dodge(width = 1))+
  geom_point(mapping = aes(x = individual, y = prop-prop+93, group = Class_fr), 
             size = 1, position = position_dodge(width = 1), color = "white")+
  ## End Bar and Percentage text
  geom_col(mapping = aes(x = individual, y = prop, 
                         fill = Class_fr),
           position = position_dodge())+
  geom_text(mapping = aes(x = individual, y = prop - prop + 105,
                          label = paste0(round(prop, 1), "%"), 
                          group = Class_fr),
            position = position_dodge(width = 1),
            vjust = 1, hjust = .5, family = "mr", size = 6)+
  geomtextpath::geom_textline(data = strip_data,
            mapping = aes(x = individual, y = prop - prop + 115,
                          label = gsub("\\s", "\n", individual)),
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
fisher_extact_test <- maimer::mm_to_community(data = hr_lulc, species_column = individual,
                        site_column = Class_fr, size_column = n,
                        values_fill = 0) %>% 
  tibble::column_to_rownames("Class_fr") 
## Write contingency table
write.csv(fisher_extact_test, "tables/contengency_table_species_habitat.csv",
          fileEncoding = "ISO-8859-1")

# Fisher Exact test
fisher_extact_test %>%
  fisher.test(simulate.p.value=TRUE)
# p-value = 0.0004998


for_fisher <- read.csv("tables/hrLULC.csv") %>% 
  mutate(individual = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$|_Hivernage",
                           "", individual),
         individual = case_when(grepl('Tha', individual) ~ "Thalasseus sandvicensis",
                                TRUE ~ individual)) %>%
  dplyr::select(LULC, individual) %>% 
  dplyr::slice(-c(143, 637)) %>% 
  dplyr::rename(Value = LULC) %>% 
  dplyr::mutate(Value = as.numeric(Value)) %>% 
  dplyr::left_join(x = ., y = lulc %>% dplyr::select(-c(Class, Description)), by = "Value")


wrappedtools::pairwise_fisher_test(dep_var = for_fisher$individual,
                                   indep_var = for_fisher$Class_fr)
