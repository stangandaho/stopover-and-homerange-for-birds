jacob_index <- function(used, available) {
  ji <- (used - available)/(used + available - 2*used*available)
  
  return(ji)
}

## Import overall habitat proportion
habitat_prop <- read.csv("datasets/overallHabitatProp.csv") %>% 
  dplyr::select(-c(system.index, area, id, percent, .geo))

hr_class_seq <- 10:18
buffer_class_seq <- 1:9
index <- 1:9
all_ji <- data.frame()

for (rw in 1:nrow(habitat_prop)) {
  single_ji <- list()
  message(paste0("On row: ", rw, " (", round(rw*100/nrow(habitat_prop), 1), "%)"))
  for (i in index) {
    each_site <- habitat_prop[rw, ] %>% 
      dplyr::select(c(hr_class_seq[i], buffer_class_seq[i])) %>% 
      dplyr::rename(hr = 1, buffer = 2) %>% 
      mutate(jacob_index = jacob_index(used = hr, available = buffer))
    
    single_ji[["individual"]] <- habitat_prop[rw, ][["individual"]]
    single_ji[[paste0("class_", i-1)]] <- ifelse(is.nan(each_site[["jacob_index"]]),
                                                 NA, each_site[["jacob_index"]])
  }
  all_ji <- rbind(all_ji, bind_cols(single_ji))
}

## Summarise all_ji
summ_ji <- all_ji %>% 
  rename(Species = individual) %>% 
  fix_name(species_col = "Species") %>% 
  mutate(Species = gsub("_Hivernage", "", Species)) %>% 
  group_by(Species) %>% 
  summarise(across(.cols = class_0:class_8, .fns = \(x)mean(x, na.rm = TRUE)))


summ_ji %>% write.csv("tables/jacob_index.csv", row.names = FALSE)
