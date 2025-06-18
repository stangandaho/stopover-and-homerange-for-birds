# Load setup
source("scripts/utils.R")
source("setup.R")

## Import wintering data 
wintering_data <- read.csv("datasets/hivernages.csv", check.names = FALSE)

## Select only where we have at least 2 season/wintering
ind4_afinity_index <- wintering_data %>%
  #dplyr::filter(`individual-local-identifier` == test_ind)
  group_by(`individual-local-identifier`) %>% 
  summarise(total = length(unique(th_hivernage))) %>% 
  dplyr::filter(total > 1) %>% 
  dplyr::pull(1)

## Message to track progress
lvl <- 0; total <- length(ind4_afinity_index)
## File to save
afinity_file <- file("tables/afinity_index.txt", "a")

for (ind in ind4_afinity_index) {
  lvl <- lvl + 1
  message(paste0("On ", lvl, "/", total, " (", round((lvl/total)*100, 1), "%)"))
  ahiv <- wintering_data %>%
    dplyr::filter(`individual-local-identifier` == ind)
  
  afinity <- afinity_index(data = ahiv, lon = "location-long", lat = "location-lat",
                           season = "th_hivernage", method = "HR")
  cat(ind, file = afinity_file, sep = "\n")
  cat(capture.output(as.matrix(round(afinity, 3))), file = afinity_file, sep = "\n")
  cat("", file = afinity_file, sep = "\n\n\n\n")
}
close(afinity_file)
