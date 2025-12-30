# Load setup
source("scripts/utils.R")
source("setup.R")


dataset_root <- "datasets/Base_David"

all_files <- list.files(path = dataset_root, pattern = ".csv$", full.names = TRUE,
                        recursive = TRUE)

## Select only the part for migration
hivernage_files <- all_files[grepl(pattern = "ivernage", all_files)]

# Select folder that point to individual birds
all_species_names <- lapply(hivernage_files, function(x){
  split <- dirname(dirname(dirname(x)))
  split
}) %>% unlist() %>% unique()

all_hiv_data <- lapply(all_species_names, function(x){
  all_in_file <- list.files(path = x, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  all_in_file <- all_in_file[grepl(pattern = "ivernage", all_in_file)]
  ind_vol_data <- lapply(all_in_file, function(y){
  
    independen_event <- read.csv(y, check.names = FALSE) %>% 
      dplyr::select("location-lat", "location-long", "timestamp", "individual-local-identifier") %>%
      # Evaluate event independence
      ct::ct_independence(datetime = timestamp, format = "%Y-%m-%d %H:%M:%OS", threshold = 3600)
    
    ## Add i-th wintering information
    independen_event <- independen_event%>% 
      dplyr::mutate(th_hivernage =  basename(dirname(y)))
    independen_event
  }) %>% dplyr::bind_rows() %>% 
    dplyr::select("location-lat", "location-long", "datetime", 
           "individual-local-identifier", 'th_hivernage')
})%>% bind_rows()
# Write to disk
write.csv(all_hiv_data, "datasets/hivernages.csv", row.names = FALSE)

# Read back (not necessary if all_hiv_data was runned)
wintering_data <- read.csv("datasets/hivernages.csv", check.names = FALSE)

## Get unique species ID
species_id <- unique(wintering_data$`individual-local-identifier`)


{# Create data frame to store home range stat
all_stat <- data.frame()

# Create a data feame to store fidelity for each species in species_id
fidelity_df <- data.frame()

# Track progress
lvl <- 0; total <- length(species_id)

for (sp_id in species_id) {
  ind_df <- all_hiv_data %>% filter(`individual-local-identifier` == sp_id)
  # Select wintering for each sp_id
  number_of_hiv <- unique(ind_df$th_hivernage)

  # Show progress
  lvl <- lvl + 1
  message(paste0("On species ", sp_id, " (", lvl, "/", total, ")"))
  ## Create a list to store home range for each watering
  all_wintering <- list()
  
  for (hiv in number_of_hiv) {
    wintering_df <- ind_df %>% dplyr::filter(th_hivernage == hiv)
    message(paste0("Total points: ", nrow(wintering_df)))
    
    hr <- home_range(data = wintering_df, grid = 1000,
                     lon = "location-long", lat = "location-lat",
                     species_name = paste0(sp_id, "_", hiv))
    
    # wintering time
    wintering_time <- base::diff(range(wintering_df$datetime), units = 'days')
    sites <- as.numeric(sf::st_area(hr %>% st_cast(to = "POLYGON")))/1e6
    
    sites_stats <- ct::ct_describe_df(data.frame(sites = sites), 
                                          fn = list('sd', 'sum', 'std_error',
                                                    "Q1", "Q3")) %>% 
      dplyr::rename(homerange_size = sum) %>% 
      dplyr::mutate(wintering = hiv,
                    wintering_duration = wintering_time, 
                    individual = sp_id) %>% 
      dplyr::select(individual, wintering, homerange_size, wintering_duration)
    
    print(sites_stats)
    all_stat <- rbind(all_stat, sites_stats)
    
    
    # Combine home range to calculate fidelity later
    all_wintering[[hiv]] <- hr
    
  }
  
}

}


# 
hr_stats <- all_stat %>% 
  fix_name(individual = 'individual') %>% 
  dplyr::relocate(species, .before = 1) %>% 
  dplyr::mutate(wintering = gsub("\\s*", "", wintering),
                wintering_duration = as.numeric(wintering_duration))
# Write hr_stats
write.csv(hr_stats, "tables/home_range_stats.csv", row.names = FALSE)

hr_stats %>% group_by(species, wintering) %>% 
summarise(`Min area` = min(homerange_size, na.rm = TRUE), 
          `Max area` = max(homerange_size, na.rm = TRUE), 
          Median = median(homerange_size),
          `Mean area` = mean(homerange_size, na.rm = TRUE),
          `SE area` = sd(homerange_size, na.rm = TRUE)/n(),
          ###
          `Min duration` = min(wintering_duration, na.rm = TRUE), 
          `Max duration` = max(wintering_duration, na.rm = TRUE), 
          `Median duration` = median(wintering_duration),
          `Mean duration` = mean(wintering_duration, na.rm = TRUE),
          `SE duration` = sd(wintering_duration, na.rm = TRUE)/n()) %>% 
  ungroup() %>%
  write.csv("tables/home_range_stats_per_species.csv", row.names = FALSE)