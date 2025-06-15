# Load setup
source("scripts/utils.R")
source("setup.R")


dataset_root <- "E:\\David\\Base_David\\"

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
    print(basename(x))
    df <- read.csv(y, check.names = FALSE)
      # Evaluate event independence
    independen_event <- track_independent(data = df, at_least = 5,
                                          decrease_by = 0.05,
                                          datetime = 'timestamp',
                                          format = "%Y-%m-%d %H:%M:%OS",
                                          threshold = 60*60,
                                          only = TRUE)
    
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

## Exclude non-event (ie. select events within 1 hour interval) data 
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
                     species_name = paste0(sp_id, "_", hiv)
    )
    
    #total_site <- length(st_geometry(hr)[[1]])
    total_area <- as.numeric(st_area(st_geometry(hr)))/1e6 # km2
    print(total_area)
    # wintering time
    wintering_time <- base::diff(range(wintering_df$datetime), units = 'days')
    
    sites <- as.numeric(sf::st_area(hr %>% st_cast(to = "POLYGON")))/1e6
    sites_stats <- maimer::mm_describe_df(data.frame(sites = sites), 
                                          fn = list('sd', 'sum', 
                                                    'std_error')) %>% 
      dplyr::mutate(Hivernage = hiv,
                    `Temps d'hivernage` = wintering_time, 
                    Species = sp_id)
    all_stat <- rbind(all_stat, sites_stats)
    
    
    # Combine home range to calculate fidelity later
    all_wintering[[hiv]] <- hr
    
  }
  
  # Calculate fidelity for each sp_id
  names(all_wintering) <- NULL
  intersections <- sf::st_area(do.call(sf::st_intersection, all_wintering))
  unions <- sf::st_area(do.call(sf::st_union, all_wintering))
  fidelity <- intersections/unions
  
  fidelity_df <- rbind(fidelity_df, data.frame(Species = sp_id, Fidelity = fidelity))
  
  
}
}

# Write all_stat and fidelity_df

write.csv(all_stat, "tables/home_range_stats.csv", row.names = FALSE)
write.csv(fidelity_df, "tables/fidelity.csv", row.names = FALSE)

# 
all_stat <- read.csv("tables/home_range_stats.csv", check.names = FALSE) %>% 
  fix_name(species_col = 'Species') %>% 
  dplyr::select(-c(Variable, `CI Left`, `CI Right`)) %>% 
  dplyr::relocate(Species, .before = 1) %>% 
  dplyr::mutate(Hivernage = gsub("\\s*", "", Hivernage ))

all_stat %>% group_by(Species, Hivernage) %>% 
summarise(N = round(mean(N)), `Min area` = min(Min, na.rm = TRUE), 
          `Max area` = max(Max, na.rm = TRUE), `Mean area` = mean(Mean, na.rm = TRUE),
          `SE area` = mean(std_error, na.rm = TRUE),
          `Temps d'hivernage` = mean(`Temps d'hivernage`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  write.csv("tables/home_range_stats_per_species.csv", row.names = FALSE)
