# Load setup
source("scripts/utils.R")
source("setup.R")

dataset_root <- "datasets/Base_David" #"E:\\David\\Base_David\\"
all_files <- list.files(path = dataset_root, pattern = ".csv$", full.names = TRUE,
                       recursive = TRUE)

## Select only the part for migration
migration_files <- all_files[grepl(pattern = "igration", all_files)]
phase_pattern <- "Sud_Nord"#"Nord_Sud" # 
phase <- tolower(phase_pattern); 
direction_color <- switch (phase,
                           "sud_nord" = "#00c301",
                           "nord_sud" = "#b70000"
)
mf <- migration_files[grepl(phase_pattern, migration_files)]

# Select folder that point to individual birds
season <- switch (phase,
  "sud_nord" = "Spring",
  "nord_sud" = "Autumn"
)

all_vol_data <- lapply(mf, function(x){
   ct::ct_read(x, check.names = FALSE) %>% 
    dplyr::mutate(vol_no = basename(dirname(dirname(x)))) %>% 
    dplyr::select("location-lat", "location-long", "timestamp", "individual-local-identifier", "vol_no") %>%
    ct::ct_independence(datetime = timestamp, format = "%Y-%m-%d %H:%M:%OS", threshold = 3600)
})

# Area of Interest Extend
aoi_extend <- read_sf("datasets/aoi_extend/aoi_extend.shp")

# World boundaries
wb <- giscoR::gisco_countries %>% 
  st_intersection(aoi_extend)

# Initiate a dataframe to store table of number of stopover per individual
stopover_table <- data.frame(); progress_idx <- 0;



# Loop to calculate stopovers, plot map and save to plots/stopovers directory
global_duration_df <- dplyr::tibble()
global_time_info_df <- dplyr::tibble()
all_stopover <- dplyr::tibble()
stopover_and_no <- dplyr::tibble()


for (df in all_vol_data) {
  
  for (vol in unique(df$vol_no)) {
    single_ind_vol <- df %>% dplyr::filter(vol_no == vol)
    species_id <- unique(single_ind_vol$`individual-local-identifier`)
    raw_data <-  single_ind_vol %>% rename(longitude = `location-long`, 
                                                  latitude = `location-lat`) %>% 
      dplyr::select(c("longitude", "latitude", "individual-local-identifier")) %>% 
      mutate(datatype = "raw")
    
    # Apply stepover detection
    stopover1 <- detect_stopovers(df = single_ind_vol,
                                  longitude = "location-long",
                                  latitude = "location-lat",
                                  timestamp = "datetime",
                                  min_duration_hrs = 24,
                                  radius_km = 20)
    
    if (nrow(stopover1) == 0) {
      summary_df <- dplyr::tibble(species_id = species_id,
                                  season = season,
                                  year = NA,
                                  total_stopover = 0,
                                  total_duration = 0,
                                  average_duration = 0,
                                  min_duration = 0,
                                  max_duration = 0)
      
      time_info <- dplyr::tibble(duration = 0,
                start_stopover = NA,
                end_stopover = NA,
                stopover_id = NA,
                species_id = species_id)
      
      global_time_info_df <- bind_rows(global_time_info_df, time_info)
      all_stopover_df <- raw_data
      total_spv <- 0
      
    }else{
    # Duration vector
    total_spv <- length(unique(stopover1$stopover_id[!is.na(stopover1$stopover_id)]))
    duration <- stopover1 %>% 
      slice(1, .by = stopover_id) %>% 
      reframe(duration = difftime(time1 = last_stopover_date,
                                  time2 = first_stopover_date,
                                  units = "hours")) %>% 
      mutate(duration = as.numeric(duration)) %>% 
      pull(duration) 
    # Duration summary
    summary_df <- dplyr::tibble(species_id = species_id,
                             season = season,
                             year = lubridate::year(stopover1$timestamp[1]),
                             total_stopover = total_spv,
                             total_duration = sum(duration),
                             average_duration = mean(duration),
                             min_duration = min(duration),
                             max_duration = max(duration)
                             )
    
    ## Summary
    time_info <- stopover1 %>% 
      slice(1, .by = stopover_id) %>% 
      reframe(duration = difftime(time1 = last_stopover_date,
                                  time2 = first_stopover_date,
                                  units = "hours"),
              start_stopover = first_stopover_date,
              end_stopover = last_stopover_date,
              stopover_id = stopover_id,
              species_id =  `individual-local-identifier`
              ) %>% 
      mutate(duration = as.numeric(duration))

    stopover_sf <- stopover1 %>% 
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
      summarise(geometry = st_union(geometry), .by = "stopover_id")
    # Write to disk
    shp_file <- paste0("datasets/stopovers/", phase, "/", species_id, "_", vol, ".shp")
    if(file.exists(shp_file)){sf::st_delete(shp_file, quiet = TRUE)}
    sf::write_sf(stopover_sf, shp_file)
    all_stopover <- bind_rows(all_stopover, stopover1)
    
    ## Merge stopover and non-stopover point
    all_stopover_df <-  raw_data %>% 
      bind_rows(stopover1 %>% 
                  dplyr::select(c("longitude", "latitude", "individual-local-identifier")) %>% 
                  mutate(datatype = "stopover"))
    }
 
  global_duration_df <- bind_rows(global_duration_df, summary_df %>% mutate(vol_no = vol))
  global_time_info_df <- bind_rows(global_time_info_df, time_info%>% mutate(vol_no = vol))
  stopover_and_no <- bind_rows(stopover_and_no, all_stopover_df%>% mutate(vol_no = vol))
  
  
  ## Convert for map
  # stopovers <- all_stopover_df %>% 
  #   dplyr::filter(datatype == "stopover") %>% 
  #   dplyr::pull(stopover_id) %>% 
  #   unique() %>% length()
  
  #unique(stopover1$stopover_id[!is.na(stopover1$stopover_id)])
  sptv4map <- all_stopover_df %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  plt <- ggplot()+
    geom_sf(data = wb, fill = "gray85", color = 'white')+
    geom_sf(data = sptv4map,
            color = ifelse(sptv4map$datatype == "stopover", direction_color, "gray10"),
            size = ifelse(sptv4map$datatype == "stopover", 1.5, 0.5))+
    labs(title = species_id, subtitle = paste("Total stopover: ", total_spv))+
    theme_minimal()+
    theme(
      panel.background = element_rect(colour = 'gray90', linewidth = .1),
      plot.title = element_text(hjust = .5, size = 60), 
      plot.subtitle = element_text(hjust = .5, color = "gray20", size = 50),
      axis.text = element_text(size = 35)
    )
  
  ggsave(plot = plt, paste0("plots/stopovers/", species_id, "_", phase, "_", vol, ".jpeg"), 
         width = 18, height = 20, units = "cm")
  
  }
}


# Write table
write.csv(global_duration_df, paste0("tables/global_duration_", phase, ".csv"), row.names = FALSE)
write.csv(global_time_info_df, paste0("tables/global_time_info_", phase, ".csv"), row.names = FALSE)

# Add species column
species_so <- all_stopover %>% fix_name(phase = phase)
write.csv(species_so, paste0("tables/all_stopovers_", phase, ".csv"), row.names = FALSE)
write.csv(stopover_and_no %>% fix_name(phase = phase), 
          paste0("tables/location_and_stopover_", phase, ".csv"),
          row.names = FALSE)


## Statistics
duration_df <- global_duration_df %>% 
  mutate(species = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$", 
                        "", species_id),
         species = case_when(grepl('Tha', species) ~ "Thalasseus sandvicensis", 
                             TRUE ~ species),
         phase = phase) %>% 
  relocate(species, .after = species_id)

so_stats <- lapply(unique(duration_df$species), function(x){
  idf <- duration_df %>% 
    dplyr::filter(species == x) %>% 
    dplyr::select(total_stopover)
  
  if (x == "Thalasseus sandvicensis") {
    print(idf)
  }
  
  # Use lognormal confidence intervals. Because  Lognormal data is restricted to 
  # positive values like mean of number of stopover.
  CI <- lnorm_confint(estimate = mean(idf$total_stopover),
                      se = sd(idf$total_stopover)/nrow(idf))
  
  idf %>% ct::ct_describe_df(fn = list('sd', 'std_error', "sum")) %>%
      bind_cols(CI) %>% 
    mutate(species = x) %>% 
    dplyr::select(-c(`CI Left`, `CI Right`))
  
}) %>% dplyr::bind_rows() %>% 
  dplyr::select(-Variable) %>% 
  dplyr::relocate(species, .before = 1)
  
write.csv(so_stats, paste0("tables/number_stopovers_statistics_", phase, ".csv"), row.names = FALSE)

# Apply statistics test on number of stopover
kt <- rstatix::kruskal_test(duration_df, 
                      formula = total_stopover ~ species)
kt
if (kt$p < 0.05) {
  rstatix::dunn_test(species_so %>% filter(individual != ''), 
                     formula = total_stopover ~ individual) %>% 
  write.csv(paste0("tables/stopovers_dunn_test_", phase, ".csv"), row.names = FALSE)
}

kt %>% write.csv(paste0("tables/kruskal_test_stopover", "_", phase, ".csv"))

## Apply test on duration
kt <- rstatix::kruskal_test(global_time_info_df %>% fix_name(individual = "species_id", phase = phase), 
                            formula = duration ~ species)
kt
if (kt$p < 0.05) {
  rstatix::dunn_test(global_time_info_df %>% fix_name(individual = "species_id", phase = phase), 
                     formula = duration ~ species) %>% 
    write.csv(paste0("tables/duration_stopovers_dunn_test_", phase, ".csv"), row.names = FALSE)
}

kt %>% write.csv(paste0("tables/duration_stopover_kruskal_test", "_", phase, ".csv"))

# Import back stopover site to merge and plot per species
so_sf <- list.files(path = paste0("datasets/stopovers/", phase), pattern = ".shp$", full.names = TRUE) %>% 
  lapply(X = ., function(x){
    sf::read_sf(x, quiet = TRUE) %>% 
      dplyr::mutate(individual = basename(x)) %>%
      dplyr::mutate(Species = individual) %>% 
      dplyr::mutate(Species = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$|.shp$", 
                               "", Species),
                    Species = case_when(grepl('Thau', Species) ~ "Thalasseus sandvicensis", 
                                    TRUE ~ Species),
                    Species = gsub("*_.*$", "", Species))
      }) %>% 
  dplyr::bind_rows()

# Read back location and stopover points
location_and_stopover <- read.csv(paste0("tables/location_and_stopover_", phase, ".csv"))

so_plot <- lapply(unique(so_sf$Species), function(x){
  ind_df <- so_sf %>% dplyr::filter(Species == x)
  total_site <- ind_df %>% 
    group_by(individual) %>% 
    summarise(n_stopover = length(unique(stpvr_d))) %>% 
    pull(n_stopover) %>% 
    sum()
  
  #total_site <- sum()
  
  no_stopover <- location_and_stopover %>% 
    dplyr::filter(species == x, datatype == "raw") %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  individual_number <- length(unique(no_stopover %>% pull(individual)))
  
  ggplot()+
    geom_sf(data = wb, fill = "gray85", color = 'white')+
    geom_sf(data = no_stopover, color = "gray10", size = 0.5)+
    geom_sf(data = ind_df, color = direction_color, size = 1.5)+
    labs(title = x, subtitle = paste0("Total stopover: ", total_site, "; Season: ", season,
                                      "\nIndividual number: ", individual_number))+
    theme_minimal()+
    theme(
      panel.background = element_rect(colour = 'gray90', linewidth = .1),
      plot.title = element_text(hjust = .5, size = 60, face = "italic"), 
      plot.subtitle = element_text(hjust = .5, color = "gray50", size = 50, lineheight = 0.3),
      axis.text = element_text(size = 35)
    )
})


cowplot::plot_grid(plotlist = so_plot, ncol = 2, nrow = 2)
ggsave(paste0("plots/stopovers/stopovers_per_species_", phase, ".jpeg"), width = 25, height = 30,
       units = "cm")