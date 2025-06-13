# Load setup
source("scripts/utils.R")


dataset_root <- "E:\\David\\Base_David\\"

all_files <- list.files(path = dataset_root, pattern = ".csv$", full.names = TRUE,
                       recursive = TRUE)

## Select only the part for migration
migration_files <- all_files[grepl(pattern = "igration", all_files)]

# Select folder that point to individual birds
all_species_names <- lapply(migration_files, function(x){
  split <- dirname(dirname(dirname(x)))
  split
}) %>% unlist() %>% unique()

all_vol_data <- lapply(all_species_names, function(x){
  all_in_file <- list.files(path = x, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  mf <- all_in_file[grepl(pattern = "igration", all_in_file)]

  ind_vol_data <- lapply(all_in_file, function(y){
    read.csv(y, check.names = FALSE)
  }) %>% dplyr::bind_rows() %>% 
    dplyr::select("location-lat", "location-long", "timestamp", "individual-local-identifier") %>% 
    # Evaluate event independence
    track_independent(at_least = 5,
                      decrease_by = 0.05,
                      datetime = 'timestamp',
                      format = "%Y-%m-%d %H:%M:%OS",
                      threshold = 60*60,
                      only = TRUE)
  
})
# Area of Interest Extend
aoi_extend <- read_sf("datasets/aoi_extend/aoi_extend.shp")

# World boundaries
wb <- giscoR::gisco_countries %>% 
  st_intersection(aoi_extend)

# Initiate a dataframe to store table of number of stopover per individual
stopover_table <- data.frame(); progress_idx <- 0;

# Loop to calculate stopovers, plot map and save to plots/stopovers directory
for (df in all_vol_data) {
  
  # Message for progress
  progress_idx <- progress_idx + 1
  message(paste0("On ", progress_idx , "/", length(all_vol_data)))
  
  # Apply stepover detection
  stopover1 <- detect_stopovers(df = df,
                                longitude = "location-long",
                                latitude = "location-lat",
                                timestamp = "datetime",
                                min_duration_hrs = 24,
                                radius_km = 20)
  
  # Get species ID
  species_id <- unique(stopover1$`individual-local-identifier`)
  
  ## Plot
  stopover_sf <- stopover1 %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
    summarise(geometry = st_union(geometry), .by = "stopover_id") %>% 
    mutate(has_so = case_when(!is.na(stopover_id) ~ "1", TRUE ~ "0"),
           stopover_id = as.character(stopover_id))
  # Ensure all geometries are MULTIPOINT
  stopover_sf$geometry <- lapply(stopover_sf$geometry, function(geom) {
    if (inherits(geom, "POINT")) {
      st_multipoint(matrix(st_coordinates(geom), ncol = 2))
    } else {
      geom
    }
  }) %>% st_sfc(crs = st_crs(stopover_sf))
  
  # Get total stoppover
  total_stopover <- nrow(stopover_sf %>% dplyr::filter(stopover_id != 'NA'))
  
  # Write to disk
  shp_file <- paste0("datasets/stopovers/", species_id, ".shp")
  if(file.exists(shp_file)){sf::st_delete(shp_file)}
  sf::write_sf(stopover_sf, shp_file)
  
  # Create table
  so_tbl <- data.frame(individual = species_id, total_stopover = total_stopover)
  stopover_table <- rbind(stopover_table, so_tbl)
  
  
  plt <- ggplot()+
    geom_sf(data = wb, fill = "gray85", color = 'white')+
    geom_sf(data = stopover_sf,
            color = ifelse(stopover_sf$has_so == "1", "#b70000", "gray60"),
            size = ifelse(stopover_sf$has_so == "1", 2.5, 1))+
    labs(title = species_id, subtitle = paste("Total stopover: ", total_stopover))+
    theme_minimal()+
    theme(
      panel.background = element_rect(colour = 'gray90', linewidth = .1),
      plot.title = element_text(hjust = .5, size = 15), 
      plot.subtitle = element_text(hjust = .5, color = "gray50", size = 13),
      axis.text = element_text(size = 10)
    )
  
  ggsave(plot = plt, paste0("plots/stopovers/", species_id, ".jpeg"), width = 15, height = 20,
         units = "cm", dpi = 250)
  
}


## Statistics
species_so <- stopover_table %>% 
  mutate(individual = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$", 
                           "", individual),
         individual = case_when(grepl('Tha', individual) ~ "Thalasseus sandvicensis", 
                                TRUE ~ individual))
write.csv(stopover_table, "tables/stopovers.csv", row.names = FALSE)

so_stats <- lapply(unique(species_so$individual), function(x){
  species_so %>% 
    dplyr::filter(individual == x) %>% 
    dplyr::select(total_stopover) %>% 
    maimer::mm_describe_df(fn = list('sd', 'std_error')) %>% 
    mutate(species = x)
}) %>% dplyr::bind_rows() %>% 
  dplyr::select(-Variable) %>% 
  dplyr::relocate(species, .before = 1)
  
write.csv(so_stats, "tables/stopovers_statistics.csv", row.names = FALSE)
species_so <- read.csv( "tables/stopovers.csv")
species_so %>% summarise(ct = sum(total_stopover), .by = 'individual')

# Apply statistics test
rstatix::kruskal_test(species_so %>% filter(individual != ''), formula = total_stopover ~ individual)
summary(aov(data=species_so, formula = total_stopover ~ individual))
# A tibble: 1 × 6
# .y.                n statistic    df      p method        
# * <chr>          <int>     <dbl> <int>  <dbl> <chr>         
#   1 total_stopover    38      9.44     4 0.0511 Kruskal-Wallis

# Import back stopover site to merge and plot per species
so_sf <- list.files(path = "datasets/stopovers/", pattern = ".shp$", full.names = TRUE) %>% 
  lapply(X = ., function(x){
    sf::read_sf(x, quiet = TRUE) %>% 
      dplyr::mutate(Species = basename(x)) %>% 
      dplyr::mutate(Species = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$|.shp$", 
                               "", Species),
                    Species = case_when(grepl('Thau', Species) ~ "Thalasseus sandvicensis", 
                                    TRUE ~ Species))
      }) %>% 
  dplyr::bind_rows()

so_plot <- lapply(unique(so_sf$Species), function(x){
  ind_df <- so_sf %>% dplyr::filter(Species == x)
  total_site <- sum(as.numeric(ind_df[['has_so']]), na.rm = TRUE)
  ggplot()+
    geom_sf(data = wb, fill = "gray85", color = 'white')+
    geom_sf(data = ind_df,
            color = ifelse(ind_df$has_so == "1", "#b70000", "gray60"),
            size = ifelse(ind_df$has_so == "1", 1.2, .75))+
    labs(title = x, subtitle = paste("Total stopover: ", total_site))+
    theme_minimal()+
    theme(
      panel.background = element_rect(colour = 'gray90', linewidth = .1),
      plot.title = element_text(hjust = .5, size = 15), 
      plot.subtitle = element_text(hjust = .5, color = "gray50", size = 13),
      axis.text = element_text(size = 10)
    )
})


cowplot::plot_grid(plotlist = so_plot, ncol = 5, nrow = 1)
ggsave(paste0("plots/stopovers/stopovers_per_species.jpeg"), width = 35, height = 15,
       units = "cm", dpi = 250)
