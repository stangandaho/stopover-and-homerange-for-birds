# Stopover detection

#### Logic for stopover detection ###
# 1- Iterate through each point as a candidate center.
# 2- Find all subsequent points within a 20 km radius of this center point.
# 3- Check the time span covered by these points.
# 4- If the time span ≥ 24 hours → mark it as a stopover.
# 5- Then skip all points within that 20 km circle.
# 6- Move on to the next point outside the current stopover circle.
# 7- If not a stopover, move the center to the next point and repeat.

source("setup.R")
detect_stopovers <- function(df, 
                             longitude, 
                             latitude, 
                             timestamp, 
                             radius_km = 20, 
                             min_duration_hrs = 24) {
  df <- df %>% 
    dplyr::rename('longitude' = longitude, "latitude" = latitude, "timestamp" = timestamp)
  
  df <- df %>% arrange(timestamp)
  df$stopover_id <- NA_integer_
  
  i <- 1
  stopover_id <- 1
  
  while (i <= nrow(df)) {
    center_point <- sf::st_as_sf(x = data.frame(lon = df$longitude[i], 
                                                lat = df$latitude[i]),
                                 coords = c("lon", "lat"), crs = 4326)
    
    # Compute distances from current center to all points
    dists <- sf::st_distance(x = center_point,
                             y = sf::st_as_sf(x = data.frame(lon = df$longitude, 
                                                         lat = df$latitude),
                                              coords = c("lon", "lat"),
                                              crs = 4326)
                             ) / 1000  # to km
    dists <- as.numeric(dists)

    # Get indices within radius
    in_radius_idx <- which(dists <= radius_km)
    
    # Check if time span in this group is ≥ 24 hours
    time_span <- difftime(df$timestamp[max(in_radius_idx)],
                          df$timestamp[min(in_radius_idx)],
                          units = "hours")
    
    if (length(in_radius_idx) >= 2 && time_span >= min_duration_hrs) {
      # Assign stopover ID to these points
      df$stopover_id[in_radius_idx] <- stopover_id
      stopover_id <- stopover_id + 1
      
      # Skip to first point outside the current stopover radius
      i <- max(in_radius_idx) + 1
    } else {
      i <- i + 1
    }
  }
  
  return(df)
}

# Standar error
std_error <- function(x){sd(x)/sqrt(sum(!is.na(x)))}

## Home range
home_range <- function(data,
                       lon = "Longitude",
                       lat = "Latitude",
                       crs = "EPSG:4326",
                       percent = 95,
                       grid = 1000,
                       save_dir = "home_range",
                       species_name = "elephant_male") {
  ### Calculate a
  data <- data %>%
    dplyr::rename(lon = lon, lat = lat)
  
  ## Estimation of Kernel Home-Range - Utilization Distribution (UD) method
  xy <- sp::SpatialPointsDataFrame(coords = data[, c("lon", "lat")], data = data)
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  # Define EPSG:6933
  equal_area_crs <- CRS("+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs")
  xy <- sp::spTransform(xy, equal_area_crs)
  
  home_range <- adehabitatHR::kernelUD(xy = xy,
                                       h = "LSCV",
                                       grid = grid)
  
  
  # Estimating the home range from the UD
  ## Home ranges in vector mode
  hr_list <- list()
  for (prc in percent) {
    home_range_est <- adehabitatHR::getverticeshr(x = home_range,
                                                  percent = prc, # percentage level for home-range estimation,
                                                  unin = "m",
                                                  unout = "km2")
    print(home_range_est)
    
    hr_sf <- sf::st_as_sf(home_range_est)
    #st_crs(hr_sf) <- sf:
    
    hr_list[[prc]] <- hr_sf %>% dplyr::mutate(percent = prc)
  }
  
  if(!dir.exists(save_dir)){dir.create(save_dir, recursive = TRUE)}
  
  save_path <- file.path(save_dir, paste0(species_name, ".shp"))
  if(file.exists(save_path)){sf::st_delete(save_path)}
  
  hr_sf <- dplyr::bind_rows(hr_list)
  sf::write_sf(hr_sf, save_path)
  
  return(hr_sf)
}


## Track independent value check
# This function is an extension of maimer::mm_independence, whose filter 
# data to return only observations that have at least a given time interval
# to (threshold) next observation. The particluarity of track_independent is to set 
# set at_least that require the minimum number of observation to keep in the 
# output. While this condition is not met, the threshlod will start by decreasing 
# at a rate of decrease_by.

track_independent <- function(data, at_least, threshold, decrease_by = 0.1, ...) {
  df <- data %>% 
    maimer::mm_independence(threshold = threshold,...)

  
  while(nrow(df) <= at_least){
    threshold <- threshold - threshold*decrease_by
    print(paste0('treh: ', threshold))
    df <- data %>% maimer::mm_independence(threshold = threshold,...)
  }
  
  return(df)
}

### 
fix_name <- function(data, species_col) {
  data %>% 
    dplyr::mutate(Species = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$|.shp$", 
                               "", !!dplyr::sym(species_col)),
                Species = case_when(grepl('Thau', !!dplyr::sym(species_col)) ~ "Thalasseus sandvicensis", 
                                    TRUE ~ !!dplyr::sym(species_col)))
}
