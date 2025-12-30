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
    dplyr::rename('longitude' = longitude, "latitude" = latitude, "timestamp" = timestamp) %>% 
    dplyr::arrange(timestamp)
  
  
  #df$stopover_id <- NA_integer_
  #to_return <- data.frame()
  
  i <- 1
  stopover_id <- 1
  dynamic_stopover_table <- data.frame()
  
  while (nrow(df) >= 1 && i <= nrow(df)) {
  # print(paste0("I = ", i))
  # print(paste0("NROW = ", nrow(df)))
  # print(data.frame(lon = df$longitude[i], 
  #                  lat = df$latitude[i]))
  center_point <- sf::st_as_sf(x = data.frame(lon = df$longitude[i], 
                                              lat = df$latitude[i]),
                               coords = c("lon", "lat"), crs = 4326)
  
  dists <- sf::st_distance(x = center_point,
                           y = sf::st_as_sf(x = data.frame(lon = df$longitude, 
                                                           lat = df$latitude),
                                            coords = c("lon", "lat"),
                                            crs = 4326)
  ) %>% as.numeric()/ 1000  # to km
  
  # Get indices within radius
  in_radius_idx <- which(dists <= radius_km)
  
    # Check if time span in this group is ≥ 24 hours
    time_span <- difftime(df$timestamp[max(in_radius_idx)],
                          df$timestamp[min(in_radius_idx)],
                          units = "hours")
    
    if (length(in_radius_idx) >= 2 && time_span >= min_duration_hrs) {
      
      stpv_df <- df[in_radius_idx, ]
      stpv_df$stopover_id <- stopover_id
      
      # First point date
      fp_date <- stpv_df %>% 
        dplyr::slice(1) %>% 
        dplyr::pull(timestamp)
      # Last point date
      lp_date <- stpv_df %>% 
        dplyr::slice(nrow(stpv_df)) %>% 
        dplyr::pull(timestamp)
      
      # Update stopover table
      stpv_df <- stpv_df %>% 
        mutate(first_stopover_date = fp_date, last_stopover_date = lp_date)
      
      # Update stopover id for the next detection
      
      stopover_id <- stopover_id + 1
      # Remove all point after last point in the previous stop over
      df <- df %>% 
        dplyr::filter(timestamp > lp_date)
      
      i <- 1
      #i <- max(in_radius_idx) + 1
      dynamic_stopover_table <- rbind(dynamic_stopover_table, stpv_df)
    } else {
      i <- i + 1
    }
    
  }
  
  return(dynamic_stopover_table)
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
                       species_name = "species_name") {
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

## Afinity
afinity_index <- function(data, 
                    lon = "Longitude",
                    lat = "Latitude",
                    crs = "EPSG:4326",
                    season = "Season",
                    percent = 95,
                    method = "HR",
                    grid = 1000) {
  
  data <- data %>% 
    dplyr::rename(Name = !!dplyr::sym(season), lon = lon, lat = lat) %>% 
    dplyr::select(Name, lon, lat)
  
  xy <- sp::SpatialPointsDataFrame(coords = data[, c("lon", "lat")], data = data)
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  # Define EPSG:6933
  equal_area_crs <- CRS("+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs")
  xy <- sp::spTransform(xy, equal_area_crs)
  
  home_range <- adehabitatHR::kernelUD(xy = xy[, 1],
                                       h = "LSCV",
                                       grid = grid, same4all = TRUE)
  
  ovlp <- kerneloverlaphr(x = home_range, meth = method, 
                          percent = percent, conditional=TRUE)
  
  return(ovlp)
  
}

calc_angle <- function(x) {
  angle <- 90 - 360 * (x - 0.5) / length(x)
  print(ifelse(angle < -90, angle + 180, angle))
  ifelse(angle < -90, angle + 180, angle)
}


pairwise_fisher <- function (xtab, p.adjust.method = "holm", detailed = FALSE, ...) 
{
  if (is.data.frame(xtab)) 
    xtab <- as.matrix(xtab)
  if (ncol(xtab) > 2 & nrow(xtab) == 2) 
    xtab <- t(xtab)
  if (is.null(colnames(xtab)) | any(0 %in% nchar(colnames(xtab)))) {
    colnames(xtab) <- paste0("col", 1:ncol(xtab))
  }
  if (is.null(rownames(xtab)) | any(0 %in% nchar(rownames(xtab)))) {
    rownames(xtab) <- paste0("row", 1:nrow(xtab))
  }
  if (ncol(xtab) > 2) {
    stop("A two-dimensionnal contingency table required.")
  }
  compare_pair <- function(rows, xtab, ...) {
    rows <- as.character(rows)
    fisher_test(xtab[rows, ], detailed = detailed, ...) %>% 
      add_columns(group1 = rows[1], group2 = rows[2], .before = 1) %>% 
      keep_only_tbl_df_classes()
  }
  args <- c(as.list(environment()), list(...)) %>% add_item(method = "fisher_test")
  comparisons <- rownames(xtab) %>% .possible_pairs()
  results <- comparisons %>% map(compare_pair, xtab, ...) %>% 
    bind_rows() %>% adjust_pvalue("p", method = p.adjust.method) %>% 
    add_significance("p.adj") %>% mutate(p.adj = signif(.data$p.adj, 
                                                        digits = 3)) %>% select(-.data$p.signif)
  results %>% set_attrs(args = args) %>% add_class(c("rstatix_test", 
                                                     "fisher_test"))
}

Q1 <- function(x){
  round(quantile(x)[[2]], 2)
}

Q3 <- function(x){
  round(quantile(x)[[4]], 2)
}


lnorm_confint <- function(estimate, se, percent = 95){
  if(length(estimate) != length(se))
    cli::cli_abort("estimate and se must have the same number of values")
  z <- qt((1 - percent/100) / 2, Inf, lower.tail = FALSE)
  w <- exp(z * sqrt(log(1 + (se/estimate)^2)))
  data.frame(lower_bound = estimate/w, upper_bound = estimate*w)
}

### 
fix_name <- function(data, individual = "individual-local-identifier", phase = "") {
  data %>% 
    dplyr::rename(individual = individual) %>% 
    mutate(species = gsub(pattern = "\\s*\\[FRP-[A-Za-z0-9]+\\]|^\\d*_|\\s*\\d*$", 
                          "", individual),
           species = case_when(grepl('Tha', species) ~ "Thalasseus sandvicensis", 
                               TRUE ~ species),
           phase = phase) %>% 
    relocate(species, .after = individual)
}

