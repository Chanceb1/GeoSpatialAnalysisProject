# Geospatial Analysis of Frequent Locations and Movement Patterns
# Term project for Cpts 475 "Data Science"

# Load necessary libraries and Dataset

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)

# load initial data set
locations = read.csv('../data/all_locations.csv', header = TRUE, sep = ",")

head(locations)

# Perform data tidying
locations <- locations %>%
  mutate(datetime = mdy_hm(datetime)) %>% # convert datetime col to datetime format
  separate(datetime, into = c("date", "time"), sep = " ") %>% # split datetime column into date and time
  select(acc, lat, long, date, time) %>% # remove additional count column
  distinct()  # remove duplicates

head(locations)

# Exploratory Data Analysis

# Print summary statistics of the data
summary(locations)

# (GeoSpatial Plot) Display points on geo map for all locations with accuracy measure heatmap
library(ggplot2)
library(maps)
library(viridis)

# Manually set latitude and longitude ranges
min_longitude <- -125.0   
max_longitude <- -116.0  
min_latitude <- 37.5    
max_latitude <- 49.0

# Improved Geospatial Plot
geomap <- ggplot(locations, aes(x = long, y = lat)) +
  borders("world", 
          colour = "gray50", 
          fill = "gray90") +
  
  geom_point(
    aes(color = acc),
    size = 3, 
    alpha = 0.6, 
    shape = 16  
  ) +
  
  scale_color_viridis_c(
    name = "Accuracy",
    option = "plasma",
    alpha = 0.7
  ) +
  
  coord_fixed(
    xlim = c(min_longitude, max_longitude),
    ylim = c(min_latitude, max_latitude)
  ) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  ) +
  
  labs(
    title = "Total Locations",
    x = "Longitude",
    y = "Latitude",
    caption = paste("Total unique locations:", nrow(locations))
  )

print(geomap)

# Create a summary of geographical spread
location_summary <- locations %>%
  summarise(
    total_locations = n(),
    min_latitude = min(lat, na.rm = TRUE),
    max_latitude = max(lat, na.rm = TRUE),
    min_longitude = min(long, na.rm = TRUE),
    max_longitude = max(long, na.rm = TRUE)
  )

print(location_summary)

# Clustering Data Points

# Make a table with DBSCAN
library(sf)
library(dbscan)

# Convert to sf
locations_sf <- locations %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# Run DBSCAN
set.seed(100)
clusters <- dbscan(st_coordinates(locations_sf), eps = 0.0005, minPts = 5)

# Add ID
locations <- locations %>%
  mutate(
    cluster = clusters$cluster
  )

locations <- locations %>%
  arrange(date, time) %>%
  mutate(
    prev_cluster = lag(cluster),
    movement = ifelse(prev_cluster != cluster & !is.na(prev_cluster), 
                      paste(prev_cluster, "->", cluster), NA)
  )

# Summarize transitions
transition_summary <- locations %>%
  filter(!is.na(movement)) %>%
  group_by(movement) %>%
  summarise(
    count = n(),
    avg_time = mean(as.numeric(
      difftime(
        as.POSIXct(paste(date, time)), 
        lag(as.POSIXct(paste(date, time))), 
        units = "mins"
      )
    ), na.rm = TRUE)
  )

# Calculate summary statistics by cluster
location_summary <- locations %>%
  filter(cluster != 0) %>%
  mutate(month = month(as.Date(date))) %>%
  group_by(cluster, month) %>%
  summarise(
    lat = mean(lat),
    long = mean(long),
    staying_time = sum(
      as.numeric(
        difftime(
          max(as.POSIXct(paste(date, time))), 
          min(as.POSIXct(paste(date, time))), 
          units = "hours"
        )
      )
    )
  ) %>%
  ungroup()

print(location_summary)

# Plot the clustered data
plot(locations_sf, col = clusters$cluster + 1, pch = 19, cex = 0.5, main="DBSCAN Clustered Data")

# Identify the top 5 locations where the individual spends the most time each month
top_locations <- location_summary %>%
  group_by(month) %>%
  arrange(desc(staying_time), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup()

top5_locations_per_month <- split(top_locations, top_locations$month)

print(top5_locations_per_month)

# Plot the top 5 locations and mapping the typical movement sequence
library(patchwork)
library(stringr)

plot_list <- list()

for (i in seq_along(top5_locations_per_month)) {
  top5_locations <- top5_locations_per_month[[i]]
  
  long_buffer <- (max(top5_locations$long) - min(top5_locations$long)) * 0.3
  lat_buffer <- (max(top5_locations$lat) - min(top5_locations$lat)) * 0.3
  
  transitions <- transition_summary %>%
    filter(as.numeric(str_extract(movement, "\\d+")) %in% unique(top5_locations$cluster)) %>%
    separate(movement, into = c("start_cluster", "end_cluster"), sep = "->", convert = TRUE) %>%
    left_join(top5_locations, by = c("start_cluster" = "cluster")) %>%
    rename(start_long = long, start_lat = lat) %>%
    left_join(top5_locations, by = c("end_cluster" = "cluster")) %>%
    rename(end_long = long, end_lat = lat)
  
  p <- ggplot() +
    borders("world", colour = "gray50", fill = "gray95") +
    geom_point(data = top5_locations, 
               aes(x = long, y = lat, 
                   size = staying_time,
                   color = factor(cluster))) +
    geom_text(data = top5_locations,
              aes(x = long, y = lat,
                  label = sprintf("%.1f mins", staying_time)),
              vjust = -1, hjust = -0.2, size = 3) +
    geom_segment(data = transitions, 
                 aes(x = start_long, y = start_lat, 
                     xend = end_long, yend = end_lat),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "blue", size = 0.8) +
    scale_size_continuous(range = c(3, 10),
                          name = "Staying Time (mins)") +
    scale_color_viridis_d(name = "Cluster") +
    labs(title = paste("Month", i),
         x = "Longitude", 
         y = "Latitude") +
    theme_minimal() +
    coord_quickmap(
      xlim = c(min(top5_locations$long) - long_buffer, 
               max(top5_locations$long) + long_buffer),
      ylim = c(min(top5_locations$lat) - lat_buffer, 
               max(top5_locations$lat) + lat_buffer)
    ) +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical"
    )
  
  plot_list[[i]] <- p
}

for (p in plot_list) {
  print(p)
}
