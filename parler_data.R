# Packages ------------------------------------------------------------------------
library(jsonlite)
library(dplyr)
library(future)
library(furrr)
library(purrr)
library(lubridate)
library(sf)
library(ggplot2)
library(leaflet)
library(hrbrthemes)

# Data ----------------------------------------------------------------------------

# Download metadata folder
download.file("https://t.co/rHlECQs5k2?amp=1", "parler_vids_metadata.tar.gz")

# Files
fls <- list.files("metadata", full.names = T)

# Function to extract GPS data
read_metadata <- function(pos, files) {
  
  md <- jsonlite::fromJSON(files[pos])
  
  if ("GPSPosition" %in% names(md)) {
    
    if ("CreateDate" %in% names(md)) {
      
      p1 <- md %>% dplyr::select(CreateDate, GPSLatitude, GPSLongitude)
      
    } else {
      
      p1 <- md %>% 
        dplyr::select(GPSLatitude, GPSLongitude) %>% 
        dplyr::mutate(CreateDate = NA)
      
    }
    
    # Add name of metadata file
    p1 <- p1 %>% dplyr::mutate(file = files[pos])
  
    } else {
      
      p1 <- NULL
      
    }
  
  # Pos
  if (pos %% 100 == 0) cat(".")
  if (pos %% 1000 == 0) cat(pos, "\n")
  
  # Return
  return(p1)
  
  }

# Read Metadata
plan(strategy = "multisession")
dt <- future_map_dfr(1:length(fls), read_metadata, files = fls)
saveRDS(dt, "parler_vids_metadata.rds")
dt <- readRDS("parler_vids_metadata.rds")

# Functions for tidy lon/lat
tidy_lat <- function(x) {
  
  parts <- unlist(stringr::str_split(x, "[^0-9\\.]"))
  parts <- as.numeric(parts[!parts == ""])
  lat <- parts[1] + parts[2] / 60 + parts[3] / 3600
  if (stringr::str_detect(x, "S")) lat <- -1 * lat
  return(lat)
  
  }
tidy_lon <- function(x) {
  
  parts <- unlist(stringr::str_split(x, "[^0-9\\.]"))
  parts <- as.numeric(parts[!parts == ""])
  lon <- parts[1] + parts[2] / 60 + parts[3] / 3600
  if (stringr::str_detect(x, "W")) lon <- -1 * lon
  return(lon)
  
}

# Prep data
dt2 <- dt %>% 
  mutate(CreateDate = ymd_hms(CreateDate)) %>% 
  mutate(lat = map_dbl(GPSLatitude, tidy_lat)) %>% 
  mutate(lon = map_dbl(GPSLongitude, tidy_lon)) %>% 
  filter(!(lat == 0 & lon == 0))

# Capitol 2021-6-1 ----------------------------------------------------------------

# 2021-6-1 in DC
dt2_jan6 <- dt2 %>% 
  filter(date(CreateDate) == "2021-01-06") %>% 
  filter(lat > 38.6 & lat < 39.2)

# Leaflet
leaflet(dt2_jan6) %>% 
  setView(lng = -77.025, lat = 38.892, zoom = 14) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(~lon, ~lat, popup = ~as.character(CreateDate))

# Parler in Switzerland -----------------------------------------------------------

# sf
dt2_sf <- st_as_sf(dt2, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Geodata Switzerland
source("get_geodata.R")

# National boarder
ch <- get_geodata("national")
st_crs(ch) <- 21781
ch_wgs84 <- st_transform(ch, crs = 4326)

# Subset
dt2_sf_ch <- dt2_sf[t(st_contains(ch_wgs84, dt2_sf, sparse = F)),]

# Additional geodata
cantons <- get_geodata("canton")
st_crs(cantons) <- 21781
cantons_wgs84 <- st_transform(cantons, crs = 4326)

lakes <- get_geodata("lakes")
st_crs(lakes) <- 21781
lakes_wgs84 <- st_transform(lakes, crs = 4326)

# Map
ggplot() +
  geom_sf(data = cantons_wgs84, fill = "white", color = "white") +
  geom_sf(data = lakes_wgs84, fill = "grey80", color = "grey80") +
  geom_sf(data = dt2_sf_ch, color = "#ebcc34", alpha = 0.7, size = 3, shape = 16) +
  theme_ft_rc() +
  scale_color_manual(values = c(ft_cols$yellow, ft_cols$blue, ft_cols$white)) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    legend.justification = "top",
    legend.title = element_blank()
  )  +
  labs(
    title = "Parler in Switzerland", 
    subtitle = "Parler videos with recording location in Switzerland (N=28)",
    caption = "\nData: t.co/rHlECQs5k2?amp=1"
  ) 

ggsave("parler_ch.png", dpi = 500, width = 8, height = 5)
