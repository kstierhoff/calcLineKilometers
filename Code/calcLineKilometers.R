# List packages required to run the script -------------------------------------
pkgs <- c("tidyverse","swfscMisc","lubridate","cowplot","here","marmap",
          "mapdata","photobiology","ggmap")
# Install and load all CRAN packages provided from a character vector
load_pkgs = function(pkgs) {
  new_pkgs = pkgs[!(pkgs %in% installed.packages()[ ,'Package'])]
  if (length(new_pkgs) > 0) install.packages(new_pkgs,repos = "http://cran.cnr.berkeley.edu/")
  invisible(lapply(pkgs,function(x)
    suppressPackageStartupMessages(library(x,character.only = T))))
}
# Load packages
load_pkgs(pkgs)

# Set processing controls -------------------------------------------------
get.nav   <- T
get.bathy <- T

# Source survey info ------------------------------------------------------
source(here("Code/settings_1704RL.R"))

# Define ERDDAP data variables -------------------------------------------------
erddap.vars       <- c("time,latitude,longitude,platformSpeed")
erddap.classes    <- c("factor","numeric","numeric","numeric")
erddap.headers    <- c("time","lat","long","SOG")

# Import vessel nav data from ERDDAP -------------------------------------------------------
if (get.nav) {
  # Generate ERDDAP URL
  dataURL = URLencode(paste("http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip",
                            survey.vessel.erddap,".csv0?",erddap.vars,
                            "&time>=",survey.start.erddap,"&time<=",survey.end.erddap,sep = ''))
  
  # Download and parse ERDDAP nav data
  nav <- data.frame(read.csv(dataURL,header = F,colClasses = erddap.classes,row.names = NULL,skip = 0)) 
  colnames(nav) <- erddap.headers  
  
  # Save nav
  save(nav, file = here("Data/nav.Rdata"))
}

# Format nav
nav <- nav %>% 
  mutate(lon = long - 360) %>% # Put longitude into E/W format
  mutate(time = as.POSIXct(time,format = "%FT%T"),
         date = date(time),
         dist = SOG*60/1000) # Distance in km

# Reduce data by day to compute sunrise/sunset times
sun.nav <- nav %>% 
  group_by(date) %>% 
  summarise(lat = mean(lat),
            lon = mean(lon)) %>% 
  as.data.frame()

# Get sunrise/sunset for each survey day
nav.daynight <- data.frame()

for (i in 1:nrow(sun.nav)) {
  tmp <- day_night(date = sun.nav$date[i], geocode = data.frame(lat = sun.nav$lat[i],lon = sun.nav$lon[i]))
  nav.daynight <- bind_rows(nav.daynight,tmp)
}
# Format the results
nav.daynight <- nav.daynight %>% 
  mutate(sunrise = as.POSIXct(paste(day, hms::as.hms(sunrise*3600)),format = "%F %T"),
         sunset = sunrise + daylength*3600,
         sunrise = as.character(sunrise),
         sunset = as.character(sunset)) %>% 
  select(day,sunrise,sunset) %>% 
  gather(period,time,-day) %>% 
  mutate(time = as.POSIXct(time, format = "%F %T")) %>% 
  arrange(time) %>% 
  mutate(id = seq(1,nrow(.)))

# Get bathymetry data across range of nav data (plus/minus one degree lat/long)
if (get.bathy) {
  bathy <- getNOAA.bathy(lon1 = min(nav$lon - 1), lon2 = max(nav$lon + 1),
                         lat1 = max(nav$lat) + 1, lat2 = min(nav$lat) - 1, resolution = 4)
  # Save bathy results
  save(bathy, file = here("Data/bathy.Rdata"))  
} else {
  load(here("Data/bathy.Rdata"))
}

# Get nav depth and compute photoperiod
nav.depth <- get.depth(bathy, nav$lon, nav$lat, locator = F, distance = T) %>% 
  bind_cols(select(nav, time, dist)) %>% 
  mutate(depth_bin = cut(depth,c(min(depth),-250,0), include.lowest = T, labels = F),
         id = cut(time,nav.daynight$time, include.lowest = T, labels = F),
         depth_bin = case_when(
           depth_bin == 1 ~ ">250m",
           depth_bin == 2 ~ "< 250 m")) %>% 
  filter(!is.na(depth_bin)) %>% 
  left_join(select(nav.daynight,id,period)) %>% 
  mutate(day_night = case_when(
    period == "sunrise" ~ "Day",
    period == "sunset" ~ "Night"))

# Define lat and long bounds for west coast map
wc.lat  <- range(nav$lat)  #c(32, 52)
wc.long <- range(nav$lon) #c(-130, -116)

# Set west coast boundaries for stamen maps
wc.bounds.stamen <- c(left = min(wc.long), bottom = min(wc.lat),
                      right = max(wc.long), top = max(wc.lat))
# Download stamen map of west coast; zoom = 6 seems good
wc.map.stamen.toner <- get_stamenmap(wc.bounds.stamen, zoom = 6, maptype = "toner-lite") %>% 
  ggmap() + xlab("Longitude") + ylab("Latitude") + theme_bw()

# Map results by depth bin
bathy.plot <- wc.map.stamen.toner + 
  geom_point(data = nav.depth, aes(lon, lat, colour = depth_bin),size = 0.5) +
  scale_colour_manual(name = "Depth", values = c("#40C270","#1C1C8A")) +
  theme(legend.position = c(0,0),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.key = element_blank())

# Map results by day/night
daynight.plot <- wc.map.stamen.toner + 
  geom_point(data = nav.depth, aes(lon, lat, colour = day_night),size = 0.5) +
  scale_colour_manual(name = "Depth", values = c("yellow", "black")) +
  theme(legend.position = c(0,0),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.key = element_blank())

# Combine plots
bathy.photo.plot <- plot_grid(bathy.plot,daynight.plot,nrow = 1, align = "v")

# Save combo plot
ggsave(bathy.photo.plot, filename = paste(here("Figs"),"/",survey.name,"_nav_depth_day.png", sep = ""),
       height = 8, width = 8)

# Map only daytime nav by depth
day.plot <- wc.map.stamen.toner + 
  geom_point(data = filter(nav.depth, day_night == "Day"), aes(lon, lat), colour = "gray20", size = 0.5) 

# Save daytime only plot
ggsave(day.plot, filename = paste(here("Figs"),"/",survey.name,"_nav_depth.png", sep = ""),
       height = 8, width = 4)

# Summarise distance by day/night and depth
nav.summ <- nav.depth %>% 
  group_by(depth_bin,day_night) %>% 
  summarise(
    dist_km = sum(dist)) %>% 
  mutate(
    dist_nmi = dist_km * 0.539957,
    pings_ek60 = dist_km * 149,
    pings_ek80 = dist_km * 149)

# Write results to file
write_csv(nav.summ, paste(here("Output"),"/",survey.name,"_LineKilometers.csv", sep = ""))