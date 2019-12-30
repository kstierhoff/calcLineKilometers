# Survey info -------------------------------------------------------------
survey.name   <- "1901RL"
survey.vessel <- "RL"

# Set limits for latitude and longitude
survey.lat  <- c(30, 51)
survey.long <- c(-132, -117)

# Is Saildrone survey?
sd.survey <- FALSE

# Define ERDDAP data variables -------------------------------------------------
erddap.survey.start  <- "2019-02-06"  # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2019-02-13"  # End of survey for ERDDAP vessel data query
erddap.vessel        <- "WTEG"        # Lasker == WTEG; Shimada == WTED

# Configure columns and classes
erddap.vars          <- c("time,latitude,longitude,seaTemperature,platformSpeed")
erddap.classes       <- c("factor", "numeric", "numeric", "numeric","numeric")
erddap.headers       <- c("time", "lat", "long", "SST", "SOG")

# Generate ERDDAP URL
# Generate ERDDAP URL
dataURL <- URLencode(paste(
  "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip",
  erddap.vessel, ".csv0?", erddap.vars,
  "&time>=", erddap.survey.start, "&time<=", erddap.survey.end,
  sep = ""))

# Downsample settings -----------------------------------------------------
# Number of n-th samples to keep in the resulting nav data frame
# Particularly important when dealing with large data sets with frequent
# location estimates
nav.ds <- 1

# Map preferences ---------------------------------------------------------
crs.geog    = 4326
crs.proj    = 3310

survey.zoom = 6
map.height  = 8
map.width   = 5

# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")
