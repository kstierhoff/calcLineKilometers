# Survey info -------------------------------------------------------------
survey.name <- "1805RL"
survey.vessel <- "Lasker"
sd.survey   <- FALSE

# Define ERDDAP data variables -------------------------------------------------
erddap.survey.start  <- "2019-05-04"  # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2019-06-08"  # End of survey for ERDDAP vessel data query

# Configure columns and classes
erddap.vessel        <- "WTEG"    # Lasker == WTEG; Shimada == WTED; add "nrt" if during survey
erddap.vars          <- c("time,latitude,longitude,seaTemperature,platformSpeed")
erddap.classes       <- c("factor", "numeric", "numeric", "numeric","numeric")
erddap.headers       <- c("time", "lat", "long", "SST", "SOG")
survey.lat           <- c(32,51)
survey.long          <- c(-130,-117)

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
survey.zoom = 6
map.height  = 8
map.width   = 5
