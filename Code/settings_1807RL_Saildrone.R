# Survey info -------------------------------------------------------------
survey.name <- "1807RL_Saildrone"
sd.survey   <- TRUE

# Define ERDDAP data variables -------------------------------------------------
survey.start.erddap  <- "2018-06-24T00%3A00%3A00Z"  # Start of survey for ERDDAP vessel data query
survey.end.erddap    <- "2018-10-15T19%3A59%3A00Z"  # End of survey for ERDDAP vessel data query

# Configure columns and classes
erddap.headers    <- c("saildrone", "lat", "long", "COG","SOG","time")
erddap.classes <- c(rep("numeric", length(erddap.headers) - 1),"factor")

# Generate ERDDAP URL
dataURL <- URLencode(paste0("https://ferret.pmel.noaa.gov/pmel/erddap/tabledap/saildrone_west_coast_survey_2018.csv0?trajectory%2Clatitude%2Clongitude%2CCOG%2CSOG%2Ctime&time%3E=",
                                 survey.start.erddap,
                                 "&time%3C=",
                                 survey.end.erddap))

# # Download and parse ERDDAP nav data
# nav.sd <- data.frame(read.csv(saildroneURL, header = F, 
#                               colClasses = erddap.classes.sd, 
#                               row.names = NULL, skip = 0))


# Downsample settings -----------------------------------------------------
# Number of n-th samples to keep in the resulting nav data frame
# Particularly important when dealing with large data sets with frequent
# location estimates
nav.ds <- 60

# Map preferences ---------------------------------------------------------
survey.zoom = 6
map.height  = 8
map.width   = 5
