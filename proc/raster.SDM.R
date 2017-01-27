#############################################################################
###"graph/raster.SDM.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create rasters for SDM
## DEPENDS ON:
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(sp)
library(raster)
library(doMC); doMC::registerDoMC(4)
## USED BY:
# "proc/kelp.models.R"
## CREATES:
# All of the raster files used by "proc/kelp.models.R"
#############################################################################

# 1. Load data ------------------------------------------------------------

# Run this entire script to process the stats rather than saving so many data.frames
# If one seeks different threshold calculations it is best to change the source script
# system.time(source("proc/kelp.exceedence.R")) ## ~xxx seconds

# Load clims
load("data/daily_clim_pixel.Rdata")
load("data/pixel_all.Rdata")

# Load exceedence values
load("data/stats_21.Rdata")
load("data/stats_20.Rdata")
load("data/stats_16.Rdata")
load("data/stats_15.Rdata")

# Load bathy
load("~/SA_map/sa_bathy.RData")


# 2. Prep values for rasterising ------------------------------------------

# Proj4string
stdCRS <- "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=km +no_defs" 

# Dates for converting Julian date to yy/mm/dd
dates <- as.factor(seq(as.Date("2016/01/01"), as.Date("2016/12/31"), by = "day")) # 2016 used as this is the most recent leap year

# Monthly clims
monthly_clim_pixel <- melt(pixel_all, id.vars = c("index", "lon", "lat", "trend", "decade"), value.name = "temp", variable.name = "date")
levels(monthly_clim_pixel$date) <- dates
monthly_clim_pixel$month <- month(monthly_clim_pixel$date, label = T, abbr = T)
monthly_clim_pixel <- ddply(monthly_clim_pixel, .(index, lon, lat, trend, decade, month), summarise, temp = mean(temp, na.rm = T), .parallel = T)

# Seasonal min/ max values
seas_clim_pixel <- monthly_clim_pixel
seas_clim_pixel$seas[seas_clim_pixel$month %in% c("Jan","Feb","Mar")] <- "Summer"
seas_clim_pixel$seas[seas_clim_pixel$month %in% c("Apr","May","Jun")] <- "Autumn"
seas_clim_pixel$seas[seas_clim_pixel$month %in% c("Jul","Aug","Sep")] <- "Winter"
seas_clim_pixel$seas[seas_clim_pixel$month %in% c("Oct","Nov","Dec")] <- "Spring"
seas_clim_pixel <- ddply(seas_clim_pixel, .(index, lon, lat, trend, decade), summarise, .parallel = T,
                             Sumin = min(temp[seas == "Summer"], na.rm = T),
                             Sumax = max(temp[seas == "Summer"], na.rm = T),
                             Amin = min(temp[seas == "Autumn"], na.rm = T),
                             Amax = max(temp[seas == "Autumn"], na.rm = T),
                             Wmin = min(temp[seas == "Winter"], na.rm = T),
                             Wmax = max(temp[seas == "Winter"], na.rm = T),
                             Spmin = min(temp[seas == "Spring"], na.rm = T),
                             Spmax = max(temp[seas == "Spring"], na.rm = T))
## NB: If you have loaded the 'biomod2' package, melt will not function properly
seas_clim_pixel <- melt(seas_clim_pixel, id.vars = c("index", "lon", "lat", "trend", "decade"),
                            variable.name = "seas", value.name = "temp")
seas_clim_pixel <- seas_clim_pixel[order(seas_clim_pixel$index),]
row.names(seas_clim_pixel) <- NULL

# Add additional index column to exceedence stats for file name purposes
stats_21$index2 <- paste("above", stats_21$thresh, sep = " ")
stats_20$index2 <- paste("above", stats_20$thresh, sep = " ")
stats_16$index2 <- paste("below", stats_16$thresh, sep = " ")
stats_15$index2 <- paste("below", stats_15$thresh, sep = " ")

# Combine
stats_all <- rbind(stats_21, stats_20, stats_16, stats_15)
stats_all <- stats_all[,c(1:3,10:11,14,5)]
stats_all$index2 <- as.factor(stats_all$index2)

# 3. Create rasters for SDM ---------------------------------------------

# Function to create and save raster
  # This function is designed to take a X_pixel file from above and create a raster file from it
pixel_df <- filter(seas_clim_pixel, seas == "Sumax", decade == 0, trend == levels(trend)[2]) # Tester...
raster.dat <- function(pixel_df){
  df <- droplevels(pixel_df)
  dat <- droplevels(df[,c(2,3,7)])
  colnames(dat) <- c("X","Y","Z")
  rast <- rasterFromXYZ(dat, res = c(0.1, 0.1), digits = 1)
  writeRaster(rast,
              filename = paste("data/raster/",levels(df$trend)[1],"/",levels(as.factor(df$decade))[1],"/",levels(df[,6])[1],".asc", sep = ""), 
              format = "ascii", overwrite = TRUE)
}

# test <- filter(monthly_clim_pixel, decade == 0, trend == levels(trend)[1])
# system.time(ddply(test, .(trend, decade, month), raster.dat, .parallel = T)) # 6 seconds
# test <- raster::stack(
#   "data/raster/in situ/0/Jan.asc",
#   "data/raster/in situ/0/Feb.asc",
#   "data/raster/in situ/0/Mar.asc",
#   "data/raster/in situ/0/Apr.asc",
#   "data/raster/in situ/0/May.asc",
#   "data/raster/in situ/0/Jun.asc",
#   "data/raster/in situ/0/Jul.asc",
#   "data/raster/in situ/0/Aug.asc",
#   "data/raster/in situ/0/Sep.asc",
#   "data/raster/in situ/0/Oct.asc",
#   "data/raster/in situ/0/Nov.asc",
#   "data/raster/in situ/0/Dec.asc")
# plot(test) # The errors produced by this function appear erroneous and may be an artefact of ddply

## Create rasters and save
# Monthly climatologies
system.time(ddply(monthly_clim_pixel, .(trend, decade, month), raster.dat, .parallel = T)) # 19 seconds

# Seasonal climatologies
system.time(ddply(seas_clim_pixel, .(trend, decade, seas), raster.dat, .parallel = T)) # 44 seconds

# Threshold exceedences
system.time(ddply(stats_all, .(trend, decade, index2), raster.dat, .parallel = T)) # 21 seconds

# Bathymetry
# raster.dat(grid_bathy, "bathy")
