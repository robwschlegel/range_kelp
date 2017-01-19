#############################################################################
###"graph/raster.SDM.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create gridded data
# 4. Create rasters for SDM
## DEPENDS ON:
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(sp)
library(raster)
library(rgdal)
library(maptools)
library(geosphere)
library(PBSmapping)
library(mapproj)
library(gridExtra)
library(ggplot2)
library(animation)
library(doMC); doMC::registerDoMC(4)
source("func/colour.palettes.R")
## USED BY:
#
## CREATES:
# "graph/sa_plot_HiRes_day1.png"
# "daily_HiRes_anim.gif"
#############################################################################

# 1. Load data ------------------------------------------------------------

# Run this entire script to process the stats rather than saving so many data.frames
# If one seeks different threshold calculations it is best to change the source script
# system.time(source("proc/kelp.exceedence.R")) ## ~xxx seconds

# Load clims
load("data/daily_clim_hiRes.Rdata")
load("data/seas_clim_hiRes.Rdata")

# Load mapping values
source("~/SA_map/scaleBarFunc.R")
source("~/SA_map/theme.R")
load("~/SA_map/sa_shore.Rdata")
load("~/SA_map/sa_bathy.RData")


# 2. Prep values for rasterising ------------------------------------------

# Make a grid for the SA region. Use a 0.1° grid along lat and long
sa_grid <- makeGrid(x = seq(14, 34, 0.1), y = seq(-36, -26, 0.1))

# Define the South African Coast
sa_lats <- c(-36, -26); sa_lons <- c(14, 34)

# Proj4string
stdCRS <- "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=km +no_defs" 

# For use with rasters
mean_no_NA <- function(x){mean(x, na.rm =T)}

# Breaks for plotting
breaks <- seq(10,28,2)

# Dates for converting Julian date to yy/mm/dd
dates <- as.factor(seq(as.Date("2016/01/01"), as.Date("2016/12/31"), by = "day")) # 2016 used as this is the most recent leap year

# Monthly clims
monthly_clim_hiRes <- melt(daily_clim_hiRes, id.vars = c("index", "lon", "lat"), value.name = "temp", variable.name = "date")
levels(monthly_clim_hiRes$date) <- dates
monthly_clim_hiRes$month <- month(monthly_clim_hiRes$date, abbr = T)
monthly_clim_hiRes <- ddply(monthly_clim_hiRes, .(index, lon, lat, month), summarise, temp = mean(temp, na.rm = T), .parallel = T)

# Seasonal min/ max values
seas_clim_hiRes <- monthly_clim_hiRes
seas_clim_hiRes$seas[seas_clim_hiRes$month %in% c(1,2,3)] <- "Summer"
seas_clim_hiRes$seas[seas_clim_hiRes$month %in% c(4,5,6)] <- "Autumn"
seas_clim_hiRes$seas[seas_clim_hiRes$month %in% c(7,8,9)] <- "Winter"
seas_clim_hiRes$seas[seas_clim_hiRes$month %in% c(10,11,12)] <- "Spring"
seas_clim_hiRes <- ddply(seas_clim_hiRes, .(index, lon, lat), summarise, .parallel = T,
                             Sumin = min(temp[seas == "Summer"], na.rm = T),
                             Sumax = max(temp[seas == "Summer"], na.rm = T),
                             Amin = min(temp[seas == "Autumn"], na.rm = T),
                             Amax = max(temp[seas == "Autumn"], na.rm = T),
                             Wmin = min(temp[seas == "Winter"], na.rm = T),
                             Wmax = max(temp[seas == "Winter"], na.rm = T),
                             Spmin = min(temp[seas == "Spring"], na.rm = T),
                             Spmax = max(temp[seas == "Spring"], na.rm = T))
## NB: If you have loaded the 'biomod2' package, melt will not function properly
seas_clim_hiRes <- melt(seas_clim_hiRes, id.vars = c("index", "lon", "lat"),
                            variable.name = "seas", value.name = "temp")
seas_clim_hiRes <- seas_clim_hiRes[order(seas_clim_hiRes$index),]
row.names(seas_clim_hiRes) <- NULL


# 3. Create gridded data --------------------------------------------------

## Gridding function
# df <- monthly_clim_hiRes[monthly_clim_hiRes$month == 1, c(2,3,5)] # Tester...
# This function is designed to be fed x, y, z data
  # So the user must be cautious that only one z value is given
grid.dat <- function(df){
  dat <- df
  colnames(dat) <- c("X", "Y", "Z")
  # Create event data
  dat$EID <- 1:nrow(dat)
  dat <- as.EventData(dat)
  # Grid it
  datGrid <- findCells(dat, sa_grid)
  dat2 <- merge(dat, datGrid, by = c("EID"), all = FALSE)
  datData <- combineEvents(dat, datGrid, FUN = mean_no_NA)
  datDataGrid <- merge(datData, sa_grid, by = c("PID", "SID"), all = FALSE)
  datDataGrid <- datDataGrid[order(datDataGrid[,2], datDataGrid[,1], datDataGrid[,4]), ] # These MUST be ordered this way to plot correctly...
  return(datDataGrid)
}
# test <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 2, c(2,3,5)] )

## Create gridded data frames
# Monthly climatologies
grid_Jan <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 1, c(2,3,5)])
grid_Feb <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 2, c(2,3,5)])
grid_Mar <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 3, c(2,3,5)])
grid_Apr <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 4, c(2,3,5)])
grid_May <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 5, c(2,3,5)])
grid_Jun <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 6, c(2,3,5)])
grid_Jul <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 7, c(2,3,5)])
grid_Aug <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 8, c(2,3,5)])
grid_Sep <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 9, c(2,3,5)])
grid_Oct <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 10, c(2,3,5)])
grid_Nov <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 11, c(2,3,5)])
grid_Dec <- grid.dat(monthly_clim_hiRes[monthly_clim_hiRes$month == 12, c(2,3,5)])

# Seasonal climatologies
grid_Sumax <- grid.dat(seas_clim_hiRes[seas_clim_hiRes$seas == "Sumax", c(2,3,5)])
grid_Sumin <- grid.dat(seas_clim_hiRes[seas_clim_hiRes$seas == "Sumin", c(2,3,5)])
grid_Amax <- grid.dat(seas_clim_hiRes[seas_clim_hiRes$seas == "Amax", c(2,3,5)])
grid_Amin <- grid.dat(seas_clim_hiRes[seas_clim_hiRes$seas == "Amin", c(2,3,5)])
grid_Wmax <- grid.dat(seas_clim_hiRes[seas_clim_hiRes$seas == "Wmax", c(2,3,5)])
grid_Wmin <- grid.dat(seas_clim_hiRes[seas_clim_hiRes$seas == "Wmin", c(2,3,5)])
grid_Spmax <- grid.dat(seas_clim_hiRes[seas_clim_hiRes$seas == "Spmax", c(2,3,5)])
grid_Spmin <- grid.dat(seas_clim_hiRes[seas_clim_hiRes$seas == "Spmin", c(2,3,5)])

# Bathymetry
grid_bathy <- grid.dat(sa_bathy) # This takes a moment...
grid_bathy$index <- paste(grid_bathy$PID, grid_bathy$SID, sep = "-")
grid_Sumax$index <- paste(grid_Sumax$PID, grid_Sumax$SID, sep = "-")
grid_bathy <- grid_bathy[grid_bathy$index %in% grid_Sumax$index,]
  # Consider constraaining all other pixels to these as it is certain that these are not over land
  # The issue with these values is that they are meant to be taken as nearshore, but many are deeper than 60 m
  # So for the consideration of coastal SDM these values are counter-productive

# 4. Create rasters for SDM ---------------------------------------------

# Function to create and save raster
  # This function is designed to take a grid_X file from above and create a raster file from it
# grid_df <- grid_Sumax # Tester...
raster.dat <- function(grid_df, name){
  dat <- grid_df[,c(5,6,3)]
  rast <- rasterFromXYZ(dat)
  writeRaster(rast, filename = paste("data/raster/",name,".asc", sep = ""), format = "ascii")
}

## Create rasters and save
# Monthly climatologies
raster.dat(grid_Jan, "Jan")
raster.dat(grid_Feb, "Feb")
raster.dat(grid_Mar, "Mar")
raster.dat(grid_Apr, "Apr")
raster.dat(grid_May, "May")
raster.dat(grid_Jun, "Jun")
raster.dat(grid_Jul, "Jul")
raster.dat(grid_Aug, "Aug")
raster.dat(grid_Sep, "Sep")
raster.dat(grid_Oct, "Oct")
raster.dat(grid_Nov, "Nov")
raster.dat(grid_Dec, "Dec")

# Seasonal climatologies
raster.dat(grid_Sumax, "Sumax")
raster.dat(grid_Sumin, "Sumin")
raster.dat(grid_Amax, "Amax")
raster.dat(grid_Amin, "Amin")
raster.dat(grid_Wmax, "Wmax")
raster.dat(grid_Wmin, "Wmin")
raster.dat(grid_Spmax, "Spmax")
raster.dat(grid_Spmin, "Spmin")

# Bathymetry
raster.dat(grid_bathy, "bathy")

