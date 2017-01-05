#############################################################################
###"graph/raster.SDM.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create rasters
# 4. Create figures
# 5. Create animations
## DEPENDS ON:
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
system.time(source("proc/kelp.exceedence.R")) ## ~115 seconds


# Load clims
load("data/daily_clim_hiRes.Rdata")
load("data/SACTNseas_clim_v4.1.Rdata")

# Load mapping values
source("~/SA_map/scaleBarFunc.R")
source("~/SA_map/theme.R")
load("~/SA_map/sa_shore.Rdata")


# 2. Prep values for rasterising ------------------------------------------

# Make a grid for the SA region. Use a 0.1° grid along lat and long
sa_grid <- makeGrid(x = seq(14, 34, 0.1), y = seq(-35.5, -26, 0.1))

## Define the South African Coast
sa_lats <- c(-35.5, -26); sa_lons <- c(14, 34)

# For use with rasters
mean_no_NA <- function(x){mean(x, na.rm =T)}

# Breaks for plotting
breaks <- seq(10,28,2)

# 3. Create rasters -------------------------------------------------------

## For initial testing purposes ##
# Create event data
day1 <- daily_clim_hiRes[complete.cases(daily_clim_hiRes),c(1:3)]
day1$EID <- 1:length(day1$lon)
colnames(day1) <- c("X", "Y", "Z", "EID")
day1 <- as.EventData(day1)
# Grid it
day1Grid <- findCells(day1, sa_grid)
day1Data <- combineEvents(day1, day1Grid, FUN = mean_no_NA)
day1DataGrid <- merge(day1Data, sa_grid, by = c("PID", "SID"), all = FALSE)
day1DataGrid <- day1DataGrid[order(day1DataGrid[,2], day1DataGrid[,1], day1DataGrid[,4]), ]
## END ##

# Create rasters for SDM
test <- day1DataGrid[,c(6,5,3)]
colnames(test) <- c("x", "y", "z")
test <- raster(as.matrix(day1DataGrid))
plot(test)

test <- raster()
test
