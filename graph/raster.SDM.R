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

# Create rasters for SDM
