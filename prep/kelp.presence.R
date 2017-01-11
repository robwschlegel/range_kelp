#############################################################################
###"prep/kelp.presence.R"
## This script does:
# 1. Load data
# 2. Calculate locations of kelp presence
# 3. Interpolate at HiRes
# 4. Create a girdded product
## DEPENDS ON:
library(sp)
library(raster)
library(rgdal)
library(maptools)
library(geosphere)
library(PBSmapping)
library(mapproj)
library(gridExtra)
library(plyr)
library(dplyr)
library(akima)
library(doMC); doMC::registerDoMC(4)
## USED BY:
#
## CREATES:
#
#############################################################################

# 1. Load data ------------------------------------------------------------

# Presence/ absence data
kelp_presence <- read.csv("~/Private/range_kelp/data/seaweed.csv")
kelp_coords <- read.csv("~/Private/range_kelp/meta_data/58sites.csv")
  # Currently not using presence/ absence data

# Site list
load("~/SACTN/metadata/site_list_v4.1.Rdata")

# Hi-res coordinates
outCoords <- read.table("~/tempssa_v3.0/coords_to_extract/interp_HiRes_coords_348_sites.txt", header = FALSE)
colnames(outCoords) <- c("lon", "lat")


# 2. Calculate locations of kelp presence ---------------------------------

# Manual for now
site_list$kelp <- c(rep(1,12), 0, rep(1, 16), 0, rep(1, 6), rep(0, 98))


# 3. Interpolate at HiRes -------------------------------------------------

# First one
kelp_presence_hiRes <- as.data.frame(interpp(x = site_list[, "lon"], y = site_list[, "lat"], site_list[, "kelp"],
                             xo = outCoords$lon, yo = outCoords$lat, linear = TRUE, 
                             extrap = FALSE, dupl = "mean"))

# Smooth and fill gaps
colnames(kelp_presence_hiRes) <- c("lon", "lat", "presence")
kelp_presence_hiRes$presence <- round(kelp_presence_hiRes$presence)
kelp_presence_hiRes$presence[c(1,62:64,87:89,117)] <- 1
kelp_presence_hiRes$presence[c(65,129:137,278,329:340)] <- 0
kelp_presence_hiRes <- kelp_presence_hiRes[-348,]

# Save it
save(kelp_presence_hiRes, file = "data/kelp_presence_hiRes.Rdata")


# 4. Create a girdded product ---------------------------------------------

# Make a grid for the SA region. Use a 0.1° grid along lat and long
sa_grid <- makeGrid(x = seq(14, 34, 0.1), y = seq(-36, -26, 0.1))

# For use with rasters
mean_no_NA <- function(x){mean(x, na.rm =T)}

# coerce data frame for gridding
kelp <- kelp_presence_hiRes
kelp$EID <- 1:length(kelp$lon)
colnames(kelp)[c(1:3)] <- c("X", "Y","Z")
kelp <- as.EventData(kelp)

# Grid it
kelpGrid <- findCells(kelp, sa_grid)
kelpData <- combineEvents(kelp, kelpGrid, FUN = mean_no_NA)
kelpDataGrid <- merge(kelpData, sa_grid, by = c("PID", "SID"), all = FALSE)
kelp_presence_grid <- kelpDataGrid[order(kelpDataGrid[,2], kelpDataGrid[,1], kelpDataGrid[,4]), ] # These MUST be ordered this way to plot correctly...
kelp_presence_grid$Z <- round(kelp_presence_grid$Z)

# Save it
save(kelp_presence_grid, file = "data/kelp_presence_grid.Rdata")

