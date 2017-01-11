#############################################################################
###"graph/raster.exceedence.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create rasters
# 4. Create threshold exceedence figures and GIFs
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
library(viridis)
library(animation)
library(colourpicker)
source("func/colour.palettes.R")
## USED BY:
#
## CREATES:
# The naming convention is: 1_2_3_4_5.x
# 1. The data used, 2. The statistic measured, 3. The decadal projection, 4. The thermal threshold, 5. The number of years of projection
  # Note that the GIFs show all decades of projection so there is no 5th designation
#############################################################################

# 1. Load data ------------------------------------------------------------

# Run this entire script to process the stats rather than saving so many data.frames
# If one seeks different threshold calculations it is best to change the source script
system.time(source("proc/kelp.exceedence.R")) ## ~200 seconds

# Load mapping values
source("~/SA_map/scaleBarFunc.R")
source("~/SA_map/theme.R")
load("~/SA_map/sa_shore.Rdata")
load("~/SA_map/sa_provinces_new.RData")
load("~/SA_map/africa_borders.Rdata")

# Load site names
load("~/SACTN/metadata/site_list_v4.1.Rdata")

# Kelp presence
load("data/kelp_presence_hiRes.Rdata")
load("data/kelp_presence_grid.Rdata")
present <- kelp_presence_grid[kelp_presence_grid$Z == 1,]
absent <- kelp_presence_grid[kelp_presence_grid$Z == 0,]


# 2. Prep values for rasterising ------------------------------------------

# Make a grid for the SA region. Use a 0.1° grid along lat and long
sa_grid <- makeGrid(x = seq(14, 34, 0.1), y = seq(-36, -26, 0.1))

# Define the South African Coast
sa_lats <- c(-36, -26); sa_lons <- c(14, 34)

# For use with rasters
mean_no_NA <- function(x){mean(x, na.rm =T)}

# Site names for plotting
site_names <- site_list[c(1,13,29,35,40,47,59,65,71,113,130,132),] # Manually select sites...
site_names$site <- as.character(site_names$site)
# Split into three data frames for better plotting
site_names1 <- site_names[1:3,] # West Coast
site_names2 <- site_names[4:7,] # South Coast
site_names2[1,2] <- "Cape\nAgulhas"
site_names2[4,2] <- "Port\nElizabeth" # South Coast
site_names3 <- site_names[8:12,] # South Coast

# 3. Create rasters -------------------------------------------------------

# Gridding function
# df <- stats_inSitu_20 # Tester...
# Use numerical values to select the stat you want to project
# 6 = "duration", 7 = "int_max", 8 = "int_cum", 9 = "int_max_abs", 10 = "int_cum_abs" 
grid.dat <- function(df){
  dat <- df
  # dat <- dat[dat$stat == "mean",]
  # Create event data
  dat$EID <- 1:length(dat$lon)
  # colnames(dat) <- c("X", "Y", "exceede", "Z", "EID")
  colnames(dat)[c(2:3,5)] <- c("X", "Y","Z")
  dat <- as.EventData(dat)
  # Grid it
  datGrid <- findCells(dat, sa_grid)
  dat2 <- merge(dat, datGrid, by = c("EID"), all = FALSE)
  datData <- combineEvents(dat, datGrid, FUN = mean_no_NA)
  datDataGrid <- merge(datData, sa_grid, by = c("PID", "SID"), all = FALSE)
  datDataGrid <- merge(datDataGrid, dat2[,c(1:2,5,7:16)], by = c("PID", "SID"), all = FALSE)
  datDataGrid <- datDataGrid[order(datDataGrid[,2], datDataGrid[,1], datDataGrid[,4]), ] # These MUST be ordered this way to plot correctly...
  colnames(datDataGrid)[3] <- "duration"
  datDataGrid <- datDataGrid[,c(8,7,1,2,4:6,9,3,10:17)]
  # Breaks for plotting duration
  # CUrrently run in the figure function
  # datDataGrid$bins <- cut(datDataGrid$Z, breaks = breaks)
  return(datDataGrid)
}
# test <- grid.dat(stats_20_inSitu_0)

## Grid data ABOVE a threshold
# In situ
grid_inSitu_22 <- ddply(stats_inSitu_22, .(decade), grid.dat)
grid_inSitu_21 <- ddply(stats_inSitu_21, .(decade), grid.dat)
grid_inSitu_20 <- ddply(stats_inSitu_20, .(decade), grid.dat)

# Static 0.1
grid_0.1_22 <- ddply(stats_0.1_22, .(decade), grid.dat)
grid_0.1_21 <- ddply(stats_0.1_21, .(decade), grid.dat)
grid_0.1_20 <- ddply(stats_0.1_20, .(decade), grid.dat)

## Grid data BELOW a threshold
# In situ
grid_inSitu_16 <- ddply(stats_inSitu_16, .(decade), grid.dat)
grid_inSitu_15 <- ddply(stats_inSitu_15, .(decade), grid.dat)
grid_inSitu_14 <- ddply(stats_inSitu_14, .(decade), grid.dat)

# Static 0.1
grid_0.1_16 <- ddply(stats_0.1_16, .(decade), grid.dat)
grid_0.1_15 <- ddply(stats_0.1_15, .(decade), grid.dat)
grid_0.1_14 <- ddply(stats_0.1_14, .(decade), grid.dat)

# Check results
test <- filter(grid_inSitu_20, index == 128, POS == 1)

# 4. Create threshold exceedence figures and GIFs -------------------------

### Combine grids for threshold animations
## In situ
# ABOVE threshold
grid_inSitu_a <- rbind(filter(grid_inSitu_22, decade == 0), filter(grid_inSitu_21, decade == 0), filter(grid_inSitu_20, decade == 0))
# BELOW threshold
grid_inSitu_b <- rbind(filter(grid_inSitu_16, decade == 0), filter(grid_inSitu_15, decade == 0), filter(grid_inSitu_14, decade == 0))

## Static 0.1C
# ABOVE threshold
grid_0.1_a <- rbind(filter(grid_0.1_22, decade == 0), filter(grid_0.1_21, decade == 0), filter(grid_0.1_20, decade == 0))
# BELOW threshold
grid_0.1_b <- rbind(filter(grid_0.1_16, decade == 0), filter(grid_0.1_15, decade == 0), filter(grid_0.1_14, decade == 0))

## Testing ##
# df = grid_0.1_a
# thresh = 21
## END ##
draw.thresh.fig <- function(df, thresh){
  if(df$below[1] == TRUE) side = "below"
  if(df$below[1] == "FALSE") side = "above"
  df2 <- df[,c(1:9,14:17)] # Use this data frame for max value for breaks
  excDataGrid <- df2[df2$thresh == thresh,]
  colnames(excDataGrid)[9] <- "Z"
  # Breaks for plotting
  breaks <- c(0, round(seq(1, 365, length.out = 6)), 366)
  excDataGrid$bins <- cut(excDataGrid$Z, breaks = breaks)
  levels(excDataGrid$bins)[c(1,7)] <- c("(0]", "(366]")
  excDataGrid$bins[excDataGrid$Z == 0] <- levels(excDataGrid$bins)[1]
  excDataGrid$bins[excDataGrid$Z == 366] <- levels(excDataGrid$bins)[7]
  # Subsetted data frames for ease of plotting
  excDataGrid_complete <- excDataGrid[complete.cases(excDataGrid$bins),]
  # excDataGrid_complete$bins <- ordered(excDataGrid_complete$bins)
  breaks2 <- levels(excDataGrid$bins)
  # Crerate the raster figure
  sa_plot_exc <- ggplot() + bw_update +
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
    geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
    geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
    geom_text(data = site_names1, aes(lon, lat, label = site), hjust = 1.15, vjust = 0.5, size = 3, colour = "white") +
    geom_text(data = site_names2, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.8, size = 3, colour = "white", 
              angle = 330, lineheight = 0.8) +
    geom_text(data = site_names3, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.5, size = 3, colour = "white") +
    # De Hoop!
    geom_point(aes(x = 20.4055089, y = -34.4730239), fill = "black", size = 5, shape = 23, alpha = 0.5) +
    geom_text(aes(x = 20.4055089, y = -34.4730239, label = "de Hoop"), hjust = 0.4, vjust = -0.8, size = 4, 
              colour = "black", angle = 45) +
    # Thresholds
    geom_polygon(data = excDataGrid_complete, aes(x = X, y = Y, group = paste(excDataGrid_complete$PID, excDataGrid_complete$SID,
                                                                              sep = "_"), fill = bins), colour = NA, size = 0.1) +
    # Kelp presence absence boxes
    geom_polygon(data = present, aes(x = X, y = Y, group = paste(present$SID, present$PID, sep = "_")), 
                 show.legend = FALSE, fill = NA, colour = "white", size = 0.1) +
    geom_polygon(data = absent, aes(x = X, y = Y, group = paste(absent$SID, absent$PID, sep = "_")), 
                 show.legend = FALSE, fill = NA, colour = "black", size = 0.3) +
    # Trend label
    # geom_label(aes(x = 24.25, y = -29, label = thresh)) +
    # Colour scale
    scale_fill_viridis(discrete = TRUE, drop = FALSE) +#, labels = breaks2, breaks = breaks2) +
    guides(fill = guide_legend(paste("Days\n",side," ",thresh,"°C", sep = ""))) +
    ggtitle("Exceedence") +
    # ggtitle("Exceedence") + # Title for static images
    scale_y_continuous(breaks = seq(-34,-28,2)) +
    scale_x_continuous(breaks = seq(18,30,4)) +
    scaleBar(lon = 30, lat = -34.2, distanceLon = 100, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
             arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
    coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "grey30"),
          legend.justification = c(1,0), legend.position = c(0.55, 0.40),
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.direction = "vertical",
          legend.box = "horizontal")
  print(sa_plot_exc)
}

## Exceedence ABOVE thresholds
# In situ and 0.1C figures are the same for this information
draw.thresh.fig(grid_inSitu_a, 22)
ggsave("graph/exc_dur_inSitu_22.png", height = 6, width = 10)
draw.thresh.fig(grid_inSitu_a, 21)
ggsave("graph/exc_dur_inSitu_21.png", height = 6, width = 10)
draw.thresh.fig(grid_inSitu_a, 20)
ggsave("graph/exc_dur_inSitu_20.png", height = 6, width = 10)

## Exceedence BELOW thresholds
draw.thresh.fig(grid_inSitu_b, 16)
ggsave("graph/exc_dur_inSitu_16.png", height = 6, width = 10)
draw.thresh.fig(grid_inSitu_b, 15)
ggsave("graph/exc_dur_inSitu_15.png", height = 6, width = 10)
draw.thresh.fig(grid_inSitu_b, 14)
ggsave("graph/exc_dur_inSitu_14.png", height = 6, width = 10)

# It doesn't appear possible to set an output directory within saveGIF
# So I am setting it here
setwd("~/range_kelp/graph/")

## Exceedence ABOVE thresholds
animate.thresh.fig <- function() {
  lapply(seq(20,22), function(i) {
    draw.thresh.fig(grid_inSitu_a, i)
  })
}
system.time(saveGIF(animate.thresh.fig(), interval = 2, ani.width = 800, movie.name = "thresh_dur_inSitu_a.gif")) ## ~4 seconds

## Exceedence BELOW thresholds
animate.thresh.fig <- function() {
  lapply(rev(seq(14,16)), function(i) {
    draw.thresh.fig(grid_inSitu_b, i)
  })
}
system.time(saveGIF(animate.thresh.fig(), interval = 2, ani.width = 800, movie.name = "thresh_dur_inSitu_b.gif")) ## ~4 seconds

setwd("~/range_kelp/")

