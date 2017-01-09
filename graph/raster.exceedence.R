#############################################################################
###"graph/raster.exceedence.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create rasters
# 4. Create exceedence figures
# 5. Create all of the gifs
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
# library(gganimate)
# library(grid)
# library(gtable)
library(viridis)
library(animation)
source("func/colour.palettes.R")
## USED BY:
#
## CREATES:
# The naming convention is: 1_2_3_4_5.x
# 1. The data used, 2. The statistic measured, 3. The decadal projection, 4. The thermal threshold, 5. The number of years of projection
  # Note that the GIFs show all decades of projection so there is no 5th designation
# "graph/exc_dur_inSitu_20_0.png"

#############################################################################

# 1. Load data ------------------------------------------------------------

# Run this entire script to process the stats rather than saving so many data.frames
# If one seeks different threshold calculations it is best to change the source script
system.time(source("proc/kelp.exceedence.R")) ## ~230 seconds

# Load mapping values
source("~/SA_map/scaleBarFunc.R")
source("~/SA_map/theme.R")
load("~/SA_map/sa_shore.Rdata")
load("~/SA_map/sa_provinces_new.RData")
load("~/SA_map/africa_borders.Rdata")

# Load site names
load("~/SACTN/metadata/site_list_v4.1.Rdata")


# 2. Prep values for rasterising ------------------------------------------

# Make a grid for the SA region. Use a 0.1° grid along lat and long
sa_grid <- makeGrid(x = seq(14, 34, 0.1), y = seq(-36, -26, 0.1))

## Define the South African Coast
sa_lats <- c(-36, -26); sa_lons <- c(14, 34)

# For use with rasters
mean_no_NA <- function(x){mean(x, na.rm =T)}


# 3. Create rasters -------------------------------------------------------

# Gridding function
# df <- stats_20_0.1_0 # Tester...
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
  datDataGrid <- merge(datData, sa_grid, by = c("PID", "SID"), all = TRUE)
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
grid_inSitu_20 <- ddply(stats_inSitu_20, .(decade), grid.dat)

# Static 0.1
grid_0.1_20 <- ddply(stats_0.1_20, .(decade), grid.dat)

## Grid data BELOW a threshold
# In situ
grid_inSitu_19 <- ddply(stats_inSitu_19, .(decade), grid.dat)
grid_inSitu_18 <- ddply(stats_inSitu_18, .(decade), grid.dat)
grid_inSitu_17 <- ddply(stats_inSitu_17, .(decade), grid.dat)
grid_inSitu_16 <- ddply(stats_inSitu_16, .(decade), grid.dat)
grid_inSitu_15 <- ddply(stats_inSitu_15, .(decade), grid.dat)
grid_inSitu_14 <- ddply(stats_inSitu_14, .(decade), grid.dat)

# Static 0.1
grid_0.1_19 <- ddply(stats_0.1_19, .(decade), grid.dat)
grid_0.1_18 <- ddply(stats_0.1_18, .(decade), grid.dat)
grid_0.1_17 <- ddply(stats_0.1_17, .(decade), grid.dat)
grid_0.1_16 <- ddply(stats_0.1_16, .(decade), grid.dat)
grid_0.1_15 <- ddply(stats_0.1_15, .(decade), grid.dat)
grid_0.1_14 <- ddply(stats_0.1_14, .(decade), grid.dat)


# 4. Create exceedence figures -------------------------------------------------------

# Site names for plotting
site_names <- site_list[c(1,13,29,35,40,47,59,65,71,113,130,132),] # Manually select sites...
site_names$site <- as.character(site_names$site)
# Split into three data frames for better plotting
site_names1 <- site_names[1:3,] # West Coast
site_names2 <- site_names[4:7,] # South Coast
site_names2[1,2] <- "Cape\nAgulhas"
site_names2[4,2] <- "Port\nElizabeth" # South Coast
site_names3 <- site_names[8:12,] # South Coast

## Testing ##
# df = grid_inSitu_20
# decade = 2
# stat = 10
## END ##
# Use numerical values to select the stat you want to project
# 9 = "duration", 10 = "int_max", 11 = "int_cum", 12 = "int_max_abs", 13 = "int_cum_abs" 
draw.exc.fig <- function(df, decade, stat){
  if(stat == 9){
    colnames(df)[9] <- "Duration (days)"
  }
  if(df$below[1] == TRUE) side = "below"
  if(df$below[1] == "FALSE") side = "above"
  df2 <- df[,c(1:8,stat,14:17)] # Use this data frame for max value for breaks
  colnames(df2)[9] <- "Z"
  excDataGrid <- df2[df$decade == decade,]
  # Breaks for plotting
  if(stat == 9){
    breaks <- c(0, round(seq(1, 365, length.out = 6)), 366)
    excDataGrid$bins <- cut(excDataGrid$Z, breaks = breaks)
    levels(excDataGrid$bins)[c(1,7)] <- c("(0]", "(366]")
    excDataGrid$bins[excDataGrid$Z == 0] <- levels(excDataGrid$bins)[1]
    excDataGrid$bins[excDataGrid$Z == 366] <- levels(excDataGrid$bins)[7]
  } else {
    breaks <- c(0, round_any(seq(min(df2$Z, na.rm = T), max(df2$Z, na.rm = T), length.out = 6), 0.1), max(df2$Z, na.rm = T)+0.1)
    if(breaks[1] == breaks[2]){
      breaks[2] <- breaks[2]+0.1
    }
    excDataGrid$bins <- cut(excDataGrid$Z, breaks = breaks)
    levels(excDataGrid$bins)[c(1)] <- "(0]"
    levels(excDataGrid$bins)[c(7)] <- paste("(>",round_any(max(df2$Z, na.rm = T), 0.1),"]", sep = "")
    excDataGrid$bins[excDataGrid$exceede == "below"] <- levels(excDataGrid$bins)[1]
    excDataGrid$bins[excDataGrid$exceede == "above"] <- levels(excDataGrid$bins)[7]
  }
  # Subsetted data frames for ease of plotting
  excDataGrid_complete <- excDataGrid[complete.cases(excDataGrid$bins),]
  # Create title data
  if(df2$trend[1] == "in situ"){
    subtitle <- paste("Projections based on decadal in situ trends", sep = "")
  } else if(df2$trend[1] == "0.1"){
    subtitle <- paste(paste("Projections based on 0.1°C/dec trends", sep = ""))
  }
  p_point <- decade+21.5
  # Crerate the raster figure
  sa_plot_exc <- ggplot() + bw_update +
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
    geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
    geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
    geom_text(data = site_names1, aes(lon, lat, label = site), hjust = 1.15, vjust = 0.5, size = 3, colour = "white") +
    geom_text(data = site_names2, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.8, size = 3, colour = "white", 
              angle = 330, lineheight = 0.8) +
    geom_text(data = site_names3, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.5, size = 3, colour = "white") +
    geom_polygon(data = excDataGrid_complete, aes(x = X, y = Y, group = paste(excDataGrid_complete$PID, excDataGrid_complete$SID,
                                                                            sep = "_"), fill = bins), colour = NA, size = 0.1) +
    # Manually create progress bar #
    geom_rect(aes(xmin = 21.0, xmax = 27.0, ymin = -26.02, ymax = -27.02), fill = "white", colour = "black") +
    geom_segment(aes(x = p_point, xend = p_point, y = -26.52, yend = -26.77)) +
    geom_point(aes(x = p_point, y = -26.42), colour = "red", size = 5, alpha = 0.7) +
    geom_text(aes(label = decade, x = p_point, y = -26.42), size = 4) +
    geom_line(aes(x = 21.5:26.5, y = -26.62)) +
    # END progress bar  #
    scale_fill_viridis(discrete = TRUE, drop = FALSE) +
    # scale_colour_discrete(breaks = rev(levels(excDataGrid_NA$exceede))) +
    guides(fill = guide_legend(paste(colnames(df[stat]),"\n",side," ",df2$thresh[1],"°C", sep = ""), order = 2)) +#,
           # colour = guide_legend("Relation to threshold", override.aes = list(size = 4), order = 1)) +
    ggtitle("Exceedence", subtitle = subtitle) +
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
# In situ figures
draw.exc.fig(grid_inSitu_20, 0, 9)
ggsave("graph/exc_dur_inSitu_20_0.png",  height = 6, width = 10)

# Static 0.1C figures
draw.exc.fig(grid_0.1_20, 0, 9)
ggsave("graph/exc_dur_0.1_20_0.png",  height = 6, width = 10)

## Exceedence BELOW thresholds
# Static 0.1C figures
draw.exc.fig(grid_0.1_19, 0, 9)
ggsave("graph/exc_dur_0.1_19_0.png",  height = 6, width = 10)
draw.exc.fig(grid_0.1_18, 0, 9)
ggsave("graph/exc_dur_0.1_18_0.png",  height = 6, width = 10)
draw.exc.fig(grid_0.1_17, 0, 9)
ggsave("graph/exc_dur_0.1_17_0.png",  height = 6, width = 10)
draw.exc.fig(grid_0.1_16, 0, 9)
ggsave("graph/exc_dur_0.1_16_0.png",  height = 6, width = 10)
draw.exc.fig(grid_0.1_15, 0, 9)
ggsave("graph/exc_dur_0.1_15_0.png",  height = 6, width = 10)
draw.exc.fig(grid_0.1_14, 0, 9)
ggsave("graph/exc_dur_0.1_14_0.png",  height = 6, width = 10)


# 5. Create all of the gifs -----------------------------------------------

# It doesn't appear possible to set an output directory within saveGIF
# So I am setting it here
setwd("~/range_kelp/graph/")

## Exceedence ABOVE thresholds
## inSitu
# Duration
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_20, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_inSitu_20.gif")) ## ~9 seconds
# Int_max
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_20, i, 10)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_intMax_inSitu_20.gif"))
# Int_cum
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_20, i, 11)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_intCum_inSitu_20.gif"))

## Static
# Duration
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_20, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_0.1_20.gif"))
# Int_max
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_20, i, 10)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_intMax_0.1_20.gif"))
# Int_cum
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_20, i, 11)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_intCum_0.1_20.gif"))


### BELOW thresholds
## In situ
# Duration 19
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_19, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_inSitu_19.gif"))
# Duration 18
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_18, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_inSitu_18.gif"))
# Duration 17
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_17, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_inSitu_17.gif"))
# Duration 16
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_16, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_inSitu_16.gif"))
# Duration 15
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_15, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_inSitu_15.gif"))
# Duration 14
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_inSitu_14, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_inSitu_14.gif"))

## In situ
# Duration 19
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_19, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_0.1_19.gif"))
# Duration 18
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_18, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_0.1_18.gif"))
# Duration 17
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_17, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_0.1_17.gif"))
# Duration 16
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_16, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_0.1_16.gif"))
# Duration 15
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_15, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_0.1_15.gif"))
# Duration 14
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(grid_0.1_14, i, 9)
  })
}
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_dur_0.1_14.gif"))

# CHange working directory back to project directory
setwd("~/range_kelp/")
