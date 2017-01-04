#############################################################################
###"graph/raster.exceedence.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create duration figure
# 4. Create exceedence animation function
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
library(animation)
source("func/colour.palettes.R")
## USED BY:
#
## CREATES:
# "graph/sa_plot_exceedence_day1.png"
# "exceedence_HiRes_anim.gif"
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



# 3. Create duration figure -----------------------------------------------

# Create event data
dur <- stats_20_0.1_0[,c(1:6)]
dur <- dur[dur$stat == "mean",]
dur$EID <- 1:length(dur$lon)
colnames(dur) <- c("index", "X", "Y", "exceede", "stat", "Z", "EID")
dur <- as.EventData(dur)
# Grid it
durGrid <- findCells(dur, sa_grid)
dur2 <- merge(dur, durGrid, by = c("EID"), all = FALSE)
durData <- combineEvents(dur, durGrid, FUN = mean_no_NA)
durDataGrid <- merge(durData, sa_grid, by = c("PID", "SID"), all = FALSE)
durDataGrid <- merge(durDataGrid, dur2[,c(5,6,8,9)], by = c("PID", "SID"), all = FALSE)
durDataGrid <- durDataGrid[order(durDataGrid[,2], durDataGrid[,1], durDataGrid[,4]), ]

# Breaks for plotting
breaks <- round_any(seq(0, ceiling(max(durDataGrid$Z, na.rm = T)), length.out = 8), 10)
breaks[length(breaks)] <- breaks[length(breaks)]+10 # Catch the largest couple of points
durDataGrid$bins <- cut(durDataGrid$Z, breaks = breaks)

# Subsetted data frames for ease of plotting
durDataGrid_within <- droplevels(durDataGrid[durDataGrid$exceede == "within" & complete.cases(durDataGrid$Z),])
durDataGrid_NA <- droplevels(durDataGrid[is.na(durDataGrid$Z),])

# The duration figure
sa_plot_dur <- ggplot() + bw_update +
  geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey70") +
  geom_polygon(data = durDataGrid_within, aes(x = X, y = Y, group = paste(durDataGrid_within$PID, durDataGrid_within$SID,
                                                                    sep = "_"), fill = bins), colour = NA, size = 0.1) +
  geom_polygon(data = durDataGrid_NA, aes(x = X, y = Y, group = paste(durDataGrid_NA$PID, durDataGrid_NA$SID, sep = "_"),
                                          colour = as.factor(durDataGrid_NA$exceede)), fill = NA, size = 0.2, show.legend = TRUE) +
  scale_fill_manual(values = cols9) +
  guides(fill = guide_legend("Duration (days)\n above 20°C"),
         colour = guide_legend("Relation to threshold", override.aes = list(size = 4))) +
  ggtitle("Exceedence") +
  # Scale bar
  scaleBar(lon = 30, lat = -34.2, distanceLon = 100, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
           arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
  coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey30"))
print(sa_plot_dur)
ggsave("graph/sa_plot_exceedence_dur.png", height = 6, width = 10)


# 4. Create exceedence animation function ---------------------------------

# Combine all projections
# Real
stats_20_real <- rbind(stats_20_real_0, stats_20_real_1, stats_20_real_2, stats_20_real_3, stats_20_real_4, stats_20_real_5)
stats_20_real$decade <- c(rep(0,518), rep(1,518), rep(2,518), rep(3,518), rep(4,518), rep(5,518))
stats_20_real$proj <- "in situ"
# Static
stats_20_0.1 <- rbind(stats_20_0.1_0, stats_20_0.1_1, stats_20_0.1_2, stats_20_0.1_3, stats_20_0.1_4, stats_20_0.1_5)
stats_20_0.1$decade <- c(rep(0,672), rep(1,672), rep(2,672), rep(3,672), rep(4,672), rep(5,672))
stats_20_0.1$proj <- "0.1"

# The gif function
# Testing
# decade_select <- 0
# proj_select <- stats_20_real
# stat_select <- 7
# Use numerical values to select the stat you want to project
# 6 = "duration", 7 = "int_max", 8 = "int_cum", 9 = "int_max_abs", 10 = "int_cum_abs" 
draw.exc.fig <- function(decade_select, proj_select, stat_select){
  dat <- proj_select
  # Create event data
  exc <- dat[dat$decade == decade_select, c(1:5, stat_select)]
  exc <- exc[exc$stat == "mean",]
  exc$EID <- 1:length(exc$lon)
  colnames(exc) <- c("index", "X", "Y", "exceede", "stat", "Z", "EID")
  exc <- as.EventData(exc)
  # Grid it
  excGrid <- findCells(exc, sa_grid)
  exc2 <- merge(exc, excGrid, by = c("EID"), all = FALSE)
  excData <- combineEvents(exc, excGrid, FUN = mean_no_NA)
  excDataGrid <- merge(excData, sa_grid, by = c("PID", "SID"), all = FALSE)
  excDataGrid <- merge(excDataGrid, exc2[,c(5,6,8,9)], by = c("PID", "SID"), all = FALSE)
  excDataGrid <- excDataGrid[order(excDataGrid[,2], excDataGrid[,1], excDataGrid[,4]), ]
  # Breaks for plotting
  breaks <- round_any(seq(0, max(excDataGrid$Z, na.rm = T), length.out = 8), 0.1)
  # breaks[length(breaks)] <- breaks[length(breaks)]+10 # Catch the largest couple of points... doesn't work with small values
  excDataGrid$bins <- cut(excDataGrid$Z, breaks = breaks)
  # Subsetted data frames for ease of plotting
  excDataGrid_within <- droplevels(excDataGrid[excDataGrid$exceede == "within" & complete.cases(excDataGrid$Z),])
  excDataGrid_NA <- droplevels(excDataGrid[is.na(excDataGrid$Z),])
  # Subtitle data
  if(dat$proj[1] == "in situ"){
    subtitle <- paste("Projections based on in situ trends after ", decade_select, " decade(s)", sep = "")
  } else if (dat$proj[1] == "0.1"){
    subtitle <- paste(paste("Projections based on 0.1°C/dec after ", decade_select, " decade(s)", sep = ""))
  }
  # Create figure
  sa_plot_exc <- ggplot() + bw_update +
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey70") +
    geom_polygon(data = excDataGrid_within, aes(x = X, y = Y, group = paste(excDataGrid_within$PID, excDataGrid_within$SID,
                                                                            sep = "_"), fill = bins), colour = NA, size = 0.1) +
    geom_polygon(data = excDataGrid_NA, aes(x = X, y = Y, group = paste(excDataGrid_NA$PID, excDataGrid_NA$SID, sep = "_"),
                                            colour = as.factor(excDataGrid_NA$exceede)), fill = NA, size = 0.2, show.legend = TRUE) +
    scale_fill_manual(values = cols9) +
    guides(fill = guide_legend(paste(colnames(dat[stat_select]),"\n above 20°C", sep = ""), order = 2),
           colour = guide_legend("Relation to\nthreshold", override.aes = list(size = 4), order = 1)) +
    ggtitle("Exceedence", subtitle = subtitle) +
    # Scale bar
    scaleBar(lon = 30, lat = -34.2, distanceLon = 100, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
             arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
    coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "grey30"),
          legend.justification = c(1,0), legend.position = c(0.55, 0.45),
          legend.direction = "vertical",
          legend.box = "horizontal")
  print(sa_plot_exc)
}

# tester
# animate.daily.fig <- function() {
#   lapply(seq(1,31), function(i) {
#     draw.daily.fig(i)
#   })
# }
# saveGIF(animate.daily.fig(), interval = 0.1, ani.width = 800, movie.name = "test_anim.gif")


# 5. Create all of the gifs -----------------------------------------------

## Real
# Duration
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_real, 6)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_real_dur_anim.gif")) ## 18 seconds
# Int_max
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_real, 7)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_real_int_max_anim.gif")) ## xx seconds
# Int_cum
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_real, 8)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_real_int_cum_anim.gif")) ## xx seconds

## Static
# Duration
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_0.1, 6)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_0.1_dur_anim.gif")) ## xx seconds
# Int_max
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_0.1, 7)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_0.1_int_max_anim.gif")) ## xx seconds
# Int_cum
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_0.1, 8)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_0.1_int_cum_anim.gif")) ## xx seconds
