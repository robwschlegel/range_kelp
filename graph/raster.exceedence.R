#############################################################################
###"graph/raster.exceedence.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create rasters
# 4. Create figures
# 5. Create exceedence animation function
# 6. Create all of the gifs
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
breaks <- seq(0, 190, 31)
durDataGrid$bins <- cut(durDataGrid$Z, breaks = breaks)

# Subsetted data frames for ease of plotting
durDataGrid_within <- droplevels(durDataGrid[durDataGrid$exceede == "within" & complete.cases(durDataGrid$Z),])
durDataGrid_NA <- droplevels(durDataGrid[is.na(durDataGrid$Z),])
durDataGrid_NA$exceede <- as.factor(durDataGrid_NA$exceede)
# durDataGrid_NA$exceede <- factor(durDataGrid_NA$exceede,levels(as.factor(durDataGrid_NA$exceede))[c(2,1)])


# 4. Create figures -------------------------------------------------------

# Site names for plotting
site_names <- site_list[c(1,13,29,35,40,47,59,65,71,113,130,132),] # Manually select sites...
site_names$site <- as.character(site_names$site)
# Split into three data frames for better plotting
site_names1 <- site_names[1:3,] # West Coast
site_names2 <- site_names[4:7,] # South Coast
site_names2[1,2] <- "Cape\nAgulhas"
site_names2[4,2] <- "Port\nElizabeth" # South Coast
site_names3 <- site_names[8:12,] # South Coast

# Prerender the figure
sa_plot <- ggplot() + bw_update +
  geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
  geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
  geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
  geom_text(data = site_names1, aes(lon, lat, label = site), hjust = 1.15, vjust = 0.5, size = 3, colour = "white") +
  geom_text(data = site_names2, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.8, size = 3, colour = "white", 
            angle = 330, lineheight = 0.8) +
  geom_text(data = site_names3, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.5, size = 3, colour = "white") +
  scale_y_continuous(breaks = seq(-34,-28,2)) +
  scale_x_continuous(breaks = seq(18,30,4)) +
  scaleBar(lon = 30, lat = -34.2, distanceLon = 100, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
           arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
  coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey30"),
        legend.justification = c(1,0), legend.position = c(0.55, 0.45),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box = "horizontal")
sa_plot

# Then add the data
sa_plot_dur <- sa_plot +
  geom_polygon(data = durDataGrid_within, aes(x = X, y = Y, group = paste(durDataGrid_within$PID, durDataGrid_within$SID,
                                                                          sep = "_"), fill = bins), colour = NA, size = 0.1) +
  geom_polygon(data = durDataGrid_NA, aes(x = X, y = Y, group = paste(durDataGrid_NA$PID, durDataGrid_NA$SID, sep = "_"),
                                          colour = durDataGrid_NA$exceede), fill = NA, size = 0.2, show.legend = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_colour_discrete(breaks = rev(levels(durDataGrid_NA$exceede))) +
  guides(fill = guide_legend("Duration (days)\n above 20°C", order = 2),
         colour = guide_legend("Relation to threshold", override.aes = list(size = 4), order = 1)) +
  ggtitle("Exceedence", subtitle = "Projections based on in situ trends after 0 decade(s)")
sa_plot_dur
ggsave("graph/sa_plot_exceedence_dur.png", height = 6, width = 10)


# 5. Create exceedence animation function ---------------------------------

# The correct  legend and title lines of code saved up here for safe keeping during editing
# These are the only line that should be different from the same bit above
# guides(fill = guide_legend(paste(colnames(dat[stat_select]),"\n above 20°C", sep = ""), order = 2),
#        colour = guide_legend("Relation to\nthreshold", override.aes = list(size = 4), order = 1)) +
#   ggtitle("Exceedence", subtitle = subtitle) +


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
decade_select <- 3
proj_select <- stats_20_real
stat_select <- 7
# Use numerical values to select the stat you want to project
# 6 = "duration", 7 = "int_max", 8 = "int_cum", 9 = "int_max_abs", 10 = "int_cum_abs" 
draw.exc.fig <- function(decade_select, proj_select, stat_select){
  dat <- proj_select
  if(stat_select == 6){
    colnames(dat)[6] <- "duration (days)"
  }
  dat2 <- dat[dat$stat == "mean", c(2:4, stat_select, 11)]
  # Create event data
  exc <- dat2[dat2$decade == decade_select, 1:4]
  exc$EID <- 1:length(exc$lon)
  colnames(exc) <- c("X", "Y", "exceede", "Z", "EID")
  exc <- as.EventData(exc)
  # Grid it
  excGrid <- findCells(exc, sa_grid)
  exc2 <- merge(exc, excGrid, by = c("EID"), all = FALSE)
  excData <- combineEvents(exc, excGrid, FUN = mean_no_NA)
  excDataGrid <- merge(excData, sa_grid, by = c("PID", "SID"), all = FALSE)
  excDataGrid <- merge(excDataGrid, exc2[,c(4,6,7)], by = c("PID", "SID"), all = FALSE)
  excDataGrid <- excDataGrid[order(excDataGrid[,2], excDataGrid[,1], excDataGrid[,4]), ]
  # Breaks for plotting
  if(stat_select == 6){
    breaks <- seq(0, 190, 31)
  } else {
    breaks <- round_any(seq(0, max(dat2[,4], na.rm = T), length.out = 7), 0.1)
    breaks[length(breaks)] <- round_any(max(dat2[,4], na.rm = T), 0.1)+0.1
  }
  excDataGrid$bins <- cut(excDataGrid$Z, breaks = breaks)
  # Subsetted data frames for ease of plotting
  excDataGrid_within <- excDataGrid[excDataGrid$exceede == "within",]
  excDataGrid_NA <- droplevels(excDataGrid[is.na(excDataGrid$Z),])
  excDataGrid_NA$exceede <- as.factor(excDataGrid_NA$exceede)
  # Subtitle data
  if(dat$proj[1] == "in situ"){
    subtitle <- paste("Projections based on in situ trends after ", decade_select, " decade(s)", sep = "")
  } else if (dat$proj[1] == "0.1"){
    subtitle <- paste(paste("Projections based on 0.1°C/dec after ", decade_select, " decade(s)", sep = ""))
  }
  # Crerate the figure
  sa_plot_exc <- ggplot() + bw_update +
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
    geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
    geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
    geom_text(data = site_names1, aes(lon, lat, label = site), hjust = 1.15, vjust = 0.5, size = 3, colour = "white") +
    geom_text(data = site_names2, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.8, size = 3, colour = "white", 
              angle = 330, lineheight = 0.8) +
    geom_text(data = site_names3, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.5, size = 3, colour = "white") +
    geom_polygon(data = excDataGrid_within, aes(x = X, y = Y, group = paste(excDataGrid_within$PID, excDataGrid_within$SID,
                                                                            sep = "_"), fill = bins), colour = NA, size = 0.1) +
    geom_polygon(data = excDataGrid_NA, aes(x = X, y = Y, group = paste(excDataGrid_NA$PID, excDataGrid_NA$SID, sep = "_"),
                                            colour = excDataGrid_NA$exceede), fill = NA, size = 0.2, show.legend = TRUE) +
    scale_fill_viridis(discrete = TRUE, drop = FALSE, breaks = levels(excDataGrid_within$bins)) +
    scale_colour_discrete(breaks = rev(levels(excDataGrid_NA$exceede))) +
    guides(fill = guide_legend(paste(colnames(dat[stat_select]),"\n above 20°C", sep = ""), order = 2),
           colour = guide_legend("Relation to threshold", override.aes = list(size = 4), order = 1)) +
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
          legend.justification = c(1,0), legend.position = c(0.55, 0.45),
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.direction = "vertical",
          legend.box = "horizontal")
  print(sa_plot_exc)
}


# 6. Create all of the gifs -----------------------------------------------

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
