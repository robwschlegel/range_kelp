#############################################################################
###"graph/raster.existence.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Calculate existence
# 4. Create rasters
# 5. Create existence figures
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
system.time(source("graph/raster.exceedence.R")) ## ~xxx seconds

# Daily clims
load("data/SACTNdaily_clim_wide_v4.1.Rdata")
load("data/SACTNdaily_clim_wide_10y.Rdata")

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

# Define the West Coast
wc_lats <- c(-36, -30); wc_lons <- c(14, 21)

# For use with rasters
mean_no_NA <- function(x){mean(x, na.rm =T)}


# 3. Calculate existence --------------------------------------------------

## Create composite grid data frames
# In situ
grid_inSitu_all <- cbind(grid_inSitu_20[,c(1:7,9)], grid_inSitu_16[,9], grid_inSitu_15[,c(9,14,15)])
colnames(grid_inSitu_all)[8:10] <- c("thresh_20", "thresh_16", "thresh_15")


# Calculate de Hoop stats
# deHoop <- melt(SACTNdaily_clim_wide_v4.1[33,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
deHoop <- melt(daily_clim_hiRes[124,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
limit_20 <- nrow(filter(deHoop, temp >= 20))
limit_16 <- nrow(filter(deHoop, temp <= 16))
limit_15 <- nrow(filter(deHoop, temp <= 15))

## THis function works on climatology data frames
# Testing
dat <- SACTNdaily_clim_wide_v4.1[15,]
# dat <- hiRes_0.1[66,]
exist.test <- function(dat){
  dat2 <- melt(dat[,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
  if(nrow(filter(dat2, temp >= 20)) == 0 & nrow(filter(dat2, temp < 16)) <= limit_16 & nrow(filter(dat2, temp < 15)) <= limit_15){
    exist <- data.frame(exist = 1)
  } else {
    exist <- data.frame(exist = 0)
  }
  return(exist)
}
# test
test <- ddply(SACTNdaily_clim_wide_v4.1, .(index, lon, lat), exist.test)

# In situ
exist_inSitu <- ddply(hiRes_inSitu[complete.cases(hiRes_inSitu$`1`),], .(index, lon, lat, trend, decade), exist.test)

# Static 0.1C
exist_0.1 <- ddply(hiRes_0.1[complete.cases(hiRes_0.1$`1`),], .(index, lon, lat, trend, decade), exist.test)


# 4. Create rasters -------------------------------------------------------

# Gridding function
# df <- filter(exist_0.1, decade == 0) # Tester...
grid.dat <- function(df){
  # coerce data frame for gridding
  df$EID <- 1:length(df$lon)
  colnames(df)[c(2,3,6)] <- c("X", "Y","Z")
  df <- as.EventData(df)
  # Grid it
  dfGrid <- findCells(df, sa_grid)
  df2 <- merge(df, dfGrid, by = c("EID"), all = FALSE)
  dfData <- combineEvents(df, dfGrid, FUN = mean_no_NA)
  dfDataGrid <- merge(dfData, sa_grid, by = c("PID", "SID"), all = FALSE)
  dfDataGrid <- merge(dfDataGrid, df2[,c(2,5,6,8,9)], by = c("PID", "SID"), all = FALSE)
  dfDataGrid <- dfDataGrid[order(dfDataGrid[,2], dfDataGrid[,1], dfDataGrid[,4]), ] # These MUST be ordered this way to plot correctly...
  dfDataGrid$Z <- round(dfDataGrid$Z)
  colnames(dfDataGrid)[3] <- "exist"
  return(dfDataGrid)
}
# test <- grid.dat(stats_20_inSitu_0)

## Grid data
# In situ
exist_grid_inSitu <- ddply(exist_inSitu, .(decade), grid.dat)

# Static 0.1
exist_grid_0.1 <- ddply(exist_0.1, .(decade), grid.dat)


# 5. Create existence figures -------------------------------------------------------

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
df = exist_grid_0.1
# decade = 0
## END ##
draw.exist.fig <- function(df, decade){
  df2 <- df[df$decade == decade,]
  yes <- df2[df2$exist == 1,]
  no <- df2[df2$exist == 0,]
  # Create title data
  if(df$trend[1] == "in situ"){
    subtitle <- paste("Projections based on decadal in situ trends", sep = "")
  } else if(df$trend[1] == "0.1"){
    subtitle <- paste(paste("Projections based on 0.1°C/dec trends", sep = ""))
  }
  p_point <- decade+21.5
  # Crerate the raster figure
  sa_plot_exist <- ggplot() + bw_update +
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
    geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
    geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
    geom_text(data = site_names1, aes(lon, lat, label = site), hjust = 1.15, vjust = 0.5, size = 3, colour = "white") +
    geom_text(data = site_names2, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.8, size = 3, colour = "white", 
              angle = 330, lineheight = 0.8) +
    geom_text(data = site_names3, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.5, size = 3, colour = "white") +
    # Kelp existence/ not boxes
    geom_polygon(data = yes, aes(x = X, y = Y, group = paste(yes$PID, yes$SID, sep = "_"), fill = as.factor(exist)),
                 colour = NA) +
    geom_polygon(data = no, aes(x = X, y = Y, group = paste(no$PID, no$SID, sep = "_"), fill = as.factor(exist)),
                 colour = NA) +
    # Kelp presence absence boxes
    geom_polygon(data = present, aes(x = X, y = Y, group = paste(present$SID, present$PID, sep = "_")),
                 show.legend = FALSE, fill = NA, colour = "white", size = 0.1) +
    geom_polygon(data = absent, aes(x = X, y = Y, group = paste(absent$SID, absent$PID, sep = "_")),
                 show.legend = FALSE, fill = NA, colour = "black", size = 0.3) +
    
    geom_point(data = df2[df2$index == 125,], aes(x = X, y = Y), colour = "red", size = 0.1) + # Highlight specific pixels
    
    # Manually create progress bar #
    # geom_rect(aes(xmin = 21.0, xmax = 27.0, ymin = -26.02, ymax = -27.02), fill = "white", colour = "black") +
    # geom_segment(aes(x = p_point, xend = p_point, y = -26.52, yend = -26.77)) +
    # geom_point(aes(x = p_point, y = -26.42), colour = "red", size = 5, alpha = 0.7) +
    # geom_text(aes(label = decade, x = p_point, y = -26.42), size = 4) +
    # geom_line(aes(x = 21.5:26.5, y = -26.62)) +
    # END progress bar  #
    scale_fill_discrete(labels = c("yes", "no")) +#, labels = breaks2, breaks = breaks2) +
    guides(fill = guide_legend("Threshold\nexceeded")) +
    # ggtitle("Existence", subtitle = subtitle) +
    ggtitle("Existence") + # Title for static images
    scale_y_continuous(breaks = seq(-34,-28,2)) +
    scale_x_continuous(breaks = seq(18,30,4)) +
    scaleBar(lon = 30, lat = -34.2, distanceLon = 100, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
             arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
    coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "grey30"),
          legend.justification = c(1,0), legend.position = c(0.55, 0.55),
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.direction = "vertical",
          legend.box = "horizontal")
  print(sa_plot_exist)
}

## Exceedence days ABOVE thresholds
# In situ figures
draw.exist.fig(exist_grid_inSitu, 0)
ggsave("graph/exist_inSitu_0.png",  height = 6, width = 10)

# Static 0.1C figures
draw.exist.fig(exist_grid_0.1, 0)
ggsave("graph/exist_0.1_0.png",  height = 6, width = 10)


# 6. Create all of the gifs -----------------------------------------------

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

