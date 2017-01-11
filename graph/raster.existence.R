#############################################################################
###"graph/raster.existence.R"
## This script does:
# 1. Load data
# 2. Calculate existence
# 3. Create existence figures and GIFs
# 4. Create de Hoop figures and GIFs
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
system.time(source("graph/raster.exceedence.R")) ## ~280 seconds


# 2. Calculate existence --------------------------------------------------

## Create composite grid data frames
# In situ
grid_inSitu_all <- cbind(grid_inSitu_21[,c(1:7,9)], grid_inSitu_20[,9], grid_inSitu_16[,9], grid_inSitu_15[,c(9,14,15)])
colnames(grid_inSitu_all)[8:11] <- c("thresh_21", "thresh_20", "thresh_16", "thresh_15")

# Static 0.1C
grid_0.1_all <- cbind(grid_0.1_21[,c(1:7,9)], grid_0.1_20[,9], grid_0.1_16[,9], grid_0.1_15[,c(9,14,15)])
colnames(grid_0.1_all)[8:11] <- c("thresh_21", "thresh_20", "thresh_16", "thresh_15")

# Derive stats from the gridded data
guide <- filter(grid_inSitu_all, decade == 0, POS == 1)
limit_20 <- round(guide$thresh_20[guide$index == 129][1])
limit_15 <- round(guide$thresh_15[guide$index == 115][1])


# Calculate de Hoop and Gansbaai stats
gansbaai <- melt(daily_clim_hiRes[115,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
# limit_20 <- nrow(filter(gansbaai, temp >= 20))
# limit_16 <- nrow(filter(gansbaai, temp <= 16))
# limit_15 <- nrow(filter(gansbaai, temp <= 15))
deHoop <- melt(daily_clim_hiRes[129,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
# limit_20 <- nrow(filter(deHoop, temp >= 20))
# limit_16 <- nrow(filter(deHoop, temp <= 16))
# limit_15 <- nrow(filter(deHoop, temp <= 15))



# Testing
# dat <- grid_0.1_all[10,]
# dat <- hiRes_0.1[66,]
exist.test <- function(dat){
  # if(mean(dat$thresh_20, na.rm = T) == 0 & mean(dat$thresh_16, na.rm = T) >= limit_16 & dat$thresh_15 >= limit_15){
  if(mean(dat$thresh_20, na.rm = T) <= limit_20 & mean(dat$thresh_15, na.rm = T) >= limit_15){ # Only 20C and 15C thresholds
    exist <- data.frame(exist = 1)
  } else {
    exist <- data.frame(exist = 0)
  }
  return(exist)
}

# In situ
exist_grid_inSitu <- ddply(grid_inSitu_all, .(index, PID, SID, POS, X, Y, trend, decade), exist.test)

# Static 0.1C
exist_grid_0.1 <- ddply(grid_0.1_all, .(index, PID, SID, POS, X, Y, trend, decade), exist.test)

# Check one decade
test <- filter(exist_grid_0.1, decade == 0, POS == 1)

# 3. Create existence figures and GIFs ------------------------------------

scale_cols <- c("skyblue", "peru")

## Testing ##
# df = exist_grid_0.1
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
    
    # geom_point(data = df2[df2$index == 147,], aes(x = X, y = Y), colour = "red", size = 0.1) + # Highlight specific pixels
    
    # Manually create progress bar #
    geom_rect(aes(xmin = 21.0, xmax = 27.0, ymin = -26.02, ymax = -27.02), fill = "white", colour = "black") +
    geom_segment(aes(x = p_point, xend = p_point, y = -26.52, yend = -26.77)) +
    geom_point(aes(x = p_point, y = -26.42), colour = "red", size = 5, alpha = 0.7) +
    geom_text(aes(label = decade, x = p_point, y = -26.42), size = 4) +
    geom_line(aes(x = 21.5:26.5, y = -26.62)) +
    # END progress bar  #
    
    scale_fill_manual(labels = c("yes", "no"), values = scale_cols) +#, labels = breaks2, breaks = breaks2) +
    guides(fill = guide_legend("Threshold\nexceeded")) +
    ggtitle("Existence", subtitle = subtitle) +
    # ggtitle("Existence") + # Title for static images
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


# It doesn't appear possible to set an output directory within saveGIF
# So I am setting it here
setwd("~/range_kelp/graph/")

## Existence GIFs for the entire country
# inSitu
animate.exist.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exist.fig(exist_grid_inSitu, i)
  })
}
system.time(saveGIF(animate.exist.fig(), interval = 2, ani.width = 800, movie.name = "exist_inSitu.gif")) ## ~9 seconds
# Static 0.1C
animate.exist.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exist.fig(exist_grid_0.1, i)
  })
}
saveGIF(animate.exist.fig(), interval = 2, ani.width = 800, movie.name = "exist_0.1.gif")

# CHange working directory back to project directory
setwd("~/range_kelp/")


# 4. Create de Hoop figures and GIFs --------------------------------------
  # The De Hoop figure requires enough fine tuning so as to warrant its own creation function
 
# Define the West Coast
wc_lats <- c(-36, -33); wc_lons <- c(17, 21)

## Testing ##
# df = exist_grid_0.1
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
  p_point <- (decade*(2/6))+19.1
  # Crerate the raster figure
  sa_plot_exist <- ggplot() + bw_update +
    # Map shapes
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
    geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
    geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
    # Site labels
    geom_text(data = site_names1[2,], aes(lon, lat, label = site), hjust = 1.15, vjust = 1.5, size = 5, colour = "white") +
    geom_text(aes(x = 18.9, y = -34.6, label = "False\nBay"), hjust = 1.15, vjust = 0.5, size = 5, colour = "white", lineheight = 0.8) +
    geom_text(data = site_names2, aes(lon, lat, label = site), hjust = -0.0, vjust = 1.4, size = 5, colour = "white", lineheight = 0.8) +
    # Kelp existence/ not boxes
    geom_polygon(data = yes, aes(x = X, y = Y, group = paste(yes$PID, yes$SID, sep = "_"), fill = as.factor(exist)),
                 colour = NA) +
    geom_polygon(data = no, aes(x = X, y = Y, group = paste(no$PID, no$SID, sep = "_"), fill = as.factor(exist)),
                 colour = NA) +
    # Kelp presence absence boxes
    geom_polygon(data = present, aes(x = X, y = Y, group = paste(present$SID, present$PID, sep = "_")),
                 show.legend = FALSE, fill = NA, colour = "white", size = 0.3) +
    geom_polygon(data = absent, aes(x = X, y = Y, group = paste(absent$SID, absent$PID, sep = "_")),
                 show.legend = FALSE, fill = NA, colour = "black", size = 0.3) +
    # De Hoop!
    geom_point(aes(x = 20.4055089, y = -34.4730239), colour = "gold", size = 7, shape = 13, alpha = 0.7) +
    geom_text(aes(x = 20.4055089, y = -34.4730239, label = "de Hoop"), hjust = 0.5, vjust = -0.7, size = 7, 
              colour = "gold", angle = 45) +
    # Manually create progress bar #
    geom_rect(aes(xmin = 19.0, xmax = 21.0, ymin = -33.0, ymax = -33.25), fill = "white", colour = "black") +
    geom_segment(aes(x = p_point, xend = p_point, y = -33.21, yend = -33.14)) +
    geom_point(aes(x = p_point, y = -33.10), colour = "red", size = 5, alpha = 0.7) +
    geom_text(aes(label = decade, x = p_point, y = -33.10), size = 4) +
    geom_line(aes(x = seq(19.1,20.9,0.1), y = -33.17)) +
    # END progress bar  #
    scale_fill_discrete(labels = c("yes", "no")) +
    guides(fill = guide_legend("Threshold\nexceeded")) +
    ggtitle("Existence", subtitle = subtitle) +
    # ggtitle("Existence") + # Title for static images
    scale_y_continuous(breaks = seq(-35,-34,1)) +
    scale_x_continuous(breaks = seq(18,20,2)) +
    scaleBar(lon = 16.5, lat = -35.7, distanceLon = 100, distanceLat = 10, distanceLegend = 20, dist.unit = "km",
             arrow.length = 30, arrow.distance = 30, arrow.North.size = 4) +
    coord_map(xlim = wc_lons, ylim = wc_lats, projection = "mercator") +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "grey30"),
          legend.justification = c(1,0), legend.position = c(0.7, 0.7),
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
ggsave("graph/exist_inSitu_0_deHoop.png",  height = 6, width = 10)

# Static 0.1C figures
draw.exist.fig(exist_grid_0.1, 0)
ggsave("graph/exist_0.1_0_deHoop.png",  height = 6, width = 10)


# It doesn't appear possible to set an output directory within saveGIF
# So I am setting it here
setwd("~/range_kelp/graph/")

## Existence GIFs for the entire country
# inSitu
animate.exist.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exist.fig(exist_grid_inSitu, i)
  })
}
system.time(saveGIF(animate.exist.fig(), interval = 2, ani.width = 800, movie.name = "exist_inSitu_deHoop.gif")) ## ~9 seconds
# Static 0.1C
animate.exist.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exist.fig(exist_grid_0.1, i)
  })
}
system.time(saveGIF(animate.exist.fig(), interval = 2, ani.width = 800, movie.name = "exist_0.1_deHoop.gif")) ## ~9 seconds

# CHange working directory back to project directory
setwd("~/range_kelp/")

