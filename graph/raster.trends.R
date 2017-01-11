#############################################################################
###"graph/raster.trends.R"
## This script does:
# 1. Load data
# 2. Prep data for plotting
# 3. Create trend figures and GIFs
## DEPENDS ON:
library(plyr)
library(dplyr)
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
library(RColorBrewer)
source("func/colour.palettes.R")
## USED BY:
#
## CREATES:
# The naming convention is: 1_2_3_4_5.x
# 1. The data used, 2. The statistic measured, 3. The decadal projection, 4. The thermal threshold, 5. The number of years of projection
# Note that the GIFs show all decades of projection so there is no 5th designation
#############################################################################

# 1. Load data ------------------------------------------------------------

# Daily data
load("data/daily_clim_hiRes.Rdata")

# Trends
load("data/trend_hiRes.Rdata")

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


# 2. Prep data for plotting -----------------------------------------------

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

# Prep trend data frames
trend_inSitu <- cbind(daily_clim_hiRes[,1:3], trend = trend_hiRes, type = "in situ")
trend_0.1 <- cbind(daily_clim_hiRes[,1:3], trend = 0.1, type = "0.1")
trend_all <- rbind(trend_inSitu, trend_0.1)

# Gridding function
# df = trend_0.1 # Tester...
grid.dat <- function(df){
  dat <- df
  # Create event data
  dat$EID <- 1:length(dat$lon)
  colnames(dat)[c(2:4)] <- c("X", "Y","Z")
  dat <- as.EventData(dat)
  # Grid it
  datGrid <- findCells(dat, sa_grid)
  datData <- combineEvents(dat, datGrid, FUN = mean_no_NA)
  datDataGrid <- merge(datData, sa_grid, by = c("PID", "SID"), all = FALSE)
  datDataGrid <- datDataGrid[order(datDataGrid[,2], datDataGrid[,1], datDataGrid[,4]), ] # These MUST be ordered this way to plot correctly...
  colnames(datDataGrid)[3] <- "trend"
  datDataGrid$type <- df$type[1]
  return(datDataGrid)
}
# test <- grid.dat(stats_20_inSitu_0)

## Grid all trends at once
grid_trend_all <- ddply(trend_all , .(type), grid.dat)
grid_trend_all$trend <- as.factor(round_any(grid_trend_all$trend, 0.1))

# 3. Create trend figures and GIFs --------------------------------------

## Testing ##
df = grid_trend_all
type = 2
## END ##
draw.trend.fig <- function(type){
  df <- grid_trend_all
  # Subset by type
  df2 <- df[as.numeric(df$type) == type,]
  # legend data
  if(df2$type[1] == "in situ"){
    subtitle <- "In situ"
    site_list_25y <- site_list[(site_list$length/365.25)>25,]
  } else if(df2$type[1] == "0.1"){
    subtitle <- "0.1°C/dec"
    site_list_25y <- data.frame(lon = 100, lat = -60) # A throw away point...
  }
  # Define colour scale breaks
  breaks <- levels(df$trend)[c(2,4,6,7,8,10)]
  # labels <- seq(-0.7, 0.5, 0.2)
  scale_cols <- c("slateblue4", "slateblue4", "royalblue2", "royalblue2", "steelblue1", "steelblue1", 
                  "white", "pink1", "pink1", "salmon", "salmon")
  # Create figure
  sa_plot_trend <- ggplot() + bw_update +
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
    geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
    geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
    geom_text(data = site_names1, aes(lon, lat, label = site), hjust = 1.15, vjust = 0.5, size = 3, colour = "white") +
    geom_text(data = site_names2, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.8, size = 3, colour = "white", 
              angle = 330, lineheight = 0.8) +
    geom_text(data = site_names3, aes(lon, lat, label = site), hjust = -0.15, vjust = 0.5, size = 3, colour = "white") +
    # 53 in situ time series used for interpolation
    geom_point(data = site_list_25y, aes(x = lon, y = lat), fill = "black", size = 4, shape = 23, alpha = 0.5) +
    # Trend data
    geom_polygon(data = df2, aes(x = X, y = Y, group = paste(df2$PID, df2$SID, sep = "_"), fill = trend), 
                 colour = NA, size = 0.1) +
    # Trend label
    geom_label(aes(x = 24.25, y = -29, label = subtitle)) +
    # Kelp presence absence boxes
    geom_polygon(data = present, aes(x = X, y = Y, group = paste(present$SID, present$PID, sep = "_")), 
                 show.legend = FALSE, fill = NA, colour = "white", size = 0.1) +
    geom_polygon(data = absent, aes(x = X, y = Y, group = paste(absent$SID, absent$PID, sep = "_")), 
                 show.legend = FALSE, fill = NA, colour = "black", size = 0.3) +
    # Trend label
    scale_fill_manual(breaks = rev(breaks), labels = rev(breaks), values = scale_cols, drop = FALSE) +
    guides(fill = guide_legend("Decadal\ntrend (°C)")) +
    ggtitle("Trends") +
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
  print(sa_plot_trend)
}

# It doesn't appear possible to set an output directory within saveGIF
# So I am setting it here
setwd("~/range_kelp/graph/")

animate.trend.fig <- function() {
  lapply(seq(1,2), function(i) {
    draw.trend.fig(i)
  })
}
saveGIF(animate.trend.fig(), interval = c(5,2), ani.width = 800, movie.name = "trend_all.gif")

setwd("~/range_kelp/")

