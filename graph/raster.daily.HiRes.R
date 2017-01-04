#############################################################################
###"graph/raster.daily.HiRes.R"
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

# Load daily clims
load("data/daily_clim_hiRes.Rdata")

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


# 4. Create figures -------------------------------------------------------

# Melt for ggplot2
# all_exceedences_stats_melt <- melt(all_exceedences_stats, id.vars = c("index", "threshold", "metric"),
#                                    variable.name = "stat",
#                                    value.name = "value")

# Breaks for plotting
breaks <- seq(10,28,2)
day1DataGrid$bins <- cut(day1DataGrid$Z, breaks = breaks)

# The initial proof of concept
sa_plot_day1 <- ggplot() + bw_update +
  geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey80") +
  geom_polygon(data = day1DataGrid, aes(x = X, y = Y, group = paste(day1DataGrid$PID, day1DataGrid$SID,
                                                                          sep = "_"), fill = bins), colour = NA, size = 0.1) +
  # geom_polygon(data = annualDataGrid, aes(x = X, y = Y, group = paste(annualDataGrid$PID, annualDataGrid$SID, sep = "_"),
  #                                         fill = bins), colour = "red", size = 0.1, show_guide = FALSE) +
  # geom_point(data = sub_dist, aes(lon, lat), shape = 16, size = 2.0, colour = "Black", alpha = 0.5) + # Insert major cities
  # geom_text(data = sub_dist, aes(lon, lat, label = site), hjust = -0.1, vjust = 1, size = 2.2) + # Label cities
  scale_fill_manual(values = cols27) + # Sets colours
  guides(fill = guide_legend(expression(paste("Temp. (",degree,"C)")))) +
  ggtitle("Daily HiRes Interpolation", subtitle = "01/01") +
  # Scale bar
  scaleBar(lon = 30, lat = -34.2, distanceLon = 100, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
           arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
  # scale_x_continuous(expand = c(0, 0)) +
  # scale_y_continuous(expand = c(0, 0)) +
  coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
  theme(plot.title = element_text(size = 8, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.title = element_blank())
sa_plot_day1
ggsave("graph/sa_plot_HiRes_day1.png", height = 6, width = 10)

# 5. Create animations ----------------------------------------------------

draw.daily.fig <- function(day_select){
  # Create event data
  day1 <- daily_clim_hiRes[complete.cases(daily_clim_hiRes),c(1,2,(day_select+2))] # +2 to account for lon/ lat
  day1$EID <- 1:length(day1$lon)
  colnames(day1) <- c("X", "Y", "Z", "EID")
  day1 <- as.EventData(day1)
  # Grid it
  day1Grid <- findCells(day1, sa_grid)
  day1Data <- combineEvents(day1, day1Grid, FUN = mean)
  day1DataGrid <- merge(day1Data, sa_grid, by = c("PID", "SID"), all = FALSE)
  day1DataGrid$bins <- cut(day1DataGrid$Z, breaks = breaks) 
  day1DataGrid <- day1DataGrid[order(day1DataGrid[,2], day1DataGrid[,1], day1DataGrid[,4]), ]
  # Subtitle date
  day_date <- seq(as.Date("2016/01/01"), as.Date("2016/12/31"), by = "day")[day_select]
  day_date <- format(day_date, "%m/%d")
  # Create figure
  sa_plot_day1 <- ggplot() + #bw_update +
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
    geom_polygon(data = day1DataGrid, aes(x = X, y = Y, group = paste(day1DataGrid$PID, day1DataGrid$SID,
                                                                      sep = "_"), fill = bins), colour = NA, size = 0.1) +
    scale_fill_manual(values = cols27) +
    guides(fill = guide_legend(expression(paste("Temp. (",degree,"C)")))) +
    ggtitle("Daily HiRes Interpolation", subtitle = day_date) +
    scaleBar(lon = 30, lat = -34.2, distanceLon = 100, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
             arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
    coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "grey30"))
  print(sa_plot_day1)
}

# tester
animate.daily.fig <- function() {
  lapply(seq(1,31), function(i) {
    draw.daily.fig(i)
  })
}
saveGIF(animate.daily.fig(), interval = 0.1, ani.width = 800, movie.name = "test_anim.gif")

# Animate the daily data
animate.daily.fig <- function() {
  lapply(seq(1,366), function(i) {
    draw.daily.fig(i)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.daily.fig(), interval = 0.1, ani.width = 800, movie.name = "daily_HiRes_anim.gif")) ## xx seconds
