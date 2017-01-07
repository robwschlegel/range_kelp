#############################################################################
###"graph/raster.daily.HiRes.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create rasters
# 4. Create figures
# 5. Create animations
# 6. Create range figure
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

# Site names for plotting
site_names <- site_list[c(1,13,29,35,40,47,59,65,71,113,130,132),] # Manually select sites...
site_names$site <- as.character(site_names$site)
# Split into three data frames for better plotting
site_names1 <- site_names[1:3,] # West Coast
site_names2 <- site_names[4:7,] # South Coast
site_names2[1,2] <- "Cape\nAgulhas"
site_names2[4,2] <- "Port\nElizabeth" # South Coast
site_names3 <- site_names[8:12,] # South Coast

# Breaks for plotting
breaks <- seq(10,28,2)
day1DataGrid$bins <- cut(day1DataGrid$Z, breaks = breaks)
# test <- day1DataGrid[day1DataGrid$bins == levels(day1DataGrid$bins)[8],]

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
sa_plot_day1 <- sa_plot +
  geom_polygon(data = day1DataGrid, aes(x = X, y = Y, group = paste(day1DataGrid$PID, day1DataGrid$SID,
                                                                    sep = "_"), fill = bins), colour = NA, size = 0.1) +
  # geom_point(data = site_names, aes(lon, lat), shape = 16, size = 3, colour = "white", alpha = 0.5) +
  scale_fill_manual(values = cols27, drop = FALSE) +
  guides(fill = guide_legend(expression(paste("Temp. (",degree,"C)")))) +
  ggtitle("Daily 0.1° Grid Interpolation", subtitle = "01/01")
sa_plot_day1
ggsave("graph/sa_plot_HiRes_day1.png", height = 6, width = 10)

# 5. Create animations ----------------------------------------------------

# The correct title line of code saved up here for safe keeping during editing
# This is the only line that should be different from the same bit above
# ggtitle("Daily HiRes Interpolation", subtitle = day_date)

## Testing ##
day_select <- 45
## END ##
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
  sa_plot_day1 <- sa_plot +
    geom_polygon(data = day1DataGrid, aes(x = X, y = Y, group = paste(day1DataGrid$PID, day1DataGrid$SID,
                                                                      sep = "_"), fill = bins), colour = NA, size = 0.1) +
    # geom_point(data = site_names, aes(lon, lat), shape = 16, size = 3, colour = "white", alpha = 0.5) +
    scale_fill_manual(values = cols27, drop = FALSE) +
    guides(fill = guide_legend(expression(paste("Temp. (",degree,"C)")))) +
    ggtitle("Daily 0.1° Grid Interpolation", subtitle = day_date)
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
system.time(saveGIF(animate.daily.fig(), interval = 0.1, ani.width = 800, movie.name = "daily_HiRes_anim.gif")) ## 701 seconds


# 6. Create range figure --------------------------------------------------


