#############################################################################
###"graph/raster.exceedence.R"
## This script does:
# 1. Load data
# 2. Prep values for rasterising
# 3. Create rasters
# 4. Create figures
# 5. Create exceedence animation function
# 6. Create threshold animation function
# 7. Create all of the gifs
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
library(gganimate)
library(grid)
library(gtable)
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

# Breaks for plotting duration above a threshold
breaks_above <- seq(0, 190, 31)

# Breaks for plotting duration below a threshold
breaks_below <- seq(0, 366, 24)

# 3. Create rasters -------------------------------------------------------

# Gridding function
# proj_select <- stats_20_0.1_0 # Tester...
# Use numerical values to select the stat you want to project
# 6 = "duration", 7 = "int_max", 8 = "int_cum", 9 = "int_max_abs", 10 = "int_cum_abs" 
grid.dat <- function(proj_select, stat_select){
  dat <- proj_select
  dat$EID <- 1:length(dat$lon)
  dat <- dat[dat$stat == "mean", c(2:4, stat_select, 11)]
  # Create event data
  colnames(dat) <- c("X", "Y", "exceede", "Z", "EID")
  dat <- as.EventData(dat)
  # Grid it
  datGrid <- findCells(dat, sa_grid)
  dat2 <- merge(dat, datGrid, by = c("EID"), all = FALSE)
  datData <- combineEvents(dat, datGrid, FUN = mean_no_NA)
  datDataGrid <- merge(datData, sa_grid, by = c("PID", "SID"), all = FALSE)
  datDataGrid <- merge(datDataGrid, dat2[,c(4,6,7)], by = c("PID", "SID"), all = FALSE)
  datDataGrid <- datDataGrid[order(datDataGrid[,2], datDataGrid[,1], datDataGrid[,4]), ]
  # Breaks for plotting duration
  # CUrrently run in the figure function
  # datDataGrid$bins <- cut(datDataGrid$Z, breaks = breaks)
  return(datDataGrid)
}
# test <- grid.dat(stats_20_inSitu_0, stat_select = 6)

## Grid data
# In situ
grid_20_inSitu_0 <- grid.dat(stats_20_inSitu_0, 6)

# Static 0.1
grid_20_0.1_0 <- grid.dat(stats_20_0.1_0, 6)
grid_20_0.1_1 <- grid.dat(stats_20_0.1_1, 6)
grid_20_0.1_2 <- grid.dat(stats_20_0.1_2, 6)
grid_20_0.1_3 <- grid.dat(stats_20_0.1_3, 6)
grid_20_0.1_4 <- grid.dat(stats_20_0.1_4, 6)
grid_20_0.1_5 <- grid.dat(stats_20_0.1_5, 6)

## Grid data BELOW a threshold
grid_19_0.1_0 <- grid.dat(stats_19_0.1_0, 6)
grid_18_0.1_0 <- grid.dat(stats_18_0.1_0, 6)
grid_17_0.1_0 <- grid.dat(stats_17_0.1_0, 6)
grid_16_0.1_0 <- grid.dat(stats_16_0.1_0, 6)
grid_15_0.1_0 <- grid.dat(stats_15_0.1_0, 6)
grid_14_0.1_0 <- grid.dat(stats_14_0.1_0, 6)

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

# Currently only works for duration
# Other stats will not plot nor be labelled correctly
## Testing ##
# durDataGrid <- grid_19_0.1_0 
# type = "0.1"; thresh = 20; proj = 0; side = "below"
## END ##
# 'type', 'side' and 'proj' are used in the legend and subtitle
# 'thresh' and 'proj' are used to tell ggplot what to save the image as
sa.plot.dur <- function(durDataGrid, type, thresh, proj, side){
  if(side == "below"){
    durDataGrid$bins <- cut(durDataGrid$Z, breaks = breaks_below)
    l.y <- 0.35 # Change y axis of legend to compensate for longer legend
  } else if(side == "above"){
    durDataGrid$bins <- cut(durDataGrid$Z, breaks = breaks_above)
    l.y <- 0.45
  }
  durDataGrid_within <- durDataGrid[durDataGrid$exceede == "within",]
  durDataGrid_NA <- droplevels(durDataGrid[is.na(durDataGrid$Z) & durDataGrid$exceede != "NA",])
  durDataGrid_NA$exceede <- as.factor(durDataGrid_NA$exceede)
  sa_plot_dur <- ggplot() + bw_update +
    geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
    geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
    geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
    geom_polygon(data = durDataGrid_within, aes(x = X, y = Y, group = paste(durDataGrid_within$PID, durDataGrid_within$SID,
                                                                            sep = "_"), fill = bins), colour = NA, size = 0.1) +
    geom_polygon(data = durDataGrid_NA, aes(x = X, y = Y, group = paste(durDataGrid_NA$PID, durDataGrid_NA$SID, sep = "_"),
                                            colour = durDataGrid_NA$exceede), fill = NA, size = 0.2, show.legend = TRUE) +
    scale_fill_viridis(discrete = TRUE, drop = FALSE) +
    scale_colour_discrete(breaks = rev(levels(durDataGrid_NA$exceede))) +
    guides(fill = guide_legend(paste("Duration (days)\n", side, " ", thresh, "°C", sep = ""), order = 2),
           colour = guide_legend("Relation to threshold", override.aes = list(size = 4), order = 1)) +
    ggtitle("Exceedence", subtitle = paste("Projections based on ", type, " trends after ", proj, " decade(s)")) +
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
          legend.justification = c(1,0), legend.position = c(0.55, l.y),
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.direction = "vertical",
          legend.box = "horizontal")
  sa_plot_dur
  ggsave(paste("graph/sa_plot_exceedence_dur_", thresh,"_",type,"_",proj,".png", sep = ""), height = 6, width = 10)
}

## Exceedence above thresholds
# In situ figures
sa.plot.dur(grid_20_inSitu_0, "in situ", 20, 0, "above")

# Static 0.1C figures
sa.plot.dur(grid_20_0.1_0, "0.1", 20, 0, "above")

## Exceedence below thresholds
# Static 0.1C figures
sa.plot.dur(grid_19_0.1_0, "0.1", 19, 0, "below")
sa.plot.dur(grid_18_0.1_0, "0.1", 18, 0, "below")
sa.plot.dur(grid_17_0.1_0, "0.1", 17, 0, "below")
sa.plot.dur(grid_16_0.1_0, "0.1", 16, 0, "below")
sa.plot.dur(grid_15_0.1_0, "0.1", 15, 0, "below")
sa.plot.dur(grid_14_0.1_0, "0.1", 14, 0, "below")

# 5. Create exceedence animation function ---------------------------------

# The correct  legend and title lines of code saved up here for safe keeping during editing
# These are the only line that should be different from the same bit above
# guides(fill = guide_legend(paste(colnames(dat[stat_select]),"\n above 20°C", sep = ""), order = 2),
#        colour = guide_legend("Relation to\nthreshold", override.aes = list(size = 4), order = 1)) +
#   ggtitle("Exceedence", subtitle = subtitle) +


# Combine all projections
# inSitu
stats_20_inSitu <- rbind(stats_20_inSitu_0, stats_20_inSitu_1, stats_20_inSitu_2, stats_20_inSitu_3, stats_20_inSitu_4, stats_20_inSitu_5)
stats_20_inSitu$decade <- rep(0:5, each = length(stats_20_inSitu_0$index))
stats_20_inSitu$proj <- "in situ"
# Static
stats_20_0.1 <- rbind(stats_20_0.1_0, stats_20_0.1_1, stats_20_0.1_2, stats_20_0.1_3, stats_20_0.1_4, stats_20_0.1_5)
stats_20_0.1$decade <- rep(0:5, each = length(stats_20_0.1_0$index))
stats_20_0.1$proj <- "0.1"

# The gif function
# Testing
decade_select <- 3
proj_select <- stats_20_0.1
stat_select <- 6
side = "above"; thresh = 20
# Use numerical values to select the stat you want to project
# 6 = "duration", 7 = "int_max", 8 = "int_cum", 9 = "int_max_abs", 10 = "int_cum_abs" 
draw.exc.fig <- function(decade_select, proj_select, stat_select, side, thresh){
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
  if(stat_select == 6 & side == "above"){
    breaks <- breaks_above
    l.y <- 0.35
  } else if(stat_select == 6 & side == "below"){
    breaks <- breaks_below
    l.y <- 0.45
  } else {
    breaks <- round_any(seq(0, max(dat2[,4], na.rm = T), length.out = 7), 0.1)
    breaks[length(breaks)] <- round_any(max(dat2[,4], na.rm = T), 0.1)+0.1
    l.y <- 0.45
  }
  excDataGrid$bins <- cut(excDataGrid$Z, breaks = breaks)
  # Subsetted data frames for ease of plotting
  excDataGrid_within <- excDataGrid[excDataGrid$exceede == "within",]
  excDataGrid_NA <- droplevels(excDataGrid[is.na(excDataGrid$Z) & excDataGrid$exceede != "NA",])
  excDataGrid_NA$exceede <- as.factor(excDataGrid_NA$exceede)
  # Create title data
  if(dat$proj[1] == "in situ"){
    subtitle <- paste("Projections based on decadal in situ trends", sep = "")
  } else if (dat$proj[1] == "0.1"){
    subtitle <- paste(paste("Projections based on 0.1°C/dec trends", sep = ""))
  }
  p_bar <- data.frame(x = unique(dat$decade), y = rep(0, length(unique(dat$decade))))
  # Creat the title figure with progress bar
  sa_plot_title <- ggplot(data = p_bar) + bw_update +
    ggtitle("Exceedence", subtitle = subtitle) +
    geom_line(aes(x = x, y = y)) +
    geom_point(aes(x = decade_select, y = y), colour = "red", size = 3) +
    labs(x = "Decade(s)") +
    # scale_y_continuous(expand=c(0,0)) +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.background = element_blank(),
          panel.border = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
  sa_plot_title
  
  # g1 <- ggplotGrob(sa_plot_title)
  # # g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
  # g2 <- ggplotGrob(sa_plot_exc)
  # g <- rbind(g1, g2, size = "last") # stack the two plots
  # g$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
  # # center the legend vertically
  # # g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  # grid.newpage()
  # grid.draw(g)
  
  # Crerate the raster figure
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
    scale_fill_viridis(discrete = TRUE, drop = FALSE) +
    scale_colour_discrete(breaks = rev(levels(excDataGrid_NA$exceede))) +
    guides(fill = guide_legend(paste(colnames(dat[stat_select]),"\n",side," ",thresh,"°C", sep = ""), order = 2),
           colour = guide_legend("Relation to threshold", override.aes = list(size = 4), order = 1)) +
    # ggtitle("Exceedence", subtitle = subtitle) +
    scale_y_continuous(breaks = seq(-34,-28,2)) +
    scale_x_continuous(breaks = seq(18,30,4)) +
    scaleBar(lon = 30, lat = -34.2, distanceLon = 100, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
             arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
    coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
    theme(#plot.title = element_text(size = 10, hjust = 0.5),
          #plot.subtitle = element_text(size = 10, hjust = 0.5),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "grey30"),
          legend.justification = c(1,0), legend.position = c(0.55, 0.45),
          legend.background = element_rect(fill = "white", colour = "black"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.direction = "vertical",
          legend.box = "horizontal")
  jpeg("inset_map.jpeg", width = 800, height = 600, pointsize = 10) # Set PNG dimensions
  vp1 <- viewport(x = 0.5, y = -0.02, w = 1.00, h = 0.95, just = c("bottom")) # Africa
  vp2 <- viewport(x = 0.5, y = 0.2, w = 0.40, h = 1.00, just = c("top")) # Title
  print(sa_plot_exc, vp = vp1)
  print(sa_plot_title, vp = vp2)
  print()
  dev.off()
  # g1 <- ggplotGrob(sa_plot_title)
  # # g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
  # g2 <- ggplotGrob(sa_plot_exc)
  # g <- rbind(g1, g2, size = "last") # stack the two plots
  # g$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
  # # center the legend vertically
  # # g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  # grid.newpage()
  # grid.draw(g)
  # # print(g)
}


# 6. Create threshold animation function ----------------------------------

# Combine all thresholds BELOW
# inSitu

# Static
stats_thresh_0.1_0 <- rbind(stats_14_0.1_0, stats_15_0.1_0, stats_16_0.1_0, stats_17_0.1_0, stats_18_0.1_0, stats_19_0.1_0)
stats_thresh_0.1_0$threshold <- rep(0:5, each = length(stats_14_0.1_0$index))
stats_thresh_0.1_0$proj <- "0.1"


x <-  c(rep(0,696), rep(1,696), rep(2,696), rep(3,696), rep(4,696), rep(5,696))
y <- rep(0:5, each = length(stats_19_0.1_0$index))

# The gif function
# Testing
decade_select <- 3
proj_select <- stats_20_0.1
stat_select <- 6
side = "above"; thresh = 20
# Use numerical values to select the stat you want to project
# 6 = "duration", 7 = "int_max", 8 = "int_cum", 9 = "int_max_abs", 10 = "int_cum_abs" 
draw.exc.fig <- function(thresh_select, proj_select, stat_select, side, thresh){
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
  if(stat_select == 6 & side == "above"){
    breaks <- breaks_above
    l.y <- 0.35
  } else if(stat_select == 6 & side == "below"){
    breaks <- breaks_below
    l.y <- 0.45
  } else {
    breaks <- round_any(seq(0, max(dat2[,4], na.rm = T), length.out = 7), 0.1)
    breaks[length(breaks)] <- round_any(max(dat2[,4], na.rm = T), 0.1)+0.1
    l.y <- 0.45
  }
  excDataGrid$bins <- cut(excDataGrid$Z, breaks = breaks)
  # Subsetted data frames for ease of plotting
  excDataGrid_within <- excDataGrid[excDataGrid$exceede == "within",]
  excDataGrid_NA <- droplevels(excDataGrid[is.na(excDataGrid$Z) & excDataGrid$exceede != "NA",])
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
    scale_fill_viridis(discrete = TRUE, drop = FALSE) +
    scale_colour_discrete(breaks = rev(levels(excDataGrid_NA$exceede))) +
    guides(fill = guide_legend(paste(colnames(dat[stat_select]),"\n",side," ",thresh,"°C", sep = ""), order = 2),
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


# 7. Create all of the gifs -----------------------------------------------

## inSitu
# Duration
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_inSitu, 6, "above", 20)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig()[1:6], interval = 2, ani.width = 800, movie.name = "exc_inSitu_dur_anim.gif")) ## 11 seconds
# Int_max
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_inSitu, 7, "above", 20)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_inSitu_int_max_anim.gif")) ## xx seconds
# Int_cum
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_inSitu, 8, "above", 20)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_inSitu_int_cum_anim.gif")) ## xx seconds

## Static
# Duration
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_0.1, 6, "above", 20)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_0.1_dur_anim.gif")) ## xx seconds
# Int_max
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_0.1, 7, "above", 20)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_0.1_int_max_anim.gif")) ## xx seconds
# Int_cum
animate.exc.fig <- function() {
  lapply(seq(0,5), function(i) {
    draw.exc.fig(i, stats_20_0.1, 8, "above", 20)
  })
}
# Create gif for a single time series
system.time(saveGIF(animate.exc.fig(), interval = 2, ani.width = 800, movie.name = "exc_0.1_int_cum_anim.gif")) ## xx seconds
