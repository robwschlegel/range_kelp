#############################################################################
###"proc/thresh.exceedence.R"
## This script does:
# 1. Load data
# 2. Project daily clims given different trends
# 3. Calculate threshold exceedence stats
## DEPENDS ON:
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(RmarineHeatWaves)
library(doMC); doMC::registerDoMC(4)
source("func/proj.temp.R")
# source("func/proj.temp.R")
## USED BY:
#
## CREATES:
#
#############################################################################

# 1. Load data ------------------------------------------------------------

# Clims
load("data/daily_clim_pixel.Rdata")


# 2. Project daily clims given different trends ---------------------------

# In situ projections
# These data frames are slightly smaller than other projection bases 
# as not every pixel has an ins situ trend for it
pixel_inSitu <- data.frame()
for(i in 0:5){
  dat <- proj.temp.pixel(daily_clim_pixel, "in situ", i)
  pixel_inSitu <- rbind(pixel_inSitu, dat)
}

# Static 0.1C projection
pixel_0.1 <- data.frame()
for(i in 0:5){
  dat <- proj.temp.pixel(daily_clim_pixel, 0.1, i)
  pixel_0.1 <- rbind(pixel_0.1, dat)
}

# Combine projections
pixel_all <- rbind(pixel_inSitu, pixel_0.1)
save(pixel_all, file = "data/pixel_all.Rdata")

# test <- filter(pixel_inSitu, index == 89)
# test <- melt(test[,c(4:369,371)], id.vars = "decade", variable.name = "doy", value.name = "temp")
# ggplot(data = test, aes(x = doy, y = temp, group = decade)) +
#   geom_line(aes(colour = decade))
# nrow(test[test$temp<=15 & test$decade == 5,])

# 3. Calculate threshold exceedence stats  --------------------------------

# The surrogate data.frame used when the given data lay outside of the exceedence threshold
# guide <- melt(pixel_inSitu_0[100,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
# guide$date <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
# guide <- exceedence(guide, threshold = 16, below = FALSE, 
#                           min_duration = 3, join_across_gaps = TRUE, max_gap = 0, max_pad_length = 3)
# guide <- guide$exceedence[1,]
# guide[,1:length(guide)] <- NA
guide <- data.frame(index = NA, lon = NA, lat = NA,
  exceede = "NA", duration = NA, int_max = NA, # exceede = "NA" is intentional
  int_cum = NA, int_max_abs = NA, int_cum_abs = NA)
# Define function for use
# dat <- pixel_0.1[pixel_0.1$index == 89 & pixel_0.1$decade == 5,] # Testing...
# threshold = 15
# below = TRUE
exceedence.stats <- function(dat, threshold = 20, below = FALSE){
  ts <- melt(dat[,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
  ts$date <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
  if(is.na(ts$temp[1]) | threshold > max(ts$temp, na.rm = T) | threshold < min(ts$temp, na.rm = T)){
    exceedences_stats <- guide
    exceedences_stats$index <- dat$index[1]
    exceedences_stats$lon <- dat$lon[1]
    exceedences_stats$lat <- dat$lat[1]
    if(is.na(ts$temp[1])){
    }else if(threshold > max(ts$temp, na.rm = T)){
      exceedences_stats$exceede <- "below"
      if(!below){
        exceedences_stats$duration <- 0
      } else if(below){
        exceedences_stats$duration <- 366
      }
    } else if(threshold < min(ts$temp, na.rm = T)){
      exceedences_stats$exceede <- "above"
      if(!below){
        exceedences_stats$duration <- 366
      } else if(below){
        exceedences_stats$duration <- 0
      }
    }
  } else {
    exceedences <- exceedence(ts, threshold = threshold, below = below, 
                              min_duration = 1, join_across_gaps = TRUE, max_gap = 0, max_pad_length = 3)
    exceedences <- exceedences$exceedence
    exceedences$exceede <- "within"
    exceedences$index <- dat$index[1]
    exceedences$lon <- dat$lon[1]
    exceedences$lat <- dat$lat[1]
    exceedences <- data.frame(exceedences)
    if(!below){
      exceedences_stats <- ddply(exceedences, .(index, lon, lat, exceede), summarise,
                                 duration = sum(duration, na.rm = T),
                                 int_max = max(int_max, na.rm = T),
                                 int_cum = sum(int_cum, na.rm = T),
                                 int_max_abs = max(int_max_abs, na.rm = T),
                                 int_cum_abs = mean(int_cum_abs, na.rm = T))
    } else if (below){
      exceedences_stats <- ddply(exceedences, .(index, lon, lat, exceede), summarise,
                                 duration = sum(duration, na.rm = T),
                                 int_max = min(int_max, na.rm = T),
                                 int_cum = sum(int_cum, na.rm = T),
                                 int_max_abs = min(int_max_abs, na.rm = T),
                                 int_cum_abs = mean(int_cum_abs, na.rm = T))
    }
    exceedences_stats <- data.frame(exceedences_stats)
  }
  exceedences_stats$trend <- dat$trend[1]
  exceedences_stats$decade <- dat$decade[1]
  exceedences_stats$thresh <- threshold
  exceedences_stats$below <- below
  return(exceedences_stats)
}
# system.time(test <- ddply(pixel_inSitu_1, .(index), exceedence.stats, .progress = "text"))

# Calculate exceedence stats ABOVE a threshold
# stats_22 <- ddply(pixel_all, .(index, trend, decade), exceedence.stats, threshold = 22, .parallel = T) # To high for all kelps
stats_21 <- ddply(pixel_all, .(index, trend, decade), exceedence.stats, threshold = 21, .parallel = T)
save(stats_21, file = "data/stats_21.Rdata")
stats_20 <- ddply(pixel_all, .(index, trend, decade), exceedence.stats, threshold = 20, .parallel = T)
save(stats_20, file = "data/stats_20.Rdata")

# Calculate exceedence stats BELOW a threshold
stats_16 <- ddply(pixel_all, .(index, trend, decade), exceedence.stats, threshold = 16, below = TRUE, .parallel = T)
save(stats_16, file = "data/stats_16.Rdata")
stats_15 <- ddply(pixel_all, .(index, trend, decade), exceedence.stats, threshold = 15, below = TRUE, .parallel = T)
save(stats_15, file = "data/stats_15.Rdata")
# stats_14 <- ddply(pixel_all, .(index, trend, decade), exceedence.stats, threshold = 14, below = TRUE, .parallel = T) # To low for Cape Agulhas kelps


# Check results
# test <- filter(stats_15, index == 89)
