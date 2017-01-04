#############################################################################
###"proc/kelp.exceedence.R"
## This script does:
# 1. Load data
# 2. Project daily clims given different trends
# 3. Calculate threshold exceedences
# 4. Calculate exceedence statistics
## DEPENDS ON:
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(RmarineHeatWaves)
library(doMC); doMC::registerDoMC(4)
# source("func/proj.temp.R")
## USED BY:
#
## CREATES:
#
#############################################################################

# 1. Load data ------------------------------------------------------------

load("data/daily_clim_hiRes.Rdata")

load("data/trend_hiRes.Rdata")


# 2. Project daily clims given different trends ---------------------------

# Real projections
hiRes_real_0 <- cbind(index = 1:length(daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)]),
                      lon = daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)], lat = daily_clim_hiRes$lat[complete.cases(trend_hiRes$trend)],
                      daily_clim_hiRes[complete.cases(trend_hiRes$trend),][,3:368] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*0))
hiRes_real_1 <- cbind(index = 1:length(daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)]),
                      lon = daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)], lat = daily_clim_hiRes$lat[complete.cases(trend_hiRes$trend)],
                      daily_clim_hiRes[complete.cases(trend_hiRes$trend),][,3:368] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*1))
hiRes_real_2 <- cbind(index = 1:length(daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)]),
                      lon = daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)], lat = daily_clim_hiRes$lat[complete.cases(trend_hiRes$trend)],
                      daily_clim_hiRes[complete.cases(trend_hiRes$trend),][,3:368] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*2))
hiRes_real_3 <- cbind(index = 1:length(daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)]),
                      lon = daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)], lat = daily_clim_hiRes$lat[complete.cases(trend_hiRes$trend)],
                      daily_clim_hiRes[complete.cases(trend_hiRes$trend),][,3:368] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*3))
hiRes_real_4 <- cbind(index = 1:length(daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)]),
                      lon = daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)], lat = daily_clim_hiRes$lat[complete.cases(trend_hiRes$trend)],
                      daily_clim_hiRes[complete.cases(trend_hiRes$trend),][,3:368] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*4))
hiRes_real_5 <- cbind(index = 1:length(daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)]),
                      lon = daily_clim_hiRes$lon[complete.cases(trend_hiRes$trend)], lat = daily_clim_hiRes$lat[complete.cases(trend_hiRes$trend)],
                      daily_clim_hiRes[complete.cases(trend_hiRes$trend),][,3:368] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*5))

# Static projections
hiRes_0.1_0 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*0))
hiRes_0.1_1 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*1))
hiRes_0.1_2 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*2))
hiRes_0.1_3 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*3))
hiRes_0.1_4 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*4))
hiRes_0.1_5 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*5))


# 3. Calculate threshold exceedences --------------------------------------

# The surrogate data.frame used when the given data lay outside of the exceedence threshold
guide <- melt(hiRes_real_1[100,4:369], variable.name = "doy", value.name = "temp")
guide$date <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
guide <- exceedence(guide, threshold = 16, below = FALSE, 
                          min_duration = 3, join_across_gaps = TRUE, max_gap = 0, max_pad_length = 3)
guide <- guide$exceedence[1,]
guide[,1:length(guide)] <- NA
# Define function for use
dat <- hiRes_real_1[250,] # Testing...
detect.exceedence <- function(dat, threshold = 20, below = FALSE){
  ts <- melt(dat[,4:369], variable.name = "doy", value.name = "temp")
  ts$date <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
  # A slopy way of dealing with the sites that don't exceede the threshold
  # This logic loop finds a surrogate row of data that will work with exceedence()
  # It then calculates the stats but replaces everything with 'NA'
  # I did it this way as I didn't want to manually create all of the columns
  if(threshold > max(ts$temp, na.rm = T)){
    exceedences <- guide
    exceedences$exceede <- "under"
  } else if(threshold < min(ts$temp, na.rm = T)){
    exceedences <- guide
    exceedences$exceede <- "over"
  } else {
    exceedences <- exceedence(ts, threshold = threshold, below = below, 
                              min_duration = 3, join_across_gaps = TRUE, max_gap = 0, max_pad_length = 3)
    exceedences <- exceedences$exceedence
    exceedences$exceede <- "within"
  }
  exceedences$index <- dat$index[1]
  exceedences$lon <- dat$lon[1]
  exceedences$lat <- dat$lat[1]
  exceedences <- data.frame(exceedences)
  return(exceedences)
}
# test <- ddply(hiRes_real_1, .(index), detect.exceedence, threshold = 20, .progress = "text")

# Calculate exceedences
# Real
exceedences_20_real_0 <- ddply(hiRes_real_0, .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_real_1 <- ddply(hiRes_real_1, .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_real_2 <- ddply(hiRes_real_2, .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_real_3 <- ddply(hiRes_real_3, .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_real_4 <- ddply(hiRes_real_4, .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_real_5 <- ddply(hiRes_real_5, .(index), detect.exceedence, threshold = 20, .parallel = T)
# Static
exceedences_20_0.1_0 <- ddply(hiRes_0.1_0[complete.cases(hiRes_0.1_0),], .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_0.1_1 <- ddply(hiRes_0.1_1[complete.cases(hiRes_0.1_1),], .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_0.1_2 <- ddply(hiRes_0.1_2[complete.cases(hiRes_0.1_2),], .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_0.1_3 <- ddply(hiRes_0.1_3[complete.cases(hiRes_0.1_3),], .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_0.1_4 <- ddply(hiRes_0.1_4[complete.cases(hiRes_0.1_4),], .(index), detect.exceedence, threshold = 20, .parallel = T)
exceedences_20_0.1_5 <- ddply(hiRes_0.1_5[complete.cases(hiRes_0.1_5),], .(index), detect.exceedence, threshold = 20, .parallel = T)


# 4. Calculate exceedence statistics --------------------------------------

# Function for extracting mean and max stats
# df <- exceedences_20_real_1[1:10,] # Tester...
exceedences.stats <- function(df){
  exceedences_mean <- ddply(df, .(index, lon, lat, exceede), summarise,
                            stat = "mean",
                            duration = mean(duration, na.rm = T),
                            int_max = mean(int_max, na.rm = T),
                            int_cum = mean(int_cum, na.rm = T),
                            int_max_abs = mean(int_max_abs, na.rm = T),
                            int_cum_abs = mean(int_cum_abs, na.rm = T))
  exceedences_max <- ddply(df, .(index, lon, lat, exceede), summarise,
                               stat = "max",
                               duration = max(duration, na.rm = T),
                               int_max = max(int_max, na.rm = T),
                               int_cum = max(int_cum, na.rm = T),
                               int_max_abs = max(int_max_abs, na.rm = T),
                               int_cum_abs = max(int_cum_abs, na.rm = T))
  exceedences_stats <- rbind(exceedences_mean, exceedences_max)
  exceedences_stats <- data.frame(exceedences_stats)
  exceedences_stats[!(complete.cases(exceedences_stats[,6:10])),6:10] <- NA
  return(exceedences_stats)
}

# Calculate stats
# Real
stats_20_real_0 <- exceedences.stats(exceedences_20_real_0)
stats_20_real_1 <- exceedences.stats(exceedences_20_real_1)
stats_20_real_2 <- exceedences.stats(exceedences_20_real_2)
stats_20_real_3 <- exceedences.stats(exceedences_20_real_3)
stats_20_real_4 <- exceedences.stats(exceedences_20_real_4)
stats_20_real_5 <- exceedences.stats(exceedences_20_real_5)
# Static
stats_20_0.1_0 <- exceedences.stats(exceedences_20_0.1_0)
stats_20_0.1_1 <- exceedences.stats(exceedences_20_0.1_1)
stats_20_0.1_2 <- exceedences.stats(exceedences_20_0.1_2)
stats_20_0.1_3 <- exceedences.stats(exceedences_20_0.1_3)
stats_20_0.1_4 <- exceedences.stats(exceedences_20_0.1_4)
stats_20_0.1_5 <- exceedences.stats(exceedences_20_0.1_5)

