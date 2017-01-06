#############################################################################
###"proc/kelp.exceedence.R"
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
# source("func/proj.temp.R")
## USED BY:
#
## CREATES:
#
#############################################################################

# 1. Load data ------------------------------------------------------------

# Clims
load("data/daily_clim_hiRes.Rdata")
daily_clim_hiRes <- cbind(index = 1:length(daily_clim_hiRes$lon), daily_clim_hiRes)

# In situ trends
load("data/trend_hiRes.Rdata")


# 2. Project daily clims given different trends ---------------------------

# In situ projections
# These data frames are slightly smaller than other projection bases 
# as not every pixel has an ins situ trend for it
hiRes_inSitu_0 <- daily_clim_hiRes[complete.cases(trend_hiRes$trend),]
hiRes_inSitu_1 <- cbind(hiRes_inSitu_0[,1:3], (hiRes_inSitu_0[,4:369] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*1)))
hiRes_inSitu_2 <- cbind(hiRes_inSitu_0[,1:3], (hiRes_inSitu_0[,4:369] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*2)))
hiRes_inSitu_3 <- cbind(hiRes_inSitu_0[,1:3], (hiRes_inSitu_0[,4:369] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*3)))
hiRes_inSitu_4 <- cbind(hiRes_inSitu_0[,1:3], (hiRes_inSitu_0[,4:369] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*4)))
hiRes_inSitu_5 <- cbind(hiRes_inSitu_0[,1:3], (hiRes_inSitu_0[,4:369] + (trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*5)))

# Static 0.1C projection
hiRes_0.1_0 <- daily_clim_hiRes
hiRes_0.1_1 <- cbind(daily_clim_hiRes[,1:3], (daily_clim_hiRes[,4:369] + (0.1*1)))
hiRes_0.1_2 <- cbind(daily_clim_hiRes[,1:3], (daily_clim_hiRes[,4:369] + (0.1*2)))
hiRes_0.1_3 <- cbind(daily_clim_hiRes[,1:3], (daily_clim_hiRes[,4:369] + (0.1*3)))
hiRes_0.1_4 <- cbind(daily_clim_hiRes[,1:3], (daily_clim_hiRes[,4:369] + (0.1*4)))
hiRes_0.1_5 <- cbind(daily_clim_hiRes[,1:3], (daily_clim_hiRes[,4:369] + (0.1*5)))


# 3. Calculate threshold exceedence stats  --------------------------------

# The surrogate data.frame used when the given data lay outside of the exceedence threshold
guide <- melt(hiRes_inSitu_0[100,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
guide$date <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
guide <- exceedence(guide, threshold = 16, below = FALSE, 
                          min_duration = 3, join_across_gaps = TRUE, max_gap = 0, max_pad_length = 3)
guide <- guide$exceedence[1,]
guide[,1:length(guide)] <- NA
# Define function for use
# dat <- hiRes_0.1_0[184,] # Testing...
# threshold = 16
# below = TRUE
exceedence.stats <- function(dat, threshold = 20, below = FALSE){
  ts <- melt(dat[,4:369], id.vars = NULL, variable.name = "doy", value.name = "temp")
  ts$date <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
  if(is.na(ts$temp[1])){
    exceedences <- guide
    exceedences$exceede <- "NA"
  } else if(threshold > max(ts$temp, na.rm = T)){
    exceedences <- guide
    exceedences$exceede <- "below"
  }else if(threshold < min(ts$temp, na.rm = T)){
    exceedences <- guide
    exceedences$exceede <- "above"
  } else {
    exceedences <- exceedence(ts, threshold = threshold, below = below, 
                              min_duration = 1, join_across_gaps = TRUE, max_gap = 0, max_pad_length = 3)
    exceedences <- exceedences$exceedence
    exceedences$exceede <- "within"
  }
  exceedences$index <- dat$index[1]
  exceedences$lon <- dat$lon[1]
  exceedences$lat <- dat$lat[1]
  exceedences <- data.frame(exceedences)
  if(length(exceedences[complete.cases(exceedences$duration)]) == 0){
    exceedences_stats <- data.frame(
      index = rep(dat$index[1],2),
      lon = rep(dat$lon[1],2),
      lat = rep(dat$lat[1],2),
      exceede = rep(exceedences$exceede,2),
      stat = c("mean", "max"),
      duration = NA,
      int_max = NA,
      int_cum = NA,
      int_max_abs = NA,
      int_cum_abs = NA
    )
  } else {
    exceedences_mean <- ddply(exceedences, .(index, lon, lat, exceede), summarise,
                              stat = "mean",
                              duration = mean(duration, na.rm = T),
                              int_max = mean(int_max, na.rm = T),
                              int_cum = mean(int_cum, na.rm = T),
                              int_max_abs = mean(int_max_abs, na.rm = T),
                              int_cum_abs = mean(int_cum_abs, na.rm = T))
    exceedences_max <- ddply(exceedences, .(index, lon, lat, exceede), summarise,
                             stat = "max",
                             duration = max(duration, na.rm = T),
                             int_max = max(int_max, na.rm = T),
                             int_cum = max(int_cum, na.rm = T),
                             int_max_abs = max(int_max_abs, na.rm = T),
                             int_cum_abs = max(int_cum_abs, na.rm = T))
    exceedences_stats <- rbind(exceedences_mean, exceedences_max)
    exceedences_stats <- data.frame(exceedences_stats)
  }
  return(exceedences_stats)
}
# system.time(test <- ddply(hiRes_inSitu_1, .(index), exceedence.stats, .progress = "text"))

## Calculate exceedence stats
# In situ
stats_20_inSitu_0 <- ddply(hiRes_inSitu_0, .(index), exceedence.stats, .parallel = T)
stats_20_inSitu_1 <- ddply(hiRes_inSitu_1, .(index), exceedence.stats, .parallel = T)
stats_20_inSitu_2 <- ddply(hiRes_inSitu_2, .(index), exceedence.stats, .parallel = T)
stats_20_inSitu_3 <- ddply(hiRes_inSitu_3, .(index), exceedence.stats, .parallel = T)
stats_20_inSitu_4 <- ddply(hiRes_inSitu_4, .(index), exceedence.stats, .parallel = T)
stats_20_inSitu_5 <- ddply(hiRes_inSitu_5, .(index), exceedence.stats, .parallel = T)

# Static 0.1C
stats_20_0.1_0 <- ddply(hiRes_0.1_0, .(index), exceedence.stats, .parallel = T)
stats_20_0.1_1 <- ddply(hiRes_0.1_1, .(index), exceedence.stats, .parallel = T)
stats_20_0.1_2 <- ddply(hiRes_0.1_2, .(index), exceedence.stats, .parallel = T)
stats_20_0.1_3 <- ddply(hiRes_0.1_3, .(index), exceedence.stats, .parallel = T)
stats_20_0.1_4 <- ddply(hiRes_0.1_4, .(index), exceedence.stats, .parallel = T)
stats_20_0.1_5 <- ddply(hiRes_0.1_5, .(index), exceedence.stats, .parallel = T)

## Calculate exceedence stats BELOW a threshold
# Static 0.1C
# Using 0.1 as it has more pixels
stats_19_0.1_0 <- ddply(hiRes_0.1_0, .(index), exceedence.stats, threshold = 19, below = TRUE, .parallel = T)
stats_18_0.1_0 <- ddply(hiRes_0.1_0, .(index), exceedence.stats, threshold = 18, below = TRUE, .parallel = T)
stats_17_0.1_0 <- ddply(hiRes_0.1_0, .(index), exceedence.stats, threshold = 17, below = TRUE, .parallel = T)
stats_16_0.1_0 <- ddply(hiRes_0.1_0, .(index), exceedence.stats, threshold = 16, below = TRUE, .parallel = T)
stats_15_0.1_0 <- ddply(hiRes_0.1_0, .(index), exceedence.stats, threshold = 15, below = TRUE, .parallel = T)
stats_14_0.1_0 <- ddply(hiRes_0.1_0, .(index), exceedence.stats, threshold = 14, below = TRUE, .parallel = T)

