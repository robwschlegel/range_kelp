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
source("func/proj.temp.R")
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
hiRes_0.1_1 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*1))
hiRes_0.1_2 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*1))
hiRes_0.1_3 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*1))
hiRes_0.1_4 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*1))
hiRes_0.1_5 <- cbind(index = 1:length(daily_clim_hiRes$lon), lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + (0.1*1))


# 3. Calculate threshold exceedences --------------------------------------

# Define function for use
dat <- hiRes_real_1[180,] # Testing...
detect.exceedence <- function(dat, threshold = 18, below = FALSE){
  ts <- melt(dat[,4:369], variable.name = "doy", value.name = "temp")
  ts$date <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
  if(threshold > max(ts$temp, na.rm = T)){
    exceedences <- ts
    exceedences$thresh <- threshold
    exceedences$thresh_criterion <- FALSE
    exceedences$duration_criterion <- FALSE
    exceedences$exceedence <- FALSE
    exceedences$exceedence_no <- NA
    exceedences$index <- dat$index[1]
    exceedences$lon <- dat$lon[1]
    exceedences$lat <- dat$lat[1]
    exceedences <- data.frame(exceedences)
  } else if(threshold < min(ts$temp, na.rm = T)){
    exceedences <- ts
    exceedences$thresh <- threshold
    exceedences$thresh_criterion <- TRUE
    exceedences$duration_criterion <- TRUE
    exceedences$exceedence <- TRUE
    exceedences$exceedence_no <- 1
    exceedences$index <- dat$index[1]
    exceedences$lon <- dat$lon[1]
    exceedences$lat <- dat$lat[1]
    exceedences <- data.frame(exceedences)
  } else {
    exceedences <- exceedence(ts, threshold = threshold, below = below, 
                              min_duration = 3, join_across_gaps = TRUE, max_gap = 0, max_pad_length = 3)
    exceedences <- exceedences$threshold
    exceedences$index <- dat$index[1]
    exceedences$lon <- dat$lon[1]
    exceedences$lat <- dat$lat[1]
    exceedences <- data.frame(exceedences)
  }
  return(exceedences)
}

# Calculate exceedences
# Real
exceedences_18_real_1 <- ddply(hiRes_real_1, .(index), detect.exceedence, threshold = 18, .parallel = T)
exceedences_18_real_2 <- ddply(hiRes_real_2, .(index), detect.exceedence, threshold = 18, .parallel = T)
exceedences_18_real_3 <- ddply(hiRes_real_3, .(index), detect.exceedence, threshold = 18, .parallel = T)
exceedences_18_real_4 <- ddply(hiRes_real_4, .(index), detect.exceedence, threshold = 18, .parallel = T)
exceedences_18_real_5 <- ddply(hiRes_real_5, .(index), detect.exceedence, threshold = 18, .parallel = T)
# Static
exceedences_18_0.1_1 <- ddply(hiRes_0.1_1[complete.cases(hiRes_0.1_1),], .(index), detect.exceedence, threshold = 18, .parallel = T)
exceedences_18_0.1_2 <- ddply(hiRes_0.1_2[complete.cases(hiRes_0.1_2),], .(index), detect.exceedence, threshold = 18, .parallel = T)
exceedences_18_0.1_3 <- ddply(hiRes_0.1_3[complete.cases(hiRes_0.1_3),], .(index), detect.exceedence, threshold = 18, .parallel = T)
exceedences_18_0.1_4 <- ddply(hiRes_0.1_4[complete.cases(hiRes_0.1_4),], .(index), detect.exceedence, threshold = 18, .parallel = T)
exceedences_18_0.1_5 <- ddply(hiRes_0.1_5[complete.cases(hiRes_0.1_5),], .(index), detect.exceedence, threshold = 18, .parallel = T)


# 4. Calculate exceedence statistics --------------------------------------

# Melt for ggplot2
all_exceedences_melt <- melt(all_exceedences, id.vars = c("index", "threshold"),
                             measure.vars = c("duration", "int_max", "int_cum"),
                             variable.name = "stat", 
                             value.name = "value")

# Calculate mean lengths and intensities of threshold exceedences
all_exceedences_mean <- ddply(all_exceedences, .(index, threshold), summarise,
                              metric = "mean",
                              duration = mean(duration, na.rm = T),
                              int_max = mean(int_max, na.rm = T),
                              int_cum = mean(int_cum, na.rm = T),
                              int_max_abs = mean(int_max_abs, na.rm = T),
                              int_cum_abs = mean(int_cum_abs, na.rm = T))

all_exceedences_max <- ddply(all_exceedences, .(index, threshold), summarise,
                             metric = "max",
                             duration = max(duration, na.rm = T),
                             int_max = max(int_max, na.rm = T),
                             int_cum = max(int_cum, na.rm = T),
                             int_max_abs = max(int_max_abs, na.rm = T),
                             int_cum_abs = max(int_cum_abs, na.rm = T))

all_exceedences_stats <- rbind(all_exceedences_mean, all_exceedences_max)

# Melt for ggplot2
all_exceedences_stats_melt <- melt(all_exceedences_stats, id.vars = c("index", "threshold", "metric"),
                                   variable.name = "stat",
                                   value.name = "value")
write.csv(all_exceedences_stats_melt, file = "~/R/forOthers/John_Bolton/false_bay_exceedence.csv", row.names = F)

# Histogram showing mean statistics of exceedences
ggplot() + bw_update +
  geom_density(data = all_exceedences_melt, (aes(x = value, fill = as.factor(threshold))), alpha = 0.5) +
  scale_fill_discrete(guide_legend(title = "Threshold (C)")) +
  facet_grid(index~stat, scales = "free")
ggsave("~/R/forOthers/John_Bolton/exceedence_density.jpg", height = 8, width = 12)
