#############################################################################
###"prep/daily.clim.interp.R"
## This script does:
# 1. Load data
# 2. Calculate daily climatologies
# 3. Interpolate at HiRes
# 4. Create climate projections
## DEPENDS ON:
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(akima)
library(RmarineHeatWaves)
library(doMC); doMC::registerDoMC(4)
source("func/proj.temp.R")
## USED BY:
#
## CREATES:
#
#############################################################################

# 1. Load data ------------------------------------------------------------

# Daily temperatures
load("~/SACTNraw/data/SACTNdaily_v4.1.Rdata")

# Site list
load("~/SACTN/metadata/site_list_v4.1.Rdata")

# Hi-res coordinates
outCoords <- read.table("~/tempssa_v3.0/coords_to_extract/interp_HiRes_coords_348_sites.txt", header = FALSE)
# outCoords <- outCoords[,c(2,1)]
# colnames(outCoords) <- c("lat", "lon")
colnames(outCoords) <- c("lon", "lat")


# 2. Calculate daily climatologies ----------------------------------------

# The daily climatology calculating function
# df <- filter(SACTNdaily_v4.1, index == "Port Nolloth/ UWC") # For testing...
daily.clim <- function(df){
  # Add correct column name for make_whole()
  if("date" %in% colnames(df)){
    df$t <- df$date
  }
  # Add doy value for daily clim calculations
  df2 <- make_whole(df)
  # Fill in smaller gaps with linear interpolation
  df2$temp <- zoo::na.approx(df2$temp, maxgap = 5)
  # Create wide data frame
  df2 <- df2 %>%
    # dplyr::filter(date >= clim_start & date <= clim_end) %>% # Currently using all available data
    dplyr::mutate(date = lubridate::year(date)) %>%
    tidyr::spread(date, temp)
  # Fill gaps for time series under one year in length
  doy <- data.frame(doy = 1:366)
  df2 <- merge(df2, doy, by = "doy", all.y = T)
  # Add leap years and days before and after the year for mean windows
  all_NA <- apply(df2[59:61, ], 2, function(x) !all(is.na(x)))
  no_NA <- names(all_NA[all_NA > 0]) # compatibility with zoo < 1.7.13...
  df2[59:61, no_NA] <- zoo::na.approx(df2[59:61, no_NA], maxgap = 1, na.rm = TRUE)
  df2 <- rbind(utils::tail(df2, 5),
               df2, utils::head(df2, 5))
  # Calculate daily climatologies
  seas_clim_year <- rep(NA, nrow(df2))
  for (i in 6:((nrow(df2) - 5))) {
    seas_clim_year[i] <-
      mean(c(t(df2[(i - 5):(i + 5), 2:ncol(df2)])), na.rm = TRUE)
  }
  # seas_clim_year <- apply(as.numeric(df2[,2:ncol(df2)]), 1, function(x,n=5){stats::filter(x, rep(1/n,n), sides = 2)}) # Attempts at not using a for loop...
  # Create output data frame
  clim <-
    data.frame(
      index = df$index[1],
      doy = df2[6:371, 1],
      daily_clim = seas_clim_year[6:371])
  # Additional smoothing
    clim %<>%
      dplyr::mutate(
        daily_clim = raster::movingFun(
          daily_clim,
          n = 31,
          fun = mean,
          type = "around",
          circular = TRUE,
          na.rm = FALSE
        )
      )
    return(clim)
}
# test <- daily.clim(df) # For testing...

# Run it
system.time(SACTNdaily_clim_v4.1 <- ddply(SACTNdaily_v4.1, .(index), daily.clim, .parallel = T)) # 40 seconds

# Cast it wide and add lon/ lat
SACTNdaily_clim_wide_v4.1 <- dcast(SACTNdaily_clim_v4.1, index ~ doy, value.var = "daily_clim")
SACTNdaily_clim_wide_v4.1 %<>%
  group_by(index) %>% 
  mutate(lon = site_list$lon[site_list$index == index][1]) %>%
  mutate(lat = site_list$lat[site_list$index == index][1])
SACTNdaily_clim_wide_v4.1 <- SACTNdaily_clim_wide_v4.1[c(1,368,369,2:367)]
SACTNdaily_clim_wide_v4.1 <- SACTNdaily_clim_wide_v4.1[complete.cases(SACTNdaily_clim_wide_v4.1),]
SACTNdaily_clim_wide_v4.1 <- data.frame(SACTNdaily_clim_wide_v4.1)

# 3. Interpolate at HiRes -------------------------------------------------

# Initial interpolation
daily_clim_hiRes <- array(0, dim = c(length(outCoords$lon), (length(SACTNdaily_clim_wide_v4.1)-3))) # Premake array for following function
for(i in 1:366) {
  daily_clim_hiRes[, i] <- (interpp(x = SACTNdaily_clim_wide_v4.1[, "lon"], y = SACTNdaily_clim_wide_v4.1[, "lat"], SACTNdaily_clim_wide_v4.1[[i+3]],
                      xo = outCoords$lon, yo = outCoords$lat, linear = TRUE, 
                      extrap = FALSE, dupl = "mean"))$z
}
# Additional manual linear interpolation in gaps on the east coast
  # Not filling in gaps on west and south coast as these gaps exist due to intense spatial differences in real temperatures
# daily_clim_hiRes[2,] <- daily_clim_hiRes[3,]+(daily_clim_hiRes[3,]-daily_clim_hiRes[4,])
# daily_clim_hiRes[1,] <- daily_clim_hiRes[2,]+(daily_clim_hiRes[2,]-daily_clim_hiRes[3,])
daily_clim_hiRes[278,] <- mean(daily_clim_hiRes[277,] + daily_clim_hiRes[279,])
daily_clim_hiRes[329:340,] <- apply(daily_clim_hiRes[c(328,341),], 2, function(x){(seq(x[1], x[2], length.out = 14))[2:13]})
daily_clim_hiRes[348,] <- daily_clim_hiRes[347,]+(daily_clim_hiRes[347,]-daily_clim_hiRes[346,])

# Add additional bits and save
daily_clim_hiRes <- data.frame(lon = outCoords$lon, lat = outCoords$lat, daily_clim_hiRes)
colnames(daily_clim_hiRes) <- c("lon","lat", 1:366)
save(daily_clim_hiRes, file = "data/daily_clim_hiRes.Rdata")

# 4. Create climate projections -------------------------------------------

# Calculate real climate trends from 25+ year time series
sites_trend <- data.frame(index = site_list$index[(site_list$length/365.25)>25], temp = 0)
sites_trend <- ddply(sites_trend, .(index), proj.temp, "real", 1, .parallel = T)
sites_trend %<>%
  group_by(index) %>% 
  mutate(lon = site_list$lon[site_list$index == index][1]) %>%
  mutate(lat = site_list$lat[site_list$index == index][1])
sites_trend <- data.frame(sites_trend)

# HiRes interpolation of decadal trends
trend_hiRes <- data.frame(trend = interpp(x = sites_trend[, "lon"], y = sites_trend[, "lat"], sites_trend$temp,
                                    xo = outCoords$lon, yo = outCoords$lat, linear = TRUE, 
                                    extrap = FALSE, dupl = "mean")$z)
  # Not currently filling in any gaps as they mostly exist due to sudden changes in trend size or direction (i.e. + or -)
save(trend_hiRes, file = "data/trend_hiRes.Rdata")

# Project HiRes trends on HiRes data
  # Rather run make this calculation in "proc/kelp.exceedence.R"
# hiRes_real_1 <- daily_clim_hiRes[complete.cases(trend_hiRes$trend),][,3:368] + trend_hiRes$trend[complete.cases(trend_hiRes$trend)]
# save(hiRes_real_1, file = "data/hiRes_real_1.Rdata") # These files are over 700kb, so better to not save them

# Project statis trend on HiRes data
  # Rather run this code in "proc/kelp.exceedence.R"
# hiRes_0.1_1 <- data.frame(lon = daily_clim_hiRes$lon, lat = daily_clim_hiRes$lat, daily_clim_hiRes[,3:368] + 0.1)
