#############################################################################
###"prep/daily.clim.interp.R"
## This script does:
# 1. Load data
# 2. Calculate daily climatologies
# 3. Interpolate at 0.1
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
# "data/SACTNdaily_clim_wide_v4.1.Rdata"
# "data/SACTNdaily_clim_wide_10y.Rdata"
# "data/daily_clim_pixel.Rdata"
# "data/trend_pixel.Rdata"
#############################################################################

# 1. Load data ------------------------------------------------------------

# Site list
load("~/SACTN/metadata/site_list_v4.1.Rdata")
site_list_10y <- filter(site_list, length >= (365.25*10))
site_list_10y <- filter(site_list_10y, index != "Hout Bay/ DEA")

# Daily temperatures
load("~/SACTNraw/data/SACTNdaily_v4.1.Rdata")

# Screen out time series under 10 years in length
SACTNdaily_10y <- filter(SACTNdaily_v4.1, index %in% site_list_10y$index)

## NB: No longer using hiRes coords in favour of the hand made coastal pixel coords
# Hi-res coordinates
# outCoords <- read.table("~/tempssa_v3.0/coords_to_extract/interp_HiRes_coords_348_sites.txt", header = FALSE)
# colnames(outCoords) <- c("lon", "lat")
##

# Coastal 0.1 degree pixels
load("setupParams/sa_coast.Rdata")


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
SACTNdaily_10y <- ddply(SACTNdaily_10y, .(index), daily.clim, .parallel = T)

## Cast it wide and add lon/ lat
# ALL data
SACTNdaily_clim_wide_v4.1 <- dcast(SACTNdaily_clim_v4.1, index ~ doy, value.var = "daily_clim")
SACTNdaily_clim_wide_v4.1 %<>%
  group_by(index) %>% 
  mutate(lon = site_list$lon[site_list$index == index][1]) %>%
  mutate(lat = site_list$lat[site_list$index == index][1])
SACTNdaily_clim_wide_v4.1 <- SACTNdaily_clim_wide_v4.1[c(1,368,369,2:367)]
SACTNdaily_clim_wide_v4.1 <- SACTNdaily_clim_wide_v4.1[complete.cases(SACTNdaily_clim_wide_v4.1),]
SACTNdaily_clim_wide_v4.1 <- data.frame(SACTNdaily_clim_wide_v4.1)
colnames(SACTNdaily_clim_wide_v4.1) <- c("index", "lon", "lat", 1:366)
# 10 year time series only
SACTNdaily_clim_wide_10y <- dcast(SACTNdaily_10y, index ~ doy, value.var = "daily_clim")
SACTNdaily_clim_wide_10y %<>%
  group_by(index) %>% 
  mutate(lon = site_list$lon[site_list_10y$index == index][1]) %>%
  mutate(lat = site_list$lat[site_list_10y$index == index][1])
SACTNdaily_clim_wide_10y <- SACTNdaily_clim_wide_10y[c(1,368,369,2:367)]
SACTNdaily_clim_wide_10y <- SACTNdaily_clim_wide_10y[complete.cases(SACTNdaily_clim_wide_10y),]
SACTNdaily_clim_wide_10y <- data.frame(SACTNdaily_clim_wide_10y)
colnames(SACTNdaily_clim_wide_10y) <- c("index", "lon", "lat", 1:366)

# Save it
save(SACTNdaily_clim_wide_v4.1, file = "data/SACTNdaily_clim_wide_v4.1.Rdata")
save(SACTNdaily_clim_wide_10y, file = "data/SACTNdaily_clim_wide_10y.Rdata")


# 3. Interpolate at 0.1 ---------------------------------------------------

load("data/SACTNdaily_clim_wide_10y.Rdata")

# Initial interpolation
daily_clim_pixel <- array(0, dim = c(nrow(sa_coast), (length(SACTNdaily_clim_wide_10y)-3))) # Premake array for following function
for(i in 1:366) {
  daily_clim_pixel[, i] <- (interpp(x = SACTNdaily_clim_wide_v4.1[, "lon"], y = SACTNdaily_clim_wide_v4.1[, "lat"], SACTNdaily_clim_wide_v4.1[[i+3]],
                      xo = sa_coast$X, yo = sa_coast$Y, linear = TRUE, 
                      extrap = FALSE, dupl = "mean"))$z
}

# Additional manual linear interpolation in gaps
  # Great care was taken when interpolating temperatures on the west and south coasts to ensure fidelity to the sharp temperature gradients
# Missing pixels north of Port Nolloth
daily_clim_pixel[13,] <- (daily_clim_pixel[12,]+daily_clim_pixel[14,])/2
daily_clim_pixel[1,] <- daily_clim_pixel[12,]-((daily_clim_pixel[13,]-daily_clim_pixel[12,])*11)
daily_clim_pixel[2:11,] <- apply(daily_clim_pixel[c(1,12),], 2, function(x){(seq(x[1], x[2], length.out = 12))[2:11]})
# Missing pixels around saldanha bay
daily_clim_pixel[67,] <- daily_clim_pixel[66,]+0.3
daily_clim_pixel[68,] <- daily_clim_pixel[67,]+0.3
daily_clim_pixel[69,] <- daily_clim_pixel[68,]+0.3
daily_clim_pixel[70,] <- daily_clim_pixel[69,]+0.3
daily_clim_pixel[74,] <- daily_clim_pixel[75,]+0.3
daily_clim_pixel[73,] <- daily_clim_pixel[74,]+0.3
# Missing pixels around Cape Point
daily_clim_pixel[89,] <- (daily_clim_pixel[88,]+daily_clim_pixel[90,])/2
daily_clim_pixel[91,] <- (daily_clim_pixel[90,]+daily_clim_pixel[92,])/2
daily_clim_pixel[93,] <- daily_clim_pixel[92,]+(daily_clim_pixel[92,]-daily_clim_pixel[91,])
daily_clim_pixel[94,] <- daily_clim_pixel[93,]+(daily_clim_pixel[93,]-daily_clim_pixel[92,])
daily_clim_pixel[95,] <- daily_clim_pixel[96,]+(daily_clim_pixel[96,]-daily_clim_pixel[97,]) # The actual point, treated as a False Bay pixel
# Mssing pixels from Gansbaai to Cape Point
daily_clim_pixel[117:122,] <- apply(daily_clim_pixel[c(116,123),], 2, function(x){(seq(x[1], x[2], length.out = 8))[2:7]})
# Random missing pixel
daily_clim_pixel[192,] <- (daily_clim_pixel[191,] + daily_clim_pixel[193,])/2
# Bunches of missing bits from PE to Durban
daily_clim_pixel[215:216,] <- apply(daily_clim_pixel[c(214,217),], 2, function(x){(seq(x[1], x[2], length.out = 4))[2:3]})
daily_clim_pixel[219,] <- (daily_clim_pixel[218,] + daily_clim_pixel[220,])/2
daily_clim_pixel[223,] <- (daily_clim_pixel[222,] + daily_clim_pixel[224,])/2
daily_clim_pixel[226,] <- (daily_clim_pixel[225,] + daily_clim_pixel[227,])/2
daily_clim_pixel[228:229,] <- apply(daily_clim_pixel[c(227,230),], 2, function(x){(seq(x[1], x[2], length.out = 4))[2:3]})
daily_clim_pixel[231,] <- (daily_clim_pixel[230,] + daily_clim_pixel[232,])/2
daily_clim_pixel[234,] <- (daily_clim_pixel[233,] + daily_clim_pixel[235,])/2
daily_clim_pixel[236:237,] <- apply(daily_clim_pixel[c(235,238),], 2, function(x){(seq(x[1], x[2], length.out = 4))[2:3]})
daily_clim_pixel[239,] <- (daily_clim_pixel[238,] + daily_clim_pixel[240,])/2
daily_clim_pixel[241,] <- (daily_clim_pixel[240,] + daily_clim_pixel[242,])/2
daily_clim_pixel[244,] <- (daily_clim_pixel[243,] + daily_clim_pixel[245,])/2
daily_clim_pixel[246,] <- (daily_clim_pixel[245,] + daily_clim_pixel[247,])/2
daily_clim_pixel[248:251,] <- apply(daily_clim_pixel[c(247,252),], 2, function(x){(seq(x[1], x[2], length.out = 6))[2:5]})
daily_clim_pixel[253,] <- (daily_clim_pixel[252,] + daily_clim_pixel[254,])/2
daily_clim_pixel[255,] <- (daily_clim_pixel[254,] + daily_clim_pixel[256,])/2
daily_clim_pixel[257,] <- (daily_clim_pixel[256,] + daily_clim_pixel[258,])/2
daily_clim_pixel[259,] <- (daily_clim_pixel[258,] + daily_clim_pixel[260,])/2
daily_clim_pixel[261,] <- (daily_clim_pixel[260,] + daily_clim_pixel[262,])/2
daily_clim_pixel[263,] <- (daily_clim_pixel[262,] + daily_clim_pixel[264,])/2
daily_clim_pixel[270:284,] <- apply(daily_clim_pixel[c(269,285),], 2, function(x){(seq(x[1], x[2], length.out = 17))[2:16]})
daily_clim_pixel[286,] <- (daily_clim_pixel[285,] + daily_clim_pixel[287,])/2
daily_clim_pixel[289,] <- (daily_clim_pixel[288,] + daily_clim_pixel[290,])/2
daily_clim_pixel[291,] <- (daily_clim_pixel[290,] + daily_clim_pixel[292,])/2
# North of Sodwana
daily_clim_pixel[322:335,] <- apply(daily_clim_pixel[c(321,336),], 2, function(x){(seq(x[1], x[2], length.out = 16))[2:15]})
daily_clim_pixel[339,] <- (daily_clim_pixel[338,] + daily_clim_pixel[340,])/2
daily_clim_pixel[349,] <- daily_clim_pixel[340,] + 0.9
daily_clim_pixel[341:348,] <- apply(daily_clim_pixel[c(340,349),], 2, function(x){(seq(x[1], x[2], length.out = 10))[2:9]})

# Add additional bits and save
daily_clim_pixel <- data.frame(lon = sa_coast$X, lat = sa_coast$Y, daily_clim_pixel)
colnames(daily_clim_pixel) <- c("lon","lat", 1:366)
daily_clim_pixel <- cbind(index = 1:length(daily_clim_pixel$lon), daily_clim_pixel)
save(daily_clim_pixel, file = "data/daily_clim_pixel.Rdata")


# 4. Create climate projections -------------------------------------------

# Calculate real climate trends from 25+ year time series
sites_trend <- data.frame(index = site_list$index[(site_list$length/365.25)>25], temp = 0)
sites_trend <- ddply(sites_trend, .(index), proj.temp, "in situ", 1, .parallel = T)
sites_trend %<>%
  group_by(index) %>% 
  mutate(lon = site_list$lon[site_list$index == index][1]) %>%
  mutate(lat = site_list$lat[site_list$index == index][1])
sites_trend <- data.frame(sites_trend)

# Coastal 0.1 degree pixel interpolation of decadal trends
trend_pixel <- data.frame(trend = interpp(x = sites_trend[, "lon"], y = sites_trend[, "lat"], sites_trend$temp,
                                    xo = sa_coast$X, yo = sa_coast$Y, linear = TRUE, 
                                    extrap = FALSE, dupl = "mean")$z)

# Additional manual linear interpolation in gaps
  # Great care was taken when interpolating trends to ensure fidelity to sharp temperature gradients
  # When uncertain, trends were regressed towards the 0.1C standard
# Missing pixels north of Port Nolloth
trend_pixel[13,] <- (trend_pixel[12,]+trend_pixel[14,])/2
trend_pixel[1,] <- trend_pixel[13,]-((trend_pixel[12,]-trend_pixel[13,])*11)
trend_pixel[2:11,] <- (seq(trend_pixel[1,], trend_pixel[12,], length.out = 12))[2:11]
# Missing pixels around saldanha bay
trend_pixel[67:70,] <- (seq(trend_pixel[71,], trend_pixel[66,], length.out = 6))[5:2]
trend_pixel[73:74,] <- (seq(trend_pixel[75,], trend_pixel[72,], length.out = 4))[3:2]
# Missing pixels around Cape Point
trend_pixel[88:96,] <- (seq(trend_pixel[87,], trend_pixel[97,], length.out = 11))[2:10]
# Missing pixels around Betty's Bay
trend_pixel[106:107,] <- (seq(trend_pixel[108,], trend_pixel[105,], length.out = 4))[3:2]
# Mssing pixels from Gansbaai to Cape Point
trend_pixel[116:122,] <- (seq(trend_pixel[123,], trend_pixel[115,], length.out = 9))[8:2]
# Missing pixels past Tsitsikamma
trend_pixel[188:192,] <- (seq(trend_pixel[193,], trend_pixel[187,], length.out = 7))[6:2]
trend_pixel[194,] <- (trend_pixel[193,]+trend_pixel[195,])/2
# Before Cape Recife
trend_pixel[201:203,] <- (seq(trend_pixel[200,], trend_pixel[204,], length.out = 5))[2:4]
# PE to Cwebe
trend_pixel[214:234,] <- (seq(trend_pixel[235,], trend_pixel[213,], length.out = 23))[22:2]
# Spaterrings of missing bits up to Durban-ish
trend_pixel[236:237,] <- (seq(trend_pixel[238,], trend_pixel[235,], length.out = 4))[3:2]
trend_pixel[239,] <- (trend_pixel[238,]+trend_pixel[240,])/2
trend_pixel[241,] <- (trend_pixel[240,]+trend_pixel[242,])/2
trend_pixel[243:246,] <- (seq(trend_pixel[247,], trend_pixel[242,], length.out = 6))[5:2]
trend_pixel[248:259,] <- (seq(trend_pixel[247,], trend_pixel[260,], length.out = 14))[2:13]
trend_pixel[261,] <- (trend_pixel[260,]+trend_pixel[262,])/2
trend_pixel[263,] <- (trend_pixel[262,]+trend_pixel[264,])/2
trend_pixel[265,] <- (trend_pixel[264,]+trend_pixel[266,])/2
trend_pixel[267,] <- (trend_pixel[266,]+trend_pixel[268,])/2
trend_pixel[269:272,] <- (seq(trend_pixel[273,], trend_pixel[268,], length.out = 6))[5:2]
trend_pixel[274,] <- (trend_pixel[273,]+trend_pixel[275,])/2
trend_pixel[276,] <- (trend_pixel[275,]+trend_pixel[277,])/2
trend_pixel[278,] <- (trend_pixel[277,]+trend_pixel[279,])/2
trend_pixel[280,] <- (trend_pixel[279,]+trend_pixel[281,])/2
trend_pixel[282,] <- (trend_pixel[281,]+trend_pixel[283,])/2
trend_pixel[284,] <- (trend_pixel[283,]+trend_pixel[285,])/2
trend_pixel[286,] <- (trend_pixel[285,]+trend_pixel[287,])/2
trend_pixel[289,] <- (trend_pixel[288,]+trend_pixel[290,])/2
trend_pixel[291,] <- (trend_pixel[290,]+trend_pixel[292,])/2
# Sodwana to Mozam
trend_pixel[349,] <- 0.1
trend_pixel[322:348,] <- (seq(trend_pixel[349,], trend_pixel[321,], length.out = 29))[28:2]

# Save
save(trend_pixel, file = "data/trend_pixel.Rdata")

