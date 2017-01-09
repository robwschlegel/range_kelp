#############################################################################
###"func/proj.temp.R"
## This script does:
# 1. Uses a given decadal trend or calculates one
# 2. Adds the desired number of decades of change to a climatology
## NB: Just because this function runs on shorter (<30 year) time series, does NOT mean you should trust the results...
  ## You've been warned...
## DEPENDS ON:
library(plyr)
library(dplyr)
library(doMC); doMC::registerDoMC(cores = 4)
source("func/decadal.trend.R")
load("~/SACTN/data/SACTNmonthly_v4.1.Rdata")
## USED BY:
#"proc/climate.projections.R"
## CREATES:
#
#############################################################################

## Testing
# df <- filter(SACTNseas_clim_v4.1, index == "Port Nolloth/ SAWS")
# df <- sites_trend[1,]
# trend <- "real"
# dec <- 3
##

# For data frames with one temperature column and site names
proj.temp <- function(df, trend, dec){
  if(!(is.numeric(trend))){
    df2 <- filter(SACTNmonthly_v4.1, index == df$index[1])
    trend <- decadal.trend(df2)
  }
  df$temp <- df$temp+(trend*dec)
  return(df)
}


## Testing
df <- daily_clim_hiRes
trend <- "in situ"
trend <- 0.1
dec <- 3
##

# For data frames with a full 366 day climatology
proj.temp.hiRes <- function(df, trend, dec){
  df2 <- df[colnames(df) != c("index", "lon", "lat")]
  if(trend == "in situ"){
    df2 <- cbind(df2[complete.cases(trend_hiRes$trend),]+(trend_hiRes$trend[complete.cases(trend_hiRes$trend)]*dec), trend = "in situ", decade = dec)
    df2 <- cbind(df[complete.cases(trend_hiRes$trend), colnames(df)==c("index", "lon", "lat")], df2)
  } else if(is.numeric(trend)){
    df2 <- cbind(df2+(trend*dec), trend = as.character(trend), decade = dec)
    df2 <- cbind(df[,colnames(df)==c("index", "lon", "lat")], df2)
  } else{
    stop("Wat doen jy?")
  }
  return(df2)
}
