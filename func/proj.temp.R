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
library(lubridate)
library(doMC); doMC::registerDoMC(cores = 4)
source("func/decadal.trend.R")
load("~/SACTN/data/SACTNmonthly_v4.1.Rdata")
## USED BY:
#"proc/climate.projections.R"
## CREATES:
#
#############################################################################

# Testing
df <- filter(SACTNseas_clim_v4.1, index == "Port Nolloth/ SAWS")
trend <- "real"
dec <- 3

proj.temp <- function(df, trend, dec){
  if(!(is.numeric(trend))){
    df2 <- filter(SACTNmonthly_v4.1, index == df$index[1])
    trend <- decadal.trend(df2)
  }
  df$temp <- df$temp+(trend*dec)
  return(df)
}
