#############################################################################
###"func/decadal.trend.R"
## This script does:
# 1. Runs a GLS AR2 model on a monthly time series
# 2. Provides a decadal trend
## NB: Just because this function runs on shorter (<30 year) time series, does NOT mean you should trust the results...
  ## You've been warned...
## DEPENDS ON:
require(mgcv)
library(plyr)
library(dplyr)
library(lubridate)
library(doMC); doMC::registerDoMC(cores = 4)
## USED BY:
#"func/site.list.R"
## CREATES:
#
#############################################################################

decadal.trend <- function(df) {
  # df$date <- floor_date(df$date, "month")
  # df <- ddply(df, .(index, date), summarise,
  #             temp = mean(temp, na.rm = T), .parallel = T)
  if(!("year" %in% colnames(df))){
    df$year <- year(df$date)
  }
  if(!("num" %in% colnames(df))){
    df$num <- seq(1, length(df$date))
  }
    dt <- round(as.numeric(coef(gls(
      temp ~ num, correlation = corARMA(form = ~ 1 | year, p = 2),
      method = "REML", data = df, na.action = na.exclude
    ))[2] * 120), 3)
  return(dt)
}
