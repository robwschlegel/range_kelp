#############################################################################
###"prep/seasonal.climatologies.R"
## This script does:
# 1. Load SACTN monthly, clim and meta-data
# 2. Create seasonal min/ max climatologies
# 3. Create projected temperature data frames
## DEPENDS ON:
library(plyr)
library(dplyr)
library(reshape2)
library(doMC); doMC::registerDoMC(cores = 4)
source("func/proj.temp.R")
## USED BY:
#
## CREATES:
# "data/SACTNseas_clim_v4.1.Rdata"
# "data/SACTNseas_real_30.Rdata"
# "data/SACTNseas_0.1_30.Rdata"
#############################################################################


# 1. Load SACTN monthly, clim and meta-data -------------------------------

load("~/SACTN/data/SACTNmonthly_v4.1.Rdata")
load("~/SACTN/data/SACTNclim_v4.1.Rdata")
load("~/SACTN/metadata/site_list_v4.1.Rdata")


# 2. Create different data frames for different projected trends ----------

# Create seasonal min/ max climatologies
SACTNseas_clim_v4.1 <- SACTNmonthly_v4.1
SACTNseas_clim_v4.1$date <- month(SACTNseas_clim_v4.1$date, label = TRUE, abbr = TRUE)
SACTNseas_clim_v4.1$seas <- NA
SACTNseas_clim_v4.1$seas[SACTNseas_clim_v4.1$date %in% c("Jan","Feb","Mar")] <- "Summer"
SACTNseas_clim_v4.1$seas[SACTNseas_clim_v4.1$date %in% c("Apr","May","Jun")] <- "Autumn"
SACTNseas_clim_v4.1$seas[SACTNseas_clim_v4.1$date %in% c("Jul","Aug","Sep")] <- "Winter"
SACTNseas_clim_v4.1$seas[SACTNseas_clim_v4.1$date %in% c("Oct","Nov","Dec")] <- "Spring"
SACTNseas_clim_v4.1 <- ddply(SACTNseas_clim_v4.1, .(site, src, index), summarise, .parallel = T,
              Sumin = min(temp[seas == "Summer"], na.rm = T),
              Sumax = max(temp[seas == "Summer"], na.rm = T),
              Amin = min(temp[seas == "Autumn"], na.rm = T),
              Amax = max(temp[seas == "Autumn"], na.rm = T),
              Wmin = min(temp[seas == "Winter"], na.rm = T),
              Wmax = max(temp[seas == "Winter"], na.rm = T),
              Spmin = min(temp[seas == "Spring"], na.rm = T),
              Spmax = max(temp[seas == "Spring"], na.rm = T))
  ## NB: If you have loaded the 'biomod2' package, melt will not function properly
SACTNseas_clim_v4.1 <- melt(SACTNseas_clim_v4.1, id.vars = c("site","src","index"),
                            variable.name = "seas", value.name = "temp")
SACTNseas_clim_v4.1 <- SACTNseas_clim_v4.1[order(SACTNseas_clim_v4.1$index),]
row.names(SACTNseas_clim_v4.1) <- NULL
save(SACTNseas_clim_v4.1, file = "data/SACTNseas_clim_v4.1.Rdata")


# 3. Create projected temperature data frames -----------------------------
SACTNseas_real_30 <- ddply(SACTNseas_clim_v4.1, .(index), proj.temp, "real", 3, .parallel = T)
save(SACTNseas_real_30, file = "data/SACTNseas_real_30.Rdata")
SACTNseas_0.1_30 <- ddply(SACTNseas_clim_v4.1, .(index), proj.temp, 0.1, 3, .parallel = T)
save(SACTNseas_0.1_30, file = "data/SACTNseas_0.1_30.Rdata")
