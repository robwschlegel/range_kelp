#############################################################################
###"proc/kelp.model.R"
## This script does:
# 1. Load data
# 2. Prep data for modelling
# 3. Initial model run and evaluation
# 4. Initial projections
## DEPENDS ON:
library(plyr)
library(dplyr)
library(biomod2) ## NB: This package masks functions so may interfere with other scripts
## USED BY:
#
## CREATES:
#
#############################################################################

# 1. Load data ------------------------------------------------------------

# Species presence data
slinger <- read.csv("~/range_fish/data/species/slinger.csv")
colnames(slinger) <- c("ID","lon","lat","slinger")
speciesName <- 'slinger'

# Abiotic data
load("~/SACTN/metadata/site_list_v4.1.Rdata")
load("data/SACTNseas_clim_v4.1.Rdata")
load("data/SACTNseas_real_30.Rdata")
load("data/SACTNseas_0.1_30.Rdata")
load("data/sa_bathy.RData")


# 2. Prep data for modelling ----------------------------------------------

# Extract variabes for modelling
resp <- as.matrix(slinger[4]) # Binary species presence
coords <- as.matrix(slinger[2:3]) # lon/ lat of each count

# Add lon/ lat
SACTNseas_clim_v4.1 <- SACTNseas_clim_v4.1 %>%
  group_by(index) %>%
  mutate(lon = site_list$lon[site_list$index == index][1]) %>% 
  mutate(lat = site_list$lat[site_list$index == index][1])
SACTNseas_clim_v4.1 <- data.frame(SACTNseas_clim_v4.1)


# Coerce abiotic data for model use
sa_bathy <- sa_bathy[sa_bathy$depth >= -250,]

# Wide format
SACTNseas <- SACTNseas_clim_v4.1[,c(6,7,4,5)]
SACTNseas <- dcast(SACTNseas, lon + lat ~ seas, value.var = "temp", mean)
# SACTNseas[,1:2] <- apply(SACTNseas[,1:2], 1, round_any, 1)
# sa_bathy[,1:2] <- apply(sa_bathy[,1:2], 1, round_any, 1)
expl <- merge(SACTNseas, sa_bathy, by.x = c("lon", "lat"), all = T)

# Create pseudoabsence points
biomod_data <- BIOMOD_FormatingData(
  resp.var = resp,
  expl.var = expl,
  resp.xy = coords,
  resp.name = speciesName,
  PA.nb.rep = 1,
  PA.nb.absences = 2100,
  PA.strategy = 'sre',
  PA.dist.min = 1,
  PA.dist.max = NULL,
  PA.sre.quant = 0.01)

# check data format
biomod_data

# Check plot of data
plot(biomod_data)
