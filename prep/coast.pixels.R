#############################################################################
###"prep/coast.pixels.R"
## This script does:
# 1. Load data
# 2. Create 0.1 degree coastal pixels
## DEPENDS ON:
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(doMC); doMC::registerDoMC(4)
## USED BY:
# Pretty much every script relies on these coordinates
## CREATES:
# "meta_data/sa_coast.Rdata"
#############################################################################

# 1. Load data ------------------------------------------------------------

# Site list
load("~/SACTN/metadata/site_list_v4.1.Rdata")

# Map bits
source("~/SA_map/theme.R")
load("~/SA_map/sa_shore.Rdata")
load("~/SA_map/sa_provinces_new.RData")
load("~/SA_map/africa_borders.Rdata")

# Map coords
sa_lats <- c(-36, -26); sa_lons <- c(14, 34)


# 2. Create 0.1 degree coastal pixels -------------------------------------

# Create a rough line of the coast
sa_coast <- sa_shore[sa_shore$PID == 1,4:5] # remove islands and extra columns
sa_coast %<>%
  mutate(X = round_any(X, 0.1)) %>% 
  mutate(Y = round_any(Y, 0.1))
sa_coast <- base::unique(sa_coast)

# Hack off the non-SA bits
sa_coast <- sa_coast[-c(1:40),]
sa_coast <- sa_coast[-c(350:364),]
row.names(sa_coast) <- NULL

## Manually change a few spots for better coastal fidelity
# Bring Saldanha pixels out from land
sa_coast[70,1] <- 17.9
sa_coast[72:73,1] <- 18.0
# Correct Cape Point
sa_coast[94,1] <- 18.4
# Correct east coast of False Bay
sa_coast[101:104,1] <- 18.8
# Bit near Betty's Bay
sa_coast[109:110,2] <- -34.4
# Cape Point
# sa_coast[123:124,2] <- -34.9 # Better not to
# De Hoop
sa_coast[127,1] <- 20.3
# Stilbaai
sa_coast[133:135,2] <- -34.5
# Not sure...
sa_coast[157:158,2] <- -34.1
# Tsitsikamma
sa_coast[169:170,1] <- c(23.1, 23.2)
sa_coast[169:170,2] <- -34.1
sa_coast[183,2] <- -34.1
# Durban...ish
sa_coast[323,1] <- 32.1

# Test plot
sa_point <- sa_coast[321,] # Used for testing...
ggplot() + bw_update +
  # Map bits
  geom_polygon(data = sa_shore, aes(x = X, y = Y, group = PID), show.legend = FALSE, fill = "grey60") +
  geom_path(data = sa_provinces_new, aes(x = lon, y = lat, group = group), size = 0.5, colour = "grey40") +
  geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), size = 1.0, colour = "black") +
  # Pixels
  geom_path(data = sa_coast, aes(x = X, y = Y), colour = "red", size = 0.1) +
  geom_point(data = sa_point, aes(x = X, y = Y), colour = "blue") +
  # Minutia
  scale_y_continuous(breaks = seq(-34,-28,2)) +
  scale_x_continuous(breaks = seq(18,30,4)) +
  coord_map(xlim = sa_lons, ylim = sa_lats, projection = "mercator") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey30"),
        legend.justification = c(1,0), legend.position = c(0.55, 0.40),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box = "horizontal")
ggsave("test.pdf", width = 10)

# Save coastal 0.1 degree pixels
save(sa_coast, file = "setupParams/sa_coast.Rdata")
