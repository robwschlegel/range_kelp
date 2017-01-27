#############################################################################
###"proc/kelp.model.R"
## This script does:
# 1. Load data
# 2. Prep data for modelling
# 3. Initial model run and evaluation
# 4. Initial projections
# 5. Project into the future
## DEPENDS ON:
library(plyr)
library(dplyr)
library(raster)
library(reshape2)
## USED BY:
#
## CREATES:
#
#############################################################################

# 1. Load data ------------------------------------------------------------

# Species meta-data
kelp_meta <- read.csv("~/Private/range_kelp/metadata/58sites.csv")

# Seaweed presence data
kelp <- read.csv("~/Private/range_kelp/data/seaweed.csv")
colnames(kelp) <- c("species", seq(1,58))
kelp <- melt(kelp, id.vars = "species", variable.name = "index", value.name = "presence")
kelp <- merge(kelp, kelp_meta, by = "index")
kelp <- kelp[order(kelp[,1]),]

# Specific species
max <- droplevels(filter(kelp, species %in% c("ECKMAX")))
pal <- droplevels(filter(kelp, species %in% c("LAMPAL")))

# Site list
load("~/SACTN/metadata/site_list_v4.1.Rdata")

# Abiotic raster data
  # The file naming convention is: '1/2_3.asc'
    # 1 = The decades of temperature projection applied before the stats were calculated
    # 2 = The statistic in the raster
    # 3 = The type of projection used
      # NB: Note that the raster package fills in blank spaces with '_'
      # This makes the naming convention somewhat less clear
expl <- raster::stack(
  "data/raster/in situ/0/Sumax.asc",
  "data/raster/in situ/0/Sumin.asc",
  "data/raster/in situ/0/Amax.asc",
  "data/raster/in situ/0/Amin.asc",
  "data/raster/in situ/0/Wmax.asc",
  "data/raster/in situ/0/Wmin.asc",
  "data/raster/in situ/0/Spmax.asc",
  "data/raster/in situ/0/Spmin.asc",
  "data/raster/in situ/0/above 21.asc",
  "data/raster/in situ/0/above 20.asc",
  "data/raster/in situ/0/below 16.asc",
  "data/raster/in situ/0/below 15.asc")#,
  # "data/raster/bathy.asc") # Not currently using bathymetry
plot(expl)

# 2. Prep data for modelling ----------------------------------------------

# Extract variabes for modelling
resp_max <- as.matrix(max[3]) # Binary species presence
resp_pal <- as.matrix(pal[3]) 
coords <- as.matrix(max[4:5]) # lon/ lat of each count. Same for all species.

library(biomod2) ## NB: This package masks functions so may interfere with other scripts

# Create pseudoabsence points
biomod_data <- BIOMOD_FormatingData(
  resp.var = resp_max,
  expl.var = expl,
  resp.xy = coords,
  resp.name = as.character(max$species[1]),
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


# 3. Initial model run and evaluation -------------------------------------

# Model options
biomod_option <- BIOMOD_ModelingOptions(GLM = list(type = 'polynomial', interaction.level = 1))

# Computing models
# model_out <- BIOMOD_Modeling( # No need to run a second time
#   biomod_data,
#   models = c('GAM','GLM','MAXENT.Tsuruoka','GBM','MARS','CTA','RF'),
#   models.options = biomod_option,
#   NbRunEval = 10,
#   DataSplit = 70,
#   Yweights = NULL,
#   VarImport = 3,
#   models.eval.meth = c('TSS','ROC', 'KAPPA'),
#   SaveObj = TRUE,
#   rescal.all.models = TRUE)
# save(model_out, file = "data/model_out_max.Rdata")
load("data/model_out_max.Rdata")

# Have a look at the outputs
model_out

# Relative importance of exploratory variables
variable_importances <- get_variables_importance(model_out)
variable_importances

# Get all models evaluation
model_eval <- get_evaluations(model_out)
dimnames(model_eval)

model_eval[,,,"Full",]


# 4. Initial projections --------------------------------------------------

# prints out whichever other variable you need (See dimensions to pick which evaluations) 
# best: model_eval["ROC",,,"Full","PA1"]

model_out@models.computed

# Create projections
# projection <- BIOMOD_Projection(
#   modeling.output = model_out,
#   new.env = expl,
#   proj.name = 'GGM',
#   xy.new.env = coords,
#   selected.models = model_out@models.computed[71:77],
#   Bin.trans = TRUE,
#   slot = model_out@models.computed,
#   binary.meth ='TSS',
#   compress = 'xz',
#   clamping.mask = F,
#   SaveObj = TRUE)
# save(projection, file = "data/projection_max_0.Rdata")
load("data/projection_max_0.Rdata")

# Plot projections
plot(projection)

# Get projections
current_projection <- get_predictions(projection)
current_projection

# current_GAM <- raster(current_projection, layer = "ECKMAX_PA1_Full_GAM") # doesn't work
current_GLM <- raster(current_projection, layer = "ECKMAX_PA1_Full_GLM")
current_MAXENT <- raster(current_projection, layer = "ECKMAX_PA1_Full_MAXENT.Tsuruoka")
current_GBM <- raster(current_projection, layer = "ECKMAX_PA1_Full_GBM")
current_MARS <- raster(current_projection, layer = "ECKMAX_PA1_Full_MARS")
current_CTA <- raster(current_projection, layer = "ECKMAX_PA1_Full_CTA")
current_RF <- raster(current_projection, layer = "ECKMAX_PA1_Full_RF")

# Save individual projections
# writeRaster(current_GAM,
#             filename = 'data/ECKMAX/proj_GGM/GAM.asc',
#             format = "ascii", 
#             overwrite = TRUE ) # GAM doesn't run...

writeRaster(current_GLM,
            filename = 'data/ECKMAX/proj_GGM/GLM.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_MAXENT,
            filename = 'data/ECKMAX/proj_GGM/MAXENT.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_GBM,
            filename = 'data/ECKMAX/proj_GGM/GBM.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_MARS,
            filename = 'data/ECKMAX/proj_GGM/MARS.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_CTA,
            filename = 'data/ECKMAX/proj_GGM/CTA.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_RF,
            filename = 'data/ECKMAX/proj_GGM/RF.asc',
            format = "ascii", 
            overwrite = TRUE )


# 5. Project into the future ----------------------------------------------

# Currently only looking at 50 year projections
expl_50 <- raster::stack(
  "data/raster/in situ/5/Sumax.asc",
  "data/raster/in situ/5/Sumin.asc",
  "data/raster/in situ/5/Amax.asc",
  "data/raster/in situ/5/Amin.asc",
  "data/raster/in situ/5/Wmax.asc",
  "data/raster/in situ/5/Wmin.asc",
  "data/raster/in situ/5/Spmax.asc",
  "data/raster/in situ/5/Spmin.asc",
  "data/raster/in situ/5/above 21.asc",
  "data/raster/in situ/5/above 20.asc",
  "data/raster/in situ/5/below 16.asc",
  "data/raster/in situ/5/below 15.asc")#,
# "data/raster/bathy.asc") # Not currently using bathymetry
plot(expl_50)

# Run 50 in situ projections
# projection_50 <- BIOMOD_Projection(
#   modeling.output = model_out,
#   new.env = expl_50,
#   proj.name='GGM50',
#   xy.new.env = coords,
#   selected.models = model_out@models.computed[71:77],
#   Bin.trans = TRUE,
#   slot = model_out@models.computed,
#   binary.meth ='TSS',
#   compress = 'xz',
#   clamping.mask = F,
#   SaveObj = TRUE)
# save(projection_50, file = "data/projection_max_50.Rdata")
load("data/projection_max_50.Rdata")

# Plot projections
plot(projection_50)

# Get projections
current_projection_50 <- get_predictions(projection_50)
current_projection_50

# current_GAM <- raster(current_projection_50, layer = "ECKMAX_PA1_Full_GAM") # doesn't work
current_GLM <- raster(current_projection_50, layer = "ECKMAX_PA1_Full_GLM")
current_MAXENT <- raster(current_projection_50, layer = "ECKMAX_PA1_Full_MAXENT.Tsuruoka")
current_GBM <- raster(current_projection_50, layer = "ECKMAX_PA1_Full_GBM")
current_MARS <- raster(current_projection_50, layer = "ECKMAX_PA1_Full_MARS")
current_CTA <- raster(current_projection_50, layer = "ECKMAX_PA1_Full_CTA")
current_RF <- raster(current_projection_50, layer = "ECKMAX_PA1_Full_RF")

# Save individual projections
# writeRaster(current_GAM,
#             filename = 'data/ECKMAX/proj_GGM_50/GAM.asc',
#             format = "ascii", 
#             overwrite = TRUE ) # GAM doesn't run...

writeRaster(current_GLM,
            filename = 'data/ECKMAX/proj_GGM_50/GLM.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_MAXENT,
            filename = 'data/ECKMAX/proj_GGM/MAXENT.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_GBM,
            filename = 'data/ECKMAX/proj_GGM_50/GBM.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_MARS,
            filename = 'data/ECKMAX/proj_GGM_50/MARS.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_CTA,
            filename = 'data/ECKMAX/proj_GGM_50/CTA.asc',
            format = "ascii", 
            overwrite = TRUE )

writeRaster(current_RF,
            filename = 'data/ECKMAX/proj_GGM_50/RF.asc',
            format = "ascii", 
            overwrite = TRUE )

