# loading the package
library(blockCV)
# loading raster library
library(raster)
library(sf)

# import raster data
awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))
# import presence-absence species data
PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
# make a SpatialPointsDataFrame object from data.frame
pa_data <- st_as_sf(PA, coords = c("x", "y"), crs = crs(awt))
# see the first few rows
pa_data
# plot species data on the map
plot(awt[[1]]) # plot raster data
plot(pa_data[which(pa_data$Species==1), ], pch = 16, col="red", add=TRUE) # add presence points
plot(pa_data[which(pa_data$Species==0), ], pch = 16, col="blue", add=TRUE) # add absence points
legend(x=500000, y=8250000, legend=c("Presence","Absence"), col=c(2, 4), pch=c(16,16), bty="n")
## Blocking strategies
### Spatial block
# spatial blocking by specified range with random assignment
sb <- spatialBlock(speciesData = pa_data,
                   species = "Species",
                   rasterLayer = awt,
                   theRange = 70000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)
# adding points on saptialBlock plot
library(ggplot2)

sb$plots + geom_sf(data = pa_data, alpha = 0.5)
### Buffering
# buffering with presence-absence data
bf1 <- buffering(speciesData = pa_data,
                 theRange = 70000,
                 species = "Species", # to count the number of presences and absences/backgrounds
                 spDataType = "PA", # presence-absence  data type
                 progress = TRUE)
### Environmental block
# environmental clustering
eb <- envBlock(rasterLayer = awt,
               speciesData = pa_data,
               species = "Species",
               k = 5,
               standardization = "standard", # rescale variables between 0 and 1
               rasterBlock = FALSE,
               numLimit = 50)
## The effective range of spatial autocorrelation
sac <- spatialAutoRange(rasterLayer = awt,
                        sampleNumber = 5000,
                        doParallel = TRUE,
                        showPlots = TRUE)
a<-sac$plots$barchart + ggtitle("","") 
b<-sac$plots$mapplot
ggarrange(a,b)
plot(sac$variograms[[1]])
# The plotted block size is based on the *median* of the spatial autocorrelation ranges. 
# This could be as the **minimum block size** for 
# creating spatially separated folds.
# Variograms are computed taking a number of random points 
# (`5000` as default) from each input raster file
# class of the output result
class(sac)
# summary statistics of the output
summary(sac)
library(automap)
sac$variograms
plot(sac$variograms[[1]])
## Visualisation tools
# explore generated folds
foldExplorer(blocks = sb, 
             rasterLayer = awt, 
             speciesData = pa_data)
# explore the block size
rangeExplorer(rasterLayer = awt) # the only mandatory input

# add species data to add them on the map
rangeExplorer(rasterLayer = awt,
              speciesData = pa_data,
              species = "Species",
              rangeTable = NULL,
              minRange = 30000, # limit the search domain
              maxRange = 100000)
## Evaluating SDMs with block cross-validation: examples
### Evaluating presence-absence models
#### randomForest
# loading the libraries
library(randomForest)
library(precrec)

# extract the raster values for the species points as a dataframe
mydata <- raster::extract(awt, pa_data, df = TRUE)
# adding species column to the dataframe
mydata$Species <- as.factor(pa_data$Species)
# remove extra column (ID)
mydata <- mydata[,-1]
# extract the foldIDs in SpatialBlock object 
# created in the previous section
# the folds (list) works for all three blocking strategies
folds <- bf1$folds

# create a data.frame to store the prediction of each fold (record)
testTable <- pa_data
testTable$pred <- NA
for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(Species~., mydata[trainSet, ], ntree = 250) # model fitting on training set
  testTable$pred[testSet] <- predict(rf, mydata[testSet, ], type = "prob")[,2] # predict the test set
}
# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Species)

autoplot(precrec_obj)

############
# loading the library
library(biomod2)
# species occurrences
DataSpecies <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
# the name of studied species
myRespName <- "Species"
# the presence/absences data for our species
myResp <- as.numeric(DataSpecies[,myRespName])
# the XY coordinates of species data
myRespXY <- DataSpecies[,c("x","y")]
# change the RasterBrick to RasterStack
awt <- stack(awt)
# 1. Formatting Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = awt, # explanatory raster data
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     na.rm = TRUE)
plot(myBiomodData)
# 2. Defining the folds for DataSplitTable
# note that biomodTable should be used here not folds
DataSplitTable <- sb$biomodTable # use generated folds from spatialBlock in previous section
# 3. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()

# 4. Model fitting
myBiomodModelOut <- BIOMOD_Modeling( myBiomodData,
                                     models = c('GLM','GBM'),
                                     models.options = myBiomodOption,
                                     DataSplitTable = DataSplitTable, # blocking folds
                                     VarImport = 0,
                                     models.eval.meth = c('ROC','TSS'),
                                     do.full.models=FALSE,
                                     modeling.id="test")

# 5. Model evaluation
# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
myBiomodModelEval["ROC","Testing.data",,,]
###################
## plot evaluation models score graph
### by models
models_scores_graph( myBiomodModelOut,
                     by = 'models',
                     metrics = c('ROC','TSS') )

models_scores_graph( myBiomodModelOut,
                     by = 'cv_run',
                     metrics = c('ROC','TSS') )

# get_variables_importance
get_variables_importance(myBiomodModelOut)
##' 4. Build ensemble-models that will be taken as reference
myBiomodEM <- BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut,
                                       chosen.models = 'all',
                                       em.by = 'all',
                                       eval.metric = c('ROC'),
                                       eval.metric.quality.threshold = c(0.7),
                                       prob.mean = TRUE,
                                       prob.median = TRUE)

##' 4. Plot response curves
##' 4.1 Load the models for which we want to extract the predicted
##' response curves
myGLMs <- BIOMOD_LoadModels(myBiomodModelOut, models = 'GLM')
#myRFs <- BIOMOD_LoadModels(myBiomodModelOut, models = 'RF')
#myGAMs <- BIOMOD_LoadModels(myBiomodModelOut, models = 'GAM')
#myCTAs <- BIOMOD_LoadModels(myBiomodModelOut, models = 'CTA')
myGBMs <- BIOMOD_LoadModels(myBiomodModelOut, models = 'GBM')
##' 4.2 plot 2D response plots
#models = myGLMs[1]
myRespPlot2D <-
  response.plot2(
    models = myGLMs[1],
    Data = get_formal_data(myBiomodModelOut, 'expl.var'),
    show.variables = get_formal_data(myBiomodModelOut,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    col = c("blue", "red","black","green","orange"),
    legend = TRUE,
    data_species = get_formal_data(myBiomodModelOut, 'resp.var')
  )

##' 4.2 plot 3D response plots
###' here only for a lone model (i.e "VulpesVulpes_PA1_AllData_GLM")
myRespPlot3D <-
  response.plot2(
    models = myGLMs[1],
    Data = get_formal_data(myBiomodModelOut, 'expl.var'),
    show.variables = get_formal_data(myBiomodModelOut, 'expl.var.names'),
    do.bivariate = TRUE,
    fixed.var.metric = 'median',
    data_species = get_formal_data(myBiomodModelOut, 'resp.var'),
    display_title = FALSE
  )

##' 5. Projection on future environmental conditions
##' 
myExpl_fut<-awt
myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = myExpl_fut,
                                        proj.name = 'future',
                                        selected.models = 'all',
                                        binary.meth = 'TSS',
                                        compress = FALSE,
                                        build.clamping.mask = TRUE)    


BIOMOD_EnsembleForecasting(projection.output=myBiomodProjection,
                           EM.output=myBiomodEM,
                           binary.meth='TSS') 

##' 6. load binary projections
consensusBin <- raster::stack('Species/proj_future/proj_future_Species_ensemble_TSSbin.grd')
projectionsBin <- raster::stack('Species/proj_future/proj_future_Species_TSSbin.grd')    
plot(consensusBin)    
plot(projectionsBin)
ggR(projectionsBin,2, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "Probabilité", colours = terrain.colors(10))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
##' 7. load  projections
consensus <- raster::stack('Species/proj_future/proj_future_Species_ensemble.grd')
projections <- raster::stack('Species/proj_future/proj_future_ClampingMask.grd')
plot(consensus)    
plot(projections)
