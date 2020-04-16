library(tidyverse)
library(SDMSelect)
library(dplyr)
library(sf)
library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)
library(rgdal)
library(spdep)
library(ggplot2)
library(ggspatial)
library(blockCV)
library(randomForest)
library(rJava)
library(dismo)
# import raster data
l1<-list.files("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
worldclim<-stack(l1)
awt1 <- crop(worldclim,extent(Species))
awt1<-stack(awt1)
# import presence-absence species data
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
Base_Espece$Faidherbia_albida<-as.factor(Base_Espece$Faidherbia_albida)
data_df<-st_drop_geometry(Base_Espece)
Base_Faidherbia_Z<-data_df[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia")
Base<-Base_Faidherbia_Z
#Transform data as SpatialPointDataFrame
sp::coordinates(Base_Faidherbia_Z) <-~lon+lat
sp::proj4string(Base_Faidherbia_Z) <-"+proj=longlat +datum=WGS84"
dataF<-CovarExtract(x=Base_Faidherbia_Z,cov.paths = l1)
DataModelF<-dataF@data
PA<-Base_Faidherbia_Z
# make a sf object from data.frame
pa_data <- sf::st_as_sf(PA, coords = c("lon", "lat"), crs = raster::crs(awt))

# investigate spatial autocorrelation in raster covariates
# this helps to choose a suitable size for spatial blocks
range<-spatialAutoRange(rasterLayer = awt1, # raster file
                        doParallel = F,
                        sampleNumber = 462, # number of cells to be used
                        
                        showPlots = TRUE)
nombre_block<-round(range$range,0) #range - the suggested range, which is the median of all calculated ranges
# summary statistics of the output
summary(range)
library(automap)
plot(range$variograms[[1]])

# spatial blocking by specified range and random assignment
set.seed(1994)
sb1 <- spatialBlock(speciesData = pa_data,
                    species = "Faidherbia",
                    rasterLayer = awt1,
                    rows = 10,
                    cols = 10,
                    k = 5,
                    selection = "random",
                    biomod2Format = TRUE)
foldExplorer(sb1, awt, pa_data)
sb1$plots + geom_sf(data = pa_data, alpha = 0.5)

library(maxnet)
library(precrec)
 
  # extract the raster values for the species points as a dataframe
dataF<-CovarExtract(x=Base_Faidherbia_Z,cov.paths = l1)
DataModelF<-dataF@data
  mydata <- DataModelF
  # adding species column to the dataframe
  mydata$Faidherbia <- as.factor(pa_data$Faidherbia)
  bf1 <- buffering(speciesData = pa_data,
                                      theRange = 70000,
                                     species = "Faidherbia", # to count the number of presences and absences/backgrounds
                                     spDataType = "PA", # presence-absence  data type
                                     progress = TRUE)

  # extract the foldIDs in SpatialBlock object
  # created in the previous section
  # the folds (list) works for all three blocking strategies
  folds <- sb1$folds

  # create a data.frame to store the prediction of each fold (record)
  testTable <- pa_data
  testTable$pred <- NA

  for(k in seq_len(length(folds))){
    # extracting the training and testing indices
    # this way works with folds list (but not foldID)
    trainSet <- unlist(folds[[k]][1]) # training set indices
    testSet <- unlist(folds[[k]][2]) # testing set indices
    rf <- randomForest(Faidherbia~., mydata[trainSet, ], ntree = 250) # model fitting on training set
    testTable$pred[testSet] <- predict(rf, mydata[testSet, ], type = "prob")[,2] # predict the test set
  }

  # calculate Area Under the ROC and PR curves and plot the result
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Faidherbia)

  autoplot(precrec_obj)

  # species occurrences
  Data<-data_df[,c("xcoord","ycoord","Faidherbia_albida")] 
  names(Data)<-c("lon","lat","Faidherbia")
   DataSpecies <- Data
  #  # the name of studied species
    myRespName <- "Faidherbia"
    # the presence/absences data for our species
    myResp <- as.numeric(DataSpecies[,myRespName])
    # the XY coordinates of species data
    myRespXY <- DataSpecies[,c("lon","lat")]
  
  #  awt <- stack(awt)
  #
  #  # 1. Formatting Data
    library(biomod2)
    myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                        expl.var = awt1, # explanatory raster data
                                        resp.xy = myRespXY,
                                        resp.name = myRespName,
                                        na.rm = TRUE)
  #
  #  # 2. Defining the folds for DataSplitTable
  #  # note that biomodTable should be used here not folds
    DataSplitTable <- sb1$biomodTable # use generated folds from spatialBlock in previous section
    
  #  # 3. Defining Models Options using default options.
    myBiomodOption <- BIOMOD_ModelingOptions()
  #
  #  # 4. Model fitting
    myBiomodModelOut <- BIOMOD_Modeling( myBiomodData,
                                         models = c('GLM','MARS','GBM'),
                                         models.options = myBiomodOption,
                                         DataSplitTable = DataSplitTable, # blocking folds
                                        VarImport = 0,
                                        models.eval.meth = c('ROC'),
                                      do.full.models=FALSE,
                                      modeling.id="test")
  #

  ## ---- eval=FALSE---------------------------------------------------------
  #  # 5. Model evaluation
  #  # get all models evaluation
    myBiomodModelEval <- get_evaluations(myBiomodModelOut)
    myBiomodModelEval["ROC","Testing.data",,,]
  #
#################################
    # species occurrences
    Data<-data_df[,c("xcoord","ycoord","Faidherbia_albida")] 
    names(Data)<-c("lon","lat","Faidherbia")
    DataSpecies <- Data
    #  # the name of studied species
    myRespName <- "Faidherbia"
    # the presence/absences data for our species
    myResp <- as.numeric(DataSpecies[,myRespName])
    # the XY coordinates of species data
    myRespXY <- DataSpecies[,c("lon","lat")]
    
    # Environmental variables extracted from BIOCLIM
    myExpl<-stack(bio1,bio18,bio13,bio16,bio3,bio12)
#plot(myExpl)    
    ##' 1. Formatting Data
    myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                         expl.var = myExpl,
                                         resp.xy = myRespXY,
                                         resp.name = myRespName)
    plot(myBiomodData)
    ##' 2. Defining Models Options using default options.
    myBiomodOption <- BIOMOD_ModelingOptions()    
    ##' 3. Doing Modelisation
    myBiomodModelOut <- BIOMOD_Modeling( myBiomodData,
                                         models = c('CTA','RF','GLM','GAM'),
                                         models.options = myBiomodOption,
                                         NbRunEval=5,
                                         DataSplit=70,
                                         models.eval.meth = c('ROC','TSS'),
                                         do.full.models = FALSE,
                                         rescal.all.models=T,
                                         modeling.id='test')
     ######get output
    # get_predictions
    # get_calib_lines
     get_evaluations(myBiomodModelOut)
    # get_variables_importance
     get_options(myBiomodModelOut)
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
    myRFs <- BIOMOD_LoadModels(myBiomodModelOut, models = 'RF')
    myGAMs <- BIOMOD_LoadModels(myBiomodModelOut, models = 'GAM')
    myCTAs <- BIOMOD_LoadModels(myBiomodModelOut, models = 'CTA')
    ##' 4.2 plot 2D response plots
    #models = myGLMs[1]
    myRespPlot2D <-
      response.plot2(
        models = myGLMs,
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
    ##' all the values used to produce this plot are stored into the
    ##' returned object you can redo plots by yourself and customised
    ##' them
    dim(myRespPlot2D)
    expl.name<-myRespPlot2D$expl.name
    expl.val<-myRespPlot2D$expl.val
    pred.name<-myRespPlot2D$pred.name
    pred.val<-myRespPlot2D$pred.val
    dimnames(myRespPlot2D)
    dim(myRespPlot3D)
    dimnames(myRespPlot3D)    
    ##' 5. Projection on future environmental conditions
    ##' 
    myExpl_fut<-myExpl
    myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                            new.env = myExpl_fut,
                                            proj.name = 'future',
                                            selected.models = 'all',
                                            binary.meth = 'TSS',
                                            compress = FALSE,
                                            build.clamping.mask = TRUE)    

    plot(myBiomodProjection)
    BIOMOD_EnsembleForecasting(projection.output=myBiomodProjection,
                               EM.output=myBiomodEM,
                               binary.meth='TSS') 
    
    ##' 6. load binary projections
    consensusBin <- raster::stack('Faidherbia/proj_future/proj_future_Faidherbia_ensemble_TSSbin.grd')
    projectionsBin <- raster::stack('Faidherbia/proj_future/proj_future_Faidherbia_TSSbin.grd')    
plot(consensusBin)    
plot(projectionsBin)
ggR(projectionsBin,2, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "Probabilité", colours = terrain.colors(10))  +
  theme_bw() + xlab("Longitude") + ylab("Latitude")
##' 7. build a ref state based on ensemble-models
ref <- sampleRandom(subset(consensusBin, 1, drop=T), size=462, sp=T, na.rm=T)
##' 8. autoatic creation of groups matrix
find_groups <- function(diff_by_pix){
  data.set <- sapply(names(diff_by_pix),biomod2:::.extractModelNamesInfo,info='data.set')
  run.eval <- sapply(names(diff_by_pix),biomod2:::.extractModelNamesInfo,info='run.eval')
  models <- sapply(names(diff_by_pix),biomod2:::.extractModelNamesInfo,info='models')
  return(rbind(data.set,run.eval,models))
}
groups <- find_groups(projectionsBin)
##' 9. plot ProbDensFunct graphs
ProbDensFunc(initial = ref,
             projections = projectionsBin,
             plothist=TRUE,
             cvsn=TRUE,
             groups=groups,
             resolution=2,
             filename=NULL,
             lim=c(0.5,0.8,0.95))
##############""
## plot evaluation models score graph
### by models
gg1 <- models_scores_graph( myBiomodModelOut,
                            by = 'models',
                            metrics = c('ROC','TSS') )
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\ROC_TSS_Modeles.png",gg1)
## we see a influence of model selected on models capabilities
## e.g. RF are much better than SRE
### by cross validation run
gg2 <- models_scores_graph( myBiomodModelOut,
                            by = 'cv_run',
                            metrics = c('ROC','TSS') )
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\ROC_TSS_VC.png",gg2)
## there is no difference in models quality if we focus on
## cross validation sampling
### some graphical customisations
gg1_custom <-
  gg1 +
  ggtitle("Diff between RF and SRE evaluation scores") + ## add title
  scale_colour_manual(values=c("green", "blue")) ## change colors
gg1_custom
