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
library(ggpubr)
library(funModeling)
library(tidyselect)
library(ggcorrplot)
### Ecological Niche Factor Analysis(ENFA)
library(adehabitatHS)
library(ggarrange)
library(CENFA)

rm(list = ls()) #Effacement de toutes les données en mémoire
graphics.off() #Effacement de tous les graphiques en mémoire
##########"
Explorer<-function (blocks, rasterLayer, speciesData, num) {
  # records<-blocks$records
  # records$fold<-1:nrow(records)
  # records <- records[,c(5,1,2,3,4)] %>% 
  #   mutate(calcul= round((test_0 + test_1)*100/(test_0 + test_1+train_1 + train_0),digits = 0))
  # records.p <- ggtexttable(records,rows = NULL, theme = ttheme("mGreen"))
  # 
  polyObj <- blocks$blocks
  folds <- blocks$folds
  kmax <- length(folds)
  species <- blocks$species
  speciesData <- sf::st_as_sf(speciesData)
  samp <- raster::sampleRegular(rasterLayer[[1]], 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  mid <- stats::median(map_df$MAP)
  basePlot <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df, 
                                                       ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP")) + 
    ggplot2::scale_fill_gradient2(low = "darkred", mid = "yellow", 
                                  high = "darkgreen", midpoint = mid) + ggplot2::guides(fill = FALSE) + 
    ggplot2::theme_bw() + ggplot2::labs(x = "", y = "")
  trainSet <- unlist(folds[[num]][1])
  testSet <- unlist(folds[[num]][2])
  training <- speciesData[trainSet, ]
  testing <- speciesData[testSet, ]
  plotPoly <- polyObj[polyObj$folds ==num,]               
  plotPoly <- sf::st_as_sf(plotPoly)
  if (is.null(species)) {
    if (class(blocks) == "SpatialBlock") {
      ptr <- basePlot + ggplot2::geom_sf(data = plotPoly, 
                                         color = "red", fill = "orangered4", alpha = 0.04, 
                                         size = 0.2) + ggplot2::geom_sf(data = training, 
                                                                        alpha = 0.7, color = "blue", size = 2) + 
        ggplot2::ggtitle("Training set") + theme(plot.title = element_text(hjust = 0.5, size = 10))
      pts <- basePlot + ggplot2::geom_sf(data = plotPoly, 
                                         color = "red", fill = "orangered4", alpha = 0.04, 
                                         size = 0.2) + ggplot2::geom_sf(data = testing, 
                                                                        alpha = 0.7, color = "blue", size = 2) + 
        ggplot2::ggtitle("Testing set") + theme(plot.title = element_text(hjust = 0.5, size = 10))
    }
    else {
      ptr <- basePlot + ggplot2::geom_sf(data = training, 
                                         alpha = 0.7, color = "blue", size = 2) + 
        ggplot2::ggtitle("Training set") + theme(plot.title = element_text(hjust = 0.5, size = 10))
      pts <- basePlot + ggplot2::geom_sf(data = testing, 
                                         alpha = 0.7, color = "blue", size = 2) + 
        ggplot2::ggtitle("Testing set") + theme(plot.title = element_text(hjust = 0.5, size = 10))
    }
  }
  else {
    if (class(blocks) == "SpatialBlock") {
      ptr <- basePlot + ggplot2::geom_sf(data = plotPoly, 
                                         color = "red", fill = "orangered4", alpha = 0.04, 
                                         size = 0.2) + ggplot2::geom_sf(data = training, 
                                                                        ggplot2::aes(color = get(species)), show.legend = "point", 
                                                                        alpha = 0.7, size = 2) + ggplot2::labs(color = species) + 
        ggplot2::ggtitle("Training set") + theme(plot.title = element_text(hjust = 0.5, size = 10))
      pts <- basePlot + ggplot2::geom_sf(data = plotPoly, 
                                         color = "red", fill = "orangered4", alpha = 0.04, 
                                         size = 0.2) + ggplot2::geom_sf(data = testing, 
                                                                        ggplot2::aes(color = get(species)), show.legend = "point", 
                                                                        alpha = 0.7, size = 2) + ggplot2::labs(color = species) + 
        ggplot2::ggtitle("Testing set") + theme(plot.title = element_text(hjust = 0.5, size = 10))
    }
    else {
      ptr <- basePlot + ggplot2::geom_sf(data = training, 
                                         ggplot2::aes(color = get(species)), show.legend = "point", 
                                         alpha = 0.7, size = 2) + ggplot2::labs(color = species) + 
        ggplot2::ggtitle("Training set") + theme(plot.title = element_text(hjust = 0.5, size = 10))
      pts <- basePlot + ggplot2::geom_sf(data = testing, 
                                         ggplot2::aes(color = get(species)), show.legend = "point", 
                                         alpha = 0.7, size = 2) + ggplot2::labs(color = species) + 
        ggplot2::ggtitle("Testing set") + theme(plot.title = element_text(hjust = 0.5, size = 10))
    }
  }
  #ptr_pts<-ggpubr::ggarrange(ptr, pts,common.legend = TRUE)
  #plot(cowplot::plot_grid(ptr, pts))
  #plot(ggpubr::ggarrange(ptr,records.p, pts))
  plot(ggpubr::ggarrange(ptr, pts,common.legend = TRUE))
}
################end function
summarise_fold<-function(sb){
  records<-sb$records
  records$fold<-1:nrow(records)
  records <- records[,c(5,1,2,3,4)] %>% 
    mutate(Pourcentage= round((test_0 + test_1)*100/(test_0 + test_1+train_1 + train_0),digits = 0))
  plot(ggpubr::ggtexttable(records,rows = NULL, theme = ttheme("mGreen")))
}
##########end function
# import raster data
l1<-list.files("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
worldclim<-stack(l1)
e <- extent(-16.57, -16.33, 14.45 , 14.65)
worldclim.crop <- crop(worldclim,e)

#######"import species
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
#worldclim.crop <- crop(worldclim,extent(Species))
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
Base_Espece$Faidherbia_albida<-as.factor(Base_Espece$Faidherbia_albida)
#Balanites aegyptiaca
Base_Espece$Balanites_aegyptiaca<-as.factor(if_else(Base_Espece$Species =="Balanites aegyptiaca","1","0"))
#Anogeissus leiocarpus
Base_Espece$Anogeissus_leiocarpus<-as.factor(if_else(Base_Espece$Species =="Anogeissus leiocarpus","1","0"))
#Adansonia digitata
Base_Espece$Adansonia_digitata<-as.factor(if_else(Base_Espece$Species =="Adansonia digitata","1","0"))
#Acacia nilotica
Base_Espece$Acacia_nilotica<-as.factor(if_else(Base_Espece$Species =="Acacia nilotica","1","0"))
Base_Espece_df<-st_drop_geometry(Base_Espece)
Base_Faidherbia_Z<-Base_Espece_df[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia")
Base_Balanites_Z<-Base_Espece_df[,c("xcoord","ycoord","Balanites_aegyptiaca")] 
names(Base_Balanites_Z)<-c("lon","lat","Balanites")
Base_Anogeissus_Z<-Base_Espece_df[,c("xcoord","ycoord","Anogeissus_leiocarpus")] 
names(Base_Anogeissus_Z)<-c("lon","lat","Anogeissus")
Base_Adansonia_Z<-Base_Espece_df[,c("xcoord","ycoord","Adansonia_digitata")] 
names(Base_Adansonia_Z)<-c("lon","lat","Adansonia")
Base_Acacia_Z<-Base_Espece_df[,c("xcoord","ycoord","Acacia_nilotica")] 
names(Base_Acacia_Z)<-c("lon","lat","Acacia")
#prédicteurs des espèces
#Faidherbia albida
#bio1,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio15,bio16,bio17,bio18
Varbio<-worldclim.crop
Varbio <- dropLayer(Varbio, 6) #??? bio14
Varbio <- dropLayer(Varbio, 10) #??? bio19
Varbio <- dropLayer(Varbio, 10) #??? bio2
Varbio <- dropLayer(Varbio, 10) #??? bio3
predictors<-Varbio
#Balanites_aegyptiaca
#bio1,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio15,bio16,bio17,bio18
Varbio<-worldclim.crop
Varbio <- dropLayer(Varbio, 6) #??? bio14
Varbio <- dropLayer(Varbio, 10) #??? bio19
Varbio <- dropLayer(Varbio, 10) #??? bio2
Varbio <- dropLayer(Varbio, 10) #??? bio3
predictorsB<-Varbio
#Anogeissus_leiocarpus
#bio1,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio13,bio15,bio17,bio18
Varbio<-worldclim.crop
Varbio <- dropLayer(Varbio, 6) #??? bio14
Varbio <- dropLayer(Varbio, 10) #??? bio19
Varbio <- dropLayer(Varbio, 10) #??? bio2
Varbio <- dropLayer(Varbio, 10) #??? bio3
Varbio <- dropLayer(Varbio, 4) #??? bio12
Varbio <- dropLayer(Varbio, 6) #??? bio16
predictorsAno<-Varbio
#Adansonia.digitata
#bio1,bio4,bio6,bio8,bio9,bio10,bio11,bio13,bio15,bio17,bio18
Varbio<-worldclim.crop
Varbio <- dropLayer(Varbio, 6) #??? bio14
Varbio <- dropLayer(Varbio, 10) #??? bio19
Varbio <- dropLayer(Varbio, 10) #??? bio2
Varbio <- dropLayer(Varbio, 10) #??? bio3
Varbio <- dropLayer(Varbio, 13) #??? bio7
Varbio <- dropLayer(Varbio, 4) #??? bio12
Varbio <- dropLayer(Varbio, 6) #??? bio16
predictorsAdan<-Varbio
#Acacia_nilotica
#bio1,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio15,bio16,bio17,bio18
Varbio<-worldclim.crop
Varbio <- dropLayer(Varbio, 6) #??? bio14
Varbio <- dropLayer(Varbio, 10) #??? bio19
Varbio <- dropLayer(Varbio, 10) #??? bio2
Varbio <- dropLayer(Varbio, 10) #??? bio3
predictorsAca<-Varbio
# make a SpatialPointsDataFrame object from data.frame
pa_dataF <- st_as_sf(Base_Faidherbia_Z, coords = c("lon","lat"), crs = crs(predictors))
pa_dataB <- st_as_sf(Base_Balanites_Z, coords = c("lon","lat"), crs = crs(predictorsB))
pa_dataAno <- st_as_sf(Base_Anogeissus_Z, coords = c("lon","lat"), crs = crs(predictorsAno))
pa_dataAdan <- st_as_sf(Base_Adansonia_Z, coords = c("lon","lat"), crs = crs(predictorsAdan))
pa_dataAca <- st_as_sf(Base_Acacia_Z, coords = c("lon","lat"), crs = crs(predictorsAca))
# plot species data on the map
plot(predictors[[1]]) # plot raster data
plot(pa_dataF[which(pa_dataF$Faidherbia==1), ], pch = 16, col="red", add=TRUE) # add presence points
plot(pa_dataF[which(pa_dataF$Faidherbia==0), ], pch = 16, col="blue", add=TRUE) # add absence points
legend(x=500000, y=8250000, legend=c("Presence","Absence"), col=c(2, 4), pch=c(16,16), bty="n")
# investigate spatial autocorrelation in raster covariates
# this helps to choose a suitable size for spatial blocks
sac1<-spatialAutoRange(rasterLayer = predictors, # raster file
                        doParallel = F,
                        sampleNumber = 672, # number of cells to be used
                        
                        showPlots = TRUE)
table1<-sac1$rangeTable
table1$Species<-"Faidherbia albida"
#stable <- desc_statby(table,  measure.var = "range")
#stable.p <- ggtexttable(stable1,rows = NULL, theme = ttheme("mOrange"))
a<-sac1$plots$barchart + ggtitle("","") 
bloc1<-sac1$plots$mapplot
sac2<-spatialAutoRange(rasterLayer = predictorsB, # raster file
                       doParallel = F,
                       sampleNumber = 672, # number of cells to be used
                       
                       showPlots = TRUE)
table2<-sac2$rangeTable
table2$Species<-"Balanites aegyptiaca"
sac3<-spatialAutoRange(rasterLayer = predictorsAno, # raster file
                       doParallel = F,
                       sampleNumber = 672, # number of cells to be used
                       
                       showPlots = TRUE)
table3<-sac3$rangeTable
table3$Species<-"Anogeissus leiocarpus"
sac4<-spatialAutoRange(rasterLayer = predictorsAdan, # raster file
                       doParallel = F,
                       sampleNumber = 672, # number of cells to be used
                       
                       showPlots = TRUE)
table4<-sac4$rangeTable
table4$Species<-"Adansonia digitata"
sac5<-spatialAutoRange(rasterLayer = predictorsAca, # raster file
                       doParallel = F,
                       sampleNumber = 672, # number of cells to be used
                       
                       showPlots = TRUE)
table5<-sac5$rangeTable
table5$Species<-"Acacia nilotica"
tableRange<-rbind(table1,table2,table3,table4,table5)
stable <- desc_statby(tableRange,  measure.var = "range",grps = "Species")
# Choix de 4 colonnes à conserver dans le tableau
stable1 <- stable[, c("Species", "median","min","max")]
stable1.p <- ggtexttable(stable1,rows = NULL, theme = ttheme("mGreen"))
a<-sac1$plots$barchart + ggtitle("Autocorrelation range : Faidherbia albida") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10)) + xlab("prédicteurs")
b<-sac2$plots$barchart + ggtitle("Autocorrelation range : Balanites aegyptiaca") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10)) + xlab("prédicteurs")
c<-sac3$plots$barchart + ggtitle("Autocorrelation range : Anogeissus leiocarpus") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10)) + xlab("prédicteurs")
d<-sac4$plots$barchart + ggtitle("Autocorrelation range : Adansonia digitata") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10)) + xlab("prédicteurs")
e<-sac5$plots$barchart + ggtitle("Autocorrelation range : Acacia nilotica") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10)) + xlab("prédicteurs")
#b<-sac$plots$mapplot
ggarrange(a,b,c,d,e,stable1.p)
stable2 <- stable[, c("Species","median","sd","cv")]
stable2.p <- ggtexttable(stable2,rows = NULL, theme = ttheme("mGreen"))
a<-sac1$plots$mapplot  + ggtitle("Spatial blocks : Faidherbia albida") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10))
b<-sac2$plots$mapplot + ggtitle("Spatial blocks : Balanites aegyptiaca") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10))
c<-sac3$plots$mapplot + ggtitle("Spatial blocks : Anogeissus leiocarpus") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10))
d<-sac4$plots$mapplot + ggtitle("Spatial blocks : Adansonia digitata") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10))
e<-sac5$plots$mapplot + ggtitle("Spatial blocks : Acacia nilotica") + theme(plot.title = element_text(hjust = 0.5, size = 10),plot.subtitle = element_text(hjust = 0.5, size = 10))
ggarrange(a,b,c,d,e,stable2.p)
plot(sac4$variograms[[1]])
# The plotted block size is based on the *median* of the spatial autocorrelation ranges. 
# This could be as the **minimum block size** for 
# creating spatially separated folds.
# Variograms are computed taking a number of random points 
# (`5000` as default) from each input raster file
# class of the output result
class(sac)
# summary statistics of the output
summary(sac)
## Blocking strategies
### Spatial block
# spatial blocking by specified range with random assignment
# sb <- spatialBlock(speciesData = pa_dataF,
#                    species = "Faidherbia",
#                    rasterLayer = predictors,
#                    theRange = 19531, # size of the blocks
#                    k = 4,
#                    selection = "random",
#                    iteration = 100, # find evenly dispersed folds
#                    biomod2Format = TRUE,
#                    xOffset = 0, # shift the blocks horizontally
#                    yOffset = 0)

sb1 <- spatialBlock(speciesData = pa_dataF, # presence-background data
                    species = "Faidherbia",
                    rasterLayer = predictors,
                    rows = 6,
                    cols = 6,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)
summarise_fold(sb1)
# biomodTable1<-sb1$biomodTable
# foldID1<-sb1$foldID
# table(foldID1)
sb2 <- spatialBlock(speciesData = pa_dataB, # presence-background data
                    species = "Balanites",
                    rasterLayer = predictorsB,
                    rows = 6,
                    cols = 6,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)
summarise_fold(sb2)
sb3 <- spatialBlock(speciesData = pa_dataAno, # presence-background data
                    species = "Anogeissus",
                    rasterLayer = predictorsAno,
                    rows = 6,
                    cols = 6,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)
summarise_fold(sb3)
sb4 <- spatialBlock(speciesData = pa_dataAdan, # presence-background data
                    species = "Adansonia",
                    rasterLayer = predictorsAdan,
                    rows = 6,
                    cols = 6,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)
summarise_fold(sb4)
sb5 <- spatialBlock(speciesData = pa_dataAca, # presence-background data
                    species = "Acacia",
                    rasterLayer = predictors,
                    rows = 6,
                    cols = 6,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)
summarise_fold(sb5)
foldExplorer(sb1, predictors, pa_dataF)
e1<-Explorer(sb1, predictors, pa_dataF,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bioe1.png",e1)
e2<-Explorer(sb2, predictorsB, pa_dataB,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bioe2.png",e2)
e3<-Explorer(sb3, predictorsAno, pa_dataAno,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bioe3.png",e3)
e4<-Explorer(sb4, predictorsAdan, pa_dataAdan,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bioe4.png",e4)
e5<-Explorer(sb5, predictorsAca, pa_dataAca,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\bioe5.png",e5)

# adding points on saptialBlock plot
library(ggplot2)
sb3$plots + geom_sf(data = pa_dataAno, alpha = 0.5) 
## Evaluating SDMs with block cross-validation: examples
### Evaluating presence-absence models
#### randomForest
# loading the libraries
library(randomForest)
library(precrec)

# extract the raster values for the species points as a dataframe
mydata <- raster::extract(predictors, pa_dataF, df = TRUE)
mydata2 <- raster::extract(predictorsB, pa_dataB, df = TRUE)
# adding species column to the dataframe
mydata$Faidherbia <- as.factor(pa_dataF$Faidherbia)
mydata2$Balanites <- as.factor(pa_dataB$Balanites)
# remove extra column (ID)
mydata <- mydata[,-1]
mydata2 <- mydata2[,-1]
# extract the foldIDs in SpatialBlock object 
# created in the previous section
# the folds (list) works for all three blocking strategies
folds <- sb1$folds
folds2 <- sb2$folds
folds3 <- sb3$folds
folds4 <- sb4$folds
folds5 <- sb5$folds
# create a data.frame to store the prediction of each fold (record)
# testTable <- pa_dataB
# testTable$pred <- NA
trainSet <- unlist(folds[[1]][1]) # training set indices
testSet <- unlist(folds[[1]][2]) # testing set indices
trainSet2 <- unlist(folds2[[1]][1]) # training set indices
testSet2 <- unlist(folds2[[1]][2]) # testing set indices
trainSet3 <- unlist(folds3[[1]][1]) # training set indices
testSet3 <- unlist(folds3[[1]][2]) # testing set indices
trainSet4 <- unlist(folds4[[1]][1]) # training set indices
testSet4 <- unlist(folds4[[1]][2]) # testing set indices
trainSet5 <- unlist(folds5[[1]][1]) # training set indices
testSet5 <- unlist(folds5[[1]][2]) # testing set indices
# rf <- randomForest(Faidherbia~., mydata[trainSet, ], ntree = 250) # model fitting on training set
# testTable$pred[testSet] <- predict(rf, mydata[testSet, ], type = "prob")[,2] # predict the test set
# rf2 <- randomForest(Balanites~., mydata2[trainSet, ], ntree = 250) # model fitting on training set
# testTable$pred[testSet] <- predict(rf2, mydata2[testSet, ], type = "prob")[,2] # predict the test set
# # calculate Area Under the ROC and PR curves and plot the result
# precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Faidherbia)
# precrec_obj2 <- evalmod(scores = testTable$pred, labels = testTable$Balanites)
# autoplot(precrec_obj2)
# auc(precrec_obj)
# pr <- predict(predictors, rf2)
# #ggR_Soil(pr)
# par(mfrow=c(1,2))
# plot(pr, main='Random Forest, regression')
# erf <- evaluate(occtest, bgtest, rf)
# erf
###############

maxent()
## Loading required namespace: rJava
## This is MaxEnt version 3.4.1
train<-Base_Balanites_Z[trainSet2, ]
p<-train %>% 
  filter(Balanites==1)
p<-p[,c("lon","lat")]
a<-train %>% 
  filter(Balanites==0)
a<-a[,c("lon","lat")]
test<-Base_Balanites_Z[testSet2, ]
occtest<-test %>% 
  filter(Balanites==1)
occtest<-occtest[,c("lon","lat")] 
bgtest<-test %>% 
  filter(Balanites==0)
bgtest<-bgtest[,c("lon","lat")]
#####################
#xm <- maxent(predictors, p,a) OK
#xm2 <- maxent(predictorsB, p,a) OK
#xm3 <- maxent(predictorsAno, p,a) ok
#xm4 <- maxent(predictorsAdan, p,a) ok
#xm5 <- maxent(predictorsAca, p,a) ok
par(mfrow=c(2,3))
plot(xm, main="Faidherbia albida",xlab="Pourcentage") 
plot(xm2, main="Balanites aegyptiaca",xlab="Pourcentage") 
plot(xm3, main="Anogeissus leiocarpus",xlab="Pourcentage") 
plot(xm4, main="Adansonia digitata",xlab="Pourcentage") 
plot(xm5, main="Acacia nilotica",xlab="Pourcentage") 
########response
response(xm,var=c("bio6","bio13","bio15"))
response(xm2,var=c("bio4","bio11","bio13"))
response(xm3,var=c("bio7","bio13","bio18"))
response(xm4,var=c("bio6","bio11","bio15"))
response(xm5,var=c("bio6","bio16","bio18"))
#px <- predict(predictors, xm) OK
 #px2 <- predict(predictorsB, xm2)
# px3 <- predict(predictorsAno, xm3) ok
# px4 <- predict(predictorsAdan, xm4) ok
# px5 <- predict(predictorsAca, xm5) ok
par(mfrow=c(2,3))
plot(px, main='Faidherbia albida', xlab="Longitude",ylab="Latitude")  
plot(px2, main='Balanites aegyptiaca', xlab="Longitude",ylab="Latitude") 
plot(px3, main='Anogeissus leiocarpus', xlab="Longitude",ylab="Latitude") 
plot(px4, main='Adansonia digitata', xlab="Longitude",ylab="Latitude") 
plot(px5, main='Acacia nilotica', xlab="Longitude",ylab="Latitude") 

e <- dismo::evaluate(occtest, bgtest, xm2, predictorsB)
e
# tr <- threshold(e, 'spec_sens') 
# 0.590345
# tr2 <- threshold
# 0.6574919
# tr3 <- threshold(e, 'spec_sens')
# 0.6605981
# tr4 <- threshold(e, 'spec_sens') 
# 0.6164791
# tr5 <- threshold(e, 'spec_sens') 
# 0.6600134
##### PLOT
par(mfrow=c(2,3))
plot(px > tr, main='Faidherbia albida', xlab="Longitude",ylab="Latitude")  
plot(px2 > tr2, main='Balanites aegyptiaca', xlab="Longitude",ylab="Latitude") 
plot(px3 > tr3, main='Anogeissus leiocarpus', xlab="Longitude",ylab="Latitude") 
plot(px4 > tr4, main='Adansonia digitata', xlab="Longitude",ylab="Latitude") 
plot(px5 > tr5, main='Acacia nilotica', xlab="Longitude",ylab="Latitude") 
###########
ggR(px)
par(mfrow=c(2,2))
plot(e, 'ROC')
plot(e, 'kappa')
plot(e, 'FPR')
plot(e, 'prevalence')
#############)"










