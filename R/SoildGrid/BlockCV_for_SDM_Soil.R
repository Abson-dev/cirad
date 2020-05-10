ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("raster", "rasterVis", "RStoolbox","maptools",
              "rgdal","spdep","ggspatial","blockCV","randomForest","rJava","dismo",
              "ggpubr","funModeling","tidyselect","ggcorrplot",
              "adehabitatHS","CENFA")
ipak(packages)



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

ggR_Predict<-function(RasterLayer){
  ggR(RasterLayer,geom_raster = TRUE,ggLayer = F) +
    scale_fill_gradientn(name = "Probabilité", colours = rev(terrain.colors(10)))  +
    theme_bw() + xlab("Longitude") + ylab("Latitude") +
    ggtitle(label = names(RasterLayer)) + theme(plot.title = element_text(hjust = 0.5, size = 10))
  
}
#######"import species
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
#worldclim.crop <- crop(worldclim,extent(Species))
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-as.factor(if_else(Base_Espece$Species =="Faidherbia albida","1","0"))
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
########################## Variables sur le Sol
# import raster data
lsoil<-list.files("D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\",patt="\\.tif")
lsoil<-sprintf("D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\%s",lsoil)
e <- extent(-16.57, -16.33, 14.45 , 14.65)
AETI<-raster(lsoil[1])
AETI.crop<-crop(AETI,e)
names(AETI.crop)<-"AETI"
CLYPPT<-raster(lsoil[2])
CLYPPT.crop<-crop(CLYPPT,e)
names(CLYPPT.crop)<-"CLYPPT"
ORCDRC<-raster(lsoil[3])
ORCDRC.crop<-crop(ORCDRC,e)
names(ORCDRC.crop)<-"ORCDRC"
PHIHOX<-raster(lsoil[4])
PHIHOX.crop<-crop(PHIHOX,e)
names(PHIHOX.crop)<-"PHIHOX"
SLTPPT<-raster(lsoil[5])
SLTPPT.crop<-crop(SLTPPT,e)
names(SLTPPT.crop)<-"SLTPPT"
SNDPPT<-raster(lsoil[6])
SNDPPT.crop<-crop(SNDPPT,e)
names(SNDPPT.crop)<-"SNDPPT"
NTO<-raster(lsoil[7])
NTO.crop<-crop(NTO,e)
names(NTO.crop)<-"NTO"
P<-raster(lsoil[8])
P.crop<-crop(P,e)
names(P.crop)<-"P"
NBWP<-raster(lsoil[9])
NBWP.crop<-crop(NBWP,e)
names(NBWP.crop)<-"NBWP"
SINT<-raster(lsoil[10])
SINT.crop<-crop(SINT,e)
names(SINT.crop)<-"SINT"
SOS<-raster(lsoil[11])
SOS.crop<-crop(SOS,e)
names(SOS.crop)<-"SOS"
#CLYPPT.crop,ORCDRC.crop,PHIHOX.crop,SLTPPT.crop,NTO.crop,P.crop,SNDPPT.crop
AETI<-projectRaster(AETI.crop,P.crop)
CLYPPT<-projectRaster(CLYPPT.crop,P.crop)
#Êextent(CLYPPT)<-extent(P.crop)
ORCDRC<-projectRaster(ORCDRC.crop,P.crop)
#extent(ORCDRC)<-extent(P.crop)
PHIHOX<-projectRaster(PHIHOX.crop,P.crop)
fun <- function(x) { x / 10 }
PHIHOX<-calc(PHIHOX,fun)
names(PHIHOX)<-"PHIHOX"
#extent(PHIHOX)<-extent(P.crop)
SLTPPT<-projectRaster(SLTPPT.crop,P.crop)
#extent(SLTPPT)<-extent(P.crop)
NTO<-projectRaster(NTO.crop,P.crop)
#extent(NTO)<-extent(P.crop)
SNDPPT<-projectRaster(SNDPPT.crop,P.crop)
#extent(SNDPPT)<-extent(P.crop)
NBWP<-projectRaster(NBWP.crop,P.crop)
SINT<-projectRaster(SINT.crop,P.crop)
SOS<-projectRaster(SOS.crop,P.crop)
P<-P.crop
#,PHIHOX,SLTPPT,NTO,P,SNDPPT
SoilGrid.crop<-stack(AETI,SINT,SOS,NBWP,CLYPPT,ORCDRC,PHIHOX,SLTPPT,NTO,SNDPPT,P)
SoilGrid.crop
#prédicteurs des espèces
#AETI(1),SINT(2),SOS(3),NBWP(4),CLYPPT(5),ORCDRC(6)
#,PHIHOX(7),SLTPPT(8),NTO(9),SNDPPT(10),P(11)
#Faidherbia albida
#PHIHOX, SINT, AETI, SOS, NTO, P, NBWP, ORCDRC
predictors<-stack(PHIHOX, SINT, AETI, SOS, NTO, P, NBWP, ORCDRC)
#Balanites_aegyptiaca
#PHIHOX, SINT, AETI, SOS, NTO, P, NBWP, ORCDRC
predictorsB<-stack(PHIHOX, SINT, AETI, SOS, NTO, P, NBWP, ORCDRC)
#Anogeissus_leiocarpus
##explicatives
#PHIHOX, AETI, SOS, NTO, P, NBWP, ORCDRC
predictorsAno<-stack(PHIHOX, AETI, SOS, NTO, P, NBWP, ORCDRC)
#Adansonia.digitata
#SNDPPT, SINT, ORCDRC
predictorsAdan<-stack(SNDPPT, SINT, ORCDRC)
#Acacia_nilotica
#SINT, NBWP, AETI, NTO, SOS, P, ORCDRC
predictorsAca<-stack(SINT, NBWP, AETI, NTO, SOS, P, ORCDRC)
# make a SpatialPointsDataFrame object from data.frame
pa_dataF <- st_as_sf(Base_Faidherbia_Z, coords = c("lon","lat"), crs = crs(predictors))
pa_dataB <- st_as_sf(Base_Balanites_Z, coords = c("lon","lat"), crs = crs(predictorsB))
pa_dataAno <- st_as_sf(Base_Anogeissus_Z, coords = c("lon","lat"), crs = crs(predictorsAno))
pa_dataAdan <- st_as_sf(Base_Adansonia_Z, coords = c("lon","lat"), crs = crs(predictorsAdan))
pa_dataAca <- st_as_sf(Base_Acacia_Z, coords = c("lon","lat"), crs = crs(predictorsAca))
###################################### statistiques descriptives
# extract the raster values for the species points as a dataframe
mydata <- raster::extract(predictors, pa_dataF, df = TRUE)
mydata2 <- raster::extract(predictorsB, pa_dataB, df = TRUE)
mydata3 <- raster::extract(predictorsAno, pa_dataAno, df = TRUE)
mydata4 <- raster::extract(predictorsAdan, pa_dataAdan, df = TRUE)
mydata5 <- raster::extract(predictorsAca, pa_dataAca, df = TRUE)
# adding species column to the dataframe
mydata$Faidherbia <- as.factor(pa_dataF$Faidherbia)
mydata2$Balanites <- as.factor(pa_dataB$Balanites)
mydata3$Anogeissus <- as.factor(pa_dataAno$Anogeissus)
mydata4$Adansonia<-as.factor(pa_dataAdan$Adansonia)
mydata5$Acacia<-as.factor(pa_dataAca$Acacia)
# remove extra column (ID)
mydata <- mydata[,-1]
mydata2 <- mydata2[,-1]
mydata3 <- mydata3[,-1]
mydata4 <- mydata4[,-1]
mydata5 <- mydata5[,-1]
# by_species <- mydata %>%
#   group_by(Faidherbia)
# summarise<-by_species %>%
#   summarise_all(list(min = min, max = max))
# Tab <- desc_statby(tableRange,  measure.var = "range",grps = "Species")





#######################



# plot species data on the map
# investigate spatial autocorrelation in raster covariates
# this helps to choose a suitable size for spatial blocks
sac1<-spatialAutoRange(rasterLayer = predictors, # raster file
                       doParallel = F, # number of cells to be used
                       
                       showPlots = TRUE)
table1<-sac1$rangeTable
table1$Species<-"Faidherbia albida"
bloc1<-sac1$plots$mapplot
sac2<-spatialAutoRange(rasterLayer = predictorsB, # raster file
                       doParallel = F, # number of cells to be used
                       
                       showPlots = TRUE)
table2<-sac2$rangeTable
table2$Species<-"Balanites aegyptiaca"
bloc<-sac2$plots$mapplot
sac3<-spatialAutoRange(rasterLayer = predictorsAno, # raster file
                       doParallel = F, # number of cells to be used
                       
                       showPlots = TRUE)
table3<-sac3$rangeTable
table3$Species<-"Anogeissus leiocarpus"
bloc3<-sac3$plots$mapplot
sac4<-spatialAutoRange(rasterLayer = predictorsAdan, # raster file
                       doParallel = F, # number of cells to be used
                       
                       showPlots = TRUE)
table4<-sac4$rangeTable
table4$Species<-"Adansonia digitata"
bloc4<-sac4$plots$mapplot
sac5<-spatialAutoRange(rasterLayer = predictorsAca, # raster file
                       doParallel = F, # number of cells to be used
                       
                       showPlots = TRUE)
table5<-sac5$rangeTable
table5$Species<-"Acacia nilotica"
bloc5<-sac5$plots$mapplot
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
#                    theRange = round(sac1$range,0), # size of the blocks
#                    k = 5,
#                    selection = "random",
#                    iteration = 100, # find evenly dispersed folds
#                    biomod2Format = FALSE,
#                    xOffset = 0, # shift the blocks horizontally
#                    yOffset = 0)
sb <- spatialBlock(speciesData = pa_dataF, # presence-background data
                    species = "Faidherbia",
                    rasterLayer = predictors,
                    rows = 10,
                    cols = 10,
                    k = 10,
                    selection = "systematic",
                    biomod2Format = TRUE)

summarise_fold(sb)
sb2 <- spatialBlock(speciesData = pa_dataB,
                   species = "Balanites",
                   rasterLayer = predictorsB,
                   theRange = round(sac2$range,0), # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = FALSE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)
summarise_fold(sb2)
sb3 <- spatialBlock(speciesData = pa_dataAno,
                    species = "Anogeissus",
                    rasterLayer = predictorsAno,
                    theRange = round(sac3$range,0), # size of the blocks
                    k = 5,
                    selection = "random",
                    iteration = 100, # find evenly dispersed folds
                    biomod2Format = FALSE,
                    xOffset = 0, # shift the blocks horizontally
                    yOffset = 0)
summarise_fold(sb3)
sb4 <- spatialBlock(speciesData = pa_dataAdan,
                    species = "Adansonia",
                    rasterLayer = predictorsAdan,
                    theRange = round(sac4$range,0), # size of the blocks
                    k = 5,
                    selection = "random",
                    iteration = 100, # find evenly dispersed folds
                    biomod2Format = FALSE,
                    xOffset = 0, # shift the blocks horizontally
                    yOffset = 0)

summarise_fold(sb4)
sb5 <- spatialBlock(speciesData = pa_dataAca,
                    species = "Acacia",
                    rasterLayer = predictorsAca,
                    theRange = round(sac5$range,0), # size of the blocks
                    k = 5,
                    selection = "random",
                    iteration = 100, # find evenly dispersed folds
                    biomod2Format = FALSE,
                    xOffset = 0, # shift the blocks horizontally
                    yOffset = 0)
summarise_fold(sb5)
#########################"
foldsoil1<-summarise_fold(sb)
foldsoil2<-summarise_fold(sb2)
foldsoil3<-summarise_fold(sb3)
foldsoil4<-summarise_fold(sb4)
foldsoil5<-summarise_fold(sb5)
ggarrange(foldsoil1,foldsoil2,foldsoil3,foldsoil4,foldsoil5,
          nrow = 3,
          ncol = 2)

#foldExplorer(sb1, predictors, pa_dataF)
E1<-Explorer(sb, predictors, pa_dataF,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\soile1.png",E1)
E2<-Explorer(sb2, predictorsB, pa_dataB,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\soile2.png",E2)
E3<-Explorer(sb3, predictorsAno, pa_dataAno,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\soile3.png",E3)
E4<-Explorer(sb4, predictorsAdan, pa_dataAdan,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\soile4.png",E4)
E5<-Explorer(sb5, predictorsAca, pa_dataAca,1)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\soile5.png",E5)

# adding points on saptialBlock plot
library(ggplot2)
sb3$plots + geom_sf(data = pa_dataAno, alpha = 0.5) 



############### Modélisation des espèces
# extract the foldIDs in SpatialBlock object 
# created in the previous section
# the folds (list) works for all three blocking strategies
folds <- sb$folds
folds2 <- sb2$folds
folds3 <- sb3$folds
folds4 <- sb4$folds
folds5 <- sb5$folds
############### constitution des données d'entrainement et test
#Faidherbia_albida
#fold 1
trainSet <- unlist(folds[[1]][1]) # training set indices
testSet <- unlist(folds[[1]][2]) # testing set indices

train<-Base_Faidherbia_Z[trainSet, ] 
p<-train %>% 
  filter(Faidherbia==1)
p<-p[,c("lon","lat")]
a<-train %>% 
  filter(Faidherbia==0)
a<-a[,c("lon","lat")]
test<-Base_Faidherbia_Z[testSet, ] 
occtest<-test %>% 
  filter(Faidherbia==1)
occtest<-occtest[,c("lon","lat")] 
bgtest<-test %>% 
  filter(Faidherbia==0)
bgtest<-bgtest[,c("lon","lat")]
#Balanites_aegyptiaca
#fold 1
trainSet2 <- unlist(folds2[[5]][1]) # training set indices indice 2 cool
testSet2 <- unlist(folds2[[5]][2]) # testing set indices
train2<-Base_Balanites_Z[trainSet2, ] 
p2<-train2 %>% 
  filter(Balanites==1)
p2<-p2[,c("lon","lat")]
a2<-train2 %>% 
  filter(Balanites==0)
a2<-a2[,c("lon","lat")]
test2<-Base_Balanites_Z[testSet2, ] 
occtest2<-test2 %>% 
  filter(Balanites==1)
occtest2<-occtest2[,c("lon","lat")] 
bgtest2<-test2 %>% 
  filter(Balanites==0)
bgtest2<-bgtest2[,c("lon","lat")]
#Anogeissus_leiocarpus
#fold 1
trainSet3 <- unlist(folds3[[5]][1]) # training set indices
testSet3 <- unlist(folds3[[5]][2]) # testing set indices
train3<-Base_Anogeissus_Z[trainSet3, ] 
p3<-train3 %>% 
  filter(Anogeissus==1)
p3<-p3[,c("lon","lat")]
a3<-train3 %>% 
  filter(Anogeissus==0)
a3<-a3[,c("lon","lat")]
test3<-Base_Anogeissus_Z[testSet3, ] 
occtest3<-test3 %>% 
  filter(Anogeissus==1)
occtest3<-occtest3[,c("lon","lat")] 
bgtest3<-test3 %>% 
  filter(Anogeissus==0)
bgtest3<-bgtest3[,c("lon","lat")]
#Adansonia_digitata
#fold 1
trainSet4 <- unlist(folds4[[5]][1]) # training set indices
testSet4 <- unlist(folds4[[5]][2]) # testing set indices
train4<-Base_Adansonia_Z[trainSet4, ] 
p4<-train4 %>% 
  filter(Adansonia==1)
p4<-p4[,c("lon","lat")]
a4<-train4 %>% 
  filter(Adansonia==0)
a4<-a4[,c("lon","lat")]
test4<-Base_Adansonia_Z[testSet4, ] 
occtest4<-test4 %>% 
  filter(Adansonia==1)
occtest4<-occtest4[,c("lon","lat")] 
bgtest4<-test4 %>% 
  filter(Adansonia==0)
bgtest4<-bgtest4[,c("lon","lat")]
#Acacia_nilotica
#fold 1
trainSet5 <- unlist(folds5[[5]][1]) # training set indices
testSet5 <- unlist(folds5[[5]][2]) # testing set indices
train5<-Base_Acacia_Z[trainSet5, ] 
p5<-train5 %>% 
  filter(Acacia==1)
p5<-p5[,c("lon","lat")]
a5<-train5 %>% 
  filter(Acacia==0)
a5<-a5[,c("lon","lat")]
test5<-Base_Acacia_Z[testSet5, ] 
occtest5<-test5 %>% 
  filter(Acacia==1)
occtest5<-occtest5[,c("lon","lat")] 
bgtest5<-test5 %>% 
  filter(Acacia==0)
bgtest5<-bgtest5[,c("lon","lat")]
#################################### Modèles
maxent()
## Loading required namespace: rJava
## This is MaxEnt version 3.4.1

#####################
xm <- maxent(predictors, p) 
xm2 <- maxent(predictorsB, p2,a2) 
xm3 <- maxent(predictorsAno, p3,a3) 
xm4 <- maxent(predictorsAdan, p4,a4) 
xm5 <- maxent(predictorsAca, p5,a5) 
##############Evaluation des modèles
e1 <- evaluate(xm, p=occtest, a=bg, x=predictors)
e <- dismo::evaluate(occtest, bgtest, xm, predictors)
e
density(e)
boxplot(e, col=c('blue', 'red'))
#### fold=1
# class          : ModelEvaluation 
# n presences    : 724 
# n absences     : 1040 
# AUC            : 0.5974906 
# cor            : 0.1546418 
# max TPR+TNR at : 0.5949127 
###fold=2
# class          : ModelEvaluation 
# n presences    : 768 
# n absences     : 1101 
# AUC            : 0.5815416 
# cor            : 0.115037 
# max TPR+TNR at : 0.516754 
##fold=3
# class          : ModelEvaluation 
# n presences    : 792 
# n absences     : 1039 
# AUC            : 0.5542468 
# cor            : 0.08103588 
# max TPR+TNR at : 0.6603731 
##fold=4
# class          : ModelEvaluation 
# n presences    : 837 
# n absences     : 959 
# AUC            : 0.5953795 
# cor            : 0.171992 
# max TPR+TNR at : 0.7256166 
##fold=5
# class          : ModelEvaluation 
# n presences    : 751 
# n absences     : 1247 
# AUC            : 0.5700915 
# cor            : 0.1328598 
# max TPR+TNR at : 0.6492212 
#tr <- threshold(e, 'spec_sens') 
# 0.5990972
e2 <- dismo::evaluate(occtest2, bgtest2, xm2, predictorsB)
e2
####fold=1
# class          : ModelEvaluation 
# n presences    : 221 
# n absences     : 1755 
# AUC            : 0.6376429 
# cor            : 0.1388079 
# max TPR+TNR at : 0.6044744 
###fold=2
# class          : ModelEvaluation 
# n presences    : 213 
# n absences     : 1875 
# AUC            : 0.5943474 
# cor            : 0.07184552 
# max TPR+TNR at : 0.6115908
##fold=3
# class          : ModelEvaluation 
# n presences    : 207 
# n absences     : 1471 
# AUC            : 0.5755919 
# cor            : 0.07181564 
# max TPR+TNR at : 0.6817517 
##fold=4
# class          : ModelEvaluation 
# n presences    : 193 
# n absences     : 1523 
# AUC            : 0.5358799 
# cor            : 0.03560575 
# max TPR+TNR at : 0.691921 
#fold=5
# class          : ModelEvaluation 
# n presences    : 183 
# n absences     : 1617 
# AUC            : 0.509883 
# cor            : 0.03538444 
# max TPR+TNR at : 0.4638059 
tr2 <- threshold(e2, 'spec_sens')
# 0.6873049 
e3 <- dismo::evaluate(occtest3, bgtest3, xm3, predictorsAno)
e3
#fold=1
# class          : ModelEvaluation 
# n presences    : 162 
# n absences     : 1701 
# AUC            : 0.4259386 
# cor            : -0.06152217 
# max TPR+TNR at : 0.7204118 

##fold=2
# class          : ModelEvaluation 
# n presences    : 193 
# n absences     : 1673 
# AUC            : 0.5830038 
# cor            : 0.08997966 
# max TPR+TNR at : 0.6444521
###fold=3
# class          : ModelEvaluation 
# n presences    : 136 
# n absences     : 1744 
# AUC            : 0.4948711 
# cor            : -0.003131254 
# max TPR+TNR at : 0.7403089 
#fold=4
# class          : ModelEvaluation 
# n presences    : 120 
# n absences     : 1621 
# AUC            : 0.5061973 
# cor            : 0.004036646 
# max TPR+TNR at : 0.5534647 
#fold=5
# class          : ModelEvaluation 
# n presences    : 199 
# n absences     : 1709 
# AUC            : 0.570981 
# cor            : 0.09384865 
# max TPR+TNR at : 0.6899333 
tr3 <- threshold(e3, 'spec_sens')
# 0.602487
e4 <- dismo::evaluate(occtest4, bgtest4, xm4, predictorsAdan)
e4
#fold=1
# class          : ModelEvaluation 
# n presences    : 132 
# n absences     : 1792 
# AUC            : 0.6635171 
# cor            : 0.1885776 
# max TPR+TNR at : 0.731139  
##fold=2
# class          : ModelEvaluation 
# n presences    : 55 
# n absences     : 1793 
# AUC            : 0.6279217 
# cor            : 0.08688535 
# max TPR+TNR at : 0.536834 
#fold=3
# class          : ModelEvaluation 
# n presences    : 70 
# n absences     : 1677 
# AUC            : 0.5941647 
# cor            : 0.06326376 
# max TPR+TNR at : 0.4873978 
##fold=4
# class          : ModelEvaluation 
# n presences    : 137 
# n absences     : 1659 
# AUC            : 0.5896239 
# cor            : 0.09165383 
# max TPR+TNR at : 0.6607573 
#fold=5
# class          : ModelEvaluation 
# n presences    : 179 
# n absences     : 1764 
# AUC            : 0.5539594 
# cor            : 0.05160365 
# max TPR+TNR at : 0.5958898 
tr4 <- threshold(e4, 'spec_sens') 
# 0.6003993
e5 <- dismo::evaluate(occtest5, bgtest5, xm5, predictorsAca)
e5
##fold=1
# class          : ModelEvaluation 
# n presences    : 67 
# n absences     : 1472 
# AUC            : 0.527909 
# cor            : -0.0003224606 
# max TPR+TNR at : 0.7815231 
###fold=2
# class          : ModelEvaluation 
# n presences    : 62 
# n absences     : 1849 
# AUC            : 0.6636805 
# cor            : 0.09394309 
# max TPR+TNR at : 0.694385 
#fold=3
# class          : ModelEvaluation 
# n presences    : 82 
# n absences     : 1803 
# AUC            : 0.4448582 
# cor            : -0.01699223 
# max TPR+TNR at : 0.7694648 
#fold=4
# class          : ModelEvaluation 
# n presences    : 85 
# n absences     : 1922 
# AUC            : 0.5817745 
# cor            : 0.05798073 
# max TPR+TNR at : 0.668437 
#fold=5
# class          : ModelEvaluation 
# n presences    : 61 
# n absences     : 1855 
# AUC            : 0.6389245 
# cor            : 0.07761752 
# max TPR+TNR at : 0.5446238 
tr5 <- threshold(e5, 'spec_sens') 
#  0.4313088
##### PLOT : # importance des variables
par(mfrow=c(2,3)) 
plot(xm, main="Faidherbia albida",xlab="Pourcentage") 
plot(xm2, main="Balanites aegyptiaca",xlab="Pourcentage") 
plot(xm3, main="Anogeissus leiocarpus",xlab="Pourcentage") 
plot(xm4, main="Adansonia digitata",xlab="Pourcentage") 
plot(xm5, main="Acacia nilotica",xlab="Pourcentage") 
########response
response(xm,var=c("AETI","ORCDRC","SOS"))
response(xm2,var=c("NTO","NBWP","SINT"))
response(xm3,var=c("SOS","NBWP","AETI"))
response(xm4,var=c("SINT","SNDPPT","ORCDRC"))
response(xm5,var=c("SINT","ORCDRC","AETI"))
#######"

##############
px <- predict(predictors, xm) 
#values     : 0.05616474, 0.9720067  (min, max)
px2 <- predict(predictorsB, xm2)
#values     : 0.1171372, 1  (min, max)
px3 <- predict(predictorsAno, xm3)
#values     : 0.03481599, 1  (min, max)
px4 <- predict(predictorsAdan, xm4)
#values     : 0.1535451, 0.9979818  (min, max)
px5 <- predict(predictorsAca, xm5)
#values     : 0.0993688, 0.9996904  (min, max)
############"
# px<0.4  habitats inappropriés
# px[0.4, 0.6] moyennement adéquat
# px>0.6 habitat hautement adéquat
##########
fun <- function(x) { 
  ifelse(x<0.4,1,ifelse(x<0.6,2,3)) 
}
# pxes<-calc(px,fun)
# pxes@data@values<-as.factor(pxes@data@values)
# plot(pxes,col = rev(terrain.colors(4)))
# pxes2<-calc(px2,fun)
# pxes2@data@values<-as.factor(pxes2@data@values)
# plot(pxes2,col = rev(terrain.colors(4)))
# pxes3<-calc(px3,fun)
# pxes3@data@values<-as.factor(pxes3@data@values)
# plot(pxes3,col = rev(terrain.colors(4)))
# pxes4<-calc(px4,fun)
# pxes4@data@values<-as.factor(pxes4@data@values)
# plot(pxes4,col = rev(terrain.colors(4)))
# pxes5<-calc(px5,fun)
# pxes5@data@values<-as.factor(pxes5@data@values)
# plot(pxes5,col = rev(terrain.colors(3)))
# ggR(pxes)
# par(mfrow=c(2,3))
# plot(pxes,col = rev(terrain.colors(3)))
# plot(pxes2,col = rev(terrain.colors(3)))
# plot(pxes3,col = rev(terrain.colors(3)))
# plot(pxes4,col = rev(terrain.colors(3)))
# plot(pxes5,col = rev(terrain.colors(3)))
######## 

#######"
names(px)<-"Faidherbia albida" 
names(px2)<-"Balanites aegyptiaca" 
names(px3)<-"Anogeissus leiocarpus" 
names(px4)<-"Adansonia digitata" 
names(px5)<-"Acacia nilotica" 
ggR_Predict(px)
ggR_Predict(px2)
ggR_Predict(px3)
ggR_Predict(px4)
ggR_Predict(px5)
ggarrange(ggR_Predict(px),ggR_Predict(px2),ggR_Predict(px3),ggR_Predict(px4),ggR_Predict(px5),
          common.legend = TRUE)





par(mfrow=c(2,2))
plot(e, 'ROC')
plot(e, 'kappa')
plot(e, 'FPR')
plot(e, 'prevalence')
########
par(mfrow=c(2,2))
plot(e2, 'ROC')
plot(e2, 'kappa')
plot(e2, 'FPR')
plot(e2, 'prevalence')
#############)"

par(mfrow=c(2,2))
plot(e3, 'ROC')
plot(e3, 'kappa')
plot(e3, 'FPR')
plot(e3, 'prevalence')

#########
par(mfrow=c(2,2))
plot(e4, 'ROC')
plot(e4, 'kappa')
plot(e4, 'FPR')
plot(e4, 'prevalence')

#########
par(mfrow=c(2,2))
plot(e5, 'ROC')
plot(e5, 'kappa')
plot(e5, 'FPR')
plot(e5, 'prevalence')
##########################
Faidh<-as.data.frame(px)
Faidh$Species<-"Faidherbia albida"
Faidh<-Faidh %>%
  rename(Probabilite=Faidherbia.albida)
df_status(Faidh) #210
Bala<-as.data.frame(px2)
Bala$Species<-"Balanites aegyptiaca"
Bala<-Bala %>%
  rename(Probabilite=Balanites.aegyptiaca)
df_status(Bala) #210
Ano<-as.data.frame(px3)
Ano$Species<-"Anogeissus leiocarpus"
Ano<-Ano %>%
  rename(Probabilite=Anogeissus.leiocarpus)
df_status(Ano) #210
Adan<-as.data.frame(px4)
Adan$Species<-"Adansonia digitata"
Adan<-Adan %>%
  rename(Probabilite=Adansonia.digitata)
df_status(Adan) #96
Aca<-as.data.frame(px5)
Aca$Species<-"Acacia nilotica"
Aca<-Aca %>%
  rename(Probabilite=Acacia.nilotica)
df_status(Aca) #210

Prediction<-rbind(Faidh,Bala,Ano,Adan,Aca)
Prediction<- Prediction %>%
  filter(!is.na(Probabilite))
write.csv(Prediction,file = "PredictionSoilMaxent.csv")
df_status(Prediction)      
Pred <- desc_statby(Prediction,  measure.var = "Probabilite",grps = "Species")
write.csv(Pred,file = "ResumePredicMaxent.csv")
#######################
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

#########"
names(predictors)
model <- factor(Faidherbia)~ AETI + SINT + SOS + NBWP + ORCDRC + PHIHOX + NTO + P
rf1 <- randomForest(model, data=mydata[trainSet, ])
tes<-mydata[testSet, ]
testpresrf<-tes %>%
  filter(Faidherbia==1)
testbackgrf<-tes %>%
  filter(Faidherbia==0)
testTable <- pa_dataF
testTable$pred <- NA
erf <- dismo::evaluate(testpresrf, testbackgrf, rf1)
# class          : ModelEvaluation 
# n presences    : 746 
# n absences     : 836 
# AUC            : 0.581197 
# cor            : 0.1719116 
# max TPR+TNR at : 0.9999 
pr <- predict(predictors, rf1)
ggR(pr,geom_raster = TRUE,ggLayer = F)
#rf <- randomForest(Faidherbia~., mydata[trainSet, ], ntree = 250) # model fitting on training set
testTable$pred[testSet] <- predict(rf1, mydata[testSet, ], type = "prob")[,2] # predict the test set
# rf2 <- randomForest(Balanites~., mydata2[trainSet, ], ntree = 250) # model fitting on training set
# testTable$pred[testSet] <- predict(rf2, mydata2[testSet, ], type = "prob")[,2] # predict the test set
# # calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Faidherbia)
# precrec_obj2 <- evalmod(scores = testTable$pred, labels = testTable$Balanites)
autoplot(precrec_obj)
auc(precrec_obj)
library(kernlab)

##
## Attaching package: 'kernlab'
## The following objects are masked from 'package:raster':
##
##     buffer, rotated
rbf <- rbfdot(sigma=0.1)
svm <- ksvm(Faidherbia~ AETI + SINT + SOS + NBWP + ORCDRC + PHIHOX + NTO + P, data=mydata[trainSet, ],prob.model=TRUE,type="C-svc",kernel=rbf,C=10)
class(svm)
 esv <- dismo::evaluate(testpresrf, testbackgrf, svm)
# class          : ModelEvaluation 
# n presences    : 746 
# n absences     : 836 
# AUC            : 0.5251324 
# cor            : 0.05255647 
# max TPR+TNR at : 0.9999 
ps <- dismo::predict(predictors, svm) 
ggR(ps,geom_raster = TRUE,ggLayer = F)
testTable <- pa_dataF
testTable$pred <- NA
testTable$pred[testSet] <- predict(svm, mydata[testSet, ], type = "prob")[,2] # predict the test set
precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Faidherbia)
autoplot(precrec_obj)
auc(precrec_obj)

############
# logistic regression:
########
DataModelF<-mydata
names(DataModelF)
# [1] "AETI"       "SINT"       "SOS"        "NBWP"      
# [5] "ORCDRC"     "PHIHOX"     "NTO"        "P"         
# [9] "Faidherbia"
DataModelF$AETI3<-equal_freq(var=DataModelF$AETI, n_bins = 3)
cross_plot(DataModelF, input="AETI3",target="Faidherbia", auto_binning = F,plot_type = "both")
#EFFET TAILLE
DataModelF$AETI_2<-DataModelF$AETI * DataModelF$AETI
DataModelF$ORCDRC3<-equal_freq(var = DataModelF$ORCDRC,n_bins = 3)
cross_plot(DataModelF, input="ORCDRC3",target="Faidherbia", auto_binning = F,plot_type = "both")
#simple
DataModelF$PHIHOX3<-equal_freq(var = DataModelF$PHIHOX,n_bins = 3)
cross_plot(DataModelF, input="PHIHOX3",target="Faidherbia", auto_binning = F,plot_type = "both")
#simple
DataModelF$SINT3<-equal_freq(var = DataModelF$SINT,n_bins = 3)
cross_plot(DataModelF, input="SINT3",target="Faidherbia", auto_binning = F,plot_type = "both")
#effet taille
DataModelF$SINT_2<-DataModelF$SINT * DataModelF$SINT
DataModelF$NTO3<-equal_freq(var = DataModelF$NTO,n_bins = 3)
cross_plot(DataModelF, input="NTO3",target="Faidherbia", auto_binning = F,plot_type = "both")
#EFFET TAILLE
DataModelF$NTO_2<-DataModelF$NTO * DataModelF$NTO
DataModelF$NBWP3<-equal_freq(var = DataModelF$NBWP,n_bins = 3)
cross_plot(DataModelF, input="NBWP3",target="Faidherbia", auto_binning = F,plot_type = "both")
#SIMPLE

gm1 <- glm(Faidherbia~ AETI + SINT + SOS + NBWP + ORCDRC + PHIHOX + NTO + P, data=mydata[trainSet, ],
           family = binomial(link = "log"))
gm2 <- glm(Faidherbia~ AETI + SINT + SOS + NBWP + ORCDRC + PHIHOX + NTO + P, data=mydata[trainSet, ],
           family = binomial(link = "logit"))
DataMode<-mydata
TypeSol_Zone<- Base_Espece_df %>%
  dplyr::select(Type_Sol,Zone)

gm3 <- glm(Faidherbia~ log(AETI) + log(SINT) + log(SOS) + log(NBWP) + log(ORCDRC) + log(PHIHOX) + log(NTO) + log(P), data=mydata[trainSet, ],
           family = binomial(link = "logit"))
#DataMode<-cbind(DataMode,TypeSol_Zone)
# gm4 <- glm(Faidherbia~ log(AETI) + log(SINT) + log(SOS) + log(NBWP) + log(ORCDRC) + log(PHIHOX) + log(NTO) + log(P) + Type_Sol + Zone, data=DataMode[trainSet, ],
#            family = binomial(link = "logit"))
# tes4<-DataMode[testSet, ]
# testpres4<-tes4 %>%
#   filter(Faidherbia==1)
# testbackg4<-tes4 %>%
#   filter(Faidherbia==0)
coef(gm1)
coef(gm2)
library(xtable)
#xtable(summary.glm(gm3))
summary.glm(gm4)
dismo::evaluate(testpres4, testbackg4, gm4)
# class          : ModelEvaluation 
# n presences    : 746 
# n absences     : 836 
# AUC            : 0.6760001 
# cor            : 0.3138468 
# max TPR+TNR at : -0.4887525 
# dismo::evaluate(testpres, testbackg, gm1)
# class          : ModelEvaluation 
# n presences    : 746 
# n absences     : 836 
# AUC            : 0.6637465 
# cor            : 0.2891422 
# max TPR+TNR at : -0.8439694 
dismo::evaluate(testpres, testbackg, gm2)
# class          : ModelEvaluation 
# n presences    : 746 
# n absences     : 836 
# AUC            : 0.6701427 
# cor            : 0.2996125 
# max TPR+TNR at : -0.2500685 
pg <- predict(predictors, gm1)
pg2 <- predict(predictors, gm2)
pg3 <- predict(predictors, gm3)
pg4 <- predict(predictors, gm4)
response(gm1)
response(gm2)
ggR(pg,geom_raster = TRUE,ggLayer = F)
ggR(pg2,geom_raster = TRUE,ggLayer = F)
ggR_Predict(pg2)
ggR_Predict(pg3)
ggR_Predict(pg)
###########
models <- stack(pr, ps, pg, pg2)
plot(models)
######################
train<-Base_Faidherbia_Z[trainSet, ] 
p<-train %>% 
  filter(Faidherbia==1)
p<-p[,c("lon","lat")]
a<-train %>% 
  filter(Faidherbia==0)
a<-a[,c("lon","lat")]
test<-Base_Faidherbia_Z[testSet, ] 
occtest<-test %>% 
  filter(Faidherbia==1)
occtest<-occtest[,c("lon","lat")] 
bgtest<-test %>% 
  filter(Faidherbia==0)
bgtest<-bgtest[,c("lon","lat")]
###########
##Geographic Distance
pres_train<-p
pres_test<-occtest
backg_test<-bgtest
library(maptools)
seamask <- predictors
distm <- geoDist(pres_train, lonlat=TRUE)
ds <- predict(seamask, distm, mask=TRUE)
e <- dismo::evaluate(distm, p=pres_test, a=backg_test)
# class          : ModelEvaluation 
# n presences    : 746 
# n absences     : 836 
# AUC            : 0.553932 
# cor            : 0.07440707 
# max TPR+TNR at : 0.00063008 
ggR(ds,geom_raster = TRUE,ggLayer = F)
#######
####Convex hulls
hull <- convHull(pres_train, lonlat=TRUE)
e <- dismo::evaluate(hull, p=pres_test, a=backg_test)
#class          : ModelEvaluation 
# n presences    : 746 
# n absences     : 836 
# AUC            : 0.5870945 
# cor            : 0.2589103 
# max TPR+TNR at : 0.9999
h <- predict(seamask, hull, mask=TRUE)
plot(h)
#######
###Circles
circ <- circles(pres_train, lonlat=TRUE)
pc <- predict(seamask, circ, mask=TRUE)
plot(pc)
ggR(pc,geom_raster = TRUE,ggLayer = F)
e <- dismo::evaluate(circ, p=pres_test, a=backg_test)

# class          : ModelEvaluation 
# n presences    : 746 
# n absences     : 836 
# AUC            : 0.552715 
# cor            : 0.1765283 
# max TPR+TNR at : 0.9999 
#######
back_train<-a
library(gstat)
idwm <- geoIDW(p=pres_train, a=data.frame(back_train))
###############################################
pres <- mydata[mydata[,ncol(mydata)] == 1, 1:ncol(mydata)-1]
back <- mydata[mydata[,ncol(mydata)] == 0, 1:ncol(mydata)-1]
k <- 5
group <- kfold(pres, k)
e <- list()
for (i in 1:k) {
  train <- pres[group != i,]
  test <- pres[group == i,]
  bc <- bioclim(train)
  e[[i]] <- evaluate(p=test, a=back, bc)
}

auc <- sapply(e, function(x){x@auc})
######???
sb <- spatialBlock(speciesData = pa_dataF, # presence-background data
                   species = "Faidherbia",
                   rasterLayer = predictors,
                   rows = 15,
                   cols = 10,
                   k = 10,
                   selection = "systematic",
                   biomod2Format = TRUE)

summarise_fold(sb)
folds <- sb$folds
e <- list()
for (i in 1:10) {
     trainSet <- unlist(folds[[i]][1]) # training set indices
    testSet <- unlist(folds[[i]][2]) # testing set indices
    train<-Base_Faidherbia_Z[trainSet, ]
    p<-train %>% 
      filter(Faidherbia==1)
    p<-p[,c("lon","lat")]
    a<-train %>% 
      filter(Faidherbia==0)
    a<-a[,c("lon","lat")]
    test<-Base_Faidherbia_Z[testSet, ] 
    occtest<-test %>% 
      filter(Faidherbia==1)
    occtest<-occtest[,c("lon","lat")] 
    bgtest<-test %>% 
      filter(Faidherbia==0)
    bgtest<-bgtest[,c("lon","lat")]
    xm <- maxent(predictors, p,a)
    e[[i]] <-dismo::evaluate(occtest, bgtest, xm, predictors)
}
auc <- sapply(e, function(x){x@auc})
auc
#0.5872948 0.4671157 0.5157759 0.4643429 0.4872055
############random forest
library(randomForest)
library(precrec)
# create a data.frame to store the prediction of each fold (record)
testTable <- pa_dataF
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

