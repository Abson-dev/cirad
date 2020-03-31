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
#############"
zone_etude<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Diohine_Echanti_Classif.shp")
zone_etude@bbox<-as.matrix(extent(worldclim.crop))
zone<-st_as_sf(zone_etude)
crs(awt)
###########lecture des prédicteurs(rasters)
tmpdir<-"C:\\Users\\Hp\\Desktop\\Model"
bio1<-raster("C:\\Users\\Hp\\Desktop\\Model\\bio1.tif")
l1<-list.files("C:\\Users\\Hp\\Desktop\\Model\\",patt="\\.tif")
l1<-sprintf("C:\\Users\\Hp\\Desktop\\Model\\%s",l1)
worldclim.crop<-stack(l1)
#plot(worldclim.crop)
ncell(worldclim.crop)
#[1] 462
# investigate spatial autocorrelation in raster covariates
# this helps to choose a suitable size for spatial blocks
range<-spatialAutoRange(rasterLayer = worldclim.crop, # raster file
                 doParallel = F,
                 sampleNumber = 462, # number of cells to be used
                 
                 showPlots = TRUE)
nombre_block<-round(range$range,0) #range - the suggested range, which is the median of all calculated ranges
rangeTable<-range$rangeTable
# library(xlsx)
# write.xlsx(mydata, "c:/mydata.xlsx")
fitted_variograms<-as.data.frame(range$variograms)

#########################Modélisation de Faidherbia albida dans la zone d'étude

filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
Base_Espece$Faidherbia_albida<-as.factor(Base_Espece$Faidherbia_albida)
data_df<-st_drop_geometry(Base_Espece)
Base_Faidherbia_Z<-data_df[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia")
#Transform data as SpatialPointDataFrame
sp::coordinates(Base_Faidherbia_Z) <-~lon+lat
sp::proj4string(Base_Faidherbia_Z) <-"+proj=longlat +datum=WGS84"
# load package data
awt <- worldclim.crop
# import presence-absence species data
PA <- Base_Faidherbia_Z
# make a sf object from data.frame
pa_data <- sf::st_as_sf(PA, coords = c("lon", "lat"), crs = raster::crs(awt))
# spatial blocking by specified range and random assignment
set.seed(1994)
sb1 <- spatialBlock(speciesData = pa_data,
                    species = "Faidherbia",
                    
                    theRange = 20502,
                    k = 2,
                    selection = "random",
                    iteration = 100,
                    numLimit = NULL,
                    biomod2Format = TRUE,
                    xOffset = 0.3, # shift the blocks horizontally
                    yOffset = 0)
foldExplorer(sb1, awt, pa_data)
sb2 <- spatialBlock(speciesData = pa_data,
                    species = "Faidherbia",
                    rasterLayer = awt,
                    rows = 5,
                    cols = 8,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)
foldExplorer(sb2, awt, pa_data)




