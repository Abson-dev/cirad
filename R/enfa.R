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
# import raster data
l1<-list.files("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
worldclim<-stack(l1)
# import raster data
lsoil<-list.files("D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\",patt="\\.tif")
lsoil<-sprintf("D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\%s",lsoil)
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
e
# xmin       : -16.53864 
# xmax       : -16.35454 
# ymin       : 14.45461 
# ymax       : 14.63543 
# e <- extent(-17, -15, 14, 15)
# worldclim.crop <- crop(worldclim,e)
worldclim.crop <- crop(worldclim,e)
#plot(worldclim.crop,1)
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
#Balanites aegyptiaca
Base_Espece$Balanites_aegyptiaca<-if_else(Base_Espece$Species =="Balanites aegyptiaca","1","0")
#Anogeissus leiocarpus
Base_Espece$Anogeissus_leiocarpus<-if_else(Base_Espece$Species =="Anogeissus leiocarpus","1","0")
#Adansonia digitata
Base_Espece$Adansonia_digitata<-if_else(Base_Espece$Species =="Adansonia digitata","1","0")
#Acacia nilotica
Base_Espece$Acacia_nilotica<-if_else(Base_Espece$Species =="Acacia nilotica","1","0")


Base_Espece_df<-st_drop_geometry(Base_Espece)
Base_Espece_df$Faidherbia_albida<-as.factor(Base_Espece_df$Faidherbia_albida)
Base_Espece_df$Balanites_aegyptiaca<-as.factor(Base_Espece_df$Balanites_aegyptiaca)
Base_Espece_df$Anogeissus_leiocarpus<-as.factor(Base_Espece_df$Anogeissus_leiocarpus)
Base_Espece_df$Adansonia_digitata<-as.factor(Base_Espece_df$Adansonia_digitata)
Base_Espece_df$Acacia_nilotica<-as.factor(Base_Espece_df$Acacia_nilotica)

##########"
###############"

Base_Faidherbia_Z<-Base_Espece_df[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia.albida")
FaidherbiaPr<- Base_Faidherbia_Z %>%
  filter(Faidherbia.albida ==1)
#Transform data as SpatialPointDataFrame
sp::coordinates(FaidherbiaPr) <-~lon+lat
sp::proj4string(FaidherbiaPr) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
########
Base_Balanites_Z<-Base_Espece_df[,c("xcoord","ycoord","Balanites_aegyptiaca")] 
names(Base_Balanites_Z)<-c("lon","lat","Balanites.aegyptiaca")
BalanitesPr<- Base_Balanites_Z %>%
  filter(Balanites.aegyptiaca ==1)
#Transform data as SpatialPointDataFrame
sp::coordinates(BalanitesPr) <-~lon+lat
sp::proj4string(BalanitesPr) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#########"
Base_Anogeissus_Z<-Base_Espece_df[,c("xcoord","ycoord","Anogeissus_leiocarpus")] 
names(Base_Anogeissus_Z)<-c("lon","lat","Anogeissus.leiocarpus")
AnogeissusPr<- Base_Anogeissus_Z %>%
  filter(Anogeissus.leiocarpus ==1)
#Transform data as SpatialPointDataFrame
sp::coordinates(AnogeissusPr) <-~lon+lat
sp::proj4string(AnogeissusPr) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#############
Base_Adansonia_Z<-Base_Espece_df[,c("xcoord","ycoord","Adansonia_digitata")] 
names(Base_Adansonia_Z)<-c("lon","lat","Adansonia.digitata")
AdansoniaPr<- Base_Adansonia_Z %>%
  filter(Adansonia.digitata ==1)
#Transform data as SpatialPointDataFrame
sp::coordinates(AdansoniaPr) <-~lon+lat
sp::proj4string(AdansoniaPr) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

###
Base_Acacia_Z<-Base_Espece_df[,c("xcoord","ycoord","Acacia_nilotica")] 
names(Base_Acacia_Z)<-c("lon","lat","Acacia.nilotica")
AcaciaPr<- Base_Acacia_Z %>%
  filter(Acacia.nilotica ==1)
#Transform data as SpatialPointDataFrame
sp::coordinates(AcaciaPr) <-~lon+lat
sp::proj4string(AcaciaPr) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# ########################## Variables sur le Sol
# e <- extent(-16.57, -16.33, 14.45 , 14.65)
# AETI<-raster(lsoil[1])
# AETI.crop<-crop(AETI,e)
# names(AETI.crop)<-"AETI"
# CLYPPT<-raster(lsoil[2])
# CLYPPT.crop<-crop(CLYPPT,e)
# names(CLYPPT.crop)<-"CLYPPT"
# ORCDRC<-raster(lsoil[3])
# ORCDRC.crop<-crop(ORCDRC,e)
# names(ORCDRC.crop)<-"ORCDRC"
# PHIHOX<-raster(lsoil[4])
# PHIHOX.crop<-crop(PHIHOX,e)
# names(PHIHOX.crop)<-"PHIHOX"
# SLTPPT<-raster(lsoil[5])
# SLTPPT.crop<-crop(SLTPPT,e)
# names(SLTPPT.crop)<-"SLTPPT"
# SNDPPT<-raster(lsoil[6])
# SNDPPT.crop<-crop(SNDPPT,e)
# names(SNDPPT.crop)<-"SNDPPT"
# NTO<-raster(lsoil[7])
# NTO.crop<-crop(NTO,e)
# names(NTO.crop)<-"NTO"
# P<-raster(lsoil[8])
# P.crop<-crop(P,e)
# names(P.crop)<-"P"
# NBWP<-raster(lsoil[9])
# NBWP.crop<-crop(NBWP,e)
# names(NBWP.crop)<-"NBWP"
# SINT<-raster(lsoil[10])
# SINT.crop<-crop(SINT,e)
# names(SINT.crop)<-"SINT"
# SOS<-raster(lsoil[11])
# SOS.crop<-crop(SOS,e)
# names(SOS.crop)<-"SOS"
# #CLYPPT.crop,ORCDRC.crop,PHIHOX.crop,SLTPPT.crop,NTO.crop,P.crop,SNDPPT.crop
# AETI<-projectRaster(AETI.crop,P.crop)
# CLYPPT<-projectRaster(CLYPPT.crop,P.crop)
# #Êextent(CLYPPT)<-extent(P.crop)
# ORCDRC<-projectRaster(ORCDRC.crop,P.crop)
# #extent(ORCDRC)<-extent(P.crop)
# PHIHOX<-projectRaster(PHIHOX.crop,P.crop)
# #extent(PHIHOX)<-extent(P.crop)
# SLTPPT<-projectRaster(SLTPPT.crop,P.crop)
# #extent(SLTPPT)<-extent(P.crop)
# NTO<-projectRaster(NTO.crop,P.crop)
# #extent(NTO)<-extent(P.crop)
# SNDPPT<-projectRaster(SNDPPT.crop,P.crop)
# #extent(SNDPPT)<-extent(P.crop)
# NBWP<-projectRaster(NBWP.crop,P.crop)
# SINT<-projectRaster(SINT.crop,P.crop)
# SOS<-projectRaster(SOS.crop,P.crop)
# P<-P.crop
# #,PHIHOX,SLTPPT,NTO,P,SNDPPT
# SoilGrid.crop<-stack(AETI,SINT,SOS,NBWP,CLYPPT,ORCDRC,PHIHOX,SLTPPT,NTO,SNDPPT,P)
# SoilGrid.crop
############## ENFA
########### Voir le fichier BlockCV_for_SDM_Soil.R
#######Faidherbia albida
#FaidherbiaPr@bbox <-as.matrix(extent(SoilGrid.crop))
nlayers(ENFA_var)
names(ENFA_var)
glc <- GLcenfa(x = ENFA_var) 
FaidherbiaPr@data$Faidherbia.albida<-as.numeric(FaidherbiaPr@data$Faidherbia.albida)
mod.enfa <- CENFA::enfa(x = ENFA_var, s.dat = FaidherbiaPr, field = "Faidherbia.albida")
mod.enfa
CENFA::scatter(x = mod.enfa, y = glc,n=34,p=1)
sf.prop<-mod.enfa@sf.prop
summary(co)
#.Broken-stick method for detection of significant factors
brStick(s.factor(mod.enfa))
#[1] 17
#Cov<-CENFA::cov.enfa(mod.enfa)
# correlation matrix
Z <- CENFA::parScale(ENFA_var)
mat <- CENFA::parCov(Z)
ggcorrplot::ggcorrplot(mat,ggtheme = ggplot2::theme_gray,
           hc.order = TRUE,
           type = "lower",
           lab = FALSE,
           colors = c("#6D9EC1", "white", "#E46726")) 
pa_dataF <- st_as_sf(Base_Faidherbia_Z, coords = c("lon","lat"), crs = crs(ENFA_var))
Cor <- raster::extract(ENFA_var, pa_dataF, df = TRUE)
Cor<-Cor[,-1]
library(funModeling)
df_status(Cor)
# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient

p.mat <- ggcorrplot::cor_pmat(Cor)
ggcorrplot::ggcorrplot(mat,ggtheme = ggplot2::theme_gray,
                 hc.order = TRUE,
                 type = "lower",
                 p.mat = p.mat,
                 colors = c("#6D9EC1", "white", "#E46726")) 




mod.cnfa <- CENFA::cnfa(x = ENFA_var, s.dat = FaidherbiaPr, field = "Faidherbia.albida")
mod.cnfa
s.map <- CENFA::sensitivity_map(mod.cnfa)
plot(s.map)
par(mfrow = c(2, 2))
stretchPlot(s.map, main = "linear")
stretchPlot(s.map, type = "hist.equal", main = "Histogram equalization")
stretchPlot(s.map, type = "sd", n = 2, main = "Standard deviation (n = 2)")
############## Balanites
#BalanitesPr@bbox <-as.matrix(extent(SoilGrid.crop))
#Aglc <- GLcenfa(x = SoilGrid.crop)
BalanitesPr@data$Balanites.aegyptiaca<-as.numeric(BalanitesPr@data$Balanites.aegyptiaca)
mod.enfaB <- CENFA::enfa(x = ENFA_var, s.dat = BalanitesPr, field = "Balanites.aegyptiaca")
mod.enfaB
brStick(s.factor(mod.enfaB))
#15
CENFA::scatter(x = mod.enfaB, y = glc,n=34,p=1)
mod.cnfaB <- cnfa(x = SoilGrid.crop, s.dat = BalanitesPr, field = "Balanites.aegyptiaca")
mod.cnfaB

s.mapB <- sensitivity_map(mod.cnfaB)
plot(s.mapB)
par(mfrow = c(2, 2))
stretchPlot(s.mapB, main = "linear")
stretchPlot(s.mapB, type = "hist.equal", main = "Histogram equalization")
stretchPlot(s.mapB, type = "sd", n = 2, main = "Standard deviation (n = 2)")
####### Anogeissus.leiocarpus
#glc <- GLcenfa(x = SoilGrid.crop) 
AnogeissusPr@data$Anogeissus.leiocarpus<-as.numeric(AnogeissusPr@data$Anogeissus.leiocarpus)
mod.enfaAno <- CENFA::enfa(x = ENFA_var, s.dat = AnogeissusPr, field = "Anogeissus.leiocarpus")
mod.enfaAno  
CENFA::scatter(x = mod.enfaAno, y = glc,n=34,p=1)
brStick(s.factor(mod.enfaAno))
#14
mod.cnfaAno <- cnfa(x = SoilGrid.crop, s.dat = AnogeissusPr, field = "Anogeissus.leiocarpus")
mod.cnfaAno
s.mapAno <- sensitivity_map(mod.cnfaAno)
plot(s.mapAno)
######## Adansonia.digitata
AdansoniaPr@data$Adansonia.digitata<-as.numeric(AdansoniaPr@data$Adansonia.digitata)
mod.enfaAdan <- CENFA::enfa(x = ENFA_var, s.dat = AdansoniaPr, field = "Adansonia.digitata")
mod.enfaAdan
CENFA::scatter(x = mod.enfaAdan, y = glc,n=34,p=1)
brStick(m.factor(mod.enfaAdan))
names(mod.enfaAdan)
#11
s.arrow(mod.enfaAdan@co)
s.arrow(mod.enfaAdan@cov)
mod.cnfaAdan <- cnfa(x = SoilGrid.crop, s.dat = AdansoniaPr, field = "Adansonia.digitata")
mod.cnfaAdan
s.mapAdan <- sensitivity_map(mod.cnfaAdan)
plot(s.mapAdan)

########### Acacia.nilotica
AcaciaPr@data$Acacia.nilotica<-as.numeric(AcaciaPr@data$Acacia.nilotica)
mod.enfaAca <- CENFA::enfa(x = ENFA_var, s.dat = AcaciaPr, field = "Acacia.nilotica")
mod.enfaAca
CENFA::scatter(x = mod.enfaAca, y = glc,n=34,p=1)
brStick(s.factor(mod.enfaAca))
#13
marginality(mod.enfaAca)
specialization(mod.enfaAca)
mod.cnfaAca <- cnfa(x = SoilGrid.crop, s.dat = AcaciaPr, field = "Acacia.nilotica")
mod.cnfaAca
s.mapAca <- sensitivity_map(mod.cnfaAca)
plot(s.mapAca)
############"
par(mfrow = c(2, 3))
CENFA::scatter(x = mod.enfa, y = glc, n=35,p=1) #Faidherbia.albida
title("Faidherbia albida")
CENFA::scatter(x = mod.enfaB, y = glc, n=35,p=1) #Balanites.aegyptiaca
title("Balanites aegyptiaca")
CENFA::scatter(x = mod.enfaAno, y = glc, n=35,p=1) #Anogeissus.leiocarpus
title("Anogeissus leiocarpus")
CENFA::scatter(x = mod.enfaAdan, y = glc, n=35,p=1) #Adansonia.digitata
title("Adansonia digitata")
CENFA::scatter(x = mod.enfaAca, y = glc, n=35,p=1) #Acacia.nilotica
title("Acacia nilotica")
######
par(mfrow = c(2, 3))
plot(s.map)
title("Faidherbia albida")
plot(s.mapB)
title("Balanites aegyptiaca")
plot(s.mapAno)
title("Anogeissus leiocarpus")
plot(s.mapAdan)
title("Adansonia digitata")
plot(s.mapAca)
title("Acacia nilotica")
###########

########################## Variables bioclimatiques
temp<-worldclim.crop
Varbio<-worldclim.crop
nlayers(Varbio)
#bio14 <- raster(worldclim.crop, layer=6)
#bio19 <- raster(worldclim.crop, layer=11)
Varbio <- dropLayer(Varbio, 6) #??? bio14
nlayers(Varbio)
Varbio <- dropLayer(Varbio, 10) #??? bio19
nlayers(Varbio)
worldclim.crop<-Varbio
FaidherbiaPr@bbox
#         min       max
# lon -16.53817 -16.35454
# lat  14.45461  14.63513
extent(worldclim.crop)
# xmin       : -16.54167 
# xmax       : -16.35833 
# ymin       : 14.45833 
# ymax       : 14.63333 
#FaidherbiaPr@bbox <-as.matrix(extent(worldclim.crop))
glc <- GLcenfa(x = worldclim.crop)
FaidherbiaPr@data$Faidherbia.albida<-as.numeric(FaidherbiaPr@data$Faidherbia.albida)
mod.enfa <- enfa(x = worldclim.crop, s.dat = FaidherbiaPr, field = "Faidherbia.albida")
#Returns the number of significant factors.
brStick(s.factor(mod.enfa))
###
s.arrow(mod.enfa@co)
s.arrow(mod.enfa@cov)
#scatter(x, y, xax = 1, yax = 2, p = 0.99, n = 5, plot = TRUE, ...)
# p= the proportion of observations to include in the calculations of the minimum
# convex polygons
marginality(mod.enfa)
marbioF<-as.data.frame(m.factor(mod.enfa))
specbioF<-as.data.frame(s.factor(mod.enfa))
#scatterniche(x = mod.enfa,pr=glc)
scatter(x = mod.enfa, y = glc, n=17, p=1)
title("d")
mod.cnfa <- cnfa(x = worldclim.crop, s.dat = FaidherbiaPr, field = "Faidherbia.albida")
es<-as.data.frame(worldclim.crop)
mod.cnfa
s.map <- sensitivity_map(mod.cnfa)
plot(s.map)
par(mfrow = c(2, 2))
stretchPlot(s.map, main = "linear")
stretchPlot(s.map, type = "hist.equal", main = "Histogram equalization")
stretchPlot(s.map, type = "sd", n = 2, main = "Standard deviation (n = 2)")
############## Balanites
#BalanitesPr@bbox <-as.matrix(extent(worldclim.crop))
glc <- GLcenfa(x = worldclim.crop)
BalanitesPr@data$Balanites.aegyptiaca<-as.numeric(BalanitesPr@data$Balanites.aegyptiaca)
mod.enfaB <- enfa(x = worldclim.crop, s.dat = BalanitesPr, field = "Balanites.aegyptiaca")
mod.enfaB
scatter(x = mod.enfaB, y = glc)
mod.cnfaB <- cnfa(x = worldclim.crop, s.dat = BalanitesPr, field = "Balanites.aegyptiaca")
mod.cnfaB
s.mapB <- sensitivity_map(mod.cnfaB)
plot(s.mapB)
par(mfrow = c(2, 2))
stretchPlot(s.mapB, main = "linear")
stretchPlot(s.mapB, type = "hist.equal", main = "Histogram equalization")
stretchPlot(s.mapB, type = "sd", n = 2, main = "Standard deviation (n = 2)")
####### Anogeissus.leiocarpus
#AnogeissusPr@bbox <-as.matrix(extent(worldclim.crop))
glc <- GLcenfa(x = worldclim.crop)
AnogeissusPr@data$Anogeissus.leiocarpus<-as.numeric(AnogeissusPr@data$Anogeissus.leiocarpus)
mod.enfaAno <- enfa(x = worldclim.crop, s.dat = AnogeissusPr, field = "Anogeissus.leiocarpus")
mod.enfaAno
scatter(x = mod.enfaAno, y = glc)
mod.cnfaAno <- cnfa(x = worldclim.crop, s.dat = AnogeissusPr, field = "Anogeissus.leiocarpus")
mod.cnfaAno
s.mapAno <- sensitivity_map(mod.cnfaAno)
plot(s.mapAno)
######## Adansonia.digitata
#AdansoniaPr@bbox <-as.matrix(extent(worldclim.crop))
glc <- GLcenfa(x = worldclim.crop)
AdansoniaPr@data$Adansonia.digitata<-as.numeric(AdansoniaPr@data$Adansonia.digitata)
mod.enfaAdan <- enfa(x = worldclim.crop, s.dat = AdansoniaPr, field = "Adansonia.digitata")
mod.enfaAdan
scatter(x = mod.enfaAdan, y = glc)
mod.cnfaAdan <- cnfa(x = worldclim.crop, s.dat = AdansoniaPr, field = "Adansonia.digitata")
mod.cnfaAdan
s.mapAdan <- sensitivity_map(mod.cnfaAdan)
plot(s.mapAdan)

########### Acacia.nilotica
#AcaciaPr@bbox <-as.matrix(extent(worldclim.crop))
glc <- GLcenfa(x = worldclim.crop)
AcaciaPr@data$Acacia.nilotica<-as.numeric(AcaciaPr@data$Acacia.nilotica)
mod.enfaAca <- enfa(x = worldclim.crop, s.dat = AcaciaPr, field = "Acacia.nilotica")
mod.enfaAca
scatter(x = mod.enfaAca, y = glc)
mod.cnfaAca <- cnfa(x = worldclim.crop, s.dat = AcaciaPr, field = "Acacia.nilotica")
mod.cnfaAca
s.mapAca <- sensitivity_map(mod.cnfaAca)
plot(s.mapAca)

############"
par(mfrow = c(2, 3))
scatter(x = mod.enfa, y = glc, n=17,p=1) #Faidherbia.albida
title("Faidherbia albida")
scatter(x = mod.enfaB, y = glc, n=17,p=1) #Balanites.aegyptiaca
title("Balanites aegyptiaca")
scatter(x = mod.enfaAno, y = glc, n=17,p=1) #Anogeissus.leiocarpus
title("Anogeissus leiocarpus")
scatter(x = mod.enfaAdan, y = glc, n=17,p=1) #Adansonia.digitata
title("Adansonia digitata")
scatter(x = mod.enfaAca, y = glc, n=17,p=1) #Acacia.nilotica
title("Acacia nilotica")
par(mfrow = c(2, 3))
plot(s.map)
plot(s.mapB)
plot(s.mapAno)
plot(s.mapAdan)
plot(s.mapAca)
###########