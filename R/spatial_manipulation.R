#
install.packages("raster")
library(raster)
filename<-paste0("E:\\Stage_SDM\\SDM\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK.shp")
filename
Species_shape<-shapefile(filename)
Species_raster<-raster("E:\\Stage_SDM\\SDM\\Data\\BD_Arbre\\arbres_diohine_mai2018_par_Zone_OK.shp")
install.packages("spdep")
library(spdep)
#wr<-poly2nb(Species_shape,row.names = Species_shape$ID_GPS,queen = F)
library(sf)
map<-st_as_sf(Base_Espece)
ggplot(map) + geom_sf(aes(fill=Zone)) + theme_bw()
######################"
#######################" les données sur WorlClim
install.packages("sp")
library(sp)
install.packages("rgdal")
library(rgdal)
library(raster)
install.packages("spData")
library(spData)
devtools::install_github("Nowosad/spDataLarge")
library(spDataLarge)   # load larger geographic data
#Répertoire, fichier et noms de variable
# Chemin d'accès au dossier contenant les 19 variables bioclimatiques
setwd("E:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\")

l1<-list.files("E:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("E:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
worlClim<-stack(l1)
worlClim2<-raster("E:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\wc2.0_bio_30s_01.tif")
names(worlClim)
plot(worlClim)
plot(worlClim$wc2.0_bio_30s_17)
#############################################

#######################################
filename_zone<-paste0("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4","\\Diohine_Echanti_Classif.shp")
filename_zone
zone_etude<-shapefile(filename_zone)
zone_raster<-raster("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\Diohine_Echanti_Classif.shp")
library(sf)
library("ggspatial")
install.packages("tmap")
library(tmap)
map<-st_as_sf(zone_etude)
strat_zone<-ggplot(map)  + geom_sf(aes(fill=Zone),colour="green") + 
  theme_gray() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\strat_zone.png",strat_zone)
t_sol<-ggplot(map)  + geom_sf(aes(fill=Type_Sol),colour="green") + 
  theme_gray() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\t_sol.png",t_sol)



