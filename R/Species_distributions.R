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
rm(list = ls()) #Effacement de toutes les données en mémoire
graphics.off() #Effacement de tous les graphiques en mémoire

ggDistribution<-function(map,species,zone){
  ggplot(map)   +
    geom_sf(aes(color = species)) +
    geom_sf(data = zone, colour = "black", fill = NA)  +
    theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank())
}
########???importation des données de worldclim
###1)prendre les fichiers .tif(raster) qui se trouvent dans le dossier indiquer
l1<-list.files("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
#####################zones
zone_etude1<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_1_BON.shp")
z1<-st_as_sf(zone_etude1)
zone_etude2<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_2_BON.shp")
z2<-st_as_sf(zone_etude2)
zone_etude3<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_3_BON.shp")
z3<-st_as_sf(zone_etude3)
zone_etude4<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_4_BON.shp")
z4<-st_as_sf(zone_etude4)
filename_zone<-paste0("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones","\\Diohine_Echanti_Classif.shp")
zone_etude<-shapefile(filename_zone)
map<-st_as_sf(zone_etude)
map$Zone<-as.factor(map$Zone)
map1<-map
map2<-map
map3<-map
map4<-map
map1$Zone<-if_else(map1$Zone ==1,"Zone 1","Autre")
map2$Zone<-if_else(map2$Zone ==2,"Zone 2","Autre")
map3$Zone<-if_else(map3$Zone ==3,"Zone 3","Autre")
map4$Zone<-if_else(map4$Zone ==4,"Zone 4","Autre")
zone1<-ggplot(map1)  + geom_sf(aes(fill=Zone),colour="black") + 
  theme_gray() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_manual(values=c("white","green")) +  ggtitle(label = "Zone d'étude") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(legend.title = element_blank())
zone2<-ggplot(map2)  + geom_sf(aes(fill=Zone),colour="black") + 
  theme_gray() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_manual(values=c("white","green")) +  ggtitle(label = "Zone d'étude") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(legend.title = element_blank())

zone3<-ggplot(map3)  + geom_sf(aes(fill=Zone),colour="black") + 
  theme_gray() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_manual(values=c("white","green")) +  ggtitle(label = "Zone d'étude") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(legend.title = element_blank())

zone4<-ggplot(map4)  + geom_sf(aes(fill=Zone),colour="black") + 
  theme_gray() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_manual(values=c("white","green")) +  ggtitle(label = "Zone d'étude") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(legend.title = element_blank())
#######Faidherbia
filename_PA_Z1<-paste0("C:\\Users\\Hp\\Desktop\\Model","\\dataFZ4.shp")
PA_FZ1<-shapefile(filename_PA_Z1)
map1<-st_as_sf(PA_FZ1)
map1$Faidherbia<-if_else(map1$Faidherbia ==1,"présence","absence")
map1$Faidherbia<-as.factor(map1$Faidherbia)
fz1<-ggDistribution(map1,map1$Faidherbia,z4) + ggtitle(label = "Faidherbia albida")
#Balanites
filename_PA_B_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataBZ4.shp")
PA_BZ1<-shapefile(filename_PA_B_Z1)
map_B_1<-st_as_sf(PA_BZ1)
map_B_1$Balanites<-if_else(map_B_1$Balanites ==1,"présence","absence")
map_B_1$Balanites<-as.factor(map_B_1$Balanites)
balaz1<-ggDistribution(map_B_1,map_B_1$Balanites,z4) + ggtitle(label = "Balanites aegyptiaca")
##########Anogeissus
filename_PA_A_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAZ4.shp")
PA_AZ1<-shapefile(filename_PA_A_Z1)
map_A_1<-st_as_sf(PA_AZ1)
map_A_1$Anogeissus<-if_else(map_A_1$Anogeissus ==1,"présence","absence")
map_A_1$Anogeissus<-as.factor(map_A_1$Anogeissus)
ano1<-ggDistribution(map_A_1,map_A_1$Anogeissus,z4) + ggtitle("Anogeissus leiocarpus")
######Adansonia_digitata
filename_PA_A_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAdZ4.shp")
PA_AdZ1<-shapefile(filename_PA_A_Z1)
map_Ad_1<-st_as_sf(PA_AdZ1)
map_Ad_1$Adansonia<-if_else(map_Ad_1$Adansonia ==1,"présence","absence")
map_Ad_1$Adansonia<-as.factor(map_Ad_1$Adansonia)
adan1<-ggDistribution(map_Ad_1,map_Ad_1$Adansonia,z4) + ggtitle("Adansonia digitata")
####Acacia
filename_PA_A_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAcZ4.shp")
PA_AcZ1<-shapefile(filename_PA_A_Z1)
map_Ac_1<-st_as_sf(PA_AcZ1)
map_Ac_1$Acacia<-if_else(map_Ac_1$Acacia ==1,"présence","absence")
map_Ac_1$Acacia<-as.factor(map_Ac_1$Acacia)
aca1<-ggDistribution(map_Ac_1,map_Ac_1$Acacia,z4) + ggtitle("Acacia nilotica")

ggarrange(fz1,balaz1,ano1,adan1,aca1,zone4,
          nrow = 2,
          ncol = 3)




