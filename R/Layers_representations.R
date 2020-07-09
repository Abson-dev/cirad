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
bio1 <- raster(worldclim, layer=1)
plot(bio1)

##########
ggR_P<-function(rasterLayer){
samp <- raster::sampleRegular(rasterLayer, 5e+05, asRaster = TRUE)
map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                na.rm = TRUE)
colnames(map_df) <- c("Easting", "Northing", "MAP")
basePlot1 <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df, 
                                                      ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP"))
basePlot1<-basePlot1 + ggplot2::theme_bw() + ggplot2::labs(x = "Longitude", y = "Latitude") +
  ggtitle(label = names(rasterLayer)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 10)) + 
  ggplot2::scale_fill_gradientn(name = "Probabilité", colours = rev(terrain.colors(10)))
return(basePlot1)
}
ggR_P(bio1)
ggR_P(PHIHOX)
ggarrange(ggR_P(AETI),ggR_P(NBWP))
ggarrange(ggR_P(SINT),ggR_P(SOS))
# import presence-absence species data
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
worldclim.crop <- crop(worldclim,extent(Species))
bio1 <- raster(worldclim.crop, layer=1)
#writeRaster(bio1, filename=file.path(tmpdir, "bio1.tif"), format="GTiff", overwrite=TRUE)
##
bio10 <- raster(worldclim.crop, layer=2)
#writeRaster(bio10, filename=file.path(tmpdir, "bio10.tif"), format="GTiff", overwrite=TRUE)
bio11 <- raster(worldclim.crop, layer=3)
#writeRaster(bio11, filename=file.path(tmpdir, "bio11.tif"), format="GTiff", overwrite=TRUE)
bio12 <- raster(worldclim.crop, layer=4)
#writeRaster(bio12, filename=file.path(tmpdir, "bio12.tif"), format="GTiff", overwrite=TRUE)
bio13 <- raster(worldclim.crop, layer=5)
#writeRaster(bio13, filename=file.path(tmpdir, "bio13.tif"), format="GTiff", overwrite=TRUE)
bio14 <- raster(worldclim.crop, layer=6)
#writeRaster(bio14, filename=file.path(tmpdir, "bio14.tif"), format="GTiff", overwrite=TRUE)
bio15 <- raster(worldclim.crop, layer=7)
#writeRaster(bio15, filename=file.path(tmpdir, "bio15.tif"), format="GTiff", overwrite=TRUE)
bio16 <- raster(worldclim.crop, layer=8)
#writeRaster(bio16, filename=file.path(tmpdir, "bio16.tif"), format="GTiff", overwrite=TRUE)
bio17 <- raster(worldclim.crop, layer=9)
#writeRaster(bio17, filename=file.path(tmpdir, "bio17.tif"), format="GTiff", overwrite=TRUE)
bio18 <- raster(worldclim.crop, layer=10)
#writeRaster(bio18, filename=file.path(tmpdir, "bio18.tif"), format="GTiff", overwrite=TRUE)
bio19 <- raster(worldclim.crop, layer=11)
#writeRaster(bio19, filename=file.path(tmpdir, "bio19.tif"), format="GTiff", overwrite=TRUE)
bio2 <- raster(worldclim.crop, layer=12)
#writeRaster(bio2, filename=file.path(tmpdir, "bio2.tif"), format="GTiff", overwrite=TRUE)
bio3 <- raster(worldclim.crop, layer=13)
#writeRaster(bio3, filename=file.path(tmpdir, "bio3.tif"), format="GTiff", overwrite=TRUE)
bio4 <- raster(worldclim.crop, layer=14)
#writeRaster(bio4, filename=file.path(tmpdir, "bio4.tif"), format="GTiff", overwrite=TRUE)
bio5 <- raster(worldclim.crop, layer=15)
#writeRaster(bio5, filename=file.path(tmpdir, "bio5.tif"), format="GTiff", overwrite=TRUE)
bio6 <- raster(worldclim.crop, layer=16)
#writeRaster(bio6, filename=file.path(tmpdir, "bio6.tif"), format="GTiff", overwrite=TRUE)
bio7 <- raster(worldclim.crop, layer=17)
#writeRaster(bio7, filename=file.path(tmpdir, "bio7.tif"), format="GTiff", overwrite=TRUE)
bio8 <- raster(worldclim.crop, layer=18)
#writeRaster(bio8, filename=file.path(tmpdir, "bio8.tif"), format="GTiff", overwrite=TRUE)
bio9 <- raster(worldclim.crop, layer=19)
#writeRaster(bio9, filename=file.path(tmpdir, "bio9.tif"), format="GTiff", overwrite=TRUE)
 






b1<-ggR_Soil(bio1)
b2<-ggR_Soil(bio2)
b3<-ggR_Soil(bio3)
b4<-ggR_Soil(bio4)
b5<-ggR_Soil(bio5)
b6<-ggR_Soil(bio6)
b7<-ggR_Soil(bio7)
b8<-ggR_Soil(bio8)
b9<-ggR_Soil(bio9)
b10<-ggR_Soil(bio10)
b11<-ggR_Soil(bio11)
b12<-ggR_Soil(bio12)
b13<-ggR_Soil(bio13)
b14<-ggR_Soil(bio14)
b15<-ggR_Soil(bio15)
b16<-ggR_Soil(bio16)
b17<-ggR_Soil(bio17)
b18<-ggR_Soil(bio18)
b19<-ggR_Soil(bio19)

bio1_6<-ggarrange(b1,b2,b3,b4,b5,b6)
ggexport(bio1_6,filename ="bio1_6.png",width = 948, height = 480)
bio7_12<-ggarrange(b7,b8,b9,b10,b11,b12)
ggexport(bio7_12,filename ="bio7_12.png",width = 948, height = 480)
bio13_18<-ggarrange(b13,b14,b15,b16,b17,b18)
ggexport(bio13_19,filename ="bio13_19.png",width = 948, height = 480)
########################## Variables sur le Sol
rm(list = ls()) #Effacement de toutes les données en mémoire
graphics.off() #Effacement de tous les graphiques en mémoire
# import raster data
lsoil<-list.files("D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\",patt="\\.tif")
lsoil<-sprintf("D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\%s",lsoil)
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
class(lsoil)
#######
ggR_Soil<-function(RasterLayer){
  ggR(RasterLayer,geom_raster = TRUE,ggLayer = F) +
    scale_fill_gradientn(name = "", colours = rev(terrain.colors(100)))  +
    theme_bw() + xlab("Longitude") + ylab("Latitude") +
    ggtitle(label = names(RasterLayer)) + theme(plot.title = element_text(hjust = 0.5, size = 10))
  
}


##########
CLYPPT<-raster(lsoil[1])
CLYPPT.crop<-crop(CLYPPT,extent(Species))
names(CLYPPT.crop)<-"CLYPPT"
CLYPPT<-ggR_Soil(CLYPPT.crop)
CLYPPT<-CLYPPT + xlab("")
#ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Azote.png",g)
ORCDRC<-raster(lsoil[2])
ORCDRC.crop<-crop(ORCDRC,extent(Species))
names(ORCDRC.crop)<-"ORCDRC"
ORCDRC<-ggR_Soil(ORCDRC.crop)
ORCDRC<- ORCDRC + xlab("") + ylab("")
library(ggpubr)
#ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Carbon.png",g)
PHIHOX<-raster(lsoil[3])
PHIHOX.crop<-crop(PHIHOX,extent(Species))
names(PHIHOX.crop)<-"PHIHOX"
PHIHOX<-ggR_Soil(PHIHOX.crop)

#ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Phosphore.png",g)
SLTPPT<-raster(lsoil[4])
SLTPPT.crop<-crop(SLTPPT,extent(Species))
names(SLTPPT.crop)<-"SLTPPT"
SLTPPT<-ggR_Soil(SLTPPT.crop)
SLTPPT<-SLTPPT + ylab("")
PHIHOX_SLTPPT<-ggarrange(PHIHOX, SLTPPT,
                         nrow = 1,
                         ncol = 3,
                         legend,
                         widths=c(3,3,0.3))

graphe1<-ggarrange(CLYPPT,ORCDRC, PHIHOX,SLTPPT)
ggarrange(CLYPPT,ORCDRC, PHIHOX,SLTPPT,
          widths=c(3,3,3,0.3))
ggexport(graphe1,filename ="graphe1.png",width = 948, height = 480)
#ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Sable.png",g)
SNDPPT<-raster(lsoil[5])
SNDPPT.crop<-crop(SNDPPT,extent(Species))
names(SNDPPT.crop)<-"SNDPPT"
SNDPPT<-ggR_Soil(SNDPPT.crop)
#ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Argile.png",g)
NTO<-raster(lsoil[6])
NTO.crop<-crop(NTO,extent(Species))
names(NTO.crop)<-"NTO"
NTO<-ggR_Soil(NTO.crop)
#ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Limon.png",g)
P<-raster(lsoil[7])
P.crop<-crop(P,extent(Species))
names(P.crop)<-"P"
P<-ggR_Soil(P.crop)
NTO<-NTO + ylab("")
P<- P + ylab("")
# graphe2<-ggarrange(SNDPPT,NTO,P,
#                    ncol = 3,
#                    nrow = 1)
# ggexport(graphe2,filename ="graphe2.png",width = 948, height = 480)
ggarrange(CLYPPT,ORCDRC, PHIHOX,SLTPPT,SNDPPT,NTO,P)

############################Phénologie
# import raster data
lpheno<-list.files("D:\\Stage_SDM\\SDM\\Data\\Phenologie\\",patt="\\.tif")
lpheno<-sprintf("D:\\Stage_SDM\\SDM\\Data\\Phenologie\\%s",lpheno)
lpheno
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)

SINT<-raster(lpheno[11])
SOS<-raster(lpheno[22])
SINT.crop<-crop(SINT,extent(Species))
SOS.crop<-crop(SOS,extent(Species))
names(SINT.crop)<-"SINT"
names(SOS.crop)<-"SOS"
S<-ggR_Soil(SINT.crop)
SO<-ggR_Soil(SOS.crop)
pheno<-ggarrange(S,SO)
ggexport(pheno,filename ="pheno.png",width = 948, height = 480)
##################???wapor
# import raster data
#AETI
laeti<-list.files("D:\\Stage_SDM\\SDM\\Data\\AETI\\",patt="\\.tif")
laeti<-sprintf("D:\\Stage_SDM\\SDM\\Data\\AETI\\%s",laeti)
AETI_2009_2019<-stack(laeti)
AETI_2009_2019_mean<-calc(AETI_2009_2019,mean)
tmpdir<-"D:\\Stage_SDM\\SDM\\Data\\AETI\\"
writeRaster(AETI_2009_2019_mean, filename=file.path(tmpdir, "AETI_2009_2019_mean.tif"), format="GTiff", overwrite=TRUE)
tmpdir<-"D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\"
writeRaster(AETI_2009_2019_mean, filename=file.path(tmpdir, "AETI_2009_2019_mean.tif"), format="GTiff", overwrite=TRUE)
AETI_2009_2019.crop<-crop(AETI_2009_2019_mean,extent(Species))
names(AETI_2009_2019.crop)<-"AETI"
AETI<-ggR_Soil(AETI_2009_2019.crop)
#NETBIOMASS
lbiomas<-list.files("D:\\Stage_SDM\\SDM\\Data\\nET BIOMAS\\",patt="\\.tif")
lbiomas<-sprintf("D:\\Stage_SDM\\SDM\\Data\\nET BIOMAS\\%s",lbiomas)
#NETBIOMAS_2009_2019<-stack(lbiomas)
#plot(NETBIOMAS_2009_2019)
#NETBIOMAS_MOY<-calc(NETBIOMAS_2009_2019,mean)
#tmpdir<-"D:\\Stage_SDM\\SDM\\Data\\nET BIOMAS\\"
#writeRaster(NETBIOMAS_MOY, filename=file.path(tmpdir, "L1_NBWP_2009_2019_mean.tif"), format="GTiff", overwrite=TRUE)
NETBIOMAS_MOY<-raster(lbiomas[12])
NBWP.crop<-crop(NETBIOMAS_MOY,extent(Species))
names(NBWP.crop)<-"NBWP"
NB<-ggR_Soil(NBWP.crop)
wapor<-ggarrange(NB,AETI)
ggexport(wapor,filename ="wapor.png",width = 948, height = 480)


##########
names(SoilGrid.crop)
bio19<-ggR_Soil(bio19)
AETI<-ggR_Soil(AETI)
SINT<-ggR_Soil(SINT)
SOS<-ggR_Soil(SOS)
NBWP<-ggR_Soil(NBWP)
CLYPPT<-ggR_Soil(CLYPPT)
ggarrange(bio19,AETI,SINT,SOS,NBWP,CLYPPT)
ORCDRC<-ggR_Soil(ORCDRC)
PHIHOX<-ggR_Soil(PHIHOX)
NTO<-ggR_Soil(NTO)
SNDPPT<-ggR_Soil(SNDPPT)
P<-ggR_Soil(P)
Slo1<-ggR_Soil(Slo1)
ggarrange(ORCDRC,PHIHOX,NTO,SNDPPT,P,Slo1)
Len1<-ggR_Soil(Len1)
Sll<-ggR_Soil(Sll)
Csl<-ggR_Soil(Csl)
Wid1<-ggR_Soil(Wid1)
Dep1<-ggR_Soil(Dep1)
Elev<-ggR_Soil(Elev)
ggarrange(Len1,Sll,Csl,Wid1,Dep1,Elev)

#################################
reshapePA<-function(Rasterstack,espece){
  Pred<-list()
  data<-as.data.frame(Rasterstack)
  PA<-reshape(data, direction = "long", varying=1:ncol(data),v.names = c("Pred"),times =1:ncol(data) )
  PA$Species<-rep(espece,each=nrow(data))
  PA<-PA[,c("Pred","Species")]
  BaseStat<-as.data.frame(cprop(table(PA)))
  BaseStat<-BaseStat %>%
    filter(Pred!="Total") %>%
    filter(Species!="Ensemble")
  
  BaseStat$Freq<-round(BaseStat$Freq,2) 
  Pred[["Predict table"]]<-BaseStat
  plot<-ggbarplot(BaseStat,
                  x = "Species", y = "Freq",
                  fill = "Pred",
                  color = "Pred",
                  legend="top",label=TRUE,lab.pos = "in",ggtheme = theme_bw()) 
  
  Pred[["Plot"]]<-plot
  return(Pred)
}

#### presence absence function
PASpecies<-function(rasterLayer){
  samp <- raster::sampleRegular(rasterLayer, 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  basePlot <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df, 
                                                       ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP"))
  # ptr <- basePlot + ggplot2::geom_sf(data = speciesData, 
  #                                    alpha = 0.7, color = "blue", size = 1) +
  #   ggplot2::theme_bw() + ggplot2::labs(x = "", y = "") 
  # # +
  #   ggtitle(label = names(rasterLayer)) + theme(plot.title = element_text(hjust = 0.5, size = 10))
  # plot(cowplot::plot_grid(ptr))
  basePlot<-basePlot + ggplot2::theme_bw() + ggplot2::labs(x = "Longitude", y = "Latitude") + ggtitle(label = names(rasterLayer)) + theme(plot.title = element_text(hjust = 0.5, size = 10))+scale_fill_manual(values=c("red","green"),name="Espèce",labels=c("Absence","Présence"))
  return(basePlot)
  
}
PASpecies2<-function( rasterLayer1,rasterLayer2){
  #speciesData <- sf::st_as_sf(speciesData)
  samp <- raster::sampleRegular(rasterLayer1, 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  basePlot1 <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df, 
                                                        ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP"))
  basePlot1<-basePlot1 + ggplot2::theme_bw() + ggplot2::labs(x = "Longitude", y = "Latitude") + ggtitle(label = "(a)") + theme(plot.title = element_text(hjust = 0.5, size = 10))+scale_fill_manual(values=c("white","green"),name="EspÃ¨ce",labels=c("Absence","PrÃ©sence"))
  samp <- raster::sampleRegular(rasterLayer2, 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  basePlot2 <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df, 
                                                        ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP"))
  basePlot2<-basePlot2 + ggplot2::theme_bw() + ggplot2::labs(x = "Longitude", y = "Latitude") + ggtitle(label = "(b)") + theme(plot.title = element_text(hjust = 0.5, size = 10))+scale_fill_manual(values=c("white","green"),name="EspÃ¨ce",labels=c("Absence","PrÃ©sence"))
  ggpubr::ggarrange(basePlot1,basePlot2,common.legend = TRUE)
  
  
}


lsoil<-list.files("C:\\Users\\Hp\\OneDrive\\cirad\\Sorties\\",patt="\\.tif")
lsoil<-sprintf("C:\\Users\\Hp\\OneDrive\\cirad\\Sorties\\%s",lsoil)
# espece=c("Cordyla pinnata","Azadirachta indica")
# c("Faidherbia albida"="F. albida","Balanites aegyptiaca"="B. aegyptiaca",
#   "Anogeissus leiocarpus"="A. leiocarpus","Adansonia digitata"="A. digitata","Acacia nilotica"="A. nilotica"))


Acaciavar<-raster(lsoil[3])
names(Acaciavar)<-"Acacia nilotica"
Adansoniavar<-raster(lsoil[7])
names(Adansoniavar)<-"Adansonia digitata"
Anogeissusvar<-raster(lsoil[11])
names(Anogeissusvar)<-"Anogeissus leiocarpus"
Azadirachtavar<-raster(lsoil[15])
names(Azadirachtavar)<-"Azadirachta indica"
Balanitesvar<-raster(lsoil[19])
names(Balanitesvar)<-"Balanites aegyptiaca"
Cordylavar<-raster(lsoil[23])
names(Cordylavar)<-"Cordyla pinnata"
Faidherbiavar<-raster(lsoil[27])
names(Faidherbiavar)<-"Faidherbia albida"
Ficusvar<-raster(lsoil[32])
names(Ficusvar)<-"Ficus capensis"
Prosopisvar<-raster(lsoil[36])
names(Prosopisvar)<-"Prosopis africana"
ggarrange(ggR_P(Faidherbiavar),ggR_P(Acaciavar),
          ggR_P(Balanitesvar),ggR_P(Anogeissusvar),ggR_P(Adansoniavar),
          common.legend = TRUE)

ggarrange(ggR_P(Cordylavar),ggR_P(Azadirachtavar),ggR_P(Ficusvar),
          ggR_P(Prosopisvar),
          common.legend = TRUE)

Acaciavar<-raster(lsoil[4])
names(Acaciavar)<-"Acacia nilotica"
Adansoniavar<-raster(lsoil[8])
names(Adansoniavar)<-"Adansonia digitata"
Anogeissusvar<-raster(lsoil[12])
names(Anogeissusvar)<-"Anogeissus leiocarpus"
Azadirachtavar<-raster(lsoil[16])
names(Azadirachtavar)<-"Azadirachta indica"
Balanitesvar<-raster(lsoil[20])
names(Balanitesvar)<-"Balanites aegyptiaca"
Cordylavar<-raster(lsoil[24])
names(Cordylavar)<-"Cordyla pinnata"
Faidherbiavar<-raster(lsoil[29])
names(Faidherbiavar)<-"Faidherbia albida"
Ficusvar<-raster(lsoil[33])
names(Ficusvar)<-"Ficus capensis"
Prosopisvar<-raster(lsoil[37])
names(Prosopisvar)<-"Prosopis africana"

ggR_Soil<-function(RasterLayer){
  p<-ggR(RasterLayer,geom_raster = TRUE,ggLayer = F) +
    scale_fill_gradientn(name = "", colours = rev(terrain.colors(100)))  +
    theme_bw() + xlab("Longitude") + ylab("Latitude") +
    ggtitle(label = names(RasterLayer)) + theme(plot.title = element_text(hjust = 0.5, size = 10),legend.position = "none")
  
  return(p)
}
ggR_Soil(Faidherbiavar)
plot(Faidherbiavar)
ggarrange(ggR_Soil(Faidherbiavar),ggR_Soil(Acaciavar),
          ggR_Soil(Balanitesvar),ggR_Soil(Anogeissusvar),ggR_Soil(Adansoniavar)
          )
ggarrange(ggR_Soil(Cordylavar),ggR_Soil(Azadirachtavar),ggR_Soil(Ficusvar),
          ggR_Soil(Prosopisvar))
