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
bio13_19<-ggarrange(b13,b14,b15,b16,b17,b18,b19)
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
    scale_fill_gradientn(name = "", colours = terrain.colors(100))  +
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
